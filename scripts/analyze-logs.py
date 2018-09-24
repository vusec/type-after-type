#!/usr/bin/python

import math
import os
import re
import sys

import scipy.stats

pathscripts = os.path.abspath(os.path.dirname(sys.argv[0]))
pathroot = os.path.join(pathscripts, "..")
pathspeccpu2006 = os.path.join(pathroot, "autosetup.dir", "targets", "src", "spec-cpu2006")
resultBenchmarks = set()
resultInstances = set()
resultTuples = dict()
instanceRef = None
verbosity = 2

def grep(path, regexstr):
	regex = re.compile(regexstr)
	matches = list()
	f = open(path, "r")
	for line in f:
		match = regex.match(line)
		if match: matches.append(match)
	f.close()
	return matches

def grepone(path, regexstr):
	matches = grep(path, regexstr)
	if len(matches) != 1: return None
	return matches[0].groups()[0];

def addtuple(tuples, key, tuple):
	if key not in tuples: tuples[key] = list()
	tuples[key].append(tuple)

def getInstanceName(instance):
	if instance.startswith("MetAlloc-"): instance = instance[9:]
	if instance.endswith(".cfg"): instance = instance[:-4]
	if instance.endswith("-run"): instance = instance[:-4]
	return instance

def processresult(instance, benchmark, outcome, tuple, nodename):
	resultBenchmarks.add(benchmark)
	resultInstances.add(instance)

	if outcome == "Success":
		addtuple(resultTuples, (instance, benchmark), tuple)
	else:
		sys.stderr.write("error outcode \"%s\" for benchmark %s on instance %s\n" % (outcome, benchmark, instance))

def processlog_speccpu2006(path, logpath, instance, nodename):
	matchesresult = grep(logpath, " ([^ ]+) ([^ ]+) base ref ratio=([0-9.]+), runtime=([0-9.]+).*")
	if len(matchesresult) < 1:
		sys.stderr.write("no results in SPEC log %s referenced in %s\n" % (logpath, path))
		return

	for match in matchesresult:
		outcome = match.groups()[0]
		benchmark = match.groups()[1]
		runtime = float(match.groups()[3])
		processresult(instance, benchmark, outcome, (runtime, None), nodename)

def parse_time_runtime(runtime):
	parts = runtime.split(":")
	if (len(parts) < 2) or (len(parts) > 3): return None
	runtimehour = 0
	if len(parts) > 2: runtimehour = int(parts[0])
	runtimemin = int(parts[len(parts) - 2])
	runtimesec = float(parts[len(parts) - 1])
	return (runtimehour * 60 + runtimemin) * 60 + runtimesec

def processlog_speccpu2006_time(path, instance, nodename, memuse):
	benchmark = grepone(path, "\\[autosetup-runscript\\] cmd=(.*)\n")
	matchesstatus = grep(path, "[ \t]*Exit status: ([-0-9]*)\n")
	matchesruntime = grep(path, "[ \t]*Elapsed \\(wall clock\\) time \\(h:mm:ss or m:ss\\): ([0-9:.]*)\n")
	if (benchmark is None) or (len(matchesstatus)) < 1 or (len(matchesruntime) < 1):
		sys.stderr.write("no results in SPEC time log %s\n" % (path))
		return

	if benchmark.find(" ") >= 0:
		sys.stderr.write("multiple benchmarks run SPEC time log %s, currently not supported\n" % (path))
		return

	outcome = "Success"
	for match in matchesstatus:
		status = int(match.groups()[0])
		if status != 0: outcome = "Error"

	runtimeSum = 0
	for match in matchesruntime:
		runtime = parse_time_runtime(match.groups()[0])
		if runtime is None:
			sys.stderr.write("bad runtime in run SPEC time log %s\n" % (path))
			return
		runtimeSum += runtime
	
	processresult(instance, benchmark, outcome, (runtimeSum, memuse), nodename)

def extractlogpaths_speccpu2006_fix(logpath, path):
	if os.path.isfile(logpath): return logpath
	altlogpath = os.path.join(pathspeccpu2006, "result", os.path.basename(logpath))
	if os.path.isfile(altlogpath): return altlogpath
	sys.stderr.write("log file %s referenced in %s does not exist\n" % (logpath, path))
	return None

def extractlogpaths_speccpu2006(path):
	matches = grep(path, "The log for this run is in (.*)\n")
	if len(matches) < 1: return None

	logpaths = list()
	for match in matches:
		logpath = extractlogpaths_speccpu2006_fix(match.groups()[0], path)
		if logpath is None: continue
		logpaths.append(logpath)

	return logpaths

def processfile_speccpu2006(path, instance, nodename, memuse):
	if not (memuse is None):
		processlog_speccpu2006_time(path, instance, nodename, memuse)
	
	logpaths = extractlogpaths_speccpu2006(path)
	if not logpaths:
		sys.stderr.write("no log path for %s\n" % (path))
		return

	for logpath in logpaths:
		processlog_speccpu2006(path, logpath, instance, nodename)

def processfile_memuse(path):
	memuse = None
	matches = grep(path, "[ \t]*Maximum resident set size \\(kbytes\\): (.*)\n")
	for match in matches:
		memuseline = float(match.groups()[0])
		if (memuse is None) or (memuse < memuseline):
			memuse = memuseline

	return memuse

def processfile(path):
	target = grepone(path, "\\[autosetup-runscript\\] target=(.*)\n")
	instance = grepone(path, "\\[autosetup-runscript\\] instancename=(.*)\n")
	nodename = grepone(path, "\\[autosetup-runscript\\] node=(.*)\n")
	if (target is None) or (instance is None) or (nodename is None):
		sys.stderr.write("missing or duplicate header(s) in log file %s\n" % (path))
		return

	dateend = grepone(path, "\\[autosetup-runscript\\] date-end=(.*)\n")
	if dateend is None:
		sys.stderr.write("log file %s is incomplete\n" % (path))
		return

	memuse = processfile_memuse(path)
	if target == "spec-cpu2006":
		processfile_speccpu2006(path, instance, nodename, memuse)
	else:
		sys.stderr.write("target %s not supported in log file %s\n" % (target, path))

def processdir(path):
	files = os.listdir(path)
	files.sort()
	for file in files:
		processpath(os.path.join(path, file))

def processpath(path):
	if os.path.isfile(path):
		processfile(path)
	elif os.path.isdir(path):
		processdir(path)
	else:
		sys.stderr.write("path %s not found\n" % (path))

def extractvalues(tuples, index):
	if tuples is None: return None
	values = list()
	for tuple in tuples:
		value = tuple[index]
		if not (value is None):
			values.append(value)
	values.sort()
	return values

def compute_mean(values, index, benchmark):
	if len(values) < 1: return None
	sum_ = 0
	for value in values:
		sum_ += value
	return sum_ / len(values)

def compute_median(values, index, benchmark):
	if len(values) < 1: return None
	index = len(values) // 2
	if index * 2 == len(values):
		return (values[index - 1] + values[index]) / 2
	else:
		return values[index]

def compute_stdev(values, index, benchmark):
	if len(values) < 2: return None
	sum_ = 0
	sum2 = 0
	for value in values:
		sum_ += value
		sum2 += value * value
	var = (sum2 - sum_ * sum_ / len(values)) / (len(values) - 1)
	if (var < 0) and (var > -1e-12): var = 0
	return math.sqrt(var)

def compute_n(values, index, benchmark):
	return len(values)

def getvaluesref(index, benchmark):
	if instanceRef is None: return None
	keyRef = (instanceRef, benchmark)
	tuplesRef = resultTuples.get(keyRef)
	return extractvalues(tuplesRef, index)

def compute_oh_mean(values, index, benchmark):
	valuesRef = getvaluesref(index, benchmark)
	if valuesRef is None: return None
	value = compute_mean(values, index, benchmark)
	if value is None: return None
	valueRef = compute_mean(valuesRef, index, benchmark)
        if (valueRef is None) or (valueRef == 0): return None
	return (value / valueRef - 1.0) * 100.0

def compute_oh_median(values, index, benchmark):
	valuesRef = getvaluesref(index, benchmark)
	if valuesRef is None: return None
	value = compute_median(values, index, benchmark)
	if value is None: return None
	valueRef = compute_median(valuesRef, index, benchmark)
        if (valueRef is None) or (valueRef == 0): return None
	return (value / valueRef - 1.0) * 100.0

def computeresult(instance, benchmark, measure):
	key = (instance, benchmark)
	tuples = resultTuples.get(key)
	if tuples is None: return None
	return measure[2](extractvalues(tuples, measure[3]), measure[3], benchmark)

def printresult(measure, value):
	if value is None:
		valuestr = ""
	else:
		valuestr = measure[1] % (value)
	sys.stdout.write("\t%s" % (valuestr))

def showresult_basic(instance, benchmark, measure):
	value = computeresult(instance, benchmark, measure)
	printresult(measure, value)

def computegeomean(benchmarks, instance, measure):
	if not measure[4]: return None
	sum = 0
	n = 0
	for benchmark in benchmarks:
		value = computeresult(instance, benchmark, measure)
		if value is None: return None
		sum += math.log(value / 100 + 1)
		n += 1
	if n < 1: return None
	return (math.exp(sum / n) - 1) * 100

def getvalidbenchmarks(instance, benchmarks, measure):
	benchmarksValid = list()
	for benchmark in benchmarks:
		value = computeresult(instance, benchmark, measure)
		if not (value is None):
			benchmarksValid.append(benchmark)
	return benchmarksValid

def showresult_geomean(benchmarks, instance, measure):
	value = computegeomean(benchmarks, instance, measure)
	printresult(measure, value)

def showresults_basic_measures_row(benchmark, instances, measures):
	sys.stdout.write("%s" % (benchmark))
	for measure in measures:
		for instance in instances:
			showresult_basic(instance, benchmark, measure)
	sys.stdout.write("\n")

def showresults_basic_measures_geomean_row(benchmarks, instances, measures, instanceGeomean):
	if instanceGeomean is None:
		sys.stdout.write("geomean")
	else:
		sys.stdout.write("geomean-%s" % (instanceGeomean))

	for measure in measures:
		if instanceGeomean is None:
			benchmarksGeomean = benchmarks
		else:
			benchmarksGeomean = getvalidbenchmarks(instanceGeomean, benchmarks, measure)
		for instance in instances:
			showresult_geomean(benchmarksGeomean, instance, measure)
	sys.stdout.write("\n")

def showresults_basic_measures(benchmarks, instances, measures):
	for measure in measures:
		for instance in instances:
			sys.stdout.write("\t%s" % (measure[0]))
	sys.stdout.write("\n")
	for measure in measures:
		for instance in instances:
			sys.stdout.write("\t%s" % (getInstanceName(instance)))
	sys.stdout.write("\n")
	for benchmark in benchmarks:
		showresults_basic_measures_row(benchmark, instances, measures)

	showresults_basic_measures_geomean_row(benchmarks, instances, measures, None)
	for instanceGeomean in instances:
		showresults_basic_measures_geomean_row(benchmarks, instances, measures, instanceGeomean)

def showresults_basic(benchmarks, instances):
	measures = [
		("rt-oh",  "%.1f", compute_oh_median, 0, True),
		("rt-med", "%.3f", compute_median,    0, False),
		("rt-avg", "%.3f", compute_mean,      0, False),
		("rt-std", "%.3f", compute_stdev,     0, False),
		("rt-n",   "%d",   compute_n,         0, False)]
	measuresmem = [
		("rss-oh",  "%.1f", compute_oh_median, 1, True),
		("rss-med", "%.0f", compute_median,    1, False),
		("rss-avg", "%.0f", compute_mean,      1, False),
		("rss-std", "%.0f", compute_stdev,     1, False),
		("rss-n",   "%d",   compute_n,         1, False)]

	showresults_basic_measures(benchmarks, instances, measures)
	sys.stdout.write("\n")
	showresults_basic_measures(benchmarks, instances, measuresmem)

def showresults_all(benchmarks, instances):
	for index in [0, 1]:
		if index == 0:
			pfx = "runtime"
			fmt = "\t%.3f"
		else:
			pfx = "rss-max"
			fmt = "\t%.0f"

		for benchmark in benchmarks:
			for instance in instances:
				sys.stdout.write("%s\t%s\t%s" % (pfx, benchmark, getInstanceName(instance)))
				key = (instance, benchmark)
				tuples = resultTuples.get(key)
				values = extractvalues(tuples, index)
				if values is not None:
					for value in values:
						sys.stdout.write(fmt % (value))
				sys.stdout.write("\n")

def selectInstanceRefMatch(instances, name):
	global instanceRef
	for instance in instances:
		if instance == name:
			instanceRef = instance
			return

def selectInstanceRef(instances):
	if instanceRef is None: selectInstanceRefMatch(instances, "baseline-lto")
	if instanceRef is None: selectInstanceRefMatch(instances, "baseline")
	if instanceRef is None: selectInstanceRefMatch(instances, "default-lto")
	if instanceRef is None: selectInstanceRefMatch(instances, "default")
	if instanceRef is None: selectInstanceRefMatch(instances, "safestack-lto")
	if instanceRef is None: selectInstanceRefMatch(instances, "safestack")
	if instanceRef is None:
		sys.stdout.write("no baseline, overheads not computed\n\n")
	else:
		sys.stdout.write("baseline for overheads: %s\n\n" % (instanceRef))

def showresults():
	benchmarks = list(resultBenchmarks)
	benchmarks.sort()
	instances = list(resultInstances)
	instances.sort()

	selectInstanceRef(instances)

	showresults_basic(benchmarks, instances)
	if verbosity >= 1:
		sys.stdout.write("\n")
		showresults_all(benchmarks, instances)

def selecttuples(tuples, runtimebelow):
	tuplesNew = list()
	for tuple in tuples:
		if tuple[0] < runtimebelow:
			tuplesNew.append(tuple)
	return tuplesNew

def dictadd(d, key, value):
	if key in d:
		d[key] = d[key] + value
	else:
		d[key] = value

for path in sys.argv[1:]:
	processpath(path)

showresults()

