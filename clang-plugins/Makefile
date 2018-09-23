CC=g++
INCLUDES=-I.
CFLAGS=-c -Werror -Wall -std=c++0x -g `llvm-config --cxxflags` -O1
LDFLAGS=-g -shared
TARGETDIR=.
OBJDIR=$(TARGETDIR)/obj

EXE=$(TARGETDIR)/libplugins.so

SRCS    := $(wildcard *.cpp)
OBJS    := $(patsubst %.cpp,$(OBJDIR)/%.o,$(SRCS))
DEPS    := $(OBJS:.o=.d)

all: $(EXE) $(EXE2)

clean:
	rm $(OBJDIR)/*.o
	rm $(OBJDIR)/*.d
	rm $(EXE)

$(EXE): $(OBJS) | $(TARGETDIR)
	$(CC) $(OBJS) $(LDFLAGS) -o $@

-include $(DEPS)

$(OBJDIR)/%.o: %.cpp | $(OBJDIR)
	$(CC) $(INCLUDES) $(CFLAGS) -MMD -o $@ $<

$(OBJDIR):
	mkdir -p $(OBJDIR)

$(TARGETDIR):
	mkdir -p $(TARGETDIR)

