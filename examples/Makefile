# -----------------------------------------------------------------------------

TOP = ../..
include $(TOP)/mk/boilerplate.mk

# -----------------------------------------------------------------------------

ifeq "$(way)" ""
SUBDIRS = test

EXAMPLES    := $(wildcard *.hs)
BINS        := $(addsuffix $(exeext),$(EXAMPLES:.hs=))
CLEAN_FILES += $(BINS)

HC           = $(GHC_INPLACE)
MKDEPENDHS   = $(GHC_INPLACE)
SRC_HC_OPTS += -Wall -package HUnit

all:: $(BINS)

$(BINS): %$(exeext): %.o
	$(HC) -o $@ $(HC_OPTS) $(LD_OPTS) $<
endif

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk
