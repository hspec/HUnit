# -----------------------------------------------------------------------------

TOP = ../../..
include $(TOP)/mk/boilerplate.mk

# -----------------------------------------------------------------------------

EXAMPLES    := $(filter-out HUnitTestBase.lhs,$(wildcard *.lhs))
BINS        := $(addsuffix $(exeext),$(EXAMPLES:.lhs=))
CLEAN_FILES += $(BINS)

HC           = $(GHC_INPLACE)
MKDEPENDHS   = $(GHC_INPLACE)
SRC_HC_OPTS += -Wall -package HUnit

all:: $(BINS)

USES_HUNITTESTBASE := $(EXAMPLES:.lhs=)

.PRECIOUS: HUnitTestBase.o
$(addsuffix .o,$(USES_HUNITTESTBASE)): HUnitTestBase.hi
$(addsuffix $(exeext),$(USES_HUNITTESTBASE)): HUnitTestBase.o

$(BINS): %$(exeext): %.o
	$(HC) -o $@ $(HC_OPTS) $(LD_OPTS) $< $(patsubst %,HUnitTestBase.o,$(filter $(<:.o=),$(USES_HUNITTESTBASE)))

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk
