SRCS := Inter_mod.F90
OBJS := $(SRCS:%.F90=%.o)

all: my_tests

libsut.a: $(OBJS)
	$(AR) -r $@ $?

ifeq (nagfor,$(findstring nagfor,$(FC)))
  FFLAGS += -fpp
endif

%.o : %.F90
	$(FC) -c $(FFLAGS) $<


LATEST_PFUNIT_DIR := $(lastword $(shell echo $(wildcard $(PFUNIT_DIR)/PFUNIT-4.*) | xargs -n1 | sort -V))
include $(LATEST_PFUNIT_DIR)/include/PFUNIT.mk

FFLAGS += $(PFUNIT_EXTRA_FFLAGS)

test_Inter_mod.o: Inter_mod.mod
Inter_mod.mod: Inter_mod.o

my_tests: libsut.a

my_tests_TESTS := test_Inter_mod.pf
my_tests_REGISTRY :=
my_tests_OTHER_SOURCES :=
my_tests_OTHER_LIBRARIES := -L. -lsut -lgomp
my_tests_OTHER_INCS :=

$(eval $(call make_pfunit_test,my_tests))


clean:
	$(RM) *.o *.mod *.a *.inc test_Inter_mod.F90
