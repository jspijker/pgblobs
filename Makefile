subdirs := inst/unitTests

.PHONY: all $(subdirs)

all: $(subdirs)

$(subdirs):
	    $(MAKE) -C $@
