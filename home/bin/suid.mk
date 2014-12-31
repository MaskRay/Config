P := ip route ifconfig wpa_gui ss brctl
export CFLAGS :=

define create
.PHONY: $1
$1: suid.cc
	$(LINK.c) $$^ -o $$@
	sudo chown root: $$@
	sudo chmod u+s $$@
endef

all: $P

$(foreach i,$P,$(eval $(call create,$i)))
