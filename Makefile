GHC=ghc
SRCS=$(wildcard *.hs)
EXEC=$(patsubst %.hs,%.out,$(SRCS))
RUNCMD=true$(patsubst %.out,&&./%.out,$(EXEC))
comma=,
define defs
=return \"\"\n
endef
TEST_DEFS=$(subst $(comma),$(defs),$(TESTS))=return \"\"

all: $(EXEC)
	@$(RUNCMD)
.PHONY: all

$(EXEC):%.out:%.hs
	ghc --make $< -o $@

clean:
	$(RM) -rf *.hi *.o $(EXEC)
.PHONY: clean

new:template.hs.in
	@cp $< $(PID).hs
	@sed -i "s/#pid/$(PID)/" $(PID).hs
	@sed -i "s/#proto/$(PROTO)/" $(PID).hs
	@sed -i "s/#testlist/$(TESTS)/" $(PID).hs
	@sed -i "s/#testsuites/$(TEST_DEFS)/" $(PID).hs
.PHONY: new
