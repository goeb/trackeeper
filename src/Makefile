
SRCS = $(shell ls *.erl)
OBJS = $(SRCS:%.erl=../ebin/%.beam)

all: $(OBJS)

../ebin/%.beam: %.erl
	erlc +debug_info $^
	mv *.beam ../ebin

print:
	@echo $(SRCS)
	@echo $(OBJS)
