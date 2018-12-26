## Copyright (C) 2018 Jeremiah Orians
## This file is part of scheme_interpreter
##
## scheme_interpreter is free software: you an redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## scheme_interpreter is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with scheme_interpreter. If not, see <http://www.gnu.org/licenses/>.

# Don't rebuild the built things in bin
VPATH = bin
CC=gcc
CFLAGS=-D_GNU_SOURCE -O0 -std=c99 -ggdb -Wall -Werror -Wno-unused-variable
RUN_SCRIPT=./util/run.sh

all: sch3

sch3: src/sch3.c
	$(CC) $(CFLAGS) src/sch3.c -o bin/sch3

# Clean up after ourselves
.PHONY: clean
clean:
	rm -f bin/sch3

DESTDIR:=
PREFIX:=/usr/local
bindir:=$(DESTDIR)$(PREFIX)/bin
.PHONY: install
install: sch3
	mkdir -p $(bindir)
	cp $^ $(bindir)

.PHONY: test
test: sch3
	./tests/tests t/trivial "$(RUN_SCRIPT) {}"
	./tests/tests t/simple "$(RUN_SCRIPT) {}"
	./tests/tests t/mal "$(RUN_SCRIPT) {}"
	./tests/tests t/rosetta "$(RUN_SCRIPT) {}"
	./tests/tests t/scheme-tests "$(RUN_SCRIPT) {}"

analyze:
	echo '##' PERFORMING SCAN BUILD
	make clean
	scan-build make
	
#	echo '##' PERFORMING CPPCHECK
#	cppcheck --enable=all ./src/sch3.c
	
	echo '##' RUNNING WITH VALGRIND
	make clean
	make
	make test RUN_SCRIPT=./util/run-valgrind.sh
	
	echo '##' BUILDING WITH TCC
	make clean
	make CC=tcc
	make test

	echo '##' BUILDING WITH SANITIZE ADDRESS, MEMORY, UNDEFINED
	make clean
	make CC=clang CFLAGS='-fsanitize=address'
	make test
	make clean
	make CC=clang CFLAGS='-fsanitize=memory'
	make test
	make clean
	make CC=clang CFLAGS='-fsanitize=undefined'
	make test
