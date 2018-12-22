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
test:
	./tests/tests t/trivial './util/run.sh {}'
	./tests/tests t/simple './util/run.sh {}'
	./tests/tests t/mal './util/run.sh {}'
	./tests/tests t/rosetta './util/run.sh {}'
