# Copyright 2012 Erlware, LLC. All Rights Reserved.
#
# This file is provided to you under the Apache License,
# Version 2.0 (the "License"); you may not use this file
# except in compliance with the License.  You may obtain
# a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.
#

ERL = $(shell which erl)
RELX = $(shell which relx)
REBAR = $(shell which rebar)

PA = $(CURDIR)/src

ifeq ($(ERL),)
$(error "Erlang not available on this system")
endif

ifeq ($(REBAR),)
$(error "Rebar not available on this system")
endif

DEPS_PLT = $(CURDIR)/plt/deps_plt
DEPS = erts kernel stdlib

ERLFLAGS = -pa $(CURDIR)/ebin
CONFIG_FLAGS = -config dbgtracer.config
DEV_FLAGS = -s reloader +pc unicode

ifdef CEPHEID_DB_PER_BRANCH
BRANCH = $(shell git rev-parse --abbrev-ref HEAD)
ifneq ($(BRANCH),master)
CONFIG_FLAGS += -cepheid_model db_name "<<\"cepheid_development_$(subst .,_,$(subst -,_,$(BRANCH)))\">>"
endif
endif

RUN_CMD = $(ERL) $(ERLFLAGS) $(CONFIG_FLAGS) -boot start_sasl

NODE_NAME = dbgtracer@localhost
BACKEND_NODE_NAME = backend@localhost
RECEIVER_NODE_NAME = receiver@localhost
ENTITIES_NODE_NAME = entities@localhost

# Processor starts on the main node for Mnesia to work with both configurations
PROCESSOR_START_FLAGS = -sname $(NODE_NAME) -s cepheid_processor_app
BACKEND_START_FLAGS = -sname $(BACKEND_NODE_NAME) -s cepheid_backend_app
RECEIVER_START_FLAGS = -sname $(RECEIVER_NODE_NAME) -s cepheid_receiver_app
ENTITIES_START_FLAGS = -sname $(ENTITIES_NODE_NAME) -s entities_api_app

MONOLITH_START_FLAGS = -sname $(NODE_NAME) -s monolith_app

DBG_START_FLAGS = -sname $(NODE_NAME) -s dbgtracer_app

.PHONY: all compile doc clean test test-all dialyzer typer shell distclean pdf \
  update-deps rebuild

all: deps compile

# =============================================================================
# Rules to build the system
# =============================================================================

deps: rebar.config
	$(REBAR) get-deps
	$(REBAR) compile
	touch deps

update-deps:
	$(REBAR) update-deps
	$(REBAR) compile
	touch deps

compile: deps
	$(REBAR) skip_deps=true compile

doc:
	$(REBAR) skip_deps=true doc

data:
	mkdir -p $(CURDIR)/data

eunit: compile
	$(REBAR) skip_deps=true eunit -r

ct: compile
	$(REBAR) skip_deps=true ct -r

test: eunit

test-all: eunit ct

$(DEPS_PLT):
	@echo Building local plt at $(DEPS_PLT)
	@echo
	dialyzer --output_plt $(DEPS_PLT) --build_plt --apps $(DEPS) -r deps

dialyzer: $(DEPS_PLT)
	dialyzer --fullpath --plt $(DEPS_PLT) -Wrace_conditions -r ./ebin

typer:
	typer --plt $(DEPS_PLT) -r ./src -I ./include

pdf:
	pandoc README.md -o README.pdf

clean:
	- rm -rf $(CURDIR)/tmp/upload/*
	- rm -rf $(CURDIR)/log/*
	- rm -rf $(CURDIR)/apps/*/ct/*.beam
	- rm -rf $(CURDIR)/apps/*/ct-logs/*
	$(REBAR) skip_deps=true clean

distclean: clean
	- rm -rf $(DEPS_PLT)
	- rm -rvf $(CURDIR)/deps

rebuild: distclean deps compile escript dialyzer test

shell: compile
	$(RUN_CMD) -sname $(NODE_NAME) $(DEV_FLAGS)

schema: compile data
	$(RUN_CMD) -sname $(NODE_NAME) -noshell -eval 'kms_db:init_schema(), init:stop()'

run: compile
	$(RUN_CMD) $(MONOLITH_START_FLAGS) $(DEV_FLAGS)

run-processor: compile
	$(RUN_CMD) $(PROCESSOR_START_FLAGS) $(DEV_FLAGS)

run-backend: compile
	$(RUN_CMD) $(BACKEND_START_FLAGS) $(DEV_FLAGS)

run-receiver: compile
	$(RUN_CMD) $(RECEIVER_START_FLAGS) $(DEV_FLAGS)

run-entities: compile
	$(RUN_CMD) $(ENTITIES_START_FLAGS) $(DEV_FLAGS)

run-prod: compile
	$(APP_CMD) $(MONOLITH_START_FLAGS) -noinput +Bd

remsh:
	$(ERL) -remsh $(NODE_NAME) -sname remsh@localhost -setcookie $(shell cat .erlang.cookie)

dbg:
	$(ERL) $(ERLFLAGS) $(DBG_START_FLAGS) $(CONFIG_FLAGS)  
        #-setcookie $(shell cat .erlang.cookie)

release: all
	rm -rf $(CURDIR)/rel
	relx -o rel

dump_test_db:
	mysqldump -u root --no-data --add-drop-database --databases cepheid_test > $(CURDIR)/test/cepheid_test.sql

load_test_db:
	mysql -u root < $(CURDIR)/test/cepheid_test.sql
