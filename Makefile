# config
MAKEFLAGS += --warn-undefined-variables
SHELL := bash
.SHELLFLAGS := -eu -o pipefail -c
.DEFAULT_GOAL := all
.DELETE_ON_ERROR:
.SUFFIXES:

# Run `make all` to create a new IAO release:
# - download the latest build of ROBOT
# - merge iao-edit.owl with imports to create iao-merged.owl
# - reason over iao-merged.owl to create iao.owl
# - clean build files

# If you wish to keep build/robot.jar, run `make release` instead.

# ===============================
#           VARIABLES
# ===============================

SHELL   := /bin/bash
OBO	:= http://purl.obolibrary.org/obo
DEV	:= $(OBO)/iao/dev
ROBOT	:= java -jar build/robot.jar

# release vars
TODAY	:= $(shell date +%Y-%m-%d)
TS	:= $(shell date +'%d:%m:%Y %H:%M')

# directories
SRC = src/ontology


# ===============================
#             MAIN TASK
# ===============================

# run `make all` or `make release` to make a new release
# `make all` will remove the build dir with ROBOT on completion
all: clean

### Directories
#
# This is a temporary place to put things.
build:
	mkdir -p $@

# ===============================
#             ROBOT
# ===============================

# download the most recent build of ROBOT
build/robot.jar: | build
	@echo "Getting ROBOT" && \
	curl -L -o $@ https://github.com/ontodev/robot/releases/download/v1.6.0/robot.jar


clean: | release
	@echo "Removing build files" && \
	rm -rf build

# ===============================
#           IAO TASKS
# ===============================

# Update import file, commented on this release
# regenerate fresh import-OBO file
# .PHONY: src/ontology/import-OBO.owl
# $(SRC)/import-OBO.owl:
# 	@echo "Generating $@" && \
# 	cd src/ontology/ontofox && \
# 	curl -s -F file=@OntoFox-input.txt http://ontofox.hegroup.org/service.php > ../import-OBO.owl

### Imports
#
# Use Ontofox to import various modules.
build/import_%.owl: src/ontology/ontoFox/%_input.txt | build/robot.jar build
	curl -s -F file=@$< -o $@ https://ontofox.hegroup.org/service.php

# Use ROBOT to remove external java axioms
src/ontology/imports/import_%.owl: build/import_%.owl
	$(ROBOT) remove --input build/import_$*.owl \
	--base-iri 'http://purl.obolibrary.org/obo/$*_' \
	--axioms external \
	--preserve-structure false \
	--trim false \
	--output $@ 

IMPORT_FILES := $(wildcard src/ontology/imports/import_*.owl)

.PHONY: imports
imports: $(IMPORT_FILES)


# merge components to generate iao-merged
build/iao-merged.owl: $(SRC)/iao-edit.owl | build/robot.jar build
	@echo "Merging $< to $@" && \
	$(ROBOT) merge \
	--input $< \
	annotate \
	--ontology-iri "$(OBO)/iao/iao-merged.owl" \
	--version-iri "$(OBO)/iao/$(TODAY)/iao-merged.owl" \
	--annotation owl:versionInfo "$(TODAY)" \
	--output build/iao_merged.tmp.owl
	sed '/<owl:imports/d' build/iao_merged.tmp.owl > $@
	rm build/iao_merged.tmp.owl

# reason over iao-merged to generate IAO
iao.owl: build/iao-merged.owl
	@echo "Reasoning $< to $@" && \
	$(ROBOT) reason \
	--input $< \
	--reasoner HermiT \
	--exclude-tautologies all \
	annotate \
	--ontology-iri "$(OBO)/iao.owl" \
	--version-iri "$(OBO)/iao/$(TODAY)/iao.owl" \
	--annotation owl:versionInfo "$(TODAY)" \
	--output $@

release: build/iao-merged.owl iao.owl 
	@echo "A new release is made"


