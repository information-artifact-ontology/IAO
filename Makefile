# config
MAKEFLAGS += --warn-undefined-variables
SHELL := bash
.SHELLFLAGS := -eu -o pipefail -c
.DEFAULT_GOAL := all
.DELETE_ON_ERROR:
.SUFFIXES:
.SECONDARY:

# Run `make all` to create a new IAO release:
# - download the latest build of ROBOT
# - create a merged RO core.owl with annotations
# - merge iao-main.owl with imports to create iao-merged.owl
# - reason over iao-merged.owl to create iao.owl
# - create a new release directory
# - copy ontology files to release directory and update IRIs
# - copy catalog to release directory and update IRIs
# - clean build files

# If you wish to keep build/robot.jar, run `make release` instead.

# ===============================
#           VARIABLES
# ===============================

OBO = http://purl.obolibrary.org/obo
ROBOT = java -jar build/robot.jar

# directories
SRC = src/ontology
TARGET = releases/$(DATE)

# release vars
TS = $(shell date +'%m:%d:%Y %H:%M')
DATE = $(shell date +'%Y-%m-%d')
V = $(OBO)/iao/$(DATE)
LICENSE = http://creativecommons.org/licenses/by/4.0/

# dependencies & targets
RO = $(SRC)/ro
CAT = catalog-v001.xml

# ===============================
#           MAIN TASKS
# ===============================

# run `make all` or `make release` to make a new release
# `make all` will remove the build dir with ROBOT on completion
all: clean

# various directories
build $(RO) $(TARGET):
	@mkdir $@

# ===============================
#             ROBOT
# ===============================

# download the most recent build of ROBOT
build/robot.jar: | build
	@echo "Getting ROBOT" && \
	curl -Ls https://github.com/ontodev/robot/releases/download/v1.2.0/robot.jar > $@

clean: | release
	@echo "Removing build files" && \
	rm -rf build

# ===============================
#            RO TASKS
# ===============================

.PHONY: $(RO)/bfo-axioms.owl
$(RO)/bfo-axioms.owl: $(RO)
	@echo "Downloading $@" && \
	curl -Ls $(OBO)/ro/bfo-axioms.owl > $@ && \
	$(ROBOT) annotate --input $@ --ontology-iri $(OBO)/iao/$@\
	 --version-iri $(OBO)/iao/$(DATE)/ro/bfo-axioms.owl --output $@

.PHONY: $(RO)/bfo-classes-minimal.owl
$(RO)/bfo-classes-minimal.owl: $(RO)
	@echo "Downloading $@" && \
	curl -Ls $(OBO)/ro/bfo-classes-minimal.owl > $@ && \
	$(ROBOT) annotate --input $@ --ontology-iri $(OBO)/iao/$@\
	 --version-iri $(OBO)/iao/$(DATE)/ro/bfo-classes-minimal.owl --output $@

RO_CORE = $(RO)/core.owl
.PHONY: $(RO_CORE)
$(RO_CORE): $(RO)/bfo-axioms.owl $(RO)/bfo-classes-minimal.owl
	@echo "Downloading $@" && \
	curl -Ls $(OBO)/ro/core.owl > $@ && \
	$(ROBOT) annotate --input $@ --ontology-iri $(OBO)/iao/$@\
	 --version-iri $(OBO)/iao/$(DATE)/ro/core.owl --output $@

BFO = build/bfo.owl
$(BFO): | build
	@echo "Downloading BFO to $@" && \
	curl -Ls $(OBO)/bfo.owl > $@

# ===============================
#           IAO TASKS
# ===============================

release: move
	@echo "A new release is available in $(TARGET)"

# regenerate fresh import-OBO file
.PHONY: src/ontology/import-OBO.owl
$(SRC)/import-OBO.owl:
	@echo "Generating $@" && \
	cd src/ontology/ontofox && sh get-ontofox-imports

# merge components to generate iao-merged
$(SRC)/iao-merged.owl: $(SRC)/iao-main.owl $(SRC)/import-OBO.owl $(RO_CORE) | build/robot.jar
	@echo "Merging $< to $@" && \
	$(ROBOT) merge --input $< --collapse-import-closure true \
	annotate --ontology-iri $(OBO)/iao/iao-merged.owl\
	 -k dcterms:license $(LICENSE)\
	 --version-iri $(V)/iao-merged.owl --output $@

# reason over iao-merged to generate IAO
$(SRC)/iao.owl: $(SRC)/iao-merged.owl | build/robot.jar
	@echo "Reasoning $< to $@" && \
	$(ROBOT) reason --input $< --reasoner jfact\
	 --create-new-ontology false --annotate-inferred-axioms false \
	annotate --ontology-iri $(OBO)/iao.owl\
	 --version-iri $(V)/iao.owl --output $@

# components to be copied to release dir
IAO_COMPS = $(TARGET)/externalByHand.owl \
 $(TARGET)/import-OBO.owl \
 $(TARGET)/obsolete.owl \
 $(TARGET)/ontology-metadata.owl

# copy the catalog, updating the version IRIs
$(TARGET)/$(CAT): $(SRC)/$(CAT) $(RO_CORE) | $(TARGET)
	@echo "Copying catalog to $(TARGET)" && \
	sed -e 's#/iao/dev/#/iao/$(DATE)/#g' $< | \
	sed -e 's#ro/#iao/$(DATE)/ro/#g' > $@

# copy IAO main, updating the version IRIs
$(TARGET)/iao-main.owl: $(SRC)/iao-main.owl $(BFO) | $(TARGET)/$(CAT)
	$(eval BFO_V := $(shell grep "owl:versionIRI rdf:resource=\"[^\"]*" $(BFO) |\
	 sed 's/<owl:versionIRI rdf:resource="//g' |\
	 sed 's/"\/>//g' | awk '{$$1=$$1};1'))
	@echo "Copying $< to $@" && \
	sed -e 's#/obo/iao/dev#/obo/iao/$(DATE)#g' $< | \
	sed -e 's#ro/core#iao/$(DATE)/ro/core#g' | \
	sed -e 's#$(OBO)/bfo.owl#$(BFO_V)#g' > $@

# move the components to the release dir
# also update the version IRIs
$(IAO_COMPS): $(TARGET) | $(TARGET)/iao-main.owl build/robot.jar
	@echo "Copying $(SRC)/$(notdir $@) to $<" && \
	$(ROBOT) annotate --input $(SRC)/$(notdir $@)\
	 --version-iri $(V)/$(notdir $@) --output $@

# move the generated release files to the release dir
move: $(SRC)/iao-merged.owl $(SRC)/iao.owl $(RO) | $(IAO_COMPS)
	@echo "Copying $^ to $(TARGET)" && \
	cp $< $(TARGET)/iao-merged.owl && \
	cp $(word 2,$^) $(TARGET)/iao.owl && \
	cp -R $(word 3,$^) $(TARGET)/ro
