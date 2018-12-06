# config
MAKEFLAGS += --warn-undefined-variables
SHELL := bash
.SHELLFLAGS := -eu -o pipefail -c
.DEFAULT_GOAL := all
.DELETE_ON_ERROR:
.SUFFIXES:
.SECONDARY:

# Run `make all` to create a new IAO release:
# (1) download the latest build of ROBOT
# (2) create a new release directory
# (3) copy src/ontology files to release directory and update IRIs
# (4) copy catalog to release directory and update IRIs
# (5) create a merged RO core.owl with annotations
# (6) copy iao.owl to release directory and update import IRIs
# (7) merge imports and reason to create iao-merged.owl
# (8) clean build files

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

# dependencies & targets
CAT = catalog-v001.xml 
MERGED = $(TARGET)/iao-merged.owl
RO = $(TARGET)/ro

# ===============================
#           MAIN TASKS
# ===============================

# run `make all` or `make release` to make a new release
# `make all` will remove the build dir with ROBOT on completion
all: release clean
release: merged
	@echo "A new release is available in $(TARGET)"

# various directories
build $(RO) $(TARGET):
	@mkdir $@

# ===============================
#             ROBOT
# ===============================

build/robot.jar: | build
	@echo "Getting ROBOT" && \
	curl -Ls https://build.berkeleybop.org/job/robot/lastSuccessfulBuild/artifact/bin/robot.jar> $@

clean: merged
	@echo "Removing build files" && \
	rm -rf build

# ===============================
#            RO TASKS
# ===============================

RO_CORE = $(RO)/core.owl
# bfo axioms and bfo classes minimal are already included in core
# right now we just need to get the annotations
RO_COMPS = annotations.owl

ro: $(RO_CORE)

$(RO_COMPS): $(RO)
	@echo "Downloading RO $@" && \
	curl -Ls $(OBO)/ro/$@ > $(RO)/$@

$(RO_CORE): $(RO_COMPS) | build/robot.jar
	@echo "Generating $@" && \
	$(ROBOT) merge --input-iri $(OBO)/ro/core.owl\
	 --input $(RO)/annotations.owl --collapse-import-closure true \
	annotate --version-iri $(V)/ro/core.owl --output $@

# ===============================
#           IAO TASKS
# ===============================

IAO_COMPS = externalByHand.owl \
 iao-main.owl \
 import-OBO.owl \
 obsolete.owl \
 ontology-metadata.owl

# move the IAO components to the release dir and set the version IRI
$(IAO_COMPS): $(TARGET) | build/robot.jar
	@echo "Copying $@ to $<" && \
	$(ROBOT) annotate --input $(SRC)/$@\
	 --version-iri $(V)/$@ --output $(TARGET)/$@

# move the catalog file to the release dir
# make sure the IRIs are correct
# add the RO core mapping
catalog: $(TARGET)/$(CAT)
$(TARGET)/$(CAT): $(SRC)/$(CAT) ro | $(TARGET)
	@echo "Copying catalog to $(TARGET)" && \
	sed -e 's#/iao/dev/#/iao/$(DATE)/#g' $< | \
	sed -e '$$i\'$$'\n''<uri name="$(V)/ro/core.owl" uri="ro/core.owl"/>' > $@

# move the main file to the release dir
# make sure the IRIs are correct and set version IRI
$(TARGET)/iao.owl: $(SRC)/iao.owl | $(IAO_COMPS) catalog build/robot.jar
	@echo "Copying iao.owl to $(TARGET)" && \
	sed -e 's#/obo/iao/dev#/obo/iao/$(DATE)#g' $< | \
	sed -e 's#ro/core#iao/$(DATE)/ro/core#g' > $@ && \
	robot annotate --input $@ --version-iri $(V)/iao.owl

# merge all components then reason
# set ontology and version IRIs
merged: $(MERGED)
$(MERGED): $(TARGET)/iao.owl | build/robot.jar
	@echo "Merging release" && \
	robot merge --input $< --collapse-import-closure true \
	reason --create-new-ontology false --annotate-inferred-axioms false \
	annotate --ontology-iri $(OBO)/iao/iao-merged.owl\
	 --version-iri $(V)/iao-merged.owl --output $@
