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
	curl -Ls https://build.berkeleybop.org/job/robot/lastSuccessfulBuild/artifact/bin/robot.jar> $@

clean: | release
	@echo "Removing build files" && \
	rm -rf build

# ===============================
#            RO TASKS
# ===============================

$(RO)/bfo-axioms.owl: $(RO)
	@echo "Downloading $@" && \
	curl -Ls $(OBO)/ro/bfo-axioms.owl > $@

$(RO)/bfo-classes-minimal.owl: $(RO)
	@echo "Downloading $@" && \
	curl -Ls $(OBO)/ro/bfo-classes-minimal.owl > $@

RO_CORE = $(RO)/core.owl
$(RO_CORE): $(RO)/bfo-axioms.owl $(RO)/bfo-classes-minimal.owl
	@echo "Downloading $@" && \
	curl -Ls $(OBO)/ro/core.owl > $@

# ===============================
#           IAO TASKS
# ===============================

release: move
	@echo "A new release is available in $(TARGET)"

# merge components to generate iao-merged
$(SRC)/iao-merged.owl: $(SRC)/iao-main.owl | $(RO_CORE) build/robot.jar
	@echo "Merging $< to $@" && \
	$(ROBOT) merge --input $< --collapse-import-closure true \
	annotate --ontology-iri $(OBO)/iao/iao-merged.owl --version-iri $(V)/iao-merged.owl --output $@

# reason over iao-merged to generate IAO
$(SRC)/iao.owl: $(SRC)/iao-merged.owl | build/robot.jar
	@echo "Reasoning $< to $@" && \
	$(ROBOT) reason --input $< --create-new-ontology false\
	 --annotate-inferred-axioms false \
	annotate --ontology-iri $(OBO)/iao.owl --version-iri $(V)/iao.owl --output $@

# components to be copied to release dir
IAO_COMPS = $(TARGET)/externalByHand.owl \
 $(TARGET)/import-OBO.owl \
 $(TARGET)/obsolete.owl \
 $(TARGET)/ontology-metadata.owl

# copy the catalog, updating the version IRIs
$(TARGET)/$(CAT): $(SRC)/$(CAT) $(RO_CORE) | $(TARGET)
	@echo "Copying catalog to $(TARGET)" && \
	sed -e 's#/iao/dev/#/iao/$(DATE)/#g' $< > $@

# copy IAO main, updating the version IRIs
$(TARGET)/iao-main.owl: $(SRC)/iao-main.owl | $(TARGET)/$(CAT)
	@echo "Copying $< to $@" && \
	sed -e 's#/iao/dev/#/iao/$(DATE)/#g' $< > $@

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
