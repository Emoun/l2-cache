SCALA_MAIN_CLASS = caches.hardware.pipelined.SharedPipelinedCacheDe2115Top
GENERATED_DIR = generated
QUARTUS_PROJECT_DIR = quartus/SharedPipelinedCacheDe2115Top
VERILOG_FILE = SharedPipelinedCacheDe2115Top.v

SYNTH_TEST_SCALA_MAIN_CLASS = caches.hardware.pipelined.SharedPipelinedCacheSynthTop
SYNTH_TEST_QUARTUS_PROJECT_DIR = quartus/SharedPipelinedCacheSynthTop
SYNTH_TEST_QUARTUS_PROJECT_NAME = SharedPipelinedCacheSynthTop
SYNTH_TEST_VERILOG_FILE = SharedPipelinedCacheSynthTop.v

# Default target
buildhw: generate-verilog copy-to-quartus clean-generated

# Run the sbt tests and test if quartus can synthesize the test module
test: test-sbt test-synth

test-sbt:
	sbt test

# Test if quartus can succesfully synthesis the test module
test-synth: generate-test-synth-verilog synth-test-copy-to-quartus clean-generated synth-test

# Synthesize the test level module
synth-test:
	quartus_map $(SYNTH_TEST_QUARTUS_PROJECT_DIR)/$(SYNTH_TEST_QUARTUS_PROJECT_NAME)
	cat $(SYNTH_TEST_QUARTUS_PROJECT_DIR)/output_files/$(SYNTH_TEST_QUARTUS_PROJECT_NAME).map.summary
	quartus_fit $(SYNTH_TEST_QUARTUS_PROJECT_DIR)/$(SYNTH_TEST_QUARTUS_PROJECT_NAME)
	quartus_asm $(SYNTH_TEST_QUARTUS_PROJECT_DIR)/$(SYNTH_TEST_QUARTUS_PROJECT_NAME)
	quartus_sta $(SYNTH_TEST_QUARTUS_PROJECT_DIR)/$(SYNTH_TEST_QUARTUS_PROJECT_NAME)

# Generate Verilog for main top level module
generate-verilog:
	@echo "Generating Verilog from $(SCALA_MAIN_CLASS)..."
	@mkdir -p $(GENERATED_DIR)
	sbt "runMain $(SCALA_MAIN_CLASS)"

# Generate Verilog for synthesis test module
generate-test-synth-verilog:
	@echo "Generating Verilog from $(SYNTH_TEST_SCALA_MAIN_CLASS)..."
	@mkdir -p $(GENERATED_DIR)
	sbt "runMain $(SYNTH_TEST_SCALA_MAIN_CLASS)"

# Copy generated Verilog main top level module to Quartus project
copy-to-quartus: generate-verilog
	@echo "Copying $(VERILOG_FILE) to Quartus project..."
	@if [ -f "$(GENERATED_DIR)/$(VERILOG_FILE)" ]; then \
		cp "$(GENERATED_DIR)/$(VERILOG_FILE)" "$(QUARTUS_PROJECT_DIR)/$(VERILOG_FILE)"; \
		echo "Successfully copied $(VERILOG_FILE) to $(QUARTUS_PROJECT_DIR)"; \
	else \
		echo "Error: $(GENERATED_DIR)/$(VERILOG_FILE) not found!"; \
		exit 1; \
	fi

synth-test-copy-to-quartus: generate-test-synth-verilog
	@echo "Copying $(SYNTH_TEST_VERILOG_FILE) to Quartus project..."
	@if [ -f "$(GENERATED_DIR)/$(SYNTH_TEST_VERILOG_FILE)" ]; then \
		cp "$(GENERATED_DIR)/$(SYNTH_TEST_VERILOG_FILE)" "$(SYNTH_TEST_QUARTUS_PROJECT_DIR)/$(SYNTH_TEST_VERILOG_FILE)"; \
		echo "Successfully copied $(SYNTH_TEST_VERILOG_FILE) to $(SYNTH_TEST_QUARTUS_PROJECT_DIR)"; \
	else \
		echo "Error: $(GENERATED_DIR)/$(SYNTH_TEST_VERILOG_FILE) not found!"; \
		exit 1; \
	fi

# Clean generated files
clean-generated:
	@echo "Cleaning generated files..."
	rm -rf $(GENERATED_DIR)
	rm -rf target

# Clean Quartus project files
clean-quartus:
	@echo "Cleaning Quartus project files..."
	rm -rf $(QUARTUS_PROJECT_DIR)/db
	rm -rf $(QUARTUS_PROJECT_DIR)/incremental_db
	rm -rf $(QUARTUS_PROJECT_DIR)/output_files
	rm -f $(QUARTUS_PROJECT_DIR)/*.rpt
	rm -f $(QUARTUS_PROJECT_DIR)/*.summary

# Check if generated Verilog exists
check-verilog:
	@if [ -f "$(GENERATED_DIR)/$(VERILOG_FILE)" ]; then \
		echo "$(VERILOG_FILE) exists in $(GENERATED_DIR)"; \
		ls -la "$(GENERATED_DIR)/$(VERILOG_FILE)"; \
	else \
		echo "$(VERILOG_FILE) not found in $(GENERATED_DIR)"; \
	fi

# Show help
help:
	@echo "Available targets:"
	@echo "  buildhw                        - Generate Verilog for main top level module and copy to Quartus project: $(QUARTUS_PROJECT_DIR)"
	@echo "  test                           - Run sbt test and test synthesis"
	@echo "  test-sbt                       - Run sbt test"
	@echo "  test-synth                     - Generate Verilog for test module and synthesize with Quartus"
	@echo "  synth-test                     - Synthesisze test module in Quartus project: $(SYNTH_TEST_QUARTUS_PROJECT_DIR)"
	@echo "  generate-verilog               - Generate Verilog from Scala source"
	@echo "  generate-test-synth-verilog    - Generate Verilog for test module"
	@echo "  copy-to-quartus                - Copy generated Verilog to Quartus project"
	@echo "  synth-test-copy-to-quartus     - Copy generated test module Verilog to Quartus project"
	@echo "  check-verilog                  - Check if generated Verilog file exists"
	@echo "  clean                          - Clean generated files and build artifacts"
	@echo "  clean-quartus                  - Clean Quartus project files (use with caution)"
	@echo "  help                           - Show this help message"

# Phony targets
.PHONY: generate-verilog copy-to-quartus clean clean-quartus check-verilog help
