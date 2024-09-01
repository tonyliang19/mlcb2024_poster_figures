# This should be all the relevant figs

# Important variables
RAW_GZ=raw/results.tar.gz
DATA_DIR=data
SRC_DIR=src
OUTPUT_DIR=figures
KEEP='.gitkeep'

# Scripts
FIG1_SRC=${SRC_DIR}/fig_performance_evaluation.R
FIG2_SRC=${SRC_DIR}/fig_computational_time.R
# Input data files
FIG3_CSV=${DATA_DIR}/all_feature_selection_results.csv
FIG1_CSV=${DATA_DIR}/metrics.csv
FIG2_TRACE=${DATA_DIR}/execution_trace.txt
FIG2_METADATA=${DATA_DIR}/parsed_metadata.csv

# Output files
# For performance evaluation
FIG_REAL_OUT=${OUTPUT_DIR}/fig_performance_evaluation.png
FIG_SIM_OUT=${OUTPUT_DIR}/fig_simulated_performance.png
FIG1_OUTPUT=$(FIG_REAL_OUT) $(FIG_SIM_OUT)
# For computational time
FIG_COMP_TIME=${OUTPUT_DIR}/fig_computational_time.png
FIG2_OUTPUT=$(FIG_COMP_TIME)
# Main target starts here
all: $(FIG1_OUTPUT) $(FIG2_OUTPUT)

.PHONY: clean
clean:
	@echo "Cleaning files in ${OUTPUT_DIR} ..."
	@find ${OUTPUT_DIR} ! -name ${KEEP} -type f -exec rm -f {} +
#untar: $(RAW_GZ)
#	@tar -xf $(RAW_GZ) \
#	--transform="s/.*\///" \
#	--directory ${DATA_DIR}


# FIGURE 1 Performance Evaluation
$(FIG1_OUTPUT): ${FIG1_SRC} ${FIG1_CSV}
	@echo -e "Plotting figures of performance evaluation ... \n"
	Rscript $(FIG1_SRC) \
		--csv $(FIG1_CSV) \
		--real_out $(FIG_REAL_OUT) \
		--sim_out $(FIG_SIM_OUT)
$(FIG2_OUTPUT): ${FIG2_SRC} ${FIG2_TRACE} ${FIG2_METADATA}
	echo -e "Plotting figure of computational time\n"
	Rscript $(FIG2_SRC) \
		--metadata ${FIG2_METADATA} \
		--trace ${FIG2_TRACE} \
		--output ${FIG_COMP_TIME}
#fig3: fig_feat_weights.png
