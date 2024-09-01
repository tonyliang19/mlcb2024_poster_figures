# This should be all the relevant figs

# Important variables
RAW_GZ=raw/results.tar.gz
DATA_DIR=data
SRC_DIR=src
OUTPUT_DIR=figures

# Scripts
FIG1_SRC=${SRC_DIR}/fig1_comp_time.R
#FIG2_SRC=${SRC_DIR}/fig2.R
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


all: $(FIG1_OUTPUT)

.PHONY: clean
clean:
	@echo "Cleaning files in ${OUTPUT_DIR} ..."
	@rm -f ${OUTPUT_DIR}/*
#untar: $(RAW_GZ)
#	@tar -xf $(RAW_GZ) \
#	--transform="s/.*\///" \
#	--directory ${DATA_DIR}

#$(FIG_REAL_OUT) $(FIG_SIM_OUT): ${FIG1_SRC} ${FIG1_CSV}
$(FIG1_OUTPUT): ${FIG1_SRC} ${FIG1_CSV}
	@echo -e "Plotting figures of performance evaluation ... \n"
	Rscript $(FIG1_SRC) \
		--csv $(FIG1_CSV) \
		--real_out $(FIG_REAL_OUT) \
		--sim_out $(FIG_SIM_OUT)
#fig2: fig_comp_time.png

#fig3: fig_feat_weights.png
