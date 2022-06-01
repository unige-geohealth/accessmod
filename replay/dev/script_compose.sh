#!/bin/bash 

ANALYZE_CONF="/data/dbgrass/demo/demo/accessmodConfigs/lAnalysisParameters__ref_opt_par_unlimited.json"
ANALYZE_OUT="/tmp/out"

SCRIPT=$(cat <<EOF
source("global.R")
amAnalysisReplayExec(
  replayConf = "$ANALYZE_CONF",
  exportDirectory = "$ANALYZE_OUT"
)
EOF
)

#echo -e $SCRIPT

#docker compose exec am5_dev Rscript -e $SCRIPT 
docker compose exec am5_dev Rscript -e "$SCRIPT"
