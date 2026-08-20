#!/usr/bin/env bash
# ==================================
# MASTER REPLICATION SCRIPT
# ==================================

cd "$(dirname "$0")"

if ! command -v gams &> /dev/null; then
export PATH="/Library/Frameworks/GAMS.framework/Versions/54/Resources:$PATH"
fi

#Exit immediately if any individual script fails
set -e

echo "=== Creating GAMS Data Files ==="

#----------------------------------------------------
# R CODE TO CREATE CSV FILES FOR GAMS FROM RAW DATA
#----------------------------------------------------

Rscript "R CODE/Data-Processing/0_Data_Processing_Run_All.R"


echo "=== Starting Analysis ==="

#---------------------------
# GAMS RUNS
#---------------------------

echo "Processing GAMS runs"
cd "GAMS CODE"

echo "Running Section 1 Claim"
gams FMMO_Model_QE.gms --run_section1=1 lo=0

echo "Running Table 1"
gams FMMO_Model_QE.gms --run_table1=1 lo=0

echo "Running Tables D.1-D.3"
gams FMMO_Model_QE.gms --run_tablesd1_d3=1 lo=0

echo "Running Table D.4"
gams FMMO_Model_QE.gms --run_tabled4=1 lo=0

echo "Running Table 2"
gams FMMO_Model_QE.gms --run_table2=1 lo=0

echo "Running Main Counterfactual"
gams FMMO_Model_QE.gms --run_maincounterfactual=1 lo=0

echo "Running Table E.1"
gams FMMO_Model_QE.gms --run_sensitivity=1 lo=0

echo "Running Ideal Wedges Counterfactual"
gams FMMO_Model_QE.gms --run_wedgecounterfactual=1 lo=0

echo "Running Quota Counterfactual"
gams FMMO_Model_QE.gms --run_quotacounterfactual=1 lo=0

echo "Running Milk Trade Cost Counterfactual"
gams FMMO_Model_QE.gms --run_milktradecost=1 lo=0

echo "Running Maximum Land Rent Counterfactual"
gams FMMO_Model_QE.gms --run_landrentcounterfactual=1 lo=0

echo "Running Policy Comparison With Elastic Foreign Demand"
gams FMMO_Model_QE.gms --run_epsilonzero100=1 lo=0

echo "Running Quota Counterfactual With Full Arbitrage"
gams FMMO_Model_QE.gms --run_quotacounterfactual2=1 lo=0

echo "Running Aggregate Component Demand Elasticities"
gams FMMO_Model_QE.gms --run_aggregatecompdemand=1 lo=0

cd ..

echo "=== Starting Formatting Tables ==="

#-----------------------------
# MANUSCRIPT TABLES
#-----------------------------

echo "Processing Manuscript Tables"
cd "R CODE/Tables"
Rscript TablesQE.R
cd ..

echo "=== Starting Creating Figures ==="

#-------------------------------------------
# FIGURE A.1 (MAPS)
#-------------------------------------------

echo "Processing Maps"
cd "Figures"
Rscript MapsFMMOQE.R
cd ..

cd ..

#--------------------------------------------------------
# MATHEMATICA FIGURES AND APPENDIX G SIMULATION CLAIMS
#--------------------------------------------------------

echo "Processing Mathematica Figures and Simulation Claims"
cd "MATHEMATICA CODE"
wolframscript -file FMMOQE.wls
cd ..

echo "=== Starting Substantiating Claims and Standalone Text Numbers ==="

#----------------------------------------
# MANUSCRIPT CLAIMS
#----------------------------------------

echo "Processing Manuscript Text Numbers and Claims"
cd "R CODE/Claims"
Rscript TextClaimsQE.R

cd ..

