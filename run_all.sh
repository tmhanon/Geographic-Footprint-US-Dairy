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

echo "Creating Directories"

mkdir -p "MANUSCRIPT TABLES/Table 1"
mkdir -p "MANUSCRIPT TABLES/Table 2"
mkdir -p "MANUSCRIPT TABLES/Table 3"
mkdir -p "MANUSCRIPT TABLES/Table 4"
mkdir -p "MANUSCRIPT TABLES/Table 5"
mkdir -p "MANUSCRIPT TABLES/Table 6"
mkdir -p "MANUSCRIPT TABLES/Table 7"
mkdir -p "MANUSCRIPT TABLES/Table D.1"
mkdir -p "MANUSCRIPT TABLES/Table D.2"
mkdir -p "MANUSCRIPT TABLES/Table D.3"
mkdir -p "MANUSCRIPT TABLES/Table D.4"
mkdir -p "MANUSCRIPT TABLES/Table E.1"

mkdir -p "MANUSCRIPT CLAIMS/Section 1"
mkdir -p "MANUSCRIPT CLAIMS/Section 5.6"
mkdir -p "MANUSCRIPT CLAIMS/Section 5.7"
mkdir -p "MANUSCRIPT CLAIMS/Section 5.8"
mkdir -p "MANUSCRIPT CLAIMS/Section 6.1"
mkdir -p "MANUSCRIPT CLAIMS/Section 6.3"
mkdir -p "MANUSCRIPT CLAIMS/Section 6.4"
mkdir -p "MANUSCRIPT CLAIMS/Section 6.5"
mkdir -p "MANUSCRIPT CLAIMS/Section 6.6"
mkdir -p "MANUSCRIPT CLAIMS/Appendix F.3"
mkdir -p "MANUSCRIPT CLAIMS/Appendix G"

mkdir -p "MANUSCRIPT FIGURES/Figure 1"
mkdir -p "MANUSCRIPT FIGURES/Figure A.1"
mkdir -p "MANUSCRIPT FIGURES/Figure G.1"
mkdir -p "MANUSCRIPT FIGURES/Figure G.2"
mkdir -p "MANUSCRIPT FIGURES/Figure G.3"
mkdir -p "MANUSCRIPT FIGURES/Figure G.4"
mkdir -p "MANUSCRIPT FIGURES/Figure G.5"


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


#-----------------------------
# MANUSCRIPT TABLES
#-----------------------------

echo "=== Starting Formatting Tables ==="

echo "Processing Manuscript Tables"
Rscript "R CODE/Tables/TablesQE.R"



#-------------------------------------------
# FIGURE A.1 (MAPS)
#-------------------------------------------

echo "=== Starting Creating Figures ==="

echo "Processing Maps"
Rscript "R CODE/Figures/MapsFMMOQE.R"


#--------------------------------------------------------
# MATHEMATICA FIGURES AND APPENDIX G SIMULATION CLAIMS
#--------------------------------------------------------

echo "=== Processing Mathematica Figures and Simulation Claims ==="

cd "MATHEMATICA CODE"
wolframscript -file FMMOQE.wls
cd ..


#----------------------------------------
# MANUSCRIPT CLAIMS
#----------------------------------------

echo "=== Starting Substantiating Claims and Standalone Text Numbers ==="

echo "Processing Manuscript Text Numbers and Claims"
Rscript "R CODE/Claims/TextClaimsQE.R"


echo "=== Replication Completed Successfully ==="
