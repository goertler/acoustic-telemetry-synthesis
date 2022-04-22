# setup - Analysis steps

if(!dir.exists("data_clean")) {
   dir.create("data_clean")
   dir.create("data_clean/YoloAce")
   dir.create("data_clean/YBUS")
   dir.create("data_clean/JSATS")
   dir.create("data_clean/CMVemco")}
  
  if(!dir.exists("results")) {
    dir.create("results")
    dir.create("results/YoloAce")
    dir.create("results/YBUS")
    dir.create("results/JSATS")
    dir.create("results/CJVemco")}
  
  

# CM Vemco:
# ---------------------
source("R/cleaning_scripts/CMVemco/make_exclude_loc_csv.R")
source("R/cleaning_scripts/CMVemco/clean_CMVemco_dets.R")
source("R/cleaning_scripts/CMVemco/clean_CMVemco_dm.R")

source("R/results_scripts/make_dpd_csvs/CMVemco_dpd.R")

# JSATS
# --------------------
source("R/cleaning_scripts/JSATS/clean_JSATS_dets.R")
source("R/results_scripts/make_dpd_csvs/JSATS_dpd.R")


# YoloAce
# -------------------
source("R/cleaning_scripts/YoloAce/clean_yoloace_dets.R")
source("R/results_scripts/make_dpd_csvs/YoloAce_dpd.R")


# YBUS
# -------------------
source("R/cleaning_scripts/YBUS/clean_ybus_data.R")
source("R/results_scripts/make_dpd_csvs/YBUS_dpd.R")




