# acoustic-telemetry-synthesis
acoustic telemetry synthesis for describing juvenile Chinook diversity 

## Distance Per Day matrices needed:

JSATS - 2013, 2014, 2015, 2016, 2017
Yolo/Army corps/YBUS - 2012, 2013, 2016 
Cyril's Vemco data - 2007, 2008, 2009, 2010, 2011 (Water Years - October 1 to September 30)

recipe: script, input(s), then output (single)


# Datasets and Recipes:

## JSATS

### detections
data_clean/JSATS/jsats_detects2013-2017.rds: R/cleaning_scripts/clean_JSATS_dets R/utils.R data/tagging_data/Tagging_Fish_Metadata.txt R/utils.R

### dpd matrices:   
results/JSATS/*.csv: R/results_scripts/JSATS_dpd.R R/utils.R data_clean/JSATS/JSATs_dist_matrix_DCC-Yolo-Tisdale_closed_new.csv data_clean/JSATS/jsats_detects2013-2017.rds
                                           

## Yolo/Ace

### detections
data_clean/YoloAce/yoloace_dfa_detects.rds: R/utils.R R/cleaning_scripts/YoloAce/clean_yoloace_dets.R data/distance_matrices/Distance_Matrix_MJ_corr_mean.csv data_clean/YoloAce/ace2012-2013_dets.rds data_clean/YoloAce/yolo2012-2013_dets.rds

### dpd matrices

results/YoloAce/*.csv: R/results_scripts/YoloAce_dpd.R R/utils.R  data/distance_matrices/Distance_Matrix_MJ_corr_mean.csv data_clean/YoloAce/yoloace_dfa_detects.rds

## CMVemco

data_clean/CMVemco/CM_vemco_distance_matrix_DCC-Yolo-Tisdale_closed_clean.rds: R/cleaning_scripts/CMVemco/clean_CMVemco_dm.R data/distance_matrices/Vemco_dist_matrix_DCC-Yolo-Tisdale_closed.txt

data_clean/CMVemco/exclude_loc.clean.csv: R/cleaning_scripts/CMVemco/make_exclude_loc_csv.R

data_clean/CMVemco/v2.rds: data/CMVemco/Query3.csv, data_clean/CMVemco/exclude_loc_clean.csv, data/CMVemco/travel.time_CM.Vemco_v3.csv

### dpd matrices:
results/CJVemco/*.csv : R/results_scripts/CMVemco_dpd.R R/utils.R data_clean/CMVemco/v2.rds, data_clean/CMVemco/CM_vemco_distance_matrix_DCC-Yolo-Tisdale_closed_clean.rds

## YBUS

### detections


    
# Package Dependencies:

```
## CRAN

CRAN.packages <- c("lubridate", 
                   
                   "ggplot2", 
                   "dplyr", 
                  
                   "devtools")


new.packages <- CRAN.packages[!(CRAN.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages)

## GITHUB

github.packages <- c("fishpals", "tagtales")

  github.package_sites <- c("fishsciences/fishpals", "Myfanwy/tagtales")

  new.gh.pkgs <- github.packages[!(github.packages %in% installed.packages()[, "Package"])]

  if(length(new.gh.pkgs)) devtools::install_github(github.package_sites)

```