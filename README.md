# acoustic-telemetry-synthesis
acoustic telemetry synthesis for describing juvenile Chinook diversity 

## Distance Per Day matrices needed:

JSATS - 2013, 2014, 2015, 2016, 2017
Yolo/Army corps - 2012, 2013, 2016 
Cyril's Vemco data - 2007, 2008, 2009, 2010, 2011 (Water Years - October 1 to September 30)

recipe: script, input(s), then output (single)


# Datasets:

## JSATS

  data_clean/jsats_dfa_detects.rds: R/clean_all_detects.R R/utils.R R/clean_tagging_metadata.R data/tagging_data/Tagging_Fish_Metadata.txt
                                                          
                                                          
                                                                  
  data_clean/jsats_dfa_detects_fishpaths.rds | needs: R/clean_all_detects.R                                                     


## Yolo/Ace


## CMVemco

data_clean/CMVemco/CM_vemco_distance_matrix_DCC-Yolo-Tisdale_closed_clean.rds: R/cleaning_scripts/CMVemco/clean_CMVemco_dm.R data/distance_matrices/Vemco_dist_matrix_DCC-Yolo-Tisdale_closed.txt

data_clean/CMVemco/exclude_loc.clean.csv: R/cleaning_scripts/CMVemco/make_exclude_loc_csv.R

data_clean/CMVemco/v2.rds: data/CMVemco/Query3.csv, data_clean/CMVemco/exclude_loc_clean.csv, data/CMVemco/travel.time_CM.Vemco_v3.csv

results/CJVemco/*.csv 
    
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