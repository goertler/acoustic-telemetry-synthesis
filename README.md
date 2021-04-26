# acoustic-telemetry-synthesis
acoustic telemetry synthesis for describing juvenile Chinook diversity 

# File Dependencies:

R/utils.R : none
R/clean_all_detects.R: 
    - R/clean_tagging_metadata:
        data/tagging_data/Tagging_Fish_Metadata.txt
        
    - data/detection_data/all_JSATS.csv
    - data/tagging_data/dat4Von.csv
    
# Package dependencies:

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