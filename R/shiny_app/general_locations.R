# final location data

# clean_yolo_ace_dets.R
d <- readRDS("data_clean/YoloAce/yoloace_dfa_detects.rds")
# clean_ybus_data.R
ybus2 <- readRDS("data_clean/YBUS/ybus_clean.rds")
# clean_JSATS_dets.R
ans3 <- readRDS("data_clean/JSATS/jsats_detects2013-2017.rds")
# clean_CMVemco_dets.R
v2 <- readRDS("data_clean/CMVemco/v2.rds")

length(unique(d$GEN)) #35
length(unique(ybus2$GEN)) #30
length(unique(ans3$GEN)) # 100
length(unique(v2$GEN)) #119

vemco <- c(unique(v2$GEN), unique(d$GEN), unique(ybus2$GEN))
length(unique(vemco))

DateTime_PST

v2$year <- format(as.Date(v2$DateTime_PST, format="%d/%m/%Y"),"%Y")
d$year <- format(as.Date(d$DateTime_PST, format="%d/%m/%Y"),"%Y")
ybus2$year <- format(as.Date(ybus2$DateTime_PST, format="%d/%m/%Y"),"%Y")
ans3$year <- format(as.Date(ans3$DateTime_PST, format="%d/%m/%Y"),"%Y")

location <- rbind(unique(v2[,c(11,13)]), unique(ans3[,c(4,10)]), unique(ybus2[,c(3,5)]), unique(d[,c(11,13)]))
location$tag_type <- c(rep("Vemco", length = 373), rep("JSATS", length = 272), rep("Vemco", length = 77))
location$study <-  c(rep("CM", length = 373), rep("JSATS", length = 272), rep("YBUS", length = 30), rep("MJ", length = 47))

# 1320 in shiny app currently (needs to be 722)
