setwd("C:/Users/pgoertler/OneDrive - deltacouncil/acoustic-telemetry-synthesis/all_rivers_for_network_analysis - Copy")

Name.Key <-read.csv("Origin_Locations_YBUS.csv")
Dist.YBUS <-read.csv("Distance_Matrix_YBUS.csv")
Name.Key <- Name.Key[,c(21,22)]

# add first name
colnames(Name.Key)[1]<-"Name1"
Dist.YBUS_v2 <- merge(Dist.YBUS, Name.Key, by="Name1")
head(Dist.YBUS_v2)

# add second name
colnames(Name.Key)<-c("Name2","Name_Og2")
Dist.YBUS_v3 <- merge(Dist.YBUS_v2, Name.Key, by="Name2")
head(Dist.YBUS_v3)

write.csv(Dist.YBUS_v3, "Distance_Matrix_YBUS_corr.csv")

# MJ dta

Name.Key <-read.csv("Origin_MJ.csv")
Dist.MJ <-read.csv("Distance_Matrix_MJ.csv")
Name.Key <- Name.Key[,c(15:17)]

# add first name
colnames(Name.Key)[1]<-"Name1"
Dist.MJ_v2 <- merge(Dist.MJ, Name.Key, by="Name1")
head(Dist.MJ_v2)

# add second name
colnames(Name.Key)<-c("Name2","Name_Og2","Name_site_Og2")
Dist.MJ_v3 <- merge(Dist.MJ_v2, Name.Key, by="Name2")
head(Dist.MJ_v3)

write.csv(Dist.MJ_v3, "Distance_Matrix_MJ_corr.csv")

# means across general locations (Name_Og)


