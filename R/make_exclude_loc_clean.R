## Make location exclusions csv for CJVemco analysis
## based on csv emailed from Pascale on 2022-02-02
# M. Johnston
# Wed Feb 23 10:56:40 2022 ------------------------------

# original exclude_loc.csv is in data/exclude_loc.csv, saved as read-only.

## Made by taking the exclude_loc.csv that Pascale sent, changing "MRGC" and "Richmond Bridge" to be included rather than excluded, and then dput-ing it here:

loc <- structure(list(General_Location = c("SR_ChinaRapids", "SR_BlwDryCk", 
"SR_Abv_RBDD", "SR_BlwGCID", "SR_BlwIrvineFinch", "SR_AbvChicoCk", 
"BattleCk10", "SR_JellysFerry", "SR_BendBridge", "SR_OrdBend", 
"SR_BlwDeerCk", "Mok_GeorgianaS", "Mok_NorthFork", "SR_AbvThomes", 
"SR_BlwSutter", "SR_ButteCityBr", "SR_I-80/50Br", "SR_Freeport", 
"SR_Sutter_N", "Georg_SloughN", "SR_DCCNorth", "SR_BlwSteam", 
"SR_Ryde", "SR_Blw_RBDD", "Decker_IsS", "SR_SteamboatS", "SR_RioVista", 
"Chipps Island", "Golden Gate East Line", "Benicia Bridge", "SR_AbvColusaBr", 
"SR_Mouth", "Carquinez Bridge", "SPBay10_2007", "SR_SutterSteam", 
"SR_Steamboat", "SPBuoy9/10", "SR_SutterSlough", "SR_ElkSlough", 
"SR_MeridianBr", "SR_BlwMillCk", "Richmond Bridge", "Mar_Is", 
"SR_AbvFeather", "FranksTractE", "San Joaquin Delta", "SJ_Antioch", 
"SJ_PrisonersPt", "Mok_Will_Berm_Mar", "Georg_SloughS", "Golden Gate West Line", 
"MinerSlough_S", "OldRv", "ThreeMile", "FishermansCut", "SR_RedBluff", 
"SR_SturgeonHole", "SR_NewCk", "SR_AbvOrd", "SR_BlwButte", "SR_KnightsBr", 
"SR_BlwChinaBend", "SR_AbvTisdale", "SR_AbvFeather1", "SP_Buoy_9&10", 
"SJ_Blw_False", "Antioch_Br", "SJ_CurtisLanding", "HorseshoeBend", 
"SR_Blw_Ord", "SJ_VulcanIs", "FranksTractW", "SP_Buoy_7&8", "Mok_PotatoS", 
"Mok_Tower_Pk", "SP_Petaluma_Channel", "SFPier33", "SR_AbvDeerCk", 
"MinerSlough_N", "Tracy_Fish_Facility", "MiddleRv_Medford", "MiddleRv", 
"FR_Mouth", "SR_DCC", "Snod_Slough", "SFPier30", "SFPier27", 
"Bay Bridge East", "ChippMontezuma", "PortOakland", "Quimby", 
"SJ_MedfordChannel", "SJ_VeniceCut", "SJ_LittleConnection", "Mok_SouthFork", 
"PotatoSlough", "Rich_Point", "SR_Massacre_Flat", "SR_Woodson_Temp", 
"SR_AbvGCID", "SR_BlwAntelopeCk", "SR_AbvAntelopeCk", "SR_AbvToomes", 
"SR_Above_Freeport_2", "SR_GarciaBend", "SR_Above_SutterSlough", 
"SR_Above_Freeport_1", "SR_Elkhorn_1", "SR_Elkhorn_2", "SR_KnightsLanding", 
"SR_Below_KnightsLanding", "SR_Below_Freeport_1", "SR_Below_Freeport_2", 
"SR_Above_KnightsLanding_1", "SR_BlwGeorgiana", "SR_Above_Ryde", 
"SR_Above_KnightsLanding_2", "SR_DCCSouth", "Georgiana_SloughN1", 
"SR_Isleton", "SR_Below_RioVista_Bridge", "Alcatraz_Control", 
"SP_Array_2", "Martinez_Marina", "MontezumaWest", "SP_Control", 
"SP_Array_1", "MRGC", "SandMound", "Alcatraz", "FR_Beer_Can", 
"FR_Verona", "Point_Reyes", "Bay Bridge West", "Raccoon Straits", 
"Vallejo_Marina", "BattleCk09", "BattleCk08", "BattleCk06", "BattleCk07", 
"SR_BoulderHole", "BattleCk04", "BattleCk03", "SR_BattleCk", 
"BattleCk11", "BattleCk02", "Steam_Marina", "SR_RM85", "SR_RM92", 
"SR_RM69", "SR_RM72", "CarSt01", "Decker_IsN", "SP_Flats_Array", 
"SR_AbvSutterSteam", "Sutter_Blw_Miner", "ThreeMile2", "SR_IstBridge", 
"SR_Tower", "SR_AbvAmerican", "SR_BlwAmerican", "SF9", "SR_SRWTPD_VPS", 
"SR_Riverview_Marina", "BattleCk01"), exclude = c("N", "N", "N", 
"N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", 
"N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "Y", 
"N", "N", "N", "Y", "Y", "N", "N", "Y", "N", "N", "N", "N", "N", 
"Y", "N", "N", "N", "N", "N", "N", "N", "Y", "N", "N", "N", "N", 
"N", "N", "N", "N", "N", "N", "N", "N", "N", "Y", "N", "N", "N", 
"N", "N", "N", "N", "Y", "N", "N", "Y", "Y", "N", "N", "N", "N", 
"N", "N", "N", "N", "Y", "Y", "Y", "N", "Y", "N", "N", "N", "N", 
"N", "N", "Y", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", 
"N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", 
"N", "Y", "Y", "N", "N", "Y", "Y", "N", "N", "Y", "N", "N", "Y", 
"Y", "Y", "Y", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", 
"N", "N", "N", "N", "N", "Y", "N", "Y", "N", "N", "N", "N", "N", 
"N", "N", "Y", "N", "N", "N")), row.names = c(NA, -165L), class = "data.frame")

write.csv(loc, "data_clean/exclude_loc_clean.csv", row.names = FALSE)
Sys.chmod(paths = "data_clean/exclude_loc_clean.csv", 
          mode = "0444", # non-excutable, read by owner, read by group members, read by others
          use_umask = FALSE)
