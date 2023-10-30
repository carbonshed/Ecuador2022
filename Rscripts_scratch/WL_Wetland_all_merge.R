
WL_01 <- read.csv(here::here("Wetlands/WaterLevel_FINAL/WL_Wetland01_FINAL.csv"))
WL_02 <- read.csv(here::here("Wetlands/WaterLevel_FINAL/WL_Wetland02_FINAL.csv"))
WL_03 <- read.csv(here::here("Wetlands/WaterLevel_FINAL/WL_Wetland03_FINAL.csv"))
WL_04 <- read.csv(here::here("Wetlands/WaterLevel_FINAL/WL_Wetland04_FINAL.csv"))
WL_05 <- read.csv(here::here("Wetlands/WaterLevel_FINAL/WL_Wetland05_FINAL.csv"))
WL_06 <- read.csv(here::here("Wetlands/WaterLevel_FINAL/WL_Wetland06_FINAL.csv"))
WL_07 <- read.csv(here::here("Wetlands/WaterLevel_FINAL/WL_Wetland07_FINAL.csv"))
WL_08 <- read.csv(here::here("Wetlands/WaterLevel_FINAL/WL_Wetland08_FINAL.csv"))
WL_09 <- read.csv(here::here("Wetlands/WaterLevel_FINAL/WL_Wetland09_FINAL.csv"))
WL_10 <- read.csv(here::here("Wetlands/WaterLevel_FINAL/WL_Wetland10_FINAL.csv"))
WL_11 <- read.csv(here::here("Wetlands/WaterLevel_FINAL/WL_Wetland11_FINAL.csv"))
WL_12 <- read.csv(here::here("Wetlands/WaterLevel_FINAL/WL_Wetland12_FINAL.csv"))


WL_df <- rbind(WL_01,WL_02,WL_03,WL_04,WL_05,WL_06,WL_07,WL_08,WL_09,WL_10,WL_11,WL_12)
WL_df$X.1 <- NULL
WL_df$X <- NULL
WL_df <- unique(WL_df)

WL_df_2 <- WL_df[c("Station"
                   ,"DateTime"
                   ,"Baro_kpa","BaroTemp_c"
                   ,"WLPres_kpa","WLTemp_c","WaterLevel_m"
                   ,"depth_diff_m","depth_ave_m","surface_area_m2"
                   ,"Volumn_m3","SA_to_Vol_ratio"
                   )]

write.csv(WL_df_2, here::here("Wetlands/WaterLevel_FINAL/WL_Wetland_all_FINAL.csv"))
