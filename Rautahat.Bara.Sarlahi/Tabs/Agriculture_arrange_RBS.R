# Observations organization

Agriculture_18_19 <- rbind(Agriculture_Baseline_2018_,Agriculture_Endline_EPC_2019_) %>% 
  inner_join(Control_and_treatment_4_districts )%>% filter(type_of_crop!="")


culture_18_19 <- culture_18_19 

# Agriculture----
# Replace values in the data frame
R.Agriculture_Baseline_2018_$name_of_crop[
  R.Agriculture_Baseline_2018_$name_of_crop == "paddy"] <- "PADDY"

R.Agriculture_Baseline_2018_$name_of_crop[
  R.Agriculture_Baseline_2018_$name_of_crop == "SUAGRCANE"] <- "SUGARCANE"

R.Agriculture_Baseline_2018_$name_of_crop[
  R.Agriculture_Baseline_2018_$name_of_crop == "RED LENTLE"] <- "RED LENTIL"



# omit
# R.Agriculture_Baseline_2018_ <- R.Agriculture_Baseline_2018_ %>%
#   filter(household_questionnaire_id != "T301502075" |
#            name_of_crop != "WHEAT" )

# R.Agriculture_Baseline_2018_ <- R.Agriculture_Baseline_2018_ %>% 
#  filter(household_questionnaire_id != "T304905099" | season_of_crop != "4" )


R.Agriculture_Baseline_2018_$name_of_crop[
  R.Agriculture_Baseline_2018_$name_of_crop == "RED LENTI"] <- "RED LENTIL"

R.Agriculture_Baseline_2018_$type_of_crop[
  R.Agriculture_Baseline_2018_$type_of_crop == "01"] <- "1"

R.Agriculture_Baseline_2018_ <- R.Agriculture_Baseline_2018_ %>%
  mutate( name_of_crop_detail = name_of_crop ) #(copy=original)
 
Agriculture_18_19$season_of_crop <- as.character(Agriculture_18_19$season_of_crop)

Agriculture_18_19$season_of_crop[Agriculture_18_19$season_of_crop == "1"] <- "Monsoon"
Agriculture_18_19$season_of_crop[Agriculture_18_19$season_of_crop == "2"] <- "Winter"
Agriculture_18_19$season_of_crop[Agriculture_18_19$season_of_crop == "3"] <- "Summer"
Agriculture_18_19$season_of_crop[Agriculture_18_19$season_of_crop == "4"] <- "Annual"

Agriculture_18_19 <- Agriculture_18_19 %>%
  mutate(irri_for_season=cult_area_under_crop*hrs_irr_1katha*no_of_irrigation_for_1_katha)



Agriculture_18_19$name_of_crop[
  Agriculture_18_19$name_of_crop=="vegetables"] <- "Vegetables"


R.Agriculture_Baseline_2018_$name_of_crop[R.Agriculture_Baseline_2018_$name_of_crop %in%
                                            c("TOMATO","BITTER","BRINJAL","OKRA","GARLIC","ONIONS","RADISH"
                                              ,"POTATO","CABBAGE","CAULI","BOTTLE","BELL","SPONGE",
                                              "CHILLIES","CHILIES", "CUCUMBER","ONOINS"
                                            )] <- "vegetables"

R_Agriculture_Baseline_2018_$name_of_crop[R_Agriculture_Baseline_2018_$name_of_crop %in%
                                            c("KIDNEY", "GREEN GRAM","BLACK GRAM","GRASS PEA","PEA",
                                              "OTHER PULSES","YARD LONG","RED LENTIL"
                                            )] <- "pulses"

R.Agriculture_Baseline_2018_$name_of_crop[R.Agriculture_Baseline_2018_$name_of_crop %in%
                                            c("GRASS","OATS","BARLEY","MUSTARD","OTHER"
                                            )] <- "others"

Agriculture_18_19$name_of_crop[
  Agriculture_18_19$name_of_crop=="MUSTARD"] <- "oilseeds"

Agriculture_18_19$name_of_crop[
  Agriculture_18_19$name_of_crop=="RED LENTILS"] <- "pulses"

#  Agriculture Endline  # -------------------------------------------------------#
R.Agriculture_Endline_EPC_2019_ <- R.Agriculture_Endline_EPC_2019_ %>%
  filter(!is.na(season_of_crop)) #Clear observations with missing values

R.Agriculture_Endline_EPC_2019_ <- R.Agriculture_Endline_EPC_2019_ %>%
  mutate( name_of_crop_detail = name_of_crop ) 

R.Agriculture_Endline_EPC_2019_$name_of_crop[R.Agriculture_Endline_EPC_2019_$name_of_crop %in%
                                            c("GREEN GRAM","GRASS PEA","OTHER PULSES")] <- "pulses"

R.Agriculture_Endline_EPC_2019_$name_of_crop[
  R.Agriculture_Endline_EPC_2019_$name_of_crop=="GREEN LEAFY VEGETABLE"] <- "vegetables"


