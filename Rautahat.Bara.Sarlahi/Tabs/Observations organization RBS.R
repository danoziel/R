# Observations organization


# Agriculture----
# Replace values in the data frame
R.Agriculture_Baseline_2018_$name_of_crop[
  R.Agriculture_Baseline_2018_$name_of_crop == "paddy"] <- "PADDY"

R.Agriculture_Baseline_2018_$name_of_crop[
  R.Agriculture_Baseline_2018_$name_of_crop == "SUAGRCANE"] <- "SUGARCANE"

R.Agriculture_Baseline_2018_$name_of_crop[
  R.Agriculture_Baseline_2018_$name_of_crop == "RED LENTLE"] <- "RED LENTIL"

R.Agriculture_Baseline_2018_$name_of_crop[
  R.Agriculture_Baseline_2018_$name_of_crop == "RED LENTI"] <- "RED LENTIL"

R.Agriculture_Baseline_2018_$type_of_crop[
  R.Agriculture_Baseline_2018_$type_of_crop == "01"] <- "1"

# omit
R.Agriculture_Baseline_2018_ <- R.Agriculture_Baseline_2018_ %>%
  filter(household_questionnaire_id != "T301502075" |
           name_of_crop != "WHEAT" )

R.Agriculture_Baseline_2018_ <- R.Agriculture_Baseline_2018_ %>% 
  filter(household_questionnaire_id != "T304905099" | season_of_crop != "4" )

R.Agriculture_Baseline_2018_<- R.Agriculture_Baseline_2018_ %>%
  filter(household_questionnaire_id != "T306102001",
         household_questionnaire_id != "T306102002",
         household_questionnaire_id != "T309708020",
         household_questionnaire_id != "T308707002")

R.Agriculture_Baseline_2018_ <- R.Agriculture_Baseline_2018_ %>%
  mutate( name_of_crop_detail = name_of_crop ) 


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

R_Agriculture_Baseline_2018_$name_of_crop[
  R_Agriculture_Baseline_2018_$name_of_crop=="MUSTARD"] <- "oilseeds"

#  Agriculture Endline  # -------------------------------------------------------#
R.Agriculture_Endline_EPC_2019_ <- R.Agriculture_Endline_EPC_2019_ %>%
  filter(!is.na(season_of_crop)) #Clear observations with missing values

R.Agriculture_Endline_EPC_2019_ <- R.Agriculture_Endline_EPC_2019_ %>%
  mutate( name_of_crop_detail = name_of_crop ) 

R.Agriculture_Endline_EPC_2019_$name_of_crop[R.Agriculture_Endline_EPC_2019_$name_of_crop %in%
                                            c("GREEN GRAM","GRASS PEA","OTHER PULSES")] <- "pulses"

R.Agriculture_Endline_EPC_2019_$name_of_crop[
  R.Agriculture_Endline_EPC_2019_$name_of_crop=="GREEN LEAFY VEGETABLE"] <- "vegetables"


# WEM----