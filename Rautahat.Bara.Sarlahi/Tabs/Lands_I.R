#proprty-----

#Omitting unreliable observations
R.Lands_I_Baseline_2018_  <- Lands_I_Baseline_2018_ %>% 
  filter(homestead_dwelling_area<80,
         land_for_aquaculture_ponds<300
  )

#homestead_dwelling_area
ses <- R.Lands_I_Baseline_2018_ %>% 
  summarise(count = n(),
            sum=sum(homestead_dwelling_area),
            mean=mean(homestead_dwelling_area),
            min=min(homestead_dwelling_area),
            max=max(homestead_dwelling_area)
  )

# land_for_cultivation
ses <- R.Lands_I_Baseline_2018_ %>% 
  filter(land_for_cultivation>0) %>% 
  summarise(count = n(),
            sum= sum(land_for_cultivation),
            mean=mean(land_for_cultivation),
            min=min(land_for_cultivation),
            max=max(land_for_cultivation)
  )

# land_for_aquaculture_ponds
ses <- R.Lands_I_Baseline_2018_ %>% 
  filter(land_for_aquaculture_ponds>0) %>% 
  summarise(count = n(),
            sum=sum(land_for_aquaculture_ponds),
            mean=mean(land_for_aquaculture_ponds),
            min=min(land_for_aquaculture_ponds),
            max=max(land_for_aquaculture_ponds)
  )

# perm_fallow_land
ses <- R.Lands_I_Baseline_2018_ %>% 
  filter(perm_fallow_land>0) %>% 
  summarise(count = n(),
            sum=sum(perm_fallow_land),
            mean=mean(perm_fallow_land),
            min=min(perm_fallow_land),
            max=max(perm_fallow_land)
  )

#orchard_land            
ses <- R.Lands_I_Baseline_2018_ %>% 
  filter(orchard_land>0) %>% 
  summarise(count = n(),
            sum=sum(orchard_land),
            mean=mean(orchard_land),
            min=min(orchard_land),
            max=max(orchard_land)
  )  

# total_property
ses <- R.Lands_I_Baseline_2018_ %>% 
  filter(total_property>0) %>% 
  summarise(count = n(),
            sum=sum(total_property),
            mean=mean(total_property),
            min=min(total_property),
            max=max(total_property)
  )



#women land-----

ses <- R.Lands_I_Baseline_2018_ %>%
  filter(woman_homestead>0) %>%
  summarise(count = n(),
            sum=sum(woman_homestead),
            mean=mean(woman_homestead),
            min=min(woman_homestead),
            max=max(woman_homestead),
            )


ses <- R.Lands_I_Baseline_2018_ %>%
  filter(woman_land_cultivation>0) %>%
  summarise(count = n(),
            sum=sum(woman_land_cultivation),
            mean=mean(woman_land_cultivation),
            min=min(woman_land_cultivation),
            max=max(woman_land_cultivation),
  )

ses <- R.Lands_I_Baseline_2018_ %>%
  filter(woman_aquaculture_ponds>0) %>%
  summarise(count = n(),
            sum=sum(woman_aquaculture_ponds),
            mean=mean(woman_aquaculture_ponds),
            min=min(woman_aquaculture_ponds),
            max=max(woman_aquaculture_ponds),
  )

ses <- R.Lands_I_Baseline_2018_ %>%
  filter(woman_perm_fallow_land>0) %>%
  summarise(count = n(),
            sum=sum(woman_perm_fallow_land),
            mean=mean(woman_perm_fallow_land),
            min=min(woman_perm_fallow_land),
            max=max(woman_perm_fallow_land),
  )

ses <- R.Lands_I_Baseline_2018_ %>%
  filter(woman_orchard>0) %>%
  summarise(count = n(),
            sum=sum(woman_orchard),
            mean=mean(woman_orchard),
            min=min(woman_orchard),
            max=max(woman_orchard),
  )



ses <- R.Lands_I_Baseline_2018_ %>% 
  filter(landarea_women>0) %>% 
  summarise(sum= sum(landarea_women),
            mean=mean(landarea_women),
            min=min(landarea_women),
            max=max(landarea_women))

summary(Lands_I_Baseline_2018_$woman_homestead)
summary(Lands_I_Baseline_2018_$woman_perm_fallow_land)
summary(Lands_I_Baseline_2018_$woman_aquaculture_ponds)
summary(Lands_I_Baseline_2018_$woman_orchard)
summary(Lands_I_Baseline_2018_$woman_land_cultivation)

#rest----
summary(Lands_I_Baseline_2018_$how_many_agricultural_plots)
table(Lands_I_Baseline_2018_$how_many_agricultural_plots)

summary(Lands_I_Baseline_2018_$plot_size_pl1_bore_a)


