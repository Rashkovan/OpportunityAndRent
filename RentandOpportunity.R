colnames(OI_atlas_data_full)
Washington_DC <- OI_atlas_data_full %>% filter(state == 11 & county == 001)
Washington_DC %>% summarise(mean = mean(rent_twobed2015, na.rm = TRUE))

Washington_DC %>% ggplot(aes(rent_twobed2015, kfr_pooled_p25)) + 
  geom_point() + 
  geom_smooth(method = "lm", 
              se = FALSE, formula = "y ~ x") + 
  labs(x = "Average 2 Bedroom Rent in 2015", 
       y = "Income at Age 35", 
       title = "Average Rent vs. Opportunity in Washington, DC")
cor_coef <- cor(Washington_DC$rent_twobed2015, Washington_DC$kfr_pooled_p25, use = "complete.obs") print(cor_coef)

Washington_DC %>% ggplot(aes(rent_twobed2015, kfr_black_p25)) + geom_point(alpha = 0.1) + geom_smooth(method = "lm", se = FALSE, alpha = 0.1, formula = "y ~ x") + stat_summary_bin(fun='mean', bins=20, color='orange', size=2, geom='point') + labs(x = "Average 2 Bedroom Rent in 2015", y = "Income at Age 35, for Black residents", title = "Average Rent vs. Opportunity in Washington, DC for Black residents")

Washington_DC %>% ggplot(aes(rent_twobed2015, kfr_black_p25)) + geom_point(alpha=0.1) + geom_smooth(method = "lm", se = FALSE, formula = "y ~ x") + geom_text(aes(label = tract), size = 3.5, check_overlap=TRUE) + labs(x = "Average 2 Bedroom Rent in 2015", y = "Income at Age 35, for Black residents", title = "Average Rent vs. Opportunity in Washington, DC for Black residents")
tract8804 <- Washington_DC %>% filter(tract == 8804) tract1804 <- Washington_DC %>% filter(tract == 1804) 
Washington_DC %>% ggplot(aes(rent_twobed2015, kfr_black_p25)) 
  + geom_point() 
  + geom_point(data=tract8804, 
               aes(rent_twobed2015, kfr_black_p25), color='red', size=4) 
+ geom_point(data=tract1804, 
             aes(rent_twobed2015, kfr_black_p25), color='orange', size=4) 
+ geom_smooth(method = "lm", 
              se = FALSE, formula = "y ~ x") 
+ labs(x = "Average 2 Bedroom Rent in 2015", 
       y = "Income at Age 35, for Black residents", 
       title = "Average Rent vs. Opportunity in Washington, DC for Black residents (Tracts 8804 and 1804 Highlighted)")

tract8804 <- Washington_DC %>% filter(tract == 8804) tract1804 <- Washington_DC %>% filter(tract == 1804) compare_tracts <- rbind(tract8804, tract1804) compare_tracts["teenbrth_black_female_p25"]
tract8804 <- Washington_DC %>% filter(tract == 8804) tract1804 <- Washington_DC %>% filter(tract == 1804) compare_tracts <- rbind(tract8804, tract1804) compare_tracts["staytract_black_pooled_p25"]
tract8804 <- Washington_DC %>% filter(tract == 8804) tract1804 <- Washington_DC %>% filter(tract == 1804) Washington_DC %>% ggplot(aes(staytract_black_pooled_p25, kfr_black_p25)) + geom_point() + geom_point(data=tract8804, aes(staytract_black_pooled_p25, kfr_black_p25), color='red', size=4) + geom_point(data=tract1804, aes(staytract_black_pooled_p25, kfr_black_p25), color='orange', size=4) + geom_smooth(method = "lm", se = FALSE, formula = "y ~ x") + labs(x = "People staying in the same SES tract, among Black residents", y = "Income at Age 35, among Black residents", title = "Income at Age 35 vs. People staying in the same SES tract for Black residents in Washington, DC (Tracts 8804 and 1804 Highlighted)")

tract8804 <- Washington_DC %>% filter(tract == 8804) tract1804 <- Washington_DC %>% filter(tract == 1804) compare_tracts <- rbind(tract8804, tract1804) compare_tracts["jail_black_male_p25"]
tract8804 <- Washington_DC %>% filter(tract == 8804) tract1804 <- Washington_DC %>% filter(tract == 1804) Washington_DC %>% ggplot(aes(jail_black_male_p25, kfr_black_p25)) + geom_point() + geom_point(data=tract8804, aes(jail_black_male_p25, kfr_black_p25), color='red', size=4) + geom_point(data=tract1804, aes(jail_black_male_p25, kfr_black_p25), color='orange', size=4) + geom_smooth(method = "lm", se = FALSE, formula = "y ~ x") + labs(x = "Incarcerated Black Men", y = "Income at Age 35, among Black residents", title = "Incarcerated Black Men vs. Income at Age 35 in Washington, DC (Tracts 8804 and 1804 Highlighted)")

tract8804 <- Washington_DC %>% filter(tract == 8804) tract1804 <- Washington_DC %>% filter(tract == 1804) compare_tracts <- rbind(tract8804, tract1804) compare_tracts["kfr_top20_black_pooled_p25"]
tract8804 <- Washington_DC %>% filter(tract == 8804) tract1804 <- Washington_DC %>%filter(tract == 1804) Washington_DC %>% ggplot(aes(kfr_top20_black_pooled_p25, kfr_black_p25)) 
  + geom_point() 
  + geom_point(data=tract8804, 
               aes(kfr_top20_black_pooled_p25, kfr_black_p25), 
               color='red', size=4) + 
  geom_point(data=tract1804, 
             aes(kfr_top20_black_pooled_p25, kfr_black_p25), 
             color='orange', size=4) + geom_smooth(method = "lm", 
                                                   se = FALSE, formula = "y ~ x") + 
  labs(x = "Possibility of Reaching the Top Quintile of the National Household Income Distribution", 
       y = "Income at Age 35, among Black residents", 
       title = "Possibility of Reaching the Top Quintile of the National Household Income Distribution vs. Income at Age 35 in Washington, DC (Tracts 8804 and 1804 Highlighted)")