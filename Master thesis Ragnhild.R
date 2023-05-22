##### R-script for Ragnhild Grønntun Nissen Master's Thesis ####
# Does democracy moderate the 
# effect of environmental policies on forest cover change? 

#### Prepare data ####
# Set working directory
setwd( "C:/Users/ragnh/OneDrive/Dokumenter/Master i statsvitenskap/Masteroppgave/Data/Masteroppgave" )
getwd()

# Load necessary packages
library(car)
library(stargazer)
library (tidyverse)
library(haven)
library(dplyr)
library(WDI)
library(scales)
library (rlang)
library(tidyr)
library(readr)
library(arsenal)
library(readxl)
library(moments)
library(plm)
library(lmtest)
library(broom)
library(clubSandwich)
library(miceadds)
library(sjPlot)
library(sjmisc)
library(ggplot2)
library(marginaleffects)
library(nnet)
library(MASS)
library(lme4)
library(sandwich)
library(oglmx)
library(texreg)
library(psych)
library(corrplot)

# Importing datasets
qogdataset <- read_excel("C:/Users/ragnh/OneDrive/Dokumenter/Master i statsvitenskap/Masteroppgave/Data/Quality of government basic dataset.xlsx")
View(qogdataset)
eidataset <- read_excel("C:/Users/ragnh/OneDrive/Dokumenter/Master i statsvitenskap/Masteroppgave/Data/Environmental indicators dataset.xlsx")
View(eidataset)
reddpolicy <- read_excel("C:/Users/ragnh/OneDrive/Dokumenter/Master i statsvitenskap/Masteroppgave/Data/Masteroppgave/REDD policy dataset.xlsx")
View(reddpolicy)

# Loading WDI indicator (GDP per capita PPP adjusted) 
WDIsearch("GDP per capita")
wdi_gdpppp1 <- WDI(
  country = "all",
  indicator = "NY.GDP.PCAP.PP.CD",
  start = 2010,
  end = 2020,
  extra = FALSE,
  cache = NULL,
  latest = NULL,
  language = "en")

head(wdi_gdpppp1)

#### Merging datasets ####
# Renaming country names so they correspond in each dataset
reddpolicy$cname [reddpolicy$cname == "Guinea Bisseau"] <- "Guinea-Bissau"
reddpolicy$cname [reddpolicy$cname == "Viet nam"] <- "Viet Nam"
reddpolicy$cname [reddpolicy$cname == "Sudan (the)"] <- "Sudan"
wdi_gdpppp1$country [wdi_gdpppp1$country == "Bolivia"] <- "Bolivia (Plurinational State of)"
wdi_gdpppp1$country [wdi_gdpppp1$country == "Central African Republic"] <- "Central African Republic (the)"
wdi_gdpppp1$country [wdi_gdpppp1$country == "Congo, Rep."] <- "Congo (the)"
wdi_gdpppp1$country [wdi_gdpppp1$country == "Dominican Republic"] <- "Dominican Republic (the)"
wdi_gdpppp1$country [wdi_gdpppp1$country == "Guinea"] <- "Guinea (Republic of)"
wdi_gdpppp1$country [wdi_gdpppp1$country == "Lao PDR"] <- "Lao People's Democratic Republic (the)"
wdi_gdpppp1$country [wdi_gdpppp1$country == "Philippines"] <- "Philippines (the)"
wdi_gdpppp1$country [wdi_gdpppp1$country == "Tanzania"] <- "Tanzania, the United Republic of"
wdi_gdpppp1$country [wdi_gdpppp1$country == "Vietnam"] <- "Viet Nam"
qogdataset$cname [qogdataset$cname == "Sudan (the)"] <- "Sudan"
eidataset$cname [eidataset$cname == "Guinea Bissau"] <- "Guinea-Bissau"
eidataset$cname [eidataset$cname == "Democratic Republic of the Congo"] <- "Congo (the Democratic Republic of the)"
eidataset$cname [eidataset$cname == "Congo"] <- "Congo (the)"

# Renaming the country name variable in the WDI dataset
colnames(wdi_gdpppp1)[1] <- "cname"

# Merging datasets
redd_data1 <- merge(reddpolicy, wdi_gdpppp1, 
                     by=c("cname", "year"), all.x=TRUE)
redd_data2 <- merge(redd_data1, eidataset,
                    by=c("cname", "year"), all.x=TRUE)
redd_data3 <- merge(redd_data2, qogdataset,
                    by=c("cname", "year"), all.x=TRUE)

# Removing variables that will not be used in the analysis
redd_data_final <- redd_data3[c(1,2,4,7,28,437,577,579,627,649)]
summary(comparedf(redd_data, redd_data3, by = "cname"))

#### Tidying dataset ####
# Renaming variables
colnames(redd_data_final)[1]<- "country"
colnames(redd_data_final)[4] <- "gdp_ppp_current"
colnames(redd_data_final)[5] <- "av_temperature"
colnames(redd_data_final)[6] <- "bmr_regime"
colnames(redd_data_final)[8] <- "corruption"
colnames(redd_data_final)[9] <- "forest_percentage"
colnames(redd_data_final)[10] <- "pop_density"


# Because it was not possible to change the name of 
# "Côte d'Ivoire" in the different data frames, 
# these values went missing upon merging.
# Therefore I impute the values for Côte d'Ivoire from 
# the different datasets

# Inputing values from wdi_gdpppp1
redd_data_final$gdp_ppp_current[188] = 
  wdi_gdpppp1$NY.GDP.PCAP.PP.CD[1067]
redd_data_final$gdp_ppp_current[189] = 
  wdi_gdpppp1$NY.GDP.PCAP.PP.CD[1066]
redd_data_final$gdp_ppp_current[190] = 
  wdi_gdpppp1$NY.GDP.PCAP.PP.CD[1065]
redd_data_final$gdp_ppp_current[191] = 
  wdi_gdpppp1$NY.GDP.PCAP.PP.CD[1064]
redd_data_final$gdp_ppp_current[192] = 
  wdi_gdpppp1$NY.GDP.PCAP.PP.CD[1063]
redd_data_final$gdp_ppp_current[193] = 
  wdi_gdpppp1$NY.GDP.PCAP.PP.CD[1062]
redd_data_final$gdp_ppp_current[194] = 
  wdi_gdpppp1$NY.GDP.PCAP.PP.CD[1061]
redd_data_final$gdp_ppp_current[195] = 
  wdi_gdpppp1$NY.GDP.PCAP.PP.CD[1060]
redd_data_final$gdp_ppp_current[196] = 
  wdi_gdpppp1$NY.GDP.PCAP.PP.CD[1059]
redd_data_final$gdp_ppp_current[197] = 
  wdi_gdpppp1$NY.GDP.PCAP.PP.CD[1058]
redd_data_final$gdp_ppp_current[198] = 
  wdi_gdpppp1$NY.GDP.PCAP.PP.CD[1057]

# Inputing values from eidataset 
redd_data_final$av_temperature[188] = 
  eidataset$cckp_temp[6394]
redd_data_final$av_temperature[189] = 
  eidataset$cckp_temp[6395]
redd_data_final$av_temperature[190] = 
  eidataset$cckp_temp[6396]
redd_data_final$av_temperature[191] = 
  eidataset$cckp_temp[6397]
redd_data_final$av_temperature[192] = 
  eidataset$cckp_temp[6398]
redd_data_final$av_temperature[193] = 
  eidataset$cckp_temp[6399]
redd_data_final$av_temperature[194] = 
  eidataset$cckp_temp[6400]
redd_data_final$av_temperature[195] = 
  eidataset$cckp_temp[6401]
redd_data_final$av_temperature[196] = 
  eidataset$cckp_temp[6402]
redd_data_final$av_temperature[197] = 
  eidataset$cckp_temp[6403]
redd_data_final$av_temperature[198] = 
  eidataset$cckp_temp[6404]
View(redd_data_final)

# Inputing values from qogdataset
redd_data_final$bmr_regime[188] = 
  qogdataset$bmr_dem[6532]
redd_data_final$bmr_regime[189] = 
  qogdataset$bmr_dem[6533]
redd_data_final$bmr_regime[190] = 
  qogdataset$bmr_dem[6534]
redd_data_final$bmr_regime[191] = 
  qogdataset$bmr_dem[6535]
redd_data_final$bmr_regime[192] = 
  qogdataset$bmr_dem[6536]
redd_data_final$bmr_regime[193] = 
  qogdataset$bmr_dem[6537]
redd_data_final$bmr_regime[194] = 
  qogdataset$bmr_dem[6538]
redd_data_final$bmr_regime[195] = 
  qogdataset$bmr_dem[6539]
redd_data_final$bmr_regime[196] = 
  qogdataset$bmr_dem[6540]
redd_data_final$bmr_regime[197] = 
  qogdataset$bmr_dem[6541]
redd_data_final$bmr_regime[198] = 
  qogdataset$bmr_dem[6542]
View(redd_data_final)

redd_data_final$vdem_polyarchy[188] = 
  qogdataset$vdem_polyarchy[6532]
redd_data_final$vdem_polyarchy[189] = 
  qogdataset$vdem_polyarchy[6533]
redd_data_final$vdem_polyarchy[190] = 
  qogdataset$vdem_polyarchy[6534]
redd_data_final$vdem_polyarchy[191] = 
  qogdataset$vdem_polyarchy[6535]
redd_data_final$vdem_polyarchy[192] = 
  qogdataset$vdem_polyarchy[6536]
redd_data_final$vdem_polyarchy[193] = 
  qogdataset$vdem_polyarchy[6537]
redd_data_final$vdem_polyarchy[194] = 
  qogdataset$vdem_polyarchy[6538]
redd_data_final$vdem_polyarchy[195] = 
  qogdataset$vdem_polyarchy[6539]
redd_data_final$vdem_polyarchy[196] = 
  qogdataset$vdem_polyarchy[6540]
redd_data_final$vdem_polyarchy[197] = 
  qogdataset$vdem_polyarchy[6541]
redd_data_final$vdem_polyarchy[198] = 
  qogdataset$vdem_polyarchy[6542]
View(redd_data_final)

redd_data_final$corruption[188] = 
  qogdataset$wbgi_cce[6532]
redd_data_final$corruption[189] = 
  qogdataset$wbgi_cce[6533]
redd_data_final$corruption[190] = 
  qogdataset$wbgi_cce[6534]
redd_data_final$corruption[191] = 
  qogdataset$wbgi_cce[6535]
redd_data_final$corruption[192] = 
  qogdataset$wbgi_cce[6536]
redd_data_final$corruption[193] = 
  qogdataset$wbgi_cce[6537]
redd_data_final$corruption[194] = 
  qogdataset$wbgi_cce[6538]
redd_data_final$corruption[195] = 
  qogdataset$wbgi_cce[6539]
redd_data_final$corruption[196] = 
  qogdataset$wbgi_cce[6540]
redd_data_final$corruption[197] = 
  qogdataset$wbgi_cce[6541]
redd_data_final$corruption[198] = 
  qogdataset$wbgi_cce[6542]
View(redd_data_final)

redd_data_final$pop_density[188] = 
  qogdataset$wdi_popden[6532]
redd_data_final$pop_density[189] = 
  qogdataset$wdi_popden[6533]
redd_data_final$pop_density[190] = 
  qogdataset$wdi_popden[6534]
redd_data_final$pop_density[191] = 
  qogdataset$wdi_popden[6535]
redd_data_final$pop_density[192] = 
  qogdataset$wdi_popden[6536]
redd_data_final$pop_density[193] = 
  qogdataset$wdi_popden[6537]
redd_data_final$pop_density[194] = 
  qogdataset$wdi_popden[6538]
redd_data_final$pop_density[195] = 
  qogdataset$wdi_popden[6539]
redd_data_final$pop_density[196] = 
  qogdataset$wdi_popden[6540]
redd_data_final$pop_density[197] = 
  qogdataset$wdi_popden[6541]
redd_data_final$pop_density[198] = 
  qogdataset$wdi_popden[6542]
View(redd_data_final)

redd_data_final$forest_percentage[188] = 
  qogdataset$wdi_forest[6532]
redd_data_final$forest_percentage[189] = 
  qogdataset$wdi_forest[6533]
redd_data_final$forest_percentage[190] = 
  qogdataset$wdi_forest[6534]
redd_data_final$forest_percentage[191] = 
  qogdataset$wdi_forest[6535]
redd_data_final$forest_percentage[192] = 
  qogdataset$wdi_forest[6536]
redd_data_final$forest_percentage[193] = 
  qogdataset$wdi_forest[6537]
redd_data_final$forest_percentage[194] = 
  qogdataset$wdi_forest[6538]
redd_data_final$forest_percentage[195] = 
  qogdataset$wdi_forest[6539]
redd_data_final$forest_percentage[196] = 
  qogdataset$wdi_forest[6540]
redd_data_final$forest_percentage[197] = 
  qogdataset$wdi_forest[6541]
redd_data_final$forest_percentage[198] = 
  qogdataset$wdi_forest[6542]
View(redd_data_final)

#### Investigating variables ####
# 70 countries in total
# 11 observations per country

# Checking class
class(redd_data_final$country)
class(redd_data_final$year)
class(redd_data_final$reddpolicy)
class(redd_data_final$gdp_ppp_current)
class(redd_data_final$pop_growth)
class(redd_data_final$av_temperature)
class(redd_data_final$bmr_regime)
class(redd_data_final$vdem_polyarchy)
class(redd_data_final$corruption)
class(redd_data_final$forest_percentage)
class(redd_data_final$pop_density)

# Complete observations
table(complete.cases(redd_data_final))

#### Creating new variables to be included in the analysis ####
# Changing year from class to numeric
redd_data_final <- redd_data_final %>% 
  mutate(year = as.numeric(year))
class(redd_data_final$year)

# Creating a lagged measure of corruption
redd_data_final <- redd_data_final %>%
  group_by(country) %>%
  mutate(corruption_lag1 = dplyr::lag(corruption, n=1))

# Creating a lagged measure of vdem_polyarchy
redd_data_final <- redd_data_final %>%
  group_by(country) %>%
  mutate(vdem_polyarchy_lag1 = dplyr::lag(vdem_polyarchy, n=1))

# Creating a lagged measure of bmr_regime
redd_data_final <- redd_data_final %>%
  group_by(country) %>%
  mutate(bmr_regime_lag1 = dplyr::lag(bmr_regime, n=1))

# Creating a log-transformed and lagged measure of gdp_ppp_current
redd_data_final$log_gdp <- log(redd_data_final$gdp_ppp_current)
summary(redd_data_final$gdp_ppp_current)
redd_data_final <- redd_data_final %>%
  group_by(country) %>%
  mutate(log_gdp_lag1 = dplyr::lag(log_gdp, n=1))

# Creating a squared and lagged measure of log_gdp
redd_data_final$sq_log_gdp <- (redd_data_final$log_gdp)^2
redd_data_final <- redd_data_final %>%
  group_by(country) %>%
  mutate(sq_log_gdp_lag1 = dplyr::lag(sq_log_gdp, n=1))

# Creating a lagged measure of forest_percentage
  redd_data_final <- redd_data_final %>%
  group_by(country) %>%
  mutate(forest_percentage_lag1 = dplyr::lag(forest_percentage, n=1))
  
# Creating a variable measuring the deviation from mean temperature
redd_data_final <- redd_data_final %>%
  arrange(country, year) %>%
  group_by(country) %>%
mutate(dev_mean_temperature = av_temperature - mean(av_temperature, 
                                                    na.rm = TRUE))

# Lagging dev_mean_temperature 
redd_data_final <- redd_data_final %>%
  group_by(country) %>%
  mutate(dev_mean_temperature_lag1 = 
           dplyr::lag(dev_mean_temperature, n=1))
summary(redd_data_final$dev_mean_temperature_lag1)

# Specifying reddpolicy as a factor
redd_data_final <- redd_data_final %>% 
  mutate(reddpolicy_factor = as.factor(reddpolicy))
class(redd_data_final$reddpolicy_factor)

# Creating a dummy variable indicating whether 
# the value of reddpolicy has changed or not (REDD + policy change)
redd_data_final <- redd_data_final %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(reddpolicy_change = ifelse(!is.na(reddpolicy) & 
    reddpolicy != dplyr::lag(reddpolicy, 
                              default = last(reddpolicy)),
                                    1, 0))

# Creating a lagged reddpolicy_change variable
redd_data_final <- redd_data_final %>%
  group_by(country) %>%
  mutate(reddpolicy_change_lag1 = dplyr::lag(reddpolicy_change, n=1))
summary(redd_data_final$reddpolicy_change_lag1)

# Creating a lagged reddpolicy variable and specifying as factor
redd_data_final <- redd_data_final %>%
  group_by(country) %>%
  mutate(reddpolicy_lag1 = dplyr::lag(reddpolicy, n=1))

redd_data_final$reddpolicy_lag1_factor <- 
  as.factor(redd_data_final$reddpolicy_lag1)

# Log-transforming pop_density
redd_data_final$log_pop_density <- log(redd_data_final$pop_density)

# Creating lagged log_pop_density variable
redd_data_final <- redd_data_final %>%
  group_by(country) %>%
  mutate(log_pop_density_lag1 = dplyr::lag(log_pop_density, n = 1))

#### Checking missing values ####
table(complete.cases(redd_data_final))
# 519 after creating lagged variables
table(is.na(redd_data_final))
table(is.na(redd_data_final$reddpolicy_lag1_factor))
table(is.na(redd_data_final$reddpolicy_change_lag1))
table(is.na(redd_data_final$forest_percentage))
table(is.na(redd_data_final$forest_percentage_lag1))
table(is.na(redd_data_final$dev_mean_temperature_lag1))
table(is.na(redd_data_final$bmr_regime_lag1))
table(is.na(redd_data_final$vdem_polyarchy_lag1))
table(is.na(redd_data_final$log_gdp_lag1))
table(is.na(redd_data_final$corruption_lag1))
table(is.na(redd_data_final$log_pop_density_lag1))

#### Figure 4.2: Plotting stages of REDD+ for each country ####
ggplot(redd_data_final, aes(x = year, 
                            y = country, 
                            color = factor(reddpolicy))) +
  geom_line() +
  labs(x = "Year", 
       y = "Country",
       color = "REDD+ policy") +
  scale_x_continuous(breaks = 2010:2020) +
  scale_color_manual(values = c("skyblue", "blue", "black"),
                     na.value = "red") +
  theme_bw()

#### 5.0 Analysis ####

# 5.1 Estimating TWFE regression models

# Table 5.1
# I estimate the models both using the plm function and
# by using the lm function because most functions
# to check the assumptions of OLS do not work on plm objects

plm_m1 <- plm(forest_percentage ~ 
                reddpolicy_lag1_factor + vdem_polyarchy_lag1 + 
                log_gdp_lag1 + sq_log_gdp_lag1 +
                corruption_lag1 + 
                log_pop_density_lag1 + 
                dev_mean_temperature_lag1,
              data = redd_data_final, 
              index = c("country", "year"), 
              model = "within", 
              effect = "twoways", na.action = "na.exclude")
summary(plm_m1)

lm_m1 <- lm(forest_percentage ~ 
              reddpolicy_lag1_factor + vdem_polyarchy_lag1 + 
              log_gdp_lag1 + sq_log_gdp_lag1 +
              corruption_lag1 + 
              log_pop_density_lag1 + 
              dev_mean_temperature_lag1 +
              as.factor(country) + as.factor(year), 
            data = redd_data_final, na.action = na.exclude)
summary(lm_m1)

plm_m2 <- plm(forest_percentage ~ 
                reddpolicy_lag1_factor*vdem_polyarchy_lag1 + 
                log_gdp_lag1 + sq_log_gdp_lag1 +
                corruption_lag1 + 
                log_pop_density_lag1 + 
                dev_mean_temperature_lag1,
              data = redd_data_final, 
              index = c("country", "year"), 
              model = "within", 
              effect = "twoways", na.action = "na.exclude")
summary(plm_m2)

lm_m2 <- lm(forest_percentage ~ 
              reddpolicy_lag1_factor*vdem_polyarchy_lag1 + 
              log_gdp_lag1 + sq_log_gdp_lag1 +
              corruption_lag1 + 
              log_pop_density_lag1 + 
              dev_mean_temperature_lag1 + as.factor(country) 
            + as.factor(year), 
            data = redd_data_final, na.action = na.exclude)
summary(lm_m2)
  
# Calculating robust standard errors
plm_m1_clustered <- vcovHC(plm_m1, 
      method = "arellano", type = "HC1", cluster = "group")
summary(plm_m1, vcov = plm_m1_clustered)
se_plm_m1 <- sqrt(diag(plm_m1_clustered))

plm_m2_clustered <- vcovHC(plm_m2, 
      method = "arellano", type = "HC1", cluster = "group")
summary(plm_m2, vcov = plm_m2_clustered)
se_plm_m2 <- sqrt(diag(plm_m2_clustered))

# Combining the results 
stargazer(plm_m1, plm_m2, type = "text",
          se = list(se_plm_m1,
              se_plm_m2), notes = 
                c("Models include country and time fixed effects",
                   "Include clustered standard errors"),
              out = "table1plm.html")

# Table 5.1.2
plm_m3 <- plm(forest_percentage ~ reddpolicy_change_lag1 + 
                vdem_polyarchy_lag1 + 
                log_gdp_lag1 + sq_log_gdp_lag1 +
                corruption_lag1 + 
                log_pop_density_lag1 +
                dev_mean_temperature_lag1, 
              data = redd_data_final,
              index = c("country", "year"), 
              model = "within", 
              effect = "twoways", na.action = "na.exclude")

lm_m3 <- lm(forest_percentage ~ reddpolicy_change_lag1 + 
              vdem_polyarchy_lag1 + 
              log_gdp_lag1 + sq_log_gdp_lag1 +
              corruption_lag1 + 
              log_pop_density_lag1 + 
              dev_mean_temperature_lag1 + as.factor(country) 
            + as.factor(year), 
            data = redd_data_final, na.action = na.exclude)

plm_m4 <- plm(forest_percentage ~ 
                reddpolicy_change_lag1*vdem_polyarchy_lag1 + 
                log_gdp_lag1 + sq_log_gdp_lag1 +
                corruption_lag1 + 
                log_pop_density_lag1 +
                dev_mean_temperature_lag1, 
              data = redd_data_final,
              index = c("country", "year"), 
              model = "within", 
              effect = "twoways", na.action = "na.exclude")

lm_m4 <- lm(forest_percentage ~ reddpolicy_change_lag1*vdem_polyarchy + 
              log_gdp_lag1 + sq_log_gdp_lag1 +
              corruption_lag1 + 
              log_pop_density_lag1 +
              dev_mean_temperature_lag1 +
              as.factor(country) +
              as.factor(year), 
            data = redd_data_final, na.action = na.exclude)

# Calculating clustered standard errors
plm_m3_clustered <- vcovHC(plm_m3, 
                           method = "arellano", 
                           type = "HC1", cluster = "group")
summary(plm_m3, vcov = plm_m3_clustered)
se_plm_m3 <- sqrt(diag(plm_m3_clustered))

plm_m4_clustered <- vcovHC(plm_m4, 
                           method = "arellano", type = "HC1", 
                           cluster = "group")
summary(plm_m4, vcov = plm_m4_clustered)
se_plm_m4 <- sqrt(diag(plm_m4_clustered))

# Combining the results 
stargazer(plm_m3, plm_m4, type = "text", notes = 
            c("Models include country and time fixed effects",
              "Include clustered standard errors"),
          se = list(se_plm_m3, se_plm_m4),
          out = "table2plm.html")

# 5.2 Checking the endogeneity of the relationship
# Table 5.2
ordered_logit_model <- ologit.reg(as.factor(reddpolicy) ~ 
                      forest_percentage_lag1 +
                      vdem_polyarchy_lag1 + 
                      log_gdp_lag1 + 
                        sq_log_gdp_lag1 +
                      corruption_lag1 + 
                      log_pop_density_lag1 + 
                      dev_mean_temperature_lag1 +
                      as.factor(country) +
                      as.factor(year),
                    data = redd_data_final, 
                    start = NULL, 
                    robust = TRUE,
                    na.action = "na.exclude")

summary.oglmx(ordered_logit_model)

htmlreg(ordered_logit_model,
        custom.coef.map = list("reddpolicy" = "REDD+ policy",
          "forest_percentage_lag1" = "Forest area (% of land area) (t-1)",
              "vdem_polyarchy_lag1" = "Level of democracy (t-1)",
            "log_gdp_lag1" = "Ln GDP per capita (t-1)", 
           "sq_log_gdp_lag1" = "Ln GDP per capita (t-1)^2",
                 "corruption_lag1" = "Control of corruption (t-1)", 
               "log_pop_density_lag1" = "Ln population density (t-1)", 
            "dev_mean_temperature_lag1" = "Deviation from mean temperature (t-1)"),
        file = "orderedlogit.html")

# Creating figures 5.1 and 5.1.2
interaction_plm_m2 <- plot_predictions(plm_m2, 
                condition = c("vdem_polyarchy_lag1", 
                                   "reddpolicy_lag1_factor"),
                 draw = TRUE, vcov = "HAC")
interaction_plm_m2 +
  labs(x = "Level of democracy (t-1)", 
       y = "Predicted forest area (% of land area)") +
  scale_color_discrete(name = "REDD+ policy (t-1)", 
                       labels = c("0", "1", "2"))

interaction_plm_m4 <- plot_predictions(plm_m4, 
                      condition = c("vdem_polyarchy_lag1", 
                                       "reddpolicy_change_lag1"),
                 draw = TRUE, vcov = "HAC")
interaction_plm_m4 + 
  labs(x = "Level of democracy (t-1)", 
       y = "Predicted forest area (% of land area)") +
  scale_color_discrete(name = "REDD+ policy change (t-1)", 
                       labels = c("0", "1"))


#### Appendix A ####

# Table A.1 
test1 <- plm(forest_percentage ~ reddpolicy_lag1_factor + 
               vdem_polyarchy_lag1,
             data = redd_data_final, 
             index = c("country", "year"), 
             model = "within", 
             effect = "twoways", na.action = "na.exclude")

test2 <- plm(forest_percentage ~ reddpolicy_lag1_factor + 
               vdem_polyarchy_lag1 + 
               log_gdp_lag1 + sq_log_gdp_lag1,
             data = redd_data_final, 
             index = c("country", "year"), 
             model = "within", 
             effect = "twoways", na.action = "na.exclude")

test3 <- plm(forest_percentage ~ reddpolicy_lag1_factor + 
               vdem_polyarchy_lag1 + 
               log_gdp_lag1 + sq_log_gdp_lag1 +
               corruption_lag1,
             data = redd_data_final, 
             index = c("country", "year"), 
             model = "within", 
             effect = "twoways", na.action = "na.exclude")

test4 <- plm(forest_percentage ~ reddpolicy_lag1_factor + 
               vdem_polyarchy_lag1 + 
               log_gdp_lag1 + sq_log_gdp_lag1 +
               corruption_lag1 + 
               log_pop_density_lag1,
             data = redd_data_final, 
             index = c("country", "year"), 
             model = "within", 
             effect = "twoways", na.action = "na.exclude")

test5 <- plm(forest_percentage ~ reddpolicy_lag1_factor + 
               vdem_polyarchy_lag1 + 
               log_gdp_lag1 + sq_log_gdp_lag1 +
               corruption_lag1 + 
               log_pop_density_lag1 +
               dev_mean_temperature_lag1,
             data = redd_data_final, 
             index = c("country", "year"), 
             model = "within", 
             effect = "twoways", na.action = "na.exclude")

# Calculating robust standard errors
test1_clustered <- vcovHC(test1, 
                          method = "arellano", type = "HC1", cluster = "group")
summary(test1, vcov = test1_clustered)
se_test1 <- sqrt(diag(test1_clustered))

test2_clustered <- vcovHC(test2, 
                          method = "arellano", type = "HC1", cluster = "group")
summary(test2, vcov = test2_clustered)
se_test2 <- sqrt(diag(test2_clustered))

test3_clustered <- vcovHC(test3, 
                          method = "arellano", type = "HC1", cluster = "group")
summary(test3, vcov = test3_clustered)
se_test3 <- sqrt(diag(test3_clustered))

test4_clustered <- vcovHC(test4, 
                          method = "arellano", type = "HC1", cluster = "group")
summary(test4, vcov = test4_clustered)
se_test4 <- sqrt(diag(test4_clustered))

test5_clustered <- vcovHC(test5, 
                          method = "arellano", type = "HC1", cluster = "group")
summary(test5, vcov = test5_clustered)
se_test5 <- sqrt(diag(test5_clustered))

# Combining the results
stargazer(test1, test2, test3, test4, test5, type = "text",
          se = list(se_test1,
                    se_test2,
                    se_test3,
                    se_test4,
                    se_test5),
          notes = "Include country and year fixed effects
          abd clustered standard errors", out = "testmodels.html")

# Table A.2 and A.3
bmr_m1 <- plm(forest_percentage ~ reddpolicy_lag1_factor + 
                bmr_regime_lag1 + 
                log_gdp_lag1 + sq_log_gdp_lag1 +
                corruption_lag1 + 
                log_pop_density_lag1 + 
                dev_mean_temperature_lag1,
              data = redd_data_final, 
              index = c("country", "year"), 
              model = "within", 
              effect = "twoways", na.action = "na.exclude")
stargazer(bmr_m1, type = "text")

bmr_m2 <- plm(forest_percentage ~ 
                reddpolicy_lag1_factor*bmr_regime_lag1 + 
                log_gdp_lag1 + sq_log_gdp_lag1 +
                corruption_lag1 +
                log_pop_density_lag1 +
                dev_mean_temperature_lag1,
              data = redd_data_final, 
              index = c("country", "year"), 
              model = "within", 
              effect = "twoways", na.action = "na.exclude")
stargazer(bmr_m2, type = "text")

bmr_m3 <- plm(forest_percentage ~ 
                reddpolicy_change_lag1 + bmr_regime_lag1 + 
                log_gdp_lag1 + sq_log_gdp_lag1 +
                corruption_lag1 +
                log_pop_density_lag1 +
                dev_mean_temperature_lag1,
              data = redd_data_final, 
              index = c("country", "year"), 
              model = "within", 
              effect = "twoways", na.action = "na.exclude")
stargazer(bmr_m3, type = "text")

bmr_m4 <- plm(forest_percentage ~ 
                reddpolicy_change_lag1*bmr_regime_lag1 + 
                log_gdp_lag1 + sq_log_gdp_lag1 +
                corruption_lag1 +
                log_pop_density_lag1 +
                dev_mean_temperature_lag1,
              data = redd_data_final, 
              index = c("country", "year"), 
              model = "within", 
              effect = "twoways", na.action = "na.exclude")
stargazer(bmr_m4, type = "text")

# Calculating clustered standard errors for plm models
bmr_m1_clustered <- vcovHC(bmr_m1, 
                           method = "arellano", type = "HC1", cluster = "group")
summary(bmr_m1, vcov = bmr_m1_clustered)
se_bmr_m1 <- sqrt(diag(bmr_m1_clustered))

bmr_m2_clustered <- vcovHC(bmr_m2, 
                           method = "arellano", type = "HC1", cluster = "group")
summary(bmr_m2, vcov = bmr_m2_clustered)
se_bmr_m2 <- sqrt(diag(bmr_m2_clustered))

bmr_m3_clustered <- vcovHC(bmr_m3, 
                           method = "arellano", type = "HC1", cluster = "group")
summary(bmr_m3, vcov = bmr_m3_clustered)
se_bmr_m3 <- sqrt(diag(bmr_m3_clustered))

bmr_m4_clustered <- vcovHC(bmr_m4, 
                           method = "arellano", type = "HC1", cluster = "group")
summary(bmr_m4, vcov = bmr_m4_clustered)
se_bmr_m4 <- sqrt(diag(bmr_m4_clustered))

# Combining the results
stargazer(bmr_m1, bmr_m2, 
          type = "text", 
          se = list(se_bmr_m1,
                    se_bmr_m2),
          notes = c("Include country and year fixed effects", 
                    "Clustered standard errors"),
          out = "bmrm1and2.html")

stargazer(bmr_m3, bmr_m4, 
          type = "text", 
          se = list(se_bmr_m3,
                    se_bmr_m4),
          notes = c("Include country and year fixed effects", 
                    "Clustered standard errors"),
          out = "bmrm3and4.html")

# Table A.4
cooks_lm_m1 <- cooks.distance(lm_m1)
sum(!is.na(cooks.distance(lm_m1)))
# 524
coefficients(lm_m1)
# 8 excluding the coefficients of the fixed effects
limit.valuelm_m1 <- 4/(524-8-1)
criticalobs_lm_m1 <- which(cooks.distance(lm_m1)>limit.valuelm_m1)       
lm_m1_without_outliers <- 
  redd_data_final[-c(90:92, 95:99, 134, 178, 187, 244, 255,
                     264, 354, 508, 516, 517, 563, 564, 565, 
                     569, 570,
                     571, 572, 739, 740, 747, 748),]

cooks_lm_m2 <- cooks.distance(lm_m2)
sum(!is.na(cooks.distance(lm_m2)))
# 524
coefficients(lm_m2)
# 8
limit.valuelm_m2 <- 4/(524-8-1)
criticalobs_lm_m2 <- which(cooks.distance(lm_m2)>limit.valuelm_m2)       
criticalobs_lm_m2
lm_m2_without_outliers <- 
  redd_data_final[-c(90:92, 95:90, 178, 244, 252, 255,
                     264, 354, 516, 517,
                     563:565, 569:572, 
                     739, 740, 746:748),]

# Running models without outliers
m1_no_outliers <- plm(forest_percentage ~ reddpolicy_lag1_factor + 
                        vdem_polyarchy_lag1 + 
                        log_gdp_lag1 + sq_log_gdp_lag1 +
                        corruption_lag1 + 
                        log_pop_density_lag1 + 
                        dev_mean_temperature_lag1,
                      data = lm_m1_without_outliers, 
                      index = c("country", "year"), 
                      model = "within", 
                      effect = "twoways", na.action = "na.exclude")
summary(m1_no_outliers)

m2_no_outliers <- plm(forest_percentage ~ 
                        reddpolicy_lag1_factor*vdem_polyarchy_lag1 + 
                        log_gdp_lag1 + sq_log_gdp_lag1 +
                        corruption_lag1 + 
                        log_pop_density_lag1 + 
                        dev_mean_temperature_lag1,
                      data = lm_m2_without_outliers, 
                      index = c("country", "year"), 
                      model = "within", 
                      effect = "twoways", na.action = "na.exclude")
summary(m2_no_outliers)

# Calculate clustered standard errors              
m1_no_outliers_clustered <- vcovHC(m1_no_outliers, 
                                   method = "arellano", type = "HC1", cluster = "group")
summary(m1_no_outliers, vcov = m1_no_outliers_clustered)
se_m1_no_outliers <- sqrt(diag(m1_no_outliers_clustered))

m2_no_outliers_clustered <- 
  vcovHC(m2_no_outliers, 
         method = "arellano", type = "HC1", cluster = "group")
summary(m2_no_outliers, vcov = m2_no_outliers_clustered)
se_m2_no_outliers <- sqrt(diag(m2_no_outliers_clustered))

# Combining the results
stargazer(m1_no_outliers, m2_no_outliers, 
          type = "text", 
          se = list(se_m1_no_outliers,
                    se_m2_no_outliers),
          notes = c("Include country and year fixed effects", 
                    "Clustered standard errors"),
          out = "nooutliersm1and2.html")

# Table A.5 
cooks_lm_m3 <- cooks.distance(lm_m3)
sum(!is.na(cooks.distance(lm_m3)))
# 598
coefficients(lm_m3)
# 7
limit.valuelm_m3 <- 4/(598-7-1)
criticalobs_lm_m3 <- which(cooks.distance(lm_m3)>limit.valuelm_m3)       
criticalobs_lm_m3
lm_m3_without_outliers <- 
  redd_data_final[-c(90:92, 95:99, 178, 244, 255,
                     264, 352, 354, 365, 366, 373, 374,
                     486, 487, 495, 508,
                     516, 517, 563:565, 569:572, 
                     739, 734, 746, 747, 748),]

cooks_lm_m4 <- cooks.distance(lm_m4)
sum(!is.na(cooks.distance(lm_m4)))
# 599
coefficients(lm_m4)
# 8
limit.valuelm_m4 <- 4/(598-8-1)
criticalobs_lm_m4 <- which(cooks.distance(lm_m4)>limit.valuelm_m4)       
criticalobs_lm_m4
lm_m4_without_outliers <- 
  redd_data_final[-c(90:92, 95:99, 178, 179, 244, 264, 352,
                     354, 365, 366, 373, 374, 486, 487, 495, 
                     508, 509, 516, 517,
                     563:565, 569:572, 739, 740,
                     746, 747, 748),]

# Running models without outliers
m3_no_outliers <- plm(forest_percentage ~ reddpolicy_change_lag1 + 
                        vdem_polyarchy_lag1 + 
                        log_gdp_lag1 + sq_log_gdp_lag1 +
                        corruption_lag1 + 
                        log_pop_density_lag1 + 
                        dev_mean_temperature_lag1,
                      data = lm_m3_without_outliers, 
                      index = c("country", "year"), 
                      model = "within", 
                      effect = "twoways", na.action = "na.exclude")
summary(m3_no_outliers)

m4_no_outliers <- plm(forest_percentage ~ 
                        reddpolicy_change_lag1*vdem_polyarchy_lag1 + 
                        log_gdp_lag1 + sq_log_gdp_lag1 +
                        corruption_lag1 + 
                        log_pop_density_lag1 + 
                        dev_mean_temperature_lag1,
                      data = lm_m4_without_outliers, 
                      index = c("country", "year"), 
                      model = "within", 
                      effect = "twoways", na.action = "na.exclude")
summary(m4_no_outliers)

# Calculate clustered standard errors
m3_no_outliers_clustered <- vcovHC(m3_no_outliers, 
                                   method = "arellano", type = "HC1", cluster = "group")
summary(m3_no_outliers, vcov = m3_no_outliers_clustered)
se_m3_no_outliers <- sqrt(diag(m3_no_outliers_clustered))

m4_no_outliers_clustered <- vcovHC(m4_no_outliers, 
                                   method = "arellano", type = "HC1", cluster = "group")
summary(m4_no_outliers, vcov = m4_no_outliers_clustered)
se_m4_no_outliers <- sqrt(diag(m4_no_outliers_clustered))

# Combining the results
stargazer(m3_no_outliers, m4_no_outliers, 
          type = "text", 
          se = list(se_m3_no_outliers,
                    se_m4_no_outliers),
          notes = c("Include country and year fixed effects", 
                    "Clustered standard errors"),
          out = "nooutliersm3and4.html")

# Table A.6
t1 <- glm(reddpolicy_change ~ forest_percentage_lag1 + 
            vdem_polyarchy_lag1 + 
            log_gdp_lag1 + sq_log_gdp_lag1 +
            corruption_lag1 + 
            log_pop_density_lag1 +
            dev_mean_temperature_lag1 +
            as.factor(country) + as.factor(year),
          data = redd_data_final,
          na.action = "na.exclude",
          family = binomial ("logit"))

plm_t2 <- plm(reddpolicy ~ forest_percentage_lag1 + 
                vdem_polyarchy_lag1 + 
                log_gdp_lag1 + sq_log_gdp_lag1 +
                corruption_lag1 + 
                log_pop_density_lag1 +
                dev_mean_temperature_lag1, 
              data = redd_data_final, 
              index = c("country", "year"), 
              model = "within", 
              effect = "twoways", na.action = "na.exclude")
stargazer(plm_t2, type = "text") 

lm_t2 <- lm(reddpolicy ~ forest_percentage_lag1 + 
              vdem_polyarchy_lag1 + 
              log_gdp_lag1 + sq_log_gdp_lag1 +
              corruption_lag1 + 
              log_pop_density_lag1 +
              dev_mean_temperature_lag1 +
              as.factor(year) + as.factor(country), 
            data = redd_data_final, 
            na.action = "na.exclude")
summary(lm_t2)

# Calculate robust standard errors
t1_cluster <- miceadds::glm.cluster(reddpolicy_change ~ 
                                      forest_percentage_lag1 + 
                                      vdem_polyarchy_lag1 + 
                                      log_gdp_lag1 + sq_log_gdp_lag1 +
                                      corruption_lag1 + 
                                      log_pop_density_lag1 +
                                      dev_mean_temperature_lag1 + 
                                      as.factor(country) +
                                      as.factor(year), 
                                    data = redd_data_final,
                                    cluster = "country",
                                    family = binomial ("logit"))

se_t1 <- sqrt(diag(vcov(t1_cluster))) 
se_t1

plm_t2_cluster <- vcovHC(plm_t2, 
                         method = "arellano", type = "HC1", 
                         cluster = "group")
summary(plm_t2, vcov = plm_t2_cluster)
se_plm_t2 <- sqrt(diag(plm_t2_cluster))

# Combining the results
stargazer(t1, plm_t2, type = "text", se = list(se_t1, se_plm_t2),
          keep = 
            c("reddpolicy_change", "reddpolicy", "forest_percentage_lag1",
              "vdem_polyarchy_lag1", "log_gdp_lag1",
              "corruption_lag1", "log_pop_density_lag1", 
              "dev_mean_temperature_lag1"),
          out = "testmodel1.html")

#### Appendix B ####

# Table B.1
summary_variables <- 
  psych::describe(redd_data_final[c("forest_percentage", 
                                    "reddpolicy_lag1_factor",
                                    "reddpolicy_change_lag1",
                                    "vdem_polyarchy_lag1",
                                    "log_gdp_lag1",
                                    "sq_log_gdp_lag1",
                                    "corruption_lag1",
                                    "log_pop_density_lag1",
                                    "dev_mean_temperature_lag1")])
summary_variables
stargazer(summary_variables, type = "text",
          summary = FALSE, out = "summarystatistics.html")


# Figure B.1
cor_data <- redd_data_final[c("forest_percentage",
                              "reddpolicy_lag1",
                              "reddpolicy_change_lag1",
                              "vdem_polyarchy_lag1", 
                              "corruption_lag1",
                              "log_gdp_lag1",
                              "sq_log_gdp_lag1",
                              "dev_mean_temperature_lag1", 
                              "log_pop_density_lag1")]
new_names <- c("Forest area", 
               "REDD+ policy",
               "REDD+ change",
               "Democracy", 
               "Corruption",
               "Ln GDP",
               "Ln GDP^2",
               "Dev. mean temp.",
               "Ln pop. density") 
colnames(cor_data) <- new_names
view(cor_data)
cor_matrix <- cor(cor_data, use = "complete.obs")
cor_matrix
corrplot(cor_matrix, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

# Figure B.2
hist(redd_data_final$gdp_ppp_current, main =
       "",
     xlab = 
       "GDP per capita", ylab = "Observations",
     col = "lightblue")

hist(redd_data_final$log_gdp_lag1, main =
       "", 
     xlab = 
       "Ln GDP per capita (t-1)", ylab = "Observations",
     col = "lightblue")

hist(redd_data_final$pop_density, main =
       "", xlab = 
       "Population density", ylab = "Observations",
     col = "lightblue")

hist(redd_data_final$log_pop_density_lag1, main =
       "", 
     xlab = 
       "Ln population density (t-1)", ylab = "Observations",
     col = "lightblue")

# Figure B.3
hist(redd_data_final$forest_percentage, main = 
       "", xlab = 
       "Forest area (as percentage of land area)", 
     ylab = "Observations", 
     col = "lightblue")

hist(redd_data_final$reddpolicy_lag1, main = 
       "", xlab = 
       "REDD+ policy (t-1)", 
     ylab = "Observations", col = "lightblue", xlim = c(0,2))

hist(redd_data_final$reddpolicy_change_lag1, main = 
       "", xlab = 
       "REDD+ policy change (t-1)", 
     ylab = "Observations", col = "lightblue", xlim = c(0,2))

hist(redd_data_final$vdem_polyarchy_lag1, main =
       "", 
     xlab = 
       "Level of democracy", ylab = "Observations",
     col = "lightblue")

hist(redd_data_final$log_gdp_lag1, main =
       "", 
     xlab = 
       "Ln GDP per capita (t-1)", ylab = "Observations",
     col = "lightblue")

hist(redd_data_final$sq_log_gdp_lag1, main =
       "", xlab = 
       "Ln GDP per capita, squared", ylab = "Observations",
     col = "lightblue")

hist(redd_data_final$corruption_lag1, main =
       "", xlab = 
       "Control of corruption", ylab = "Observations",
     col = "lightblue")

hist(redd_data_final$log_pop_density_lag1, main =
       "", 
     xlab = 
       "Ln population density (t-1)", ylab = "Observations",
     col = "lightblue")

hist(redd_data_final$dev_mean_temperature_lag1, main =
       "", xlab = 
       "Deviation from mean temperature (t-1)", 
     ylab = "Observations",
     col = "lightblue")

#### Appendix C ####

# Figure C.1
ggplot(redd_data_final) + 
  geom_point(aes(y = forest_percentage, x = reddpolicy_lag1)) +
  geom_smooth(aes(y = forest_percentage, x = reddpolicy_lag1), 
              se = FALSE) + 
  labs(x = "REDD+ policy (t-1)", 
       y = "Forest area (% of land area)")
  
ggplot(redd_data_final) + 
  geom_point(aes(y = forest_percentage, x = reddpolicy_change_lag1)) +
  geom_smooth(aes(y = forest_percentage, x = reddpolicy_change_lag1), 
              se = FALSE) + 
  labs(x = "REDD+ policy change (t-1)", 
       y = "Forest area (% of land area)")

ggplot(redd_data_final) + 
  geom_point(aes(y = forest_percentage, x = vdem_polyarchy_lag1)) +
  geom_smooth(aes(y = forest_percentage, x = vdem_polyarchy_lag1), 
              se = FALSE) + 
  labs(x = "Level of democracy (t-1)", 
       y = "Forest area (% of land area)")

ggplot(redd_data_final) + 
  geom_point(aes(y = forest_percentage, x = log_gdp_lag1)) +
  geom_smooth(aes(y = forest_percentage, x = log_gdp_lag1), 
              se = FALSE) + 
  labs(x = "Ln GDP per capita (t-1)", 
       y = "Forest area (% of land area)")

ggplot(redd_data_final) + 
  geom_point(aes(y = forest_percentage, x = corruption_lag1)) +
  geom_smooth(aes(y = forest_percentage, x = corruption_lag1), 
              se = FALSE) + 
  labs(x = "Control of corruption (t-1)", 
       y = "Forest area (% of land area)")

ggplot(redd_data_final) + 
  geom_point(aes(y = forest_percentage, x = log_pop_density_lag1)) +
  geom_smooth(aes(y = forest_percentage, x = log_pop_density_lag1), 
              se = FALSE) + 
  labs(x = "Ln population density (t-1)", 
       y = "Forest area (% of land area)")

ggplot(redd_data_final) + 
  geom_point(aes(y = forest_percentage, x = dev_mean_temperature_lag1)) +
  geom_smooth(aes(y = forest_percentage, x = dev_mean_temperature_lag1), 
              se = FALSE) + 
  labs(x = "Ln population density (t-1)", 
       y = "Forest area (% of land area)")

# Figure C.2
ggplot() +
  geom_histogram(aes(x = rstandard(lm_m1),
                     y = ..density..)) + 
  stat_function(fun = dnorm, 
                color = "goldenrod2") +
  xlab("Standardized Residuals (m1)") + 
  ylab("Density")

ggplot() +
  geom_histogram(aes(x = rstandard(lm_m2),
                     y = ..density..)) + 
  stat_function(fun = dnorm, 
                color = "goldenrod2") +
xlab("Standardized Residuals (m2)")  +
  ylab("Density")

# Figure C.3
ggplot() +
  geom_histogram(aes(x = rstandard(lm_m3),
                     y = ..density..)) + 
  stat_function(fun = dnorm, 
                color = "goldenrod2") +
  xlab("Standardized Residuals (m3)") + 
  ylab("Density")

ggplot() +
  geom_histogram(aes(x = rstandard(lm_m4),
                     y = ..density..)) + 
  stat_function(fun = dnorm, 
                color = "goldenrod2") +
  xlab("Standardized Residuals (m4)") + 
  ylab("Density")

# Table C.1
vif_m1 <- lm(forest_percentage ~ 
                reddpolicy_lag1_factor + vdem_polyarchy_lag1 + 
                log_gdp_lag1 + sq_log_gdp_lag1 +
                corruption_lag1 + 
                log_pop_density_lag1 + 
                dev_mean_temperature_lag1,
   data = redd_data_final, na.action = na.exclude)

# Exporting the results
vif_table1 <- data.frame(
  VIF = vif_test_m1)
vif_table1
stargazer(vif_table1, type = "text", summary = FALSE, 
          out = "viftable1.html")

# Table C.2
vif_m2 <- lm(forest_percentage ~ 
                   reddpolicy_lag1_factor*vdem_polyarchy_lag1 + 
                   log_gdp_lag1 + sq_log_gdp_lag1 +
                   corruption_lag1 + 
                   log_pop_density_lag1 + 
                   dev_mean_temperature_lag1,
                 data = redd_data_final, na.action = na.exclude)

# Exporting the results
vif_table2 <- data.frame(
  VIF = vif_test_m2)
vif_table2
stargazer(vif_table2, type = "text", summary = FALSE, 
          out = "viftable2.html")

# Table C.3
vif_m3 <- lm(forest_percentage ~ 
                   reddpolicy_change_lag1 + vdem_polyarchy_lag1 + 
                   log_gdp_lag1 + sq_log_gdp_lag1 +
                   corruption_lag1 + 
                   log_pop_density_lag1 + 
                   dev_mean_temperature_lag1,
                 data = redd_data_final, na.action = na.exclude)

# Exporting the results
vif_table3 <- data.frame(
  VIF = vif_test_m3)
stargazer(vif_table3, type = "text", summary = FALSE, 
          out = "viftable3.html")

# Table C.4
vif_m4 <- lm(forest_percentage ~ 
               reddpolicy_change_lag1*vdem_polyarchy_lag1 + 
               log_gdp_lag1 + sq_log_gdp_lag1 +
               corruption_lag1 + 
               log_pop_density_lag1 + 
               dev_mean_temperature_lag1,
             data = redd_data_final, na.action = na.exclude)

# Exporting the results
vif_table4 <- data.frame(
  VIF = vif_test_m4)
stargazer(vif_table4, type = "text", summary = FALSE, 
          out = "viftable4.html")

# Table C.5 
country_names_outliersm1 <- redd_data_final$country[criticalobs_lm_m1]
country_names_outliersm1
outliers_table_m1 <- data.frame(Outlier 
                                = criticalobs_lm_m1, 
                        country = country_names_outliersm1)
stargazer(outliers_table_m1, type = "text",
          summary = FALSE, out = "outliersm1.html")

# Figure C.4
plot(cooks_lm_m1, pch = 19, xlab = "Observation Index", 
     ylab = "Cook's D", main = "Cook's D Outliers")
points(criticalobs_lm_m1, cooks_lm_m1[criticalobs_lm_m1],
       pch = 19, col = "red")
text(criticalobs_lm_m1, cooks_lm_m1[criticalobs_lm_m1], 
     labels = country_names_outliersm1, pos = 4, 
     col = "red", cex = 0.8)

# Table C.6
country_names_outliersm2 <- redd_data_final$country[criticalobs_lm_m2]
country_names_outliersm2
outliers_table_m2 <- data.frame(Outlier 
                                = criticalobs_lm_m2, 
                country = country_names_outliersm2)
stargazer(outliers_table_m2, type = "text",
          summary = FALSE, out = "outliersm2.html")

# Figure C.5
plot(cooks_lm_m2, pch = 19, xlab = "Observation Index", 
     ylab = "Cook's D", main = "Cook's D Outliers")
points(criticalobs_lm_m2, cooks_lm_m2[criticalobs_lm_m2],
       pch = 19, col = "red")
text(criticalobs_lm_m2, cooks_lm_m2[criticalobs_lm_m2], 
     labels = country_names_outliersm2, pos = 4, 
     col = "red", cex = 0.8)

# Table C.7
country_names_outliersm3 <- redd_data_final$country[criticalobs_lm_m3]
country_names_outliersm3
outliers_table_m3 <- data.frame(Outlier 
                                = criticalobs_lm_m3, 
                        country = country_names_outliersm3)
stargazer(outliers_table_m3, type = "text",
          summary = FALSE, out = "outliersm3.html")

# Figure C.6
plot(cooks_lm_m3, pch = 19, xlab = "Observation Index", 
     ylab = "Cook's D", main = "Cook's D Outliers")
points(criticalobs_lm_m3, cooks_lm_m3[criticalobs_lm_m3],
       pch = 19, col = "red")
text(criticalobs_lm_m3, cooks_lm_m3[criticalobs_lm_m3], 
     labels = country_names_outliersm3, pos = 4, 
     col = "red", cex = 0.8)

# Table C.8
country_names_outliersm4 <- redd_data_final$country[criticalobs_lm_m4]
country_names_outliersm4
outliers_table_m4 <- data.frame(Outlier 
    = criticalobs_lm_m4, country = country_names_outliersm4)
stargazer(outliers_table_m4, type = "text",
          summary = FALSE, out = "outliersm4.html")

# Figure C.7
plot(cooks_lm_m4, pch = 19, xlab = "Observation Index", 
     ylab = "Cook's D", main = "Cook's D Outliers")
points(criticalobs_lm_m4, cooks_lm_m4[criticalobs_lm_m4],
       pch = 19, col = "red")
text(criticalobs_lm_m4, cooks_lm_m4[criticalobs_lm_m4], 
     labels = country_names_outliersm2, pos = 4, 
     col = "red", cex = 0.8)

#### Appendix D ####

# Table D.1
# I constructed the table manually in Word, 
# adding these values (odds ratio)
exp(-0.90)
exp(9.08)
exp(11.64)
exp(-0.46)
exp(-5.85)
exp(-1.76)
exp(0.26)

# Table D.2
plm_m1_data <- redd_data_final[c("country", "year",
                                 "forest_percentage",
                                 "corruption_lag1",
                                 "vdem_polyarchy_lag1", 
                                 "log_gdp_lag1", "sq_log_gdp_lag1",
                                 "dev_mean_temperature_lag1",
                                 "reddpolicy_lag1_factor",
                                 "log_pop_density_lag1")]
view(plm_m1_data)
plm_m1_completerows <- complete.cases(plm_m1_data)
plm_m1_complete <- plm_m1_data[plm_m1_completerows,]

# Table D.3
plm_m3_data <- redd_data_final[c("country", "year",
                                 "forest_percentage",
                                 "corruption_lag1",
                                 "vdem_polyarchy_lag1", 
                                 "log_gdp_lag1", "sq_log_gdp_lag1",
                                 "dev_mean_temperature_lag1",
                                 "reddpolicy_change_lag1",
                                 "log_pop_density_lag1")]
view(plm_m3_data)
rows_with_missing <- apply(is.na(plm_m3_data), 1, any)
plm_m3_completerows <- complete.cases(plm_m3_data)
plm_m3_complete <- plm_m3_data[plm_m3_completerows,]
