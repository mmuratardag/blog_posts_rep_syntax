
library(vdemdata)
library(tidyverse)

data <- vdem # on 05 March 2021, this downloads v.10, which includes scores until 2019

d <- data %>% filter(year>=2010) %>% # this will need to be filtered to years 
                                     # 2010-2019 at later dates when the v.10 is updated
  select(country_name, year,
         v2x_polyarchy, v2x_libdem, v2x_partipdem, v2x_delibdem, v2x_egaldem) %>% 
  group_by(country_name) %>% mutate(Visit = 1:n())  %>% 
  gather("year",
         "v2x_polyarchy", "v2x_libdem", "v2x_partipdem", "v2x_delibdem", "v2x_egaldem",
         key = variable, value = number) %>% 
  unite(combi, variable, Visit) %>% 
  spread(combi, number) %>% select(country_name:v2x_polyarchy_9) %>%
  rename(delibdem_2010 = v2x_delibdem_1, delibdem_2011 = v2x_delibdem_2,
         delibdem_2012 = v2x_delibdem_3, delibdem_2013 = v2x_delibdem_4,
         delibdem_2014 = v2x_delibdem_5, delibdem_2015 = v2x_delibdem_6,
         delibdem_2016 = v2x_delibdem_7, delibdem_2017 = v2x_delibdem_8,
         delibdem_2018 = v2x_delibdem_9, delibdem_2019 = v2x_delibdem_10,
         egaldem_2010 = v2x_egaldem_1, egaldem_2011 = v2x_egaldem_2,
         egaldem_2012 = v2x_egaldem_3, egaldem_2013 = v2x_egaldem_4,
         egaldem_2014 = v2x_egaldem_5, egaldem_2015 = v2x_egaldem_6,
         egaldem_2016 = v2x_egaldem_7, egaldem_2017 = v2x_egaldem_8,
         egaldem_2018 = v2x_egaldem_9, egaldem_2019 = v2x_egaldem_10,
         libdem_2010 = v2x_libdem_1, libdem_2011 = v2x_libdem_2,
         libdem_2012 = v2x_libdem_3, libdem_2013 = v2x_libdem_4,
         libdem_2014 = v2x_libdem_5, libdem_2015 = v2x_libdem_6,
         libdem_2016 = v2x_libdem_7, libdem_2017 = v2x_libdem_8,
         libdem_2018 = v2x_libdem_9, libdem_2019 = v2x_libdem_10,
         partipdem_2010 = v2x_partipdem_1, partipdem_2011 = v2x_partipdem_2,
         partipdem_2012 = v2x_partipdem_3, partipdem_2013 = v2x_partipdem_4,
         partipdem_2014 = v2x_partipdem_5, partipdem_2015 = v2x_partipdem_6,
         partipdem_2016 = v2x_partipdem_7, partipdem_2017 = v2x_partipdem_8,
         partipdem_2018 = v2x_partipdem_9, partipdem_2019 = v2x_partipdem_10,
         polyarchy_2010 = v2x_polyarchy_1, polyarchy_2011 = v2x_polyarchy_2,
         polyarchy_2012 = v2x_polyarchy_3, polyarchy_2013 = v2x_polyarchy_4,
         polyarchy_2014 = v2x_polyarchy_5, polyarchy_2015 = v2x_polyarchy_6,
         polyarchy_2016 = v2x_polyarchy_7, polyarchy_2017 = v2x_polyarchy_8,
         polyarchy_2018 = v2x_polyarchy_9, polyarchy_2019 = v2x_polyarchy_10)

rm(data)

#define the cfa fit and get summary function
fit_cfa_and_summarize <- function(syntax) {
  fit <- lavaan::cfa(model = syntax, data=d, estimator="mlr", std.ov=T, std.lv=T, mimic="mplus", missing = "fiml")
  fit_summary <- summary(fit, fit.measures=T, standardized=T, rsquare=T)
  list_of_fit_and_summary <- list("model_fit" = fit, "model_fit_summary" = fit_summary)
  return(list_of_fit_and_summary)
}
# define the plot cfa function
# -- not used actually, is just for visual inspection, besides looking at the model summary numbers
plot_cfa <- function(a_cfa_model_output) {
  semPlot::semPaths(a_cfa_model_output, "mod", "std", intercepts = F, edge.label.cex=1.1, layout="tree3")
}


model_2010 <- "dem_2010 =~ delibdem_2010 + egaldem_2010 + libdem_2010 + partipdem_2010 + polyarchy_2010"
model_2010_output <- fit_cfa_and_summarize(model_2010)
#plot_cfa(model_2010_output$model_fit)

model_2011 <- "dem_2011 =~ delibdem_2011 + egaldem_2011 + libdem_2011 + partipdem_2011 + polyarchy_2011"
model_2011_output <- fit_cfa_and_summarize(model_2011)
#plot_cfa(model_2011_output$model_fit)

model_2012 <- "dem_2012 =~ delibdem_2012 + egaldem_2012 + libdem_2012 + partipdem_2012 + polyarchy_2012"
model_2012_output <- fit_cfa_and_summarize(model_2012)
#plot_cfa(model_2012_output$model_fit)

model_2013 <- "dem_2013 =~ delibdem_2013 + egaldem_2013 + libdem_2013 + partipdem_2013 + polyarchy_2013"
model_2013_output <- fit_cfa_and_summarize(model_2013)
#plot_cfa(model_2013_output$model_fit)

model_2014 <- "dem_2014 =~ delibdem_2014 + egaldem_2014 + libdem_2014 + partipdem_2014 + polyarchy_2014"
model_2014_output <- fit_cfa_and_summarize(model_2014)
#plot_cfa(model_2014_output$model_fit)

model_2015 <- "dem_2015 =~ delibdem_2015 + egaldem_2015 + libdem_2015 + partipdem_2015 + polyarchy_2015"
model_2015_output <- fit_cfa_and_summarize(model_2015)
#plot_cfa(model_2015_output$model_fit)

model_2016 <- "dem_2016 =~ delibdem_2016 + egaldem_2016 + libdem_2016 + partipdem_2016 + polyarchy_2016"
model_2016_output <- fit_cfa_and_summarize(model_2016)
#plot_cfa(model_2016_output$model_fit)

model_2017 <- "dem_2017 =~ delibdem_2017 + egaldem_2017 + libdem_2017 + partipdem_2017 + polyarchy_2017"
model_2017_output <- fit_cfa_and_summarize(model_2017)
#plot_cfa(model_2017_output$model_fit)

model_2018 <- "dem_2018 =~ delibdem_2018 + egaldem_2018 + libdem_2018 + partipdem_2018 + polyarchy_2018"
model_2018_output <- fit_cfa_and_summarize(model_2018)
#plot_cfa(model_2018_output$model_fit)

model_2019 <- "dem_2019 =~ delibdem_2019 + egaldem_2019 + libdem_2019 + partipdem_2019 + polyarchy_2019"
model_2019_output <- fit_cfa_and_summarize(model_2019)
#plot_cfa(model_2019_output$model_fit)

# par(mfrow=c(1,2))
# plot_cfa(model_2010_output$model_fit)
# title(sub = "Democracy CFA Model for 2010")
# plot_cfa(model_2019_output$model_fit)
# title(sub = "Democracy CFA Model for 2019")
# dev.off()

# define the get factor scores function
get_factor_scores <- function(fit_object) {
  factor_score_matrix <- lavaan::lavPredict(fit_object, type = "lv", method = "EBM", label = T, fsm = F)
  temp_data_frame <- as.data.frame(factor_score_matrix)
  return(temp_data_frame)
}
# define the normalize function to get scores ranging 0-1
normalize_factor_score <- function(variable_in_data_frame) {
  n_fs_vector <- scales::rescale(variable_in_data_frame, to = c(0, 1), from = range(variable_in_data_frame, na.rm = F, finite = T))
  return(n_fs_vector)
}


temp_DemSco_2010 <- get_factor_scores(model_2010_output$model_fit)
d$DemSco_2010 <- normalize_factor_score(temp_DemSco_2010$dem_2010)

temp_DemSco_2011 <- get_factor_scores(model_2011_output$model_fit)
d$DemSco_2011 <- normalize_factor_score(temp_DemSco_2011$dem_2011)

temp_DemSco_2012 <- get_factor_scores(model_2012_output$model_fit)
d$DemSco_2012 <- normalize_factor_score(temp_DemSco_2012$dem_2012)

temp_DemSco_2013 <- get_factor_scores(model_2013_output$model_fit)
d$DemSco_2013 <- normalize_factor_score(temp_DemSco_2013$dem_2013)

temp_DemSco_2014 <- get_factor_scores(model_2014_output$model_fit)
d$DemSco_2014 <- normalize_factor_score(temp_DemSco_2014$dem_2014)

temp_DemSco_2015 <- get_factor_scores(model_2015_output$model_fit)
d$DemSco_2015 <- normalize_factor_score(temp_DemSco_2015$dem_2015)

temp_DemSco_2016 <- get_factor_scores(model_2016_output$model_fit)
d$DemSco_2016 <- normalize_factor_score(temp_DemSco_2016$dem_2016)

temp_DemSco_2017 <- get_factor_scores(model_2017_output$model_fit)
d$DemSco_2017 <- normalize_factor_score(temp_DemSco_2017$dem_2017)

temp_DemSco_2018 <- get_factor_scores(model_2018_output$model_fit)
d$DemSco_2018 <- normalize_factor_score(temp_DemSco_2018$dem_2018)

temp_DemSco_2019 <- get_factor_scores(model_2019_output$model_fit)
d$DemSco_2019 <- normalize_factor_score(temp_DemSco_2019$dem_2019)

rm(list=setdiff(ls(), "d"))

d_wide_temp <- d %>% select(country_name, DemSco_2010:DemSco_2019) 
d_long <- gather(d_wide_temp, year, value , DemSco_2010:DemSco_2019)

d_long <- d_long %>% mutate(year_num = 
                              case_when(year == "DemSco_2010" ~ 2010,
                                        year == "DemSco_2011" ~ 2011,
                                        year == "DemSco_2012" ~ 2012,
                                        year == "DemSco_2013" ~ 2013,
                                        year == "DemSco_2014" ~ 2014,
                                        year == "DemSco_2015" ~ 2015,
                                        year == "DemSco_2016" ~ 2016,
                                        year == "DemSco_2017" ~ 2017,
                                        year == "DemSco_2018" ~ 2018,
                                        year == "DemSco_2019" ~ 2019)) %>%
  select(country_name, year_num, value) %>%
  rename(Country = country_name, Year = year_num, Democracy_Score = value)

dem_sco_alpl_sorted <- with(d_long,  d_long[order(Country), ])

rio::export(dem_sco_alpl_sorted, file = "DemocracyScores_lf.xlsx")
