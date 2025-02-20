library(tidyverse)
library(kableExtra)
library(osem)
library(ggtext)
library(modelsummary)

country <- "DE"

load(paste0("./small_examples/IJF/", country, "/model_sel_hicp_ar.RData"))
hicp_ar <- model_result_ext_sel
load(paste0("./small_examples/IJF/", country, "/model_sel.RData"))
hicp <- model_result_ext_sel
rm(model_result_ext_sel)

# create forecasts
hicp_ar_fc <- forecast_model(hicp_ar, , exog_fill_method = "auto")
hicp_fc <- forecast_model(hicp, , exog_fill_method = "auto")

# extract values
# inflation module should be the same between the models
inf1 <- hicp$module_collection %>% filter(dependent == "Inflation") %>% pull(model) %>% pluck(1)
inf2 <- hicp_ar$module_collection %>% filter(dependent == "Inflation") %>% pull(model) %>% pluck(1)
waldo::compare(inf1, inf2, tolerance = 0.000000001) # why number of estimations vastly different? same result though...
rm(inf1, inf2)
# extract actual inflation data and inflation module
inf_inf <- hicp_fc$full_forecast_data %>%
  filter(na_item == "Inflation") %>%
  select(time, na_item, values, type) %>%
  mutate(type = case_when(type == "Forecast" ~ "Forecast_Inflation",
                          type == "Insample Fit" ~ "Insample_Inflation",
                          type == "Observation" ~ "Data")) %>%
  rename(Inflation = values) %>%
  select(-na_item) %>%
  arrange(type, time) %>%
  drop_na()
# extract HICP AR model
inf_hicp_ar <- hicp_ar_fc$full_forecast_data %>%
  filter(na_item == "HICPlocal") %>%
  select(time, values, type) %>%
  rename(HICP = values) %>%
  arrange(type, time) %>%
  drop_na() %>%
  group_by(type) %>%
  mutate(Inflation = (HICP - lag(HICP, 1)) / lag(HICP, 1) * 100) %>%
  mutate(type = case_when(type == "Forecast" ~ "Forecast_HICP_AR",
                          type == "Insample Fit" ~ "Insample_HICP_AR",
                          type == "Observation" ~ "Data"))
# extract HICP AR-X model
inf_hicp <- hicp_fc$full_forecast_data %>%
  filter(na_item == "HICPlocal") %>%
  select(time, values, type) %>%
  rename(HICP = values) %>%
  # already have data from other model
  filter(type != "Observation") %>%
  arrange(type, time) %>%
  drop_na() %>%
  group_by(type) %>%
  mutate(Inflation = (HICP - lag(HICP, 1)) / lag(HICP, 1) * 100) %>%
  mutate(type = case_when(type == "Forecast" ~ "Forecast_HICP_ARX",
                          type == "Insample Fit" ~ "Insample_HICP_ARX",
                          type == "Observation" ~ "Data"))
# combine data
# actual data
inf_comparison <- full_join(x = inf_inf %>% filter(type == "Data"), y = inf_hicp_ar %>% select(time, type, HICP) %>% filter(type == "Data"), by = c("time", "type")) %>%
  select(time, type, HICP, Inflation) %>%
  arrange(type, time)
# inflation model (has HICP NA)
inf_comparison <- bind_rows(inf_comparison, inf_inf %>% filter(type != "Data"))
# HICP AR model
inf_comparison <- bind_rows(inf_comparison, inf_hicp_ar %>% filter(type != "Data"))
# HICP AR-X model
inf_comparison <- bind_rows(inf_comparison, inf_hicp %>% filter(type != "Data"))

# make model a different variable
inf_comparison <- inf_comparison %>%
  separate(type, into = c("Type", "Model"), sep = "_", extra = "merge", fill = "right") %>%
  mutate(Model = case_when(is.na(Model) ~ "Actual",
                           TRUE ~ Model))

ggplot(inf_comparison) +
  geom_line(aes(x = time, y = Inflation, color = Model), linewidth = 1, alpha = 0.7) +
  geom_vline(aes(x = time), xintercept = as.Date("2024-10-01"), linetype = "dashed") -> inf_plot

ggplot(inf_comparison) +
  geom_line(aes(x = time, y = HICP, color = Model), linewidth = 1, alpha = 0.7) +
  geom_vline(aes(x = time), xintercept = as.Date("2024-10-01"), linetype = "dashed") -> hicp_plot

library(cowplot)
plot_grid(inf_plot, hicp_plot, labels = c("Inflation", "HICP"), ncol = 1)







fix_model <- hicp$module_collection %>% filter(dependent == "HICPlocal") %>% pull(model) %>% pluck(1)
library(gets)
arx_model <- as.arx(fix_model)
regressors <- fix_model$aux$mX
depvar <- fix_model$aux$y
?predict
predict(fix_model, newdata = regressors)


