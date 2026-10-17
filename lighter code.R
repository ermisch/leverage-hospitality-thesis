library(readxl)
library(plm)
library(lmtest)
library(sandwich)
library(dplyr)
library(tidyr)
library(car)

# Load and prepare data
data <- read_excel("C:/Users/ehond/Downloads/filtered_data_final123.xlsx")

# After loading your data, rename ALL columns
names(data)[names(data) == "Global Company Key"] <- "FIRM_ID"
names(data)[names(data) == "Current ISO Country Code - Incorporation"] <- "COUNTRY"
names(data)[names(data) == "Data Year - Fiscal"] <- "YEAR"
names(data)[names(data) == "GIC Sub-Industries"] <- "SUBINDUSTRY"
names(data)[names(data) == "WINSORIZED ROA"] <- "ROA"
names(data)[names(data) == "WINSORIZED  ROE"] <- "ROE"
names(data)[names(data) == "WINSORIZED  STDEVROA"] <- "RISK"
names(data)[names(data) == "WINSORIZED TLEV"] <- "TLEV"
names(data)[names(data) == "WINSORIZED  TLEV^2"] <- "TLEV_SQ"
names(data)[names(data) == "WINSORIZED SIZE"] <- "SIZE"
names(data)[names(data) == "WINSORIZED  CASH"] <- "CASH"
names(data)[names(data) == "WINSORIZED  D/E"] <- "DE_RATIO"
names(data)[names(data) == "WINSORIZED  SALES GROWTH"] <- "SALES"
names(data)[names(data) == "WINSORIZED INTEREST COVERAGE"] <- "INTEREST_COVERAGE"
names(data)[names(data) == "WINSORIZED TANGIBILITY"] <- "TANGIBILITY"
names(data)[names(data) == "GDP Growth"] <- "GDP"

data <- data %>% 
  arrange(FIRM_ID, YEAR) %>%
  group_by(FIRM_ID) %>%
  mutate(
    LAG_ROA_1 = lag(ROA, 1), LAG_ROA_2 = lag(ROA, 2), LAG_ROA_3 = lag(ROA, 3),
    LAG_ROE_1 = lag(ROE, 1), LAG_ROE_2 = lag(ROE, 2), LAG_ROE_3 = lag(ROE, 3),
    COVID = ifelse(YEAR %in% c(2020, 2021), 1, 0),
    TLEV_COVID = TLEV * COVID
  ) %>%
  ungroup()

# Remove duplicates and convert to panel data
data <- data[!duplicated(data[, c("FIRM_ID", "YEAR")]), ]
panel_data <- pdata.frame(data, index = c("FIRM_ID", "YEAR"))
# SECTION 1: ROA ANALYSIS
#=============================================================================

# 1. Pooled OLS Models for ROA
pooled_base_roa <- plm(ROA ~ TLEV + SIZE + RISK + SALES + CASH + GDP + COVID + 
                         factor(YEAR) + factor(SUBINDUSTRY),
                       data = panel_data, model = "pooling")

pooled_interact_roa <- plm(ROA ~ TLEV + SIZE + RISK + SALES + CASH + GDP + COVID + 
                             TLEV_COVID + factor(YEAR) + factor(SUBINDUSTRY),
                           data = panel_data, model = "pooling")

# 2. Fixed Effects Models for ROA
fe_base_roa <- plm(ROA ~ TLEV + SIZE + RISK + SALES + CASH + GDP + COVID + 
                     factor(YEAR), data = panel_data, model = "within")

fe_interact_roa <- plm(ROA ~ TLEV + SIZE + RISK + SALES + CASH + GDP + COVID + 
                         TLEV_COVID + factor(YEAR), data = panel_data, model = "within")

# 3. Hausman Test for ROA
re_model_roa <- plm(ROA ~ TLEV + SIZE + RISK + SALES + CASH + GDP + COVID + TLEV_COVID + factor(YEAR),
                    data = panel_data, model = "random")
re_model_roa2 <- plm(ROA ~ TLEV + SIZE + RISK + SALES + CASH + GDP + TLEV_COVID + factor(YEAR),
                     data = panel_data, model = "random")
fe_simple_roa <- plm(ROA ~ TLEV + SIZE + RISK + SALES + CASH + GDP + COVID + TLEV_COVID + factor(YEAR),
                     data = panel_data, model = "within")
hausman_test_roa <- phtest(fe_simple_roa, re_model_roa)
summary(re_model_roa2)
print(hausman_test_roa)

# 4. Non-Linear Fixed Effects (Quadratic) for ROA
quad_fe_roa <- plm(ROA ~ TLEV + TLEV_SQ + SIZE + RISK + SALES + CASH + GDP + COVID + 
                     factor(YEAR), data = panel_data, model = "within")

# 5. GMM for ROA with reduced instrument count
firm_obs_count <- table(panel_data$FIRM_ID)
firms_to_keep <- names(firm_obs_count[firm_obs_count >= 5])
filtered_panel <- panel_data[panel_data$FIRM_ID %in% firms_to_keep, ]
gmm_data_roa <- filtered_panel

# SECTION 2: ROE ANALYSIS
#=============================================================================

# 1. Pooled OLS Models for ROE
pooled_base_roe <- plm(ROE ~ TLEV + SIZE + RISK + SALES + CASH + GDP + COVID + 
                         factor(YEAR) + factor(SUBINDUSTRY),
                       data = panel_data, model = "pooling")

pooled_interact_roe <- plm(ROE ~ TLEV + SIZE + RISK + SALES + CASH + GDP + COVID + 
                             TLEV_COVID + factor(YEAR) + factor(SUBINDUSTRY),
                           data = panel_data, model = "pooling")

# 2. Fixed Effects Models for ROE
fe_base_roe <- plm(ROE ~ TLEV + SIZE + RISK + SALES + CASH + GDP + COVID + 
                     factor(YEAR), data = panel_data, model = "within")

fe_interact_roe <- plm(ROE ~ TLEV + SIZE + RISK + SALES + CASH + GDP + COVID + 
                         TLEV_COVID + factor(YEAR), data = panel_data, model = "within")

# 3. Hausman Test for ROE
re_model_roe <- plm(ROE ~ TLEV + SIZE + RISK + SALES + CASH + GDP + COVID + TLEV_COVID + factor(YEAR),
                    data = panel_data, model = "random")
fe_simple_roe <- plm(ROE ~ TLEV + SIZE + RISK + SALES + CASH + GDP + COVID + TLEV_COVID + factor(YEAR),
                     data = panel_data, model = "within")
hausman_test_roe <- phtest(fe_simple_roe, re_model_roe)

# 4. Non-Linear Fixed Effects (Quadratic) for ROE
quad_fe_roe <- plm(ROE ~ TLEV + TLEV_SQ + SIZE + RISK + SALES + CASH + GDP + COVID + 
                     factor(YEAR), data = panel_data, model = "within")

# 5. GMM for ROE


# Calculate robust standard errors
models_robust <- list(
  pooled_base_roa = coeftest(pooled_base_roa, vcov = vcovHC(pooled_base_roa, type = "HC1")),
  pooled_interact_roa = coeftest(pooled_interact_roa, vcov = vcovHC(pooled_interact_roa, type = "HC1")),
  fe_base_roa = coeftest(fe_base_roa, vcov = vcovHC(fe_base_roa, type = "HC1")),
  fe_interact_roa = coeftest(fe_interact_roa, vcov = vcovHC(fe_interact_roa, type = "HC1")),
  quad_fe_roa = coeftest(quad_fe_roa, vcov = vcovHC(quad_fe_roa, type = "HC1")),
  pooled_base_roe = coeftest(pooled_base_roe, vcov = vcovHC(pooled_base_roe, type = "HC1")),
  pooled_interact_roe = coeftest(pooled_interact_roe, vcov = vcovHC(pooled_interact_roe, type = "HC1")),
  fe_base_roe = coeftest(fe_base_roe, vcov = vcovHC(fe_base_roe, type = "HC1")),
  fe_interact_roe = coeftest(fe_interact_roe, vcov = vcovHC(fe_interact_roe, type = "HC1")),
  quad_fe_roe = coeftest(quad_fe_roe, vcov = vcovHC(quad_fe_roe, type = "HC1"))
)
# Additional analyses
vif_roa <- vif(lm(ROA ~ TLEV + TLEV_SQ + SIZE + RISK + SALES + CASH + GDP + COVID, data = panel_data))
vif_roe <- vif(lm(ROE ~ TLEV + TLEV_SQ + SIZE + RISK + SALES + CASH + GDP + COVID, data = panel_data))

ind_year_wide <- data %>%
  group_by(SUBINDUSTRY, YEAR) %>%
  summarise(firm_years = n(), .groups = "drop") %>%
  pivot_wider(names_from = YEAR, values_from = firm_years, values_fill = 0)

cat("\nIndustry-Year Distribution (Wide Format):\n")
print(ind_year_wide)

# Observations per firm
obs_per_firm <- as.numeric(table(panel_data$FIRM_ID))
cat("\nSummary of Observations Per Firm:\n")
print(summary(obs_per_firm)) 

system_gmm_roa_v2 <- try({
  pgmm(ROA ~ lag(ROA, 1) + TLEV + TLEV_SQ + SIZE + GDP + COVID | 
         lag(ROA, 2:3) + lag(TLEV, 2:3) + lag(TLEV_SQ, 2:3) + 
         lag(SIZE, 2:2) + GDP + COVID,
       data = gmm_data_roa,
       effect = "individual",
       model = "twosteps",         # Windmeijer-corrected robust SE
       transformation = "ld",      # First-differencing (default)
       system = TRUE,
       collapse = TRUE
  )
}, silent = TRUE)

summary(system_gmm_roa_v2)
system_gmm_roa_max <- try({
  pgmm(ROA ~ lag(ROA, 1) + TLEV + TLEV_SQ + SIZE + GDP + COVID | 
         lag(ROA, 2:4) + lag(TLEV, 2:4) + lag(TLEV_SQ, 2:4) + 
         lag(SIZE, 2:3) + GDP + COVID,
       data = filtered_panel,
       effect = "individual",
       model = "twosteps",
       transformation = "ld",
       system = TRUE)  # Removed collapse = TRUE to maximize instruments
}, silent = TRUE)
summary(system_gmm_roa_max)

library(plm)
library(lmtest)
library(sandwich)

# VERSION A: Balanced instrument approach with collapse=TRUE
system_gmm_roe_improved_A <- pgmm(
  ROE ~ lag(ROE, 1) + TLEV + TLEV_SQ + SIZE + GDP + COVID | 
    lag(ROE, 2:3) + lag(TLEV, 2:3) + lag(TLEV_SQ, 2:3) + 
    lag(SIZE, 1:2) + GDP + COVID,
  data = filtered_panel,
  effect = "individual",
  model = "twosteps",
  transformation = "ld",
  system = TRUE,
  collapse = TRUE
)

# VERSION B: Longer lag exclusion to reduce endogeneity
system_gmm_roe_improved_B <- pgmm(
  ROE ~ lag(ROE, 1) + TLEV + TLEV_SQ + SIZE + GDP + COVID | 
    lag(ROE, 3:4) + lag(TLEV, 3:4) + lag(TLEV_SQ, 3:4) + 
    lag(SIZE, 2:2) + GDP + COVID,
  data = filtered_panel,
  effect = "individual",
  model = "twosteps",
  transformation = "ld",
  system = TRUE,
  collapse = TRUE
)

summary(system_gmm_roe_improved_B)
summary(system_gmm_roa_max)
