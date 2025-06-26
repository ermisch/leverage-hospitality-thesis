library(readxl)
library(plm)
library(lmtest)
library(sandwich)
library(dplyr)
library(tidyr)
library(car)

data <- read_excel("C:/Users/ehond/Downloads/filtered_data_final123.xlsx")
names(data)
names(data)[names(data) == "Global Company Key"]<- "FIRM_ID"
names(data)[names(data) == "Current ISO Country Code - Incorporation"]<- "COUNTRY"
names(data)[names(data) == "Data Year - Fiscal"]<- "YEAR"
names(data)[names(data) == "GIC Sub-Industries"]<- "SUBINDUSTRY"
names(data)[names(data) == "WINSORIZED ROA"]<- "ROA"
names(data)[names(data) == "WINSORIZED  ROE"]<- "ROE"
names(data)[names(data) == "WINSORIZED  STDEVROA"]<- "RISK"
names(data)[names(data) == "WINSORIZED TLEV"]<- "TLEV"
names(data)[names(data) == "WINSORIZED  TLEV^2"]<- "TLEVSQ"
names(data)[names(data) == "WINSORIZED SIZE"]<- "SIZE"
names(data)[names(data) == "WINSORIZED  CASH"]<- "CASH"
names(data)[names(data) == "WINSORIZED  D/E"]<- "DE_RATIO"
names(data)[names(data) == "WINSORIZED  SALES GROWTH"]<- "SALES"
names(data)[names(data) == "WINSORIZED INTEREST COVERAGE"]<- "INTEREST_COVERAGE"
names(data)[names(data) == "WINSORIZED TANGIBILITY"]<- "TANGIBILITY"
names(data)[names(data) == "GDP Growth"]<- "GDP"
names(data)[names(data) == "WINSORIZED SLEV"] <- "SLEV"
names(data)[names(data) == "WINSORIZED LLEV"] <- "LLEV"

#################Data preperation
data <- data %>%
  arrange(FIRM_ID, YEAR) %>%
  group_by(FIRM_ID) %>%
  mutate(COVID = ifelse(YEAR %in% c(2020, 2021), 1, 0)) %>%
  ungroup() %>%
  mutate(TLEV_C = TLEV - mean(TLEV, na.rm = TRUE),SIZE_C = SIZE,TLEV_SQ = TLEV_C^2,TLEV_COVID = TLEV_C * COVID,
  TLEV_SQ_COVID = TLEV_SQ * COVID,DE_RATIO_C = DE_RATIO - mean(DE_RATIO, na.rm = TRUE),DE_RATIO_SQ = DE_RATIO_C^2,
  DE_RATIO_COVID = DE_RATIO_C * COVID,DE_RATIO_COVID_SQ= DE_RATIO_SQ * COVID,LLEV_C = LLEV - mean(LLEV, na.rm = TRUE),
  SLEV_C = TLEV - mean(SLEV, na.rm = TRUE),LLEV_SQ = LLEV_C^2,LLEV_COVID = LLEV_C * COVID,
  LLEV_SQ_COVID = LLEV_SQ * COVID,SLEV_SQ = SLEV_C^2,SLEV_COVID = SLEV_C * COVID,SLEV_SQ_COVID = SLEV_SQ * COVID)

lmbasecentered <- lm(ROA ~ TLEV_C + TLEV_SQ + SIZE_C + RISK + SALES + CASH + GDP + COVID,data = data)
vif(lmbasecentered)
lmbaseuncentered <- lm(ROA ~ TLEV + I(TLEV^2) + SIZE + RISK + SALES + CASH + GDP + COVID,data = data)
vif(lmbaseuncentered)

df$TLEV_group <- cut(df$TLEV, breaks = quantile(df$TLEV, probs = seq(0, 1, 0.25), na.rm = TRUE), include.lowest = TRUE)
boxplot(ROA ~ TLEV_group, data = df, main = "ROA across Leverage Groups",xlab = "Leverage Quartiles",
        ylab = "ROA", col = "lightblue")

boxplot(ROE ~ TLEV_group, data = df, main = "ROE across Leverage Groups",
        xlab = "Leverage Quartiles", ylab = "ROE", col = "lightgreen")
library(ggplot2)

ggplot(df, aes(x = TLEV, y = ROA)) +
  geom_point(alpha = 0.4, color = "blue") +
  geom_smooth(method = "loess", se = FALSE, color = "darkred", size = 1.2) +
  labs(title = "ROA vs Total Leverage", x = "Total Leverage", y = "ROA") 

ggplot(df, aes(x = TLEV, y = ROE)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  geom_smooth(method = "loess", se = FALSE, color = "darkorange", size = 1.2) +
  labs(title = "ROE vs Total Leverage", x = "Total Leverage", y = "ROE")

filtered_panel$SUBINDUSTRY25301010 <- ifelse(filtered_panel$SUBINDUSTRY == "25301010", 1, 0)
filtered_panel$SUBINDUSTRY25301030 <- ifelse(filtered_panel$SUBINDUSTRY == "25301030", 1, 0)
filtered_panel$SUBINDUSTRY25301040 <- ifelse(filtered_panel$SUBINDUSTRY == "25301040", 1, 0)

data <- data[!duplicated(data[, c("FIRM_ID", "YEAR")]), ]
panel_data <- pdata.frame(data, index = c("FIRM_ID", "YEAR"))
summary(panel_data)
panel_data
firm_counts <- as.data.frame(table(panel_data$FIRM_ID))
names(firm_counts) <- c("FIRM_ID", "count")
valid_firms <- firm_counts$FIRM_ID[firm_counts$count >= 4]
filtered_panel <- panel_data[panel_data$FIRM_ID %in% valid_firms, ]
filtered_panel <- pdata.frame(filtered_panel, index = c("FIRM_ID", "YEAR"))

#################Pooled OlS ROA models
  
pooledbaseroa <- plm(
  ROA ~ TLEV_C + SIZE_C + RISK + SALES + CASH + GDP + factor(SUBINDUSTRY) + factor(YEAR),
  data = panel_data, model = "pooling")

pooledquadroa <- plm(
  ROA ~ TLEV_C + TLEV_SQ + SIZE_C + RISK + SALES + CASH + GDP + factor(SUBINDUSTRY) + factor(YEAR),
  data = panel_data, model = "pooling")
pooled_interact_roa <- plm(
  ROA ~ TLEV_C + TLEV_COVID + SIZE_C + RISK + SALES + CASH + GDP + factor(SUBINDUSTRY) + factor(YEAR),
  data = panel_data, model = "pooling")

pooledinteractroa2 <- plm(
  ROA ~ TLEV_C + TLEV_SQ + TLEV_COVID + TLEV_SQ_COVID + SIZE_C + RISK + SALES + CASH + GDP + factor(SUBINDUSTRY) +factor(YEAR),
  data = panel_data, model = "pooling")

################Pooled OLS ROE models
  
pooledbaseroe <- plm(
  ROE ~ TLEV_C + SIZE_C + RISK + SALES + CASH + GDP + factor(SUBINDUSTRY) + factor(YEAR),
  data = panel_data, model = "pooling")

pooledquadroe <- plm(
  ROE ~ TLEV_C + TLEV_SQ + SIZE_C + RISK + SALES + CASH + GDP + factor(SUBINDUSTRY) + factor(YEAR),
  data = panel_data, model = "pooling")
summary(pooled_quad_roe)
pooledinteractroa <- plm(
  ROE ~ TLEV_C + TLEV_SQ + TLEV_COVID + SIZE_C + RISK + SALES + CASH + GDP + factor(SUBINDUSTRY) + factor(YEAR),
  data = panel_data, model = "pooling")
pooledinteractroe2 <- plm(
  ROE ~ TLEV_C + TLEV_SQ + TLEV_COVID + TLEV_SQ_COVID + SIZE_C + RISK + SALES + CASH + GDP + factor(SUBINDUSTRY) + factor(YEAR),
  data = panel_data, model = "pooling")

#########Fixed-Effects ROA models
  
febaseroa <- plm(
  ROA ~ TLEV_C + SIZE_C + RISK + SALES + CASH + GDP + factor(SUBINDUSTRY) +factor(YEAR),
  data = panel_data, model = "within")


fequadroa <- plm(
  ROA ~ TLEV_C + TLEV_SQ + SIZE_C + RISK + SALES + CASH + GDP + factor(SUBINDUSTRY) +factor(YEAR),
  data = panel_data, model = "within")

feinteractroa <- plm(
  ROA ~ TLEV_C + TLEV_SQ + TLEV_COVID + SIZE_C + RISK + SALES + CASH + GDP + factor(SUBINDUSTRY) +factor(YEAR), 
  data = panel_data, model = "within")

feinteractroa2 <- plm(
  ROA ~ TLEV_C + TLEV_SQ + TLEV_COVID + TLEV_SQ_COVID + SIZE_C + RISK + SALES + CASH + GDP + factor(SUBINDUSTRY) +factor(YEAR),
  data = panel_data, model = "within")


remodelroa <- plm(
  ROA ~ TLEV_C + TLEV_SQ + TLEV_COVID + TLEV_SQ_COVID + SIZE_C + RISK + SALES + CASH + GDP + factor(SUBINDUSTRY) +factor(YEAR),
  data = panel_data, model = "random"
)
hausmantestroa <- phtest(fe_interact_roa, re_model_roa)
print(hausman_test_roa)

##############Fixed effects ROE models########
  
febaseroe <- plm(
  ROE ~ TLEV_C + SIZE_C + RISK + SALES + CASH + GDP + factor(SUBINDUSTRY) + factor(YEAR),
  data = panel_data, model = "within")
summary(fe_base_roe)
fequadroe <- plm(
  ROE ~ TLEV_C + TLEV_SQ + SIZE_C + RISK + SALES + CASH + GDP + factor(SUBINDUSTRY) + factor(YEAR),
  data = panel_data, model = "within")

feinteractroe <- plm(
  ROE ~ TLEV_C + TLEV_SQ + TLEV_COVID + SIZE_C + RISK + SALES + CASH + GDP + factor(SUBINDUSTRY) + factor(YEAR), 
  data = panel_data, model = "within")

feinteractroe2 <- plm(
  ROE ~ TLEV_C + TLEV_SQ + TLEV_COVID + TLEV_SQ_COVID + SIZE_C + RISK + SALES + CASH + GDP + factor(SUBINDUSTRY) + factor(YEAR),
  data = panel_data, model = "within")

remodelroe <- plm(
  ROE ~ TLEV_C + TLEV_SQ + TLEV_COVID + TLEV_SQ_COVID + SIZE_C + RISK + SALES + CASH + GDP + factor(SUBINDUSTRY),
  data = panel_data, model = "random")

hausmantestroe <- phtest(feinteractroe, remodelroe)
print(hausmantestroe)

#########Results ROA#######
  
coeftest(pooledbaseroa, vcov = vcovHC(pooledbaseroa, type = "HC1", cluster = "group"))
cat("R-squared:", summary(pooledbaseroa)$r.squared[2], "\n")



coeftest(pooledquadroa, vcov = vcovHC(pooledquadroa, type = "HC1", cluster = "group"))
cat("Adjusted R-squared:", summary(pooledquadroa)$r.squared[2], "\n")


coeftest(pooledinteractroa2, vcov = vcovHC(pooledinteractroa2, type = "HC1", cluster = "group"))
cat("Adjusted R-squared:", summary(pooledinteractroa2)$r.squared[2], "\n")


coeftest(febaseroa, vcov = vcovHC(febaseroa, type = "HC1", cluster = "group"))
cat("Adjusted R-squared:", summary(febaseroa)$r.squared[2], "\n")

coeftest(fequadroa, vcov = vcovHC(fequadroa, type = "HC1", cluster = "group"))
cat("Adjusted R-squared:", summary(fequadroa)$r.squared[2], "\n")

coeftest(feinteractroa2, vcov = vcovHC(feinteractroa2, type = "HC1", cluster = "group"))
cat("Adjusted R-squared:", summary(feinteractroa2)$r.squared[2], "\n")

#######Results ROE######

coeftest(pooledbaseroe, vcov = vcovHC(pooledbaseroe, type = "HC1", cluster = "group"))
cat("Adjusted R-squared:", summary(pooledbaseroe)$r.squared[2], "\n")

coeftest(pooledquadroe, vcov = vcovHC(pooledquadroe, type = "HC1", cluster = "group"))
cat("Adjusted R-squared:", summary(pooledquadroe)$r.squared[2], "\n")


coeftest(pooledinteractroe2, vcov = vcovHC(pooledinteractroe2, type = "HC1", cluster = "group"))
cat("Adjusted R-squared:", summary(pooledinteractroe2)$r.squared[2], "\n")

coeftest(febaseroe, vcov = vcovHC(febaseroe, type = "HC1", cluster = "group"))
cat("Adjusted R-squared:", summary(febaseroe)$r.squared[2], "\n")

coeftest(fequadroe, vcov = vcovHC(fequadroe, type = "HC1", cluster = "group"))
cat("Adjusted R-squared:", summary(fequadroe)$r.squared[2], "\n")

coeftest(feinteractroe2, vcov = vcovHC(feinteractroe2, type = "HC1", cluster = "group"))
cat("Adjusted R-squared:", summary(feinteractroe2)$r.squared[2], "\n")

##############System GMM ROA#####


filtered_panel$SUBINDUSTRY25301010 <- ifelse(filtered_panel$SUBINDUSTRY == "25301010", 1, 0)
filtered_panel$SUBINDUSTRY25301030 <- ifelse(filtered_panel$SUBINDUSTRY == "25301030", 1, 0)
filtered_panel$SUBINDUSTRY25301040 <- ifelse(filtered_panel$SUBINDUSTRY == "25301040", 1, 0)



gmm_final1 <- pgmm(ROA ~ lag(ROA, 1) + TLEV_C + TLEV_SQ + SIZE + RISK + SALES + CASH + GDP +
SUBINDUSTRY25301010 + SUBINDUSTRY25301030 + SUBINDUSTRY25301040| lag(ROA, 2:4) + lag(TLEV_C, 2:4) + lag(TLEV_SQ, 2:4) +
lag(RISK, 3:4) + lag(CASH, 3:4) + lag(SIZE_C, 3:4) + lag(SALES, 3:4),
data = filtered_panel,effect = "twoways",model = "twosteps",transformation = "ld",collapse = TRUE,
robust = TRUE,system = TRUE)

gmm_final2 <- pgmm(ROA ~ lag(ROA, 1) + TLEV_C + SIZE + RISK + SALES + CASH + GDP +
SUBINDUSTRY25301010 + SUBINDUSTRY25301030 + SUBINDUSTRY25301040|lag(ROA, 2:4) + lag(TLEV_C, 2:4) + 
lag(RISK, 3:4) + lag(CASH, 3:4) + lag(SIZE_C, 3:4) + lag(SALES, 3:4),
data = filtered_panel,effect = "twoways",model = "twosteps",transformation = "ld",collapse = TRUE,
robust = TRUE,system = TRUE)

gmm_interaction <- pgmm(ROA ~ lag(ROA, 1) + TLEV_C + TLEV_COVID + SIZE + RISK + SALES + CASH + GDP +
SUBINDUSTRY25301010 + SUBINDUSTRY25301030 + SUBINDUSTRY25301040|lag(ROA, 2:4) + lag(TLEV_C, 2:4) + 
lag(TLEV_COVID, 2:4) +lag(RISK, 3:4) + lag(CASH, 3:4) + lag(SIZE_C, 3:4) + lag(SALES, 3:4),
data = filtered_panel,effect = "twoways",model = "twosteps",transformation = "ld",collapse = TRUE,
robust = TRUE,system = TRUE)

gmm_interaction2 <- pgmm(
  ROA ~ lag(ROA, 1) + TLEV_C + TLEV_SQ + TLEV_COVID + TLEV_SQ_COVID + SIZE + RISK + SALES + CASH + GDP + 
SUBINDUSTRY25301010 + SUBINDUSTRY25301030 + SUBINDUSTRY25301040|lag(ROA, 2:4) + lag(TLEV_C, 3:4) + 
lag(TLEV_SQ, 3:4) + lag(TLEV_COVID, 3:4) + lag(TLEV_SQ_COVID, 3:4) +lag(RISK, 3:4) + lag(CASH, 3:4) + 
lag(SIZE_C, 3:4) + lag(SALES, 3:4),data = filtered_panel, effect = "twoways",model = "twosteps",transformation = "ld",
collapse = TRUE,robust = TRUE,system = TRUE)

summary(gmm_final1, diagnostics = TRUE)
summary(gmm_final2, diagnostics = TRUE)
summary(gmm_interaction, diagnostics = TRUE)
summary(gmm_interaction2, diagnostics = TRUE)

#############System GMM ROE#########

gmm_final1roe <- pgmm(ROE ~ lag(ROE, 1) + TLEV_C + TLEV_SQ + SIZE + RISK + SALES + CASH + GDP +
SUBINDUSTRY25301010 + SUBINDUSTRY25301030 + SUBINDUSTRY25301040| lag(ROE, 2:4) + lag(TLEV_C, 2:4) 
+ lag(TLEV_SQ, 2:3) + lag(RISK, 2:3) + lag(CASH, 2:3) + lag(SIZE_C, 2:3) + lag(SALES, 2:3),
data = filtered_panel,effect = "twoways",model = "twosteps",transformation = "ld",
collapse = TRUE,robust = TRUE,system = TRUE)


gmm_final2roe <- pgmm(ROE ~ lag(ROE, 1) + TLEV_C + SIZE + RISK + SALES + CASH + GDP +
SUBINDUSTRY25301010 + SUBINDUSTRY25301030 + SUBINDUSTRY25301040|lag(ROE, 2:4) + lag(TLEV_C, 3:4) + 
lag(RISK, 3:4) + lag(CASH, 3:4) + lag(SIZE_C, 3:4) + lag(SALES, 3:4),
data = filtered_panel,effect = "twoways",model = "twosteps",transformation = "ld",collapse = TRUE,
robust = TRUE,system = TRUE)

gmm_interactionroe <- pgmm(ROE ~ lag(ROE, 1) + TLEV_C + TLEV_COVID + SIZE + RISK + SALES + CASH + GDP +
SUBINDUSTRY25301010 + SUBINDUSTRY25301030 + SUBINDUSTRY25301040|lag(ROE, 2:4) + lag(TLEV_C, 2:4) + 
lag(TLEV_COVID, 3:4) +lag(RISK, 3:4) + lag(CASH, 3:4) + lag(SIZE_C, 3:4) + lag(SALES, 3:4),
data = filtered_panel,effect = "twoways",model = "twosteps",transformation = "ld",
collapse = TRUE,robust = TRUE,system = TRUE)

gmm_interaction2roe <- pgmm(ROE ~ lag(ROE, 1) + TLEV_C + TLEV_SQ + TLEV_COVID + TLEV_SQ_COVID + 
SIZE + RISK + SALES + CASH + GDP + SUBINDUSTRY25301010 + SUBINDUSTRY25301030 + SUBINDUSTRY25301040|
lag(ROE, 2:3) + lag(TLEV_C, 2:3) + lag(TLEV_SQ, 2:4) + lag(TLEV_COVID, 2:3) + lag(TLEV_SQ_COVID, 2:3) +
lag(RISK, 2:3) + lag(CASH, 2:3) + lag(SIZE_C, 2:3) + lag(SALES, 2:3),
data = filtered_panel,effect = "twoways",model = "twosteps",transformation = "ld",
collapse = TRUE,robust = FALSE,system = TRUE)

summary(gmm_final1roe, diagnostics = TRUE)
summary(gmm_final2roe, diagnostics = TRUE)
summary(gmm_interactionroe, diagnostics = TRUE)
summary(gmm_interaction2roe, diagnostics = TRUE)



###############Robustness######

gmm_llev_interaction2 <- pgmm(ROA ~ lag(ROA, 1) + LLEV_C + LLEV_SQ + LLEV_COVID + LLEV_SQ_COVID + SIZE + RISK
+ SALES + CASH + GDP + SUBINDUSTRY25301010 + SUBINDUSTRY25301030 + SUBINDUSTRY25301040 |
lag(ROA, 2:4) + lag(LLEV_C, 3:4) + lag(LLEV_SQ, 3:4) + lag(LLEV_COVID, 3:4) + lag(LLEV_SQ_COVID, 3:4) +
lag(RISK, 3:4) + lag(CASH, 3:4) + lag(SIZE, 3:4) + lag(SALES, 3:4),data = filtered_panel,
effect = "twoways",model = "twosteps",transformation = "ld",
collapse = TRUE,robust = TRUE,system = TRUE)

summary(gmm_llev_interaction2, diagnostics = TRUE)

gmm_slev_interaction2 <- pgmm(ROA ~ lag(ROA, 1) + SLEV_C + SLEV_SQ + SLEV_COVID + SLEV_SQ_COVID + SIZE + RISK + 
SALES + CASH + GDP + SUBINDUSTRY25301010 + SUBINDUSTRY25301030 + SUBINDUSTRY25301040 |
lag(ROA, 2:4) + lag(SLEV_C, 3:4) + lag(SLEV_SQ, 3:4) + lag(SLEV_COVID, 3:4) + lag(SLEV_SQ_COVID, 3:4) +
lag(RISK, 3:4) + lag(CASH, 3:4) + lag(SIZE, 3:4) + lag(SALES, 3:4),
data = filtered_panel,effect = "twoways",model = "twosteps",transformation = "ld",collapse = TRUE,
robust = TRUE,system = TRUE)
summary(gmm_slev_interaction2, diagnostics = TRUE)

gmm_slev_interaction2_roe <- pgmm(ROE ~ lag(ROE, 1) + SLEV_C + SLEV_SQ + SLEV_COVID + SLEV_SQ_COVID + SIZE + RISK + SALES + CASH + GDP +
SUBINDUSTRY25301010 + SUBINDUSTRY25301030 + SUBINDUSTRY25301040 |lag(ROE, 2:4) + lag(SLEV_C, 3:4) + lag(SLEV_SQ, 3:4) + 
lag(SLEV_COVID, 3:4) + lag(SLEV_SQ_COVID, 3:4) +lag(RISK, 3:4) + lag(CASH, 3:4) + lag(SIZE, 3:4) + lag(SALES, 3:4),
data = filtered_panel,effect = "twoways",model = "twosteps",transformation = "ld",collapse = TRUE,
robust = TRUE,system = TRUE)
summary(gmm_slev_interaction2_roe, diagnostics = TRUE)

gmm_llev_interaction2_roe <- pgmm(ROE ~ lag(ROE, 1) + LLEV_C + LLEV_SQ + LLEV_COVID + LLEV_SQ_COVID + SIZE + RISK 
+ SALES + CASH + GDP +SUBINDUSTRY25301010 + SUBINDUSTRY25301030 + SUBINDUSTRY25301040 |lag(ROE, 2:4) + 
lag(LLEV_C, 3:4) + lag(LLEV_SQ, 3:4) + lag(LLEV_COVID, 3:4) + lag(LLEV_SQ_COVID, 3:4) +lag(RISK, 3:4) + 
lag(CASH, 3:4) + lag(SIZE, 3:4) + lag(SALES, 3:4),data = filtered_panel,effect = "twoways",model = "twosteps",
transformation = "ld",collapse = TRUE,robust = TRUE,system = TRUE)
summary(gmm_llev_interaction2_roe, diagnostics = TRUE)

gmm_interaction2_lag23 <- pgmm(ROA ~ lag(ROA, 1) + TLEV_C + TLEV_SQ + TLEV_COVID + TLEV_SQ_COVID +
SIZE + RISK + SALES + CASH + GDP +SUBINDUSTRY25301010 + SUBINDUSTRY25301030 + SUBINDUSTRY25301040 |
lag(ROA, 2:3) + lag(TLEV_C, 2:3) + lag(TLEV_SQ, 2:3) + lag(TLEV_COVID, 2:3) + lag(TLEV_SQ_COVID, 2:3) +
lag(RISK, 3:4) + lag(CASH, 3:4) + lag(SIZE, 3:4) + lag(SALES, 3:4),
data = filtered_panel,effect = "twoways",model = "twosteps",transformation = "ld",
collapse = TRUE,robust = TRUE,system = TRUE)
summary(gmm_interaction2_lag23, diagnostics = TRUE)

gmm_interaction2_lag35 <- pgmm(ROA ~ lag(ROA, 1) + TLEV_C + TLEV_SQ + TLEV_COVID + TLEV_SQ_COVID +
SIZE + RISK + SALES + CASH + GDP +SUBINDUSTRY25301010 + SUBINDUSTRY25301030 + SUBINDUSTRY25301040 |
lag(ROA, 3:5) + lag(TLEV_C, 3:5) + lag(TLEV_SQ, 3:5) + lag(TLEV_COVID, 3:5) + lag(TLEV_SQ_COVID, 3:5) +
lag(RISK, 3:4) + lag(CASH, 3:4) + lag(SIZE, 3:4) + lag(SALES, 3:4),
data = filtered_panel,effect = "twoways",model = "twosteps",transformation = "ld",
collapse = TRUE,robust = TRUE,system = TRUE)
summary(gmm_interaction2_lag35, diagnostics = TRUE)


gmm_interaction2_lag23roe <- pgmm(ROE ~ lag(ROE, 1) + TLEV_C + TLEV_SQ + TLEV_COVID + TLEV_SQ_COVID +
SIZE + RISK + SALES + CASH + GDP +SUBINDUSTRY25301010 + SUBINDUSTRY25301030 + SUBINDUSTRY25301040 |
lag(ROA, 2:3) + lag(TLEV_C, 2:3) + lag(TLEV_SQ, 2:3) + lag(TLEV_COVID, 2:3) + lag(TLEV_SQ_COVID, 2:3) +lag(RISK, 3:4) + 
lag(CASH, 3:4) + lag(SIZE, 3:4) + lag(SALES, 3:4),data = filtered_panel,effect = "twoways",model = "twosteps",
transformation = "ld", collapse = TRUE,robust = TRUE,system = TRUE)
summary(gmm_interaction2_lag23roe, diagnostics = TRUE)

gmm_interaction2_lag35roe <- pgmm(ROE ~ lag(ROE, 1) + TLEV_C + TLEV_SQ + TLEV_COVID + TLEV_SQ_COVID +SIZE + RISK + 
SALES + CASH + GDP + SUBINDUSTRY25301010 + SUBINDUSTRY25301030 + SUBINDUSTRY25301040 |lag(ROA, 3:5) + lag(TLEV_C, 3:5) + 
lag(TLEV_SQ, 3:5) + lag(TLEV_COVID, 3:5) + lag(TLEV_SQ_COVID, 3:5) +lag(RISK, 3:4) + lag(CASH, 3:4) + lag(SIZE, 3:4)
+ lag(SALES, 3:4),data = filtered_panel,effect = "twoways",model = "twosteps",transformation = "ld",collapse = TRUE,
robust = TRUE,system = TRUE)
summary(gmm_interaction2_lag35roe, diagnostics = TRUE)



###############robustness without lagged
gmm_final1w <- pgmm(ROA ~ TLEV_C + TLEV_SQ + SIZE + RISK + SALES + CASH + GDP +
                     SUBINDUSTRY25301010 + SUBINDUSTRY25301030 + SUBINDUSTRY25301040|lag(TLEV_C, 2:4) + lag(TLEV_SQ, 2:4) +
                     lag(RISK, 3:4) + lag(CASH, 3:4) + lag(SIZE_C, 3:4) + lag(SALES, 3:4),
                   data = filtered_panel,effect = "twoways",model = "twosteps",transformation = "ld",collapse = TRUE,
                   robust = TRUE,system = TRUE)

gmm_final2w <- pgmm(ROA ~ TLEV_C + SIZE + RISK + SALES + CASH + GDP +
                     SUBINDUSTRY25301010 + SUBINDUSTRY25301030 + SUBINDUSTRY25301040|lag(TLEV_C, 2:4) + 
                     lag(RISK, 3:4) + lag(CASH, 3:4) + lag(SIZE_C, 3:4) + lag(SALES, 3:4),
                   data = filtered_panel,effect = "twoways",model = "twosteps",transformation = "ld",collapse = TRUE,
                   robust = TRUE,system = TRUE)

gmm_interactionw <- pgmm(ROA ~  TLEV_C + TLEV_COVID + SIZE + RISK + SALES + CASH + GDP +
                          SUBINDUSTRY25301010 + SUBINDUSTRY25301030 + SUBINDUSTRY25301040|lag(TLEV_C, 2:4) + 
                          lag(TLEV_COVID, 2:4) +lag(RISK, 3:4) + lag(CASH, 3:4) + lag(SIZE_C, 3:4) + lag(SALES, 3:4),
                        data = filtered_panel,effect = "twoways",model = "twosteps",transformation = "ld",collapse = TRUE,
                        robust = TRUE,system = TRUE)

gmm_interaction2w <- pgmm(
  ROA ~ TLEV_C + TLEV_SQ + TLEV_COVID + TLEV_SQ_COVID + SIZE + RISK + SALES + CASH + GDP + 
    SUBINDUSTRY25301010 + SUBINDUSTRY25301030 + SUBINDUSTRY25301040|lag(TLEV_C, 3:4) + 
    lag(TLEV_SQ, 3:4) + lag(TLEV_COVID, 3:4) + lag(TLEV_SQ_COVID, 3:4) +lag(RISK, 3:4) + lag(CASH, 3:4) + 
    lag(SIZE_C, 3:4) + lag(SALES, 3:4),data = filtered_panel, effect = "twoways",model = "twosteps",transformation = "ld",
  collapse = TRUE,robust = TRUE,system = TRUE)

summary(gmm_final1w, diagnostics = TRUE)
summary(gmm_final2w, diagnostics = TRUE)
summary(gmm_interactionw, diagnostics = TRUE)
summary(gmm_interaction2w, diagnostics = TRUE)

#############System GMM ROE#########

gmm_final1roew <- pgmm(ROE ~ TLEV_C + TLEV_SQ + SIZE + RISK + SALES + CASH + GDP +
                        SUBINDUSTRY25301010 + SUBINDUSTRY25301030 + SUBINDUSTRY25301040|lag(TLEV_C, 2:4) 
                      + lag(TLEV_SQ, 2:3) + lag(RISK, 2:3) + lag(CASH, 2:3) + lag(SIZE_C, 2:3) + lag(SALES, 2:3),
                      data = filtered_panel,effect = "twoways",model = "twosteps",transformation = "ld",
                      collapse = TRUE,robust = TRUE,system = TRUE)


gmm_final2roew <- pgmm(ROE ~ TLEV_C + SIZE + RISK + SALES + CASH + GDP +
                        SUBINDUSTRY25301010 + SUBINDUSTRY25301030 + SUBINDUSTRY25301040|lag(TLEV_C, 3:4) + 
                        lag(RISK, 3:4) + lag(CASH, 3:4) + lag(SIZE_C, 3:4) + lag(SALES, 3:4),
                      data = filtered_panel,effect = "twoways",model = "twosteps",transformation = "ld",collapse = TRUE,
                      robust = TRUE,system = TRUE)

gmm_interactionroew <- pgmm(ROE ~ TLEV_C + TLEV_COVID + SIZE + RISK + SALES + CASH + GDP +
                             SUBINDUSTRY25301010 + SUBINDUSTRY25301030 + SUBINDUSTRY25301040|lag(TLEV_C, 2:4) + 
                             lag(TLEV_COVID, 3:4) +lag(RISK, 3:4) + lag(CASH, 3:4) + lag(SIZE_C, 3:4) + lag(SALES, 3:4),
                           data = filtered_panel,effect = "twoways",model = "twosteps",transformation = "ld",
                           collapse = TRUE,robust = TRUE,system = TRUE)

gmm_interaction2roew <- pgmm(ROE ~ TLEV_C + TLEV_SQ + TLEV_COVID + TLEV_SQ_COVID + 
                              SIZE + RISK + SALES + CASH + GDP + SUBINDUSTRY25301010 + SUBINDUSTRY25301030 + SUBINDUSTRY25301040|
                           lag(TLEV_C, 2:3) + lag(TLEV_SQ, 2:4) + lag(TLEV_COVID, 2:3) + lag(TLEV_SQ_COVID, 2:3) +
                              lag(RISK, 2:3) + lag(CASH, 2:3) + lag(SIZE_C, 2:3) + lag(SALES, 2:3),
                            data = filtered_panel,effect = "twoways",model = "twosteps",transformation = "ld",
                            collapse = TRUE,robust = FALSE,system = TRUE)

summary(gmm_final1roew, diagnostics = TRUE)
summary(gmm_final2roew, diagnostics = TRUE)
summary(gmm_interactionroew, diagnostics = TRUE)
summary(gmm_interaction2roew, diagnostics = TRUE)














