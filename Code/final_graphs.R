# =============================================================================
# Sri Lanka PEA - Final Figures (World Bank Style)
# =============================================================================
#This script creates profiles using data from HIES 2016, HIES 2019 and 
#LFS imputed 2024

# Check intallation of required packages
packages <- c(
  "StatMatch", "survey", "questionr", "reldist", "glmnet", "useful",
  "data.table", "haven", "statar", "parallel", "foreach", "doParallel",
  "dplyr", "tidyr", "dineq", "convey", "renv", "transport", "ggridges",
  "ggplot2","forcats","scales","readxl","Hmisc","xgboost","matrixStats",
  "ggh4x"
)

# Load all packages
lapply(packages, require, character.only = TRUE)

codepath <- "C:/Users/wb553773/Github/LKA_S2S_main"
datapath <-"C:/Users/wb553773/WBG/Marta Schoch - Analysis/Data/"
dataout <-"C:/Users/wb553773/WBG/Marta Schoch - Analysis/Data/cleaned/Imputed Vectors/"
outpath <- "C:/Users/wb553773/WBG/Marta Schoch - Analysis/Out/s2s/"

# Parameters to convert vectors in 2019 prices to 2021 PPP
cpi21=0.88027848 #this is to convert to 2021PPPs
icp21=58.296108 #set up as in GMD

#HIES 2016
hies16=read_dta(paste(datapath,
  "cleaned/Harmonized/LKA_2016_HIES_v01_M_v07_A_SARMD_GMD.dta",sep="")) 
hies16 = hies16 |>
  mutate(welfare = welfare*(1/365)/.7729536/icp21,
        popwt = weight*hsize,
        industry_orig = as.character(industry_orig),
        industry_orig_year = as.character(industry_orig_year),
        industry_orig_2 = as.character(industry_orig_2),
        industry_orig_2_year = as.character(industry_orig_2_year))

#HIES 2019
hies19=read_dta(paste(datapath,
  "cleaned/Harmonized/LKA_2019_HIES_v01_M_v03_A_SARMD_GMD.dta",sep="")) 
hies19 = hies19 |>
  mutate(welfare = welfare*(12/365)/cpi21/icp21,
        popwt = weight*hsize,
    industry_orig = as.character(industry_orig),
    industry_orig_year = as.character(industry_orig_year),
    industry_orig_2 = as.character(industry_orig_2),
    industry_orig_2_year = as.character(industry_orig_2_year))

#LFS imputed 2024
lfs24.harm=read_dta(paste(datapath,
  "cleaned/Harmonized/LKA_2024_LFS_v01_M_v01_A_SARLAB_IND.dta",sep=""))

lfs24.imp=read_dta(paste(dataout,
                      "lfs2024_imputed.dta",
                      sep="")) 
lfs24.imp = lfs24.imp |>
  select(hhid,welfare)

lfs24 = lfs24.harm |>
  left_join(lfs24.imp, by="hhid")|>
  mutate(welfare = welfare*(12/365)/cpi21/icp21,
    popwt = weight*hsize,
    industry_orig = as.character(industry_orig),
    industry_orig_year = as.character(industry_orig_year),
    industry_orig_2 = as.character(industry_orig_2),
    industry_orig_2_year = as.character(industry_orig_2_year))

lfs_names <- names(lfs24)
hies16_names <- names(hies16)
hies19_names <- names(hies19)

exact_common <- intersect(intersect(lfs_names, hies16_names), 
hies19_names) |> sort()

df=bind_rows(
  hies16 |> select(all_of(exact_common),
  - c(language,occup_orig,occup_orig_year,
      occup_orig_2,occup_orig_2_year,pid,strata)) |> 
    mutate(survey = "HIES_16"),
  hies19 |> select(all_of(exact_common),
    -c(language,occup_orig,occup_orig_year,
      occup_orig_2,occup_orig_2_year,pid,strata)) |> 
    mutate(survey = "HIES_19"),
  lfs24 |> select(all_of(exact_common),
    -c(language,occup_orig,occup_orig_year,
      occup_orig_2,occup_orig_2_year,pid,strata)) |> 
    mutate(survey = "LFS_24")
)

df$pov30 = ifelse(df$welfare<3,1,0)
df$pov42 = ifelse(df$welfare<4.2,1,0)
df$pov83 = ifelse(df$welfare<8.3,1,0)

#Ensure poverty rates are correct
svydf <- svydesign(ids = ~1, data = df, 
                   weights = ~weight)
svyby(~pov30+pov42+pov83, ~survey, design=svydf, svymean,
           na.rm=TRUE,vartype = "ci")


df = df |>
  mutate(lstatus = factor(lstatus, levels=c(1,2,3),
    labels=c("Employed","Unemployed","Not in labor force")),
    empstat=factor(empstat, levels=c(1,2,3,4),
    labels=c("Paid Employee","Non-Paid Employee",
    "Employer","Self-employed")),
    industrycat4 = factor(industrycat4, levels=c(1,2,3,4),
    labels=c("Agriculture","Industry","Services","Other")),
    educat4 = factor(educat4, levels=c(1,2,3,4),
    labels=c("No education","Primary","Secondary","Tertiary")),
    poor=factor(pov42, levels=c(0,1), labels=c("Non-poor","Poor")),
    urban = factor(urban, levels=c(0,1), labels=c("Rural","Urban")))


# Harmonize subnatid1: LFS_24 uses "X - Name Province" with hyphen
# HIES uses "X – Name" with en-dash. Standardize to HIES format.

library(stringr)

df <- df |>
  mutate(subnatid1 = case_when(
    survey == "LFS_24" ~ str_replace(subnatid1, " - ", " \u2013 ") |>
      str_replace(" Province$", ""),
    TRUE ~ subnatid1
  ))


df <- df |>
  mutate(subnatid1 = case_when(
    subnatid1 == "6 – North Western" ~ "6 – North-Western",
    subnatid1 == "7 – North Central" ~ "7 – North-Central",
    TRUE ~ subnatid1
  ))

table(df$subnatid1, df$survey)


library(tidyverse)
library(scales)
library(patchwork)

# --- World Bank Color Palette ------------------------------------------------
wb_blue    <- "#002244"
wb_teal    <- "#009FDA"
wb_gold    <- "#F4A100"
wb_red     <- "#EB1C2D"
wb_gray    <- "#A8A9AD"

wb_3col <- c("HIES_16" = wb_blue, "HIES_19" = wb_teal, "LFS_24" = wb_gold)

wb_sector_fill <- c(
  "Agriculture" = "#009FDA",
  "Industry"    = "#002244",
  "Services"    = "#F4A100",
  "Other"       = "#A8A9AD"
)

# Common theme
theme_wb <- theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "gray40", size = 10),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# --- Data Preparation --------------------------------------------------------
# Harmonize subnatid1 labels (LFS_24 has " Province" suffix and uses hyphen)
df <- df |>
  mutate(subnatid1 = str_replace(subnatid1, " Province$", "")) |>
  mutate(subnatid1 = str_replace(subnatid1, " - ", " \u2013 "))

# Age groups
df <- df |>
  mutate(age_group = case_when(
    age >= 15 & age <= 29 ~ "15\u201329",
    age >= 30 & age <= 49 ~ "30\u201349",
    age >= 50             ~ "50+",
    TRUE                  ~ NA_character_
  ))

# --- Figure 1: Poverty rate by sector x year ---------------------------------
fig1_data <- df |>
  filter(!is.na(industrycat4), !is.na(pov42), lstatus == "Employed") |>
  summarise(poverty_rate = weighted.mean(pov42, weight), .by = c(industrycat4, survey))

fig1 <- ggplot(fig1_data, aes(x = industrycat4, y = poverty_rate, fill = survey)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = wb_3col) +
  labs(
    title = "Poverty Rate by Economic Sector",
    subtitle = "Share of employed workers below $4.2/day poverty line",
    x = NULL, y = "Poverty rate", fill = "Survey"
  ) +
  theme_wb

# --- Figure 2: Shift-share decomposition -------------------------------------
do_decomp <- function(data, t1, t2) {
  d1 <- data |> filter(survey == t1)
  d2 <- data |> filter(survey == t2)

  merged <- inner_join(d1, d2, by = "industrycat4", suffix = c("_1", "_2"))

  within_effect  <- sum(merged$emp_share_1 * (merged$poverty_rate_2 - merged$poverty_rate_1))
  between_effect <- sum(merged$poverty_rate_1 * (merged$emp_share_2 - merged$emp_share_1))
  interaction    <- sum((merged$poverty_rate_2 - merged$poverty_rate_1) *
                          (merged$emp_share_2 - merged$emp_share_1))

  # Shapley: split interaction equally
  tibble(
    period = paste0(t1, " \u2192 ", t2),
    within_sector = within_effect + interaction / 2,
    between_sector = between_effect + interaction / 2,
    total = within_effect + between_effect + interaction
  )
}

decomp_data <- df |>
  filter(!is.na(industrycat4), !is.na(pov42), lstatus == "Employed") |>
  summarise(
    poverty_rate = weighted.mean(pov42, weight),
    emp_share = sum(weight),
    .by = c(industrycat4, survey)
  ) |>
  mutate(emp_share = emp_share / sum(emp_share), .by = survey)

decomp_results <- bind_rows(
  do_decomp(decomp_data, "HIES_16", "HIES_19"),
  do_decomp(decomp_data, "HIES_19", "LFS_24")
)

fig2_data <- decomp_results |>
  select(period, Within = within_sector, Between = between_sector) |>
  pivot_longer(-period, names_to = "component", values_to = "contribution")

fig2 <- ggplot(fig2_data, aes(x = period, y = contribution, fill = component)) +
  geom_col(position = "dodge", width = 0.6) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = c("Within" = wb_blue, "Between" = wb_teal)) +
  labs(
    title = "Shift-Share Decomposition of Poverty Change",
    subtitle = "Shapley decomposition: within-sector vs. between-sector effects",
    x = NULL, y = "Contribution to poverty change (pp)", fill = "Component"
  ) +
  theme_wb

# --- Figure 3: Poverty rate by sector x urban/rural x year -------------------
fig3_data <- df |>
  filter(!is.na(industrycat4), !is.na(pov42), lstatus == "Employed", !is.na(urban)) |>
  summarise(poverty_rate = weighted.mean(pov42, weight), .by = c(industrycat4, survey, urban))

fig3 <- ggplot(fig3_data, aes(x = industrycat4, y = poverty_rate, color = survey)) +
  geom_point(size = 3, position = position_dodge(width = 0.4)) +
  facet_wrap(~urban) +
  scale_y_continuous(labels = percent) +
  scale_color_manual(values = wb_3col) +
  labs(
    title = "Poverty Rate by Sector and Residency",
    subtitle = "$4.2/day poverty line, employed individuals",
    x = NULL, y = "Poverty rate", color = "Survey"
  ) +
  theme_wb +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

# --- Figure 4: Provincial heatmap (Agriculture) ------------------------------
fig4_data <- df |>
  #filter(industrycat4 == "Agriculture", !is.na(pov42), lstatus == "Employed") |>
  filter(!is.na(pov42)) |>
  summarise(poverty_rate = weighted.mean(pov42, weight), .by = c(subnatid1, survey))

fig4 <- ggplot(fig4_data, aes(x = survey, y = reorder(subnatid1, poverty_rate), fill = poverty_rate)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = percent(poverty_rate, accuracy = 0.1)), size = 3, color = "black") +
  scale_fill_distiller(palette = "YlOrRd", direction = 1, labels = percent) +
  labs(
    title = "Poverty Rate by Province",
    subtitle = "$4.2/day line",
    x = NULL, y = NULL, fill = "Poverty\nrate"
  ) +
  #theme_wb +
  theme(legend.position = "right")

# --- Figure 5: Poverty rate by employment type x sector -----------------------
fig5_data <- df |>
  filter(!is.na(industrycat4), !is.na(empstat), lstatus == "Employed", !is.na(pov42)) |>
  summarise(poverty_rate = weighted.mean(pov42, weight), .by = c(industrycat4, empstat, survey))

fig5 <- ggplot(fig5_data, aes(x = empstat, y = poverty_rate, color = survey)) +
  geom_point(size = 3, position = position_dodge(width = 0.4)) +
  facet_wrap(~industrycat4) +
  scale_y_continuous(labels = percent) +
  scale_color_manual(values = wb_3col) +
  labs(
    title = "Poverty Rate by Employment Type and Sector",
    subtitle = "$4.2/day line, employed individuals",
    x = NULL, y = "Poverty rate", color = "Survey"
  ) +
  theme_wb +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# --- Figure 6: Poverty rate by age group x sector x year ---------------------
fig6_data <- df |>
  filter(!is.na(age_group), !is.na(industrycat4), !is.na(pov42), lstatus == "Employed") |>
  summarise(poverty_rate = weighted.mean(pov42, weight), .by = c(industrycat4, age_group, survey))

fig6 <- ggplot(fig6_data, aes(x = age_group, y = poverty_rate, color = survey, group = survey)) +
  geom_point(size = 3) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~industrycat4) +
  scale_y_continuous(labels = percent) +
  scale_color_manual(values = wb_3col) +
  labs(
    title = "Poverty Rate by Age Group and Sector",
    subtitle = "$4.2/day line, employed individuals",
    x = "Age group", y = "Poverty rate", color = "Survey"
  ) +
  theme_wb
