#This script creates profiles using data from HIES 2016, HIES 2019 and 
#LFS imputed 2024
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


library(ggplot2)

# Graph 1: Poverty rate by sector × year
# Filter to employed with non-missing sector

# Hmisc::summarize is masking dplyr::summarize. Use dplyr:: explicitly.
g1_data <- df |>
  filter(!is.na(industrycat4), lstatus == "Employed") |>
  dplyr::summarise(
    poverty_rate = weighted.mean(pov42, weight, na.rm = TRUE),
    .by = c(industrycat4, survey)
  )

ggplot(g1_data, aes(x = industrycat4, y = poverty_rate, fill = survey)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Poverty Rate by Economic Sector",
    subtitle = "Share of workers below $4.2/day poverty line, by survey round",
    x = NULL, y = "Poverty rate", fill = "Survey"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")


# Graph 2: Sectoral composition of poor vs non-poor (100% stacked bar)
g2_data <- df |>
  filter(!is.na(industrycat4), lstatus == "Employed", !is.na(poor)) |>
  dplyr::summarise(wt = sum(weight), .by = c(survey, poor, industrycat4)) |>
  dplyr::mutate(share = wt / sum(wt), .by = c(survey, poor))

ggplot(g2_data, aes(x = poor, y = share, fill = industrycat4)) +
  geom_col(width = 0.7) +
  facet_wrap(~survey) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Sectoral Composition of the Poor vs. Non-Poor",
    subtitle = "Among employed individuals, by survey round",
    x = NULL, y = "Share of employed", fill = "Sector"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")


# Graph 3: Shift-share decomposition
# Decompose change in overall poverty rate among employed into:
#   Within-sector: sum_j [share_j_t0 * (poverty_j_t1 - poverty_j_t0)]
#   Between-sector: sum_j [poverty_j_t0 * (share_j_t1 - share_j_t0)]
#   Interaction: sum_j [(poverty_j_t1 - poverty_j_t0) * (share_j_t1 - share_j_t0)]

decomp_data <- df |>
  filter(!is.na(industrycat4), lstatus == "Employed", !is.na(pov42)) |>
  dplyr::summarise(
    poverty_rate = weighted.mean(pov42, weight),
    emp_share = sum(weight),
    .by = c(survey, industrycat4)
  ) |>
  dplyr::mutate(emp_share = emp_share / sum(emp_share), .by = survey)

# Reshape wide by survey
library(tidyr)
decomp_wide <- decomp_data |>
  pivot_wider(
    names_from = survey,
    values_from = c(poverty_rate, emp_share)
  )

# Compute decomposition for two periods
do_decomp <- function(d, t0, t1) {
  pov0 <- d[[paste0("poverty_rate_", t0)]]
  pov1 <- d[[paste0("poverty_rate_", t1)]]
  s0 <- d[[paste0("emp_share_", t0)]]
  s1 <- d[[paste0("emp_share_", t1)]]
  
  within_eff <- sum(s0 * (pov1 - pov0))
  between_eff <- sum(pov0 * (s1 - s0))
  interaction_eff <- sum((pov1 - pov0) * (s1 - s0))
  total <- within_eff + between_eff + interaction_eff
  
  tibble(
    component = c("Within-sector", "Between-sector", "Interaction", "Total"),
    value = c(within_eff, between_eff, interaction_eff, total)
  )
}

decomp_results <- bind_rows(
  do_decomp(decomp_wide, "HIES_16", "HIES_19") |> mutate(period = "2016 → 2019"),
  do_decomp(decomp_wide, "HIES_19", "LFS_24") |> mutate(period = "2019 → 2024")
)

# Plot (exclude Total row for the bar chart)
ggplot(decomp_results |> filter(component != "Total"),
       aes(x = component, y = value, fill = component)) +
  geom_col(width = 0.6) +
  facet_wrap(~period) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    title = "Shift-Share Decomposition of Poverty Change",
    subtitle = "Among employed: within-sector vs. between-sector effects",
    x = NULL, y = "Contribution to poverty change (pp)", fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")



# Graph 4: Poverty rate by sector × urban/rural × year (faceted dot plot)
g4_data <- df |>
  filter(!is.na(industrycat4), lstatus == "Employed", !is.na(pov42)) |>
  dplyr::summarise(
    poverty_rate = weighted.mean(pov42, weight),
    .by = c(industrycat4, survey, urban)
  )

ggplot(g4_data, aes(x = industrycat4, y = poverty_rate, color = survey, group = survey)) +
  geom_point(size = 3) +
  geom_line(aes(group = survey), linewidth = 0.4, alpha = 0.5) +
  facet_wrap(~urban) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Poverty Rate by Sector and Urban/Rural Residence",
    subtitle = "$4.2/day line, employed individuals",
    x = NULL, y = "Poverty rate", color = "Survey"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")


# Graph 5: Provincial heatmap — poverty rate among employed, by province × year
g5_data <- df |>
  filter(!is.na(industrycat4), lstatus == "Employed", !is.na(pov42)) |>
  dplyr::summarise(
    poverty_rate = weighted.mean(pov42, weight),
    .by = c(subnatid1, survey)
  )

ggplot(g5_data, aes(x = survey, y = reorder(subnatid1, poverty_rate), fill = poverty_rate)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = scales::percent(poverty_rate, accuracy = 0.1)), size = 3) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1, labels = scales::percent) +
  labs(
    title = "Poverty Rate Among Employed Workers by Province",
    subtitle = "$4.2/day line",
    x = NULL, y = NULL, fill = "Poverty\nrate"
  ) +
  theme_minimal(base_size = 12)



# Graph 6: Education × sector × poverty (faceted 100% stacked bar)
g6_data <- df |>
  filter(!is.na(industrycat4), lstatus == "Employed", !is.na(poor), !is.na(educat4)) |>
  dplyr::summarise(wt = sum(weight), .by = c(survey, poor, industrycat4, educat4)) |>
  dplyr::mutate(share = wt / sum(wt), .by = c(survey, poor, industrycat4))

ggplot(g6_data, aes(x = industrycat4, y = share, fill = educat4)) +
  geom_col(width = 0.7) +
  facet_grid(poor ~ survey) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Blues") +
  labs(
    title = "Educational Composition by Sector and Poverty Status",
    subtitle = "Among employed individuals",
    x = NULL, y = "Share", fill = "Education"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 30, hjust = 1))




# Graph 7 revised: use dodge bars instead of stacked to show variation better
# Also filter to working age (15+)
g7a_data <- df |>
  filter(!is.na(lstatus), !is.na(poor), age >= 15) |>
  dplyr::summarise(wt = sum(weight), .by = c(survey, poor, lstatus)) |>
  dplyr::mutate(share = wt / sum(wt), .by = c(survey, poor))

p7a <- ggplot(g7a_data, aes(x = lstatus, y = share, fill = survey)) +
  geom_col(position = "dodge", width = 0.7) +
  facet_wrap(~poor) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Labor Force Status (age 15+)", x = NULL, y = "Share", fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 20, hjust = 1))

p7b <- ggplot(g7b_data, aes(x = empstat, y = share, fill = survey)) +
  geom_col(position = "dodge", width = 0.7) +
  facet_wrap(~poor) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Employment Type (employed only)", x = NULL, y = "Share", fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 20, hjust = 1))

p7a / p7b + plot_annotation(
  title = "Labor and Employment Status by Poverty Status"
)


# Revised Graph 7 — Employment type only (drop lstatus panel)
# Single panel: empstat × poor × survey, dodge bars, working age employed
g7_data <- df |>
  filter(!is.na(empstat), lstatus == "Employed", !is.na(poor), age >= 15) |>
  dplyr::summarise(wt = sum(weight), .by = c(survey, poor, empstat)) |>
  dplyr::mutate(share = wt / sum(wt), .by = c(survey, poor))

ggplot(g7_data, aes(x = empstat, y = share, fill = survey)) +
  geom_col(position = "dodge", width = 0.7) +
  facet_wrap(~poor) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Employment Type by Poverty Status",
    subtitle = "Among employed workers aged 15+, by survey round",
    x = NULL, y = "Share of employed", fill = "Survey"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 20, hjust = 1))


# New Graph: Poverty rate within Agriculture by province × year (mini heatmap)
g_agri_prov <- df |>
  filter(industrycat4 == "Agriculture", !is.na(pov42), lstatus == "Employed") |>
  dplyr::summarise(
    poverty_rate = weighted.mean(pov42, weight),
    .by = c(subnatid1, survey)
  )

ggplot(g_agri_prov, aes(x = survey, y = reorder(subnatid1, poverty_rate), fill = poverty_rate)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = scales::percent(poverty_rate, accuracy = 0.1)), size = 3) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1, labels = scales::percent) +
  labs(
    title = "Poverty Rate in Agriculture by Province",
    subtitle = "$4.2/day line, employed agricultural workers",
    x = NULL, y = NULL, fill = "Poverty\nrate"
  ) +
  theme_minimal(base_size = 12)



# New Graph: Poverty rate by employment type × sector (dot plot)
g_empstat_sector <- df |>
  filter(!is.na(industrycat4), !is.na(empstat), lstatus == "Employed", !is.na(pov42)) |>
  dplyr::summarise(
    poverty_rate = weighted.mean(pov42, weight),
    n = sum(weight),
    .by = c(industrycat4, empstat, survey)
  )

ggplot(g_empstat_sector, aes(x = empstat, y = poverty_rate, color = survey)) +
  geom_point(size = 3, position = position_dodge(width = 0.4)) +
  facet_wrap(~industrycat4) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Poverty Rate by Employment Type and Sector",
    subtitle = "$4.2/day line, employed individuals",
    x = NULL, y = "Poverty rate", color = "Survey"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 30, hjust = 1))


# Create age groups and compute poverty rate by age group × sector × year
df <- df |>
  dplyr::mutate(age_group = dplyr::case_when(
    age >= 15 & age <= 29 ~ "15–29",
    age >= 30 & age <= 49 ~ "30–49",
    age >= 50 ~ "50+",
    TRUE ~ NA_character_
  ))

g_age <- df |>
  filter(!is.na(age_group), !is.na(industrycat4), !is.na(pov42), lstatus == "Employed") |>
  dplyr::summarise(
    poverty_rate = weighted.mean(pov42, weight),
    n = sum(weight),
    .by = c(industrycat4, age_group, survey)
  )

ggplot(g_age, aes(x = age_group, y = poverty_rate, color = survey, group = survey)) +
  geom_point(size = 3) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~industrycat4) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Poverty Rate by Age Group and Sector",
    subtitle = "$4.2/day line, employed individuals",
    x = "Age group", y = "Poverty rate", color = "Survey"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")



g_age_urban <- df |>
  filter(!is.na(age_group), !is.na(industrycat4), !is.na(pov42), 
         lstatus == "Employed", !is.na(urban)) |>
  dplyr::summarise(
    poverty_rate = weighted.mean(pov42, weight),
    n = sum(weight),
    .by = c(industrycat4, age_group, survey, urban)
  )

ggplot(g_age_urban, aes(x = age_group, y = poverty_rate, color = survey, group = survey)) +
  geom_point(size = 2.5) +
  geom_line(linewidth = 0.6) +
  facet_grid(urban ~ industrycat4) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Poverty Rate by Age Group, Sector, and Urban/Rural",
    subtitle = "$4.2/day line, employed individuals",
    x = "Age group", y = "Poverty rate", color = "Survey"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")
