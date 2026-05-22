# Additional analyses for the PEA: poverty rates, GIC, and inequality indicators, prosperity GAP

###############
#POVERTY RATES
###############

df16$pov30 = ifelse(df16$welfare<3,1,0)
df16$pov42 = ifelse(df16$welfare<4.2,1,0)
df16$pov83 = ifelse(df16$welfare<8.3,1,0)
svydf <- svydesign(ids = ~1, data = df16, 
                   weights = ~popwt)

tab1=svyby(~pov30+pov42+pov83, ~survey, design=svydf, svymean,
           na.rm=TRUE,vartype = "ci")
tab1$urban="National"

#Poverty by urban
tab2=svyby(~pov30+pov42+pov83, ~survey+urban, design=svydf, 
           svymean,na.rm=TRUE,vartype = "ci")


tab_all=bind_rows(tab1,tab2)


#Poverty by sector
means_long <- tab_all %>%
  pivot_longer(
    cols = c(pov30, pov42, pov83),
    names_to = "variable",
    values_to = "mean"
  )

# Pivot the lower confidence intervals and clean the variable names
ci_lower_long <- tab_all %>%
  pivot_longer(
    cols = starts_with("ci_l."),
    names_to = "variable",
    values_to = "ci_lower"
  ) %>%
  mutate(variable = sub("ci_l\\.", "", variable))

# Pivot the upper confidence intervals and clean the variable names
ci_upper_long <- tab_all %>%
  pivot_longer(
    cols = starts_with("ci_u."),
    names_to = "variable",
    values_to = "ci_upper"
  ) %>%
  mutate(variable = sub("ci_u\\.", "", variable))

# Merge the long data frames by survey, urban, and variable
plot_data <- means_long %>%
  left_join(ci_lower_long, by = c("survey", "urban", "variable")) %>%
  left_join(ci_upper_long, by = c("survey", "urban", "variable"))

# Correct labels in poverty lines
plot_data$variable=factor(plot_data$variable,
                          levels=c("pov30","pov42","pov83"),
                          labels=c("$3.0 PPP21","$4.2 PPP21","$8.3 PPP21"))

plot_data$survey=factor(plot_data$survey,
                        levels=c("HIES_16","HIES_19","LFS_16_imp","LFS_19_imp","LFS_20_imp","LFS_21_imp","LFS_22_imp","LFS_23_imp","LFS_24_imp"),
                        labels=c("HIES_16","HIES_19","LFS_16_imp","LFS_19_imp","LFS_20_imp","LFS_21_imp","LFS_22_imp","LFS_23_imp","LFS_24_imp"))

plot_data$urban=factor(plot_data$urban,
                         levels=c("Urban","Rural","National"),
                         labels=c("Urban","Rural","National"))

# Create the bar plot with error bars and facet by variable (rows) and area (columns)
ggplot(plot_data[plot_data$survey %in% c("HIES_16", "LFS_16_imp"), ], 
       aes(x = survey, y = mean, fill = survey)) +
  geom_bar(stat = "identity", width = 0.7, position = position_dodge()) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                width = 0.2, 
                position = position_dodge(width = 0.7)) +
  geom_text(aes(label = percent(mean, accuracy = 0.1), y = mean/2), 
            position = position_dodge(width = 0.7),
            color = "black", size = 3) +
  facet_grid(variable ~ urban, scales = "free_y") +
  scale_y_continuous(labels = percent) +
  labs(
    x = "Sector",
    y = "Poverty Rate (%)",
    title = "Original and Imputed Poverty Rates (95% CI)"
  ) +
theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

ggsave(paste(outpath,
             "/Outputs/Main/Figures/poverty rates hies lfs 16 CI barplot.png",sep=""),
       width = 15, height = 10, units = "cm")


###############
#GIC
###############

# ====================================================
# GICs with bootstrap confidence intervals
# Official vs imputed
# ====================================================

library(purrr)

set.seed(1729)

B <- 500              # increase to 1,000 if runtime is acceptable
p_use <- 11:89        # same trimming as your original code

dftemp <- df16 %>%
  filter(survey %in% c("HIES_19", "LFS_16_imp", "HIES_16")) %>%
  filter(!is.na(welfare), !is.na(popwt), popwt > 0)

# ====================================================
# Helper: weighted percentiles
# ====================================================

weighted_pctiles <- function(data, pctiles = p_use) {
  tibble(
    pctile = pctiles,
    welfare_avg = as.numeric(
      Hmisc::wtd.quantile(
        x = data$welfare,
        weights = data$popwt,
        probs = pctiles / 100,
        na.rm = TRUE
      )
    )
  )
}

# ====================================================
# Helper: calculate GIC once
# ====================================================

calc_gic_once <- function(data) {
  
  # National percentiles
  nat <- data %>%
    group_by(survey) %>%
    group_modify(~ weighted_pctiles(.x)) %>%
    ungroup() %>%
    mutate(group = "National")
  
  # Urban/rural percentiles
  urb <- data %>%
    group_by(survey, urban) %>%
    group_modify(~ weighted_pctiles(.x)) %>%
    ungroup() %>%
    mutate(group = as.character(urban)) %>%
    select(-urban)
  
  bind_rows(nat, urb) %>%
    pivot_wider(
      names_from = survey,
      values_from = welfare_avg
    ) %>%
    mutate(
      growth_rate_imp = (`HIES_19` / `LFS_16_imp`)^(1/3) - 1,
      growth_rate_off = (`HIES_19` / `HIES_16`)^(1/3) - 1
    ) %>%
    select(group, pctile, growth_rate_imp, growth_rate_off) %>%
    pivot_longer(
      cols = starts_with("growth_rate"),
      names_to = "type",
      values_to = "growth_rate"
    ) %>%
    mutate(
      type = ifelse(type == "growth_rate_imp", "Imputed", "Official")
    )
}

# ====================================================
# Point estimates
# ====================================================

point_df <- calc_gic_once(dftemp)

# ====================================================
# Bootstrap confidence intervals
# ====================================================

boot_one <- function(b) {
  
  boot_data <- dftemp %>%
    group_by(survey) %>%
    group_modify(~ {
      .x[sample(seq_len(nrow(.x)), size = nrow(.x), replace = TRUE), ]
    }) %>%
    ungroup()
  
  calc_gic_once(boot_data) %>%
    mutate(b = b)
}

boot_df <- map_dfr(1:B, boot_one)

ci_df <- boot_df %>%
  group_by(group, pctile, type) %>%
  summarise(
    growth_lwr = quantile(growth_rate, 0.025, na.rm = TRUE),
    growth_upr = quantile(growth_rate, 0.975, na.rm = TRUE),
    .groups = "drop"
  )

final_plot_df <- point_df %>%
  left_join(ci_df, by = c("group", "pctile", "type"))

# ====================================================
# Plot with confidence bands
# ====================================================

ggplot(
  final_plot_df,
  aes(x = pctile, y = growth_rate, color = group, fill = group)
) +
  geom_ribbon(
    aes(ymin = growth_lwr, ymax = growth_upr, group = interaction(group, type)),
    alpha = 0.15,
    color = NA
  ) +
  geom_line(aes(linetype = type), linewidth = 1) +
  theme_minimal() +
  labs(
    title = "Growth Incidence Curve - Consumption (2016–2019)",
    subtitle = "Official 2016–2019 comparison and imputed 2016 LFS vs official 2019 HIES",
    x = "Consumption Percentile",
    y = "Annualized Growth Rate",
    color = "Population",
    fill = "Population",
    linetype = "Type"
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 0.1),
    limits = c(-0.02, 0.08)
  ) +
  scale_color_manual(
    values = c(
      "National" = "orange",
      "Urban" = "darkgreen",
      "Rural" = "blue"
    )
  ) +
  scale_fill_manual(
    values = c(
      "National" = "orange",
      "Urban" = "darkgreen",
      "Rural" = "blue"
    )
  ) +
  geom_hline(yintercept = 0) +
  theme(legend.position = "bottom")

#ggsave(
#  paste(outpath, "/Outputs/Main/Figures/GIC 16 19 all with CI.png", sep = ""),
#  width = 30,
#  height = 20,
#  units = "cm"
#)


# ====================================================
# Plot with confidence bands:  one sector at a time
# ====================================================


ggplot(
  final_plot_df[final_plot_df$group != "National", ],
  aes(x = pctile, y = growth_rate, color = group, fill = group)
) +
  geom_ribbon(
    aes(ymin = growth_lwr, ymax = growth_upr, group = interaction(group, type)),
    alpha = 0.15,
    color = NA
  ) +
  geom_line(aes(linetype = type), linewidth = 1) +
  theme_minimal() +
  labs(
    title = "Growth Incidence Curve - Consumption (2016–2019)",
    subtitle = "Official 2016–2019 comparison and imputed 2016 LFS vs official 2019 HIES",
    x = "Consumption Percentile",
    y = "Annualized Growth Rate",
    color = "Population",
    fill = "Population",
    linetype = "Type"
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 0.1) ,
    limits = c(-0.02, 0.09)
  ) +
  scale_color_manual(
    values = c(
      "National" = "orange",
      "Urban" = "darkgreen",
      "Rural" = "blue"
    )
  ) +
  scale_fill_manual(
    values = c(
      "National" = "orange",
      "Urban" = "darkgreen",
      "Rural" = "blue"
    )
  ) +
  geom_hline(yintercept = 0) +
  theme(legend.position = "bottom")

ggsave(
  paste(outpath, "/Outputs/Main/Figures/GIC 16 19 Rural with CI.png", sep = ""),
  width = 15,
  height = 10,
  units = "cm"
)




###############
#INEQUALITY INDICATORS
###############

# ====================================================
# Weighted Lorenz curves downsampled to percentiles
# HIES 2016 vs imputed LFS 2016
# National, Urban, Rural
# ====================================================

# ----------------------------------------------------
# Data
# ----------------------------------------------------

lorenz_data <- df16 %>%
  filter(survey %in% c("HIES_16", "LFS_16_imp")) %>%
  filter(!is.na(welfare), !is.na(popwt), popwt > 0) %>%
  mutate(
    type = case_when(
      survey == "HIES_16" ~ "Official HIES 2016",
      survey == "LFS_16_imp" ~ "Imputed LFS 2016"
    )
  )

# ----------------------------------------------------
# Helper: weighted Lorenz curve at percentile points
# ----------------------------------------------------

weighted_lorenz_pct <- function(data, pctiles = 0:100) {
  
  data_sorted <- data %>%
    arrange(welfare) %>%
    mutate(
      pop_share = popwt / sum(popwt, na.rm = TRUE),
      welfare_weighted = welfare * popwt,
      welfare_share = welfare_weighted / sum(welfare_weighted, na.rm = TRUE),
      cum_pop_share = cumsum(pop_share),
      cum_welfare_share = cumsum(welfare_share)
    ) %>%
    select(cum_pop_share, cum_welfare_share)
  
  # Add origin explicitly
  data_sorted <- bind_rows(
    tibble(cum_pop_share = 0, cum_welfare_share = 0),
    data_sorted
  )
  
  # Interpolate Lorenz values at exact percentile points
  tibble(
    pctile = pctiles,
    cum_pop_share = pctiles / 100,
    cum_welfare_share = approx(
      x = data_sorted$cum_pop_share,
      y = data_sorted$cum_welfare_share,
      xout = pctiles / 100,
      method = "linear",
      ties = "ordered",
      rule = 2
    )$y
  )
}

# ----------------------------------------------------
# National Lorenz curves
# ----------------------------------------------------

lorenz_nat <- lorenz_data %>%
  group_by(type) %>%
  group_modify(~ weighted_lorenz_pct(.x)) %>%
  ungroup() %>%
  mutate(group = "National")

# ----------------------------------------------------
# Urban / Rural Lorenz curves
# ----------------------------------------------------

lorenz_urb <- lorenz_data %>%
  group_by(type, urban) %>%
  group_modify(~ weighted_lorenz_pct(.x)) %>%
  ungroup() %>%
  mutate(group = as.character(urban)) %>%
  select(-urban)

# ----------------------------------------------------
# Combine
# ----------------------------------------------------

final_lorenz_df <- bind_rows(lorenz_nat, lorenz_urb) %>%
  mutate(
    group = factor(group, levels = c("National", "Urban", "Rural")),
    type = factor(type, levels = c("Official HIES 2016", "Imputed LFS 2016"))
  )

# ----------------------------------------------------
# Plot
# ----------------------------------------------------

ggplot(
  final_lorenz_df,
  aes(
    x = cum_pop_share,
    y = cum_welfare_share,
    color = type,
    linetype = type
  )
) +
  geom_abline(
    intercept = 0,
    slope = 1,
    color = "gray50",
    linetype = "dashed"
  ) +
  geom_line(linewidth = 1) +
  facet_wrap(~ group, nrow = 3) +
  theme_minimal() +
  labs(
    title = "Lorenz Curves - Consumption, 2016",
    subtitle = "Official HIES 2016 and imputed LFS 2016",
    x = "Cumulative population share",
    y = "Cumulative consumption share",
    color = "",
    linetype = ""
  ) +
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )

ggsave(
  paste(outpath, "/Outputs/Main/Figures/Lorenz curves HIES16 LFS16imp percentile.png", sep = ""),
  width = 15,
  height = 15,
  units = "cm"
)


# ====================================================
# National inequality table:
# Official HIES 2016 vs Imputed LFS 2016
# Indicators: Gini, Theil, percentile ratios, bottom 20% share
# ====================================================

# ----------------------------------------------------
# Data: national only
# ----------------------------------------------------

ineq_data_nat <- df16 %>%
  filter(survey %in% c("HIES_16", "LFS_16_imp")) %>%
  filter(!is.na(welfare), !is.na(popwt), popwt > 0) %>%
  mutate(
    type = case_when(
      survey == "HIES_16" ~ "Official HIES 2016",
      survey == "LFS_16_imp" ~ "Imputed LFS 2016"
    )
  )

# ----------------------------------------------------
# Helper: bottom 20% consumption share
# ----------------------------------------------------

bottom20_share <- function(x, w) {
  
  df <- tibble(welfare = x, popwt = w) %>%
    filter(!is.na(welfare), !is.na(popwt), popwt > 0) %>%
    arrange(welfare) %>%
    mutate(
      pop_share = popwt / sum(popwt),
      welfare_weighted = welfare * popwt,
      welfare_share = welfare_weighted / sum(welfare_weighted),
      cum_pop_share = cumsum(pop_share),
      cum_welfare_share = cumsum(welfare_share)
    ) %>%
    bind_rows(
      tibble(
        welfare = NA_real_,
        popwt = NA_real_,
        pop_share = NA_real_,
        welfare_weighted = NA_real_,
        welfare_share = NA_real_,
        cum_pop_share = 0,
        cum_welfare_share = 0
      ),
      .
    )
  
  approx(
    x = df$cum_pop_share,
    y = df$cum_welfare_share,
    xout = 0.20,
    method = "linear",
    ties = "ordered",
    rule = 2
  )$y
}

# ----------------------------------------------------
# Helper: calculate indicators
# ----------------------------------------------------

calc_nat_ineq <- function(data) {
  
  q <- Hmisc::wtd.quantile(
    x = data$welfare,
    weights = data$popwt,
    probs = c(0.10, 0.20, 0.25, 0.50, 0.75, 0.80, 0.90),
    na.rm = TRUE
  )
  
  tibble(
    gini = dineq::gini.wtd(data$welfare, data$popwt),
    theil = dineq::theil.wtd(data$welfare, data$popwt),
    p10p50 = q["10%"] / q["50%"],
    p25p50 = q["25%"] / q["50%"],
    p75p25 = q["75%"] / q["25%"],
    p75p50 = q["75%"] / q["50%"],
    p90p10 = q["90%"] / q["10%"],
    p90p50 = q["90%"] / q["50%"],
    p80p20 = q["80%"] / q["20%"],
    bottom20_share = bottom20_share(data$welfare, data$popwt)
  )
}

# ----------------------------------------------------
# Table: official vs imputed
# ----------------------------------------------------

ineq_table_nat <- ineq_data_nat %>%
  group_by(type) %>%
  group_modify(~ calc_nat_ineq(.x)) %>%
  ungroup() %>%
  pivot_longer(
    cols = -type,
    names_to = "indicator",
    values_to = "estimate"
  ) %>%
  pivot_wider(
    names_from = type,
    values_from = estimate
  ) %>%
  mutate(
    difference = `Imputed LFS 2016` - `Official HIES 2016`,
    percent_difference = difference / `Official HIES 2016`
  )

# ----------------------------------------------------
# Format table for display/export
# ----------------------------------------------------

ineq_table_nat_formatted <- ineq_table_nat %>%
  mutate(
    across(
      c(`Official HIES 2016`, `Imputed LFS 2016`, difference),
      ~ round(.x, 3)
    ),
    percent_difference = scales::percent(percent_difference, accuracy = 0.1)
  )

ineq_table_nat_formatted
write.csv(
  ineq_table_nat_formatted,
  paste(outpath, "/Outputs/Main/Tables/Inequality indicators HIES16 LFS16imp national.csv", sep = ""),
  row.names = FALSE
)




###############
#PROSPERITY GAP
###############

# ====================================================
# Prosperity Gap and Decomposition
# Official HIES and Imputed LFS
# Welfare already in 2021 PPP
# ====================================================

library(openxlsx)

# ----------------------------------------------------
# Parameters
# ----------------------------------------------------

floor_value <- 0.28
prosperity_line <- 28
spell_years <- c(2016, 2019)

# ----------------------------------------------------
# Data setup
# ----------------------------------------------------

pg_data <- df16 %>%
  filter(survey %in% c("HIES_16", "LFS_16_imp", "HIES_19")) %>%
  filter(!is.na(welfare), !is.na(popwt), popwt > 0) %>%
  mutate(
    welfare_pg = if_else(welfare < floor_value, floor_value, welfare),
    year = case_when(
      survey %in% c("HIES_16", "LFS_16_imp") ~ 2016,
      survey == "HIES_19" ~ 2019
    ),
    source = case_when(
      survey == "HIES_16" ~ "Official HIES 2016",
      survey == "LFS_16_imp" ~ "Imputed LFS 2016",
      survey == "HIES_19" ~ "Official HIES 2019"
    )
  )

# ====================================================
# Helper: weighted mean
# ====================================================

wmean <- function(x, w) {
  weighted.mean(x, w = w, na.rm = TRUE)
}

# ====================================================
# 1) Prosperity gap in 2016: official and imputed
# ====================================================

pg_2016_nat <- pg_data %>%
  filter(survey %in% c("HIES_16", "LFS_16_imp")) %>%
  mutate(
    prosgap = prosperity_line / welfare_pg
  ) %>%
  group_by(source, year) %>%
  summarise(
    group = "National",
    prosperity_gap = wmean(prosgap, popwt),
    mean_welfare = wmean(welfare_pg, popwt),
    .groups = "drop"
  )

pg_2016_urban <- pg_data %>%
  filter(survey %in% c("HIES_16", "LFS_16_imp")) %>%
  mutate(
    prosgap = prosperity_line / welfare_pg
  ) %>%
  group_by(source, year, urban) %>%
  summarise(
    group = as.character(first(urban)),
    prosperity_gap = wmean(prosgap, popwt),
    mean_welfare = wmean(welfare_pg, popwt),
    .groups = "drop"
  ) %>%
  select(-urban)

pg_2016_table <- bind_rows(pg_2016_nat, pg_2016_urban) %>%
  mutate(
    group = factor(group, levels = c("National", "Urban", "Rural")),
    prosperity_gap = round(prosperity_gap, 3),
    mean_welfare = round(mean_welfare, 3)
  ) %>%
  arrange(group, source)

pg_2016_table

write.csv(
  pg_2016_table,
  paste(outpath, "/Outputs/Main/Tables/Prosperity gap 2016 HIES16 LFS16imp.csv", sep = ""),
  row.names = FALSE
)


# ====================================================
# Helper: calculate PG decomposition components
# ====================================================

calc_pg_components <- function(data, year_value, source_label) {
  
  data_year <- data %>%
    filter(year == year_value)
  
  mean_welfare <- wmean(data_year$welfare_pg, data_year$popwt)
  
  data_year %>%
    mutate(
      prosgap = prosperity_line / welfare_pg,
      ineq_component = mean_welfare / welfare_pg
    ) %>%
    summarise(
      year = year_value,
      source = source_label,
      prosperity_gap = wmean(prosgap, popwt),
      mean_welfare = mean_welfare,
      inequality_component = wmean(ineq_component, popwt),
      .groups = "drop"
    )
}

# ====================================================
# 2A) Decomposition: official HIES only, 2016–2019
# HIES_16 -> HIES_19
# ====================================================

pg_decomp_official_data <- pg_data %>%
  filter(survey %in% c("HIES_16", "HIES_19")) %>%
  mutate(
    source_pair = "Official HIES 2016 to Official HIES 2019"
  )

pg_decomp_official <- bind_rows(
  calc_pg_components(
    data = pg_decomp_official_data %>% filter(survey == "HIES_16"),
    year_value = 2016,
    source_label = "Official HIES 2016"
  ),
  calc_pg_components(
    data = pg_decomp_official_data %>% filter(survey == "HIES_19"),
    year_value = 2019,
    source_label = "Official HIES 2019"
  )
) %>%
  arrange(year) %>%
  mutate(
    comparison = "Official HIES",
    ch_prosperity_gap = c(
      NA_real_,
      diff(log(prosperity_gap)) / diff(year)
    ),
    ch_mean_welfare = c(
      NA_real_,
      -diff(log(mean_welfare)) / diff(year)
    ),
    ch_inequality = c(
      NA_real_,
      diff(log(inequality_component)) / diff(year)
    )
  )

# ====================================================
# 2B) Decomposition: imputed 2016 and official 2019
# LFS_16_imp -> HIES_19
# ====================================================

pg_decomp_imputed_data <- pg_data %>%
  filter(survey %in% c("LFS_16_imp", "HIES_19")) %>%
  mutate(
    source_pair = "Imputed LFS 2016 to Official HIES 2019"
  )

pg_decomp_imputed <- bind_rows(
  calc_pg_components(
    data = pg_decomp_imputed_data %>% filter(survey == "LFS_16_imp"),
    year_value = 2016,
    source_label = "Imputed LFS 2016"
  ),
  calc_pg_components(
    data = pg_decomp_imputed_data %>% filter(survey == "HIES_19"),
    year_value = 2019,
    source_label = "Official HIES 2019"
  )
) %>%
  arrange(year) %>%
  mutate(
    comparison = "Imputed LFS 2016 + Official HIES 2019",
    ch_prosperity_gap = c(
      NA_real_,
      diff(log(prosperity_gap)) / diff(year)
    ),
    ch_mean_welfare = c(
      NA_real_,
      -diff(log(mean_welfare)) / diff(year)
    ),
    ch_inequality = c(
      NA_real_,
      diff(log(inequality_component)) / diff(year)
    )
  )

# ====================================================
# Combine decomposition results
# ====================================================

pg_decomp_table <- bind_rows(
  pg_decomp_official,
  pg_decomp_imputed
) %>%
  select(
    comparison,
    source,
    year,
    prosperity_gap,
    mean_welfare,
    inequality_component,
    ch_prosperity_gap,
    ch_mean_welfare,
    ch_inequality
  ) %>%
  mutate(
    across(
      c(prosperity_gap, mean_welfare, inequality_component),
      ~ round(.x, 3)
    ),
    across(
      c(ch_prosperity_gap, ch_mean_welfare, ch_inequality),
      ~ round(100 * .x, 2)
    )
  )

pg_decomp_table

write.csv(
  pg_decomp_table,
  paste(outpath, "/Outputs/Main/Tables/Prosperity gap decomposition official vs imputed.csv", sep = ""),
  row.names = FALSE
)


# ====================================================
# Compact decomposition table: changes only
# ====================================================

pg_decomp_changes <- pg_decomp_table %>%
  filter(year == 2019) %>%
  select(
    comparison,
    ch_prosperity_gap,
    ch_mean_welfare,
    ch_inequality
  ) %>%
  rename(
    `Annualized change in prosperity gap (%)` = ch_prosperity_gap,
    `Contribution of mean welfare growth (%)` = ch_mean_welfare,
    `Contribution of inequality change (%)` = ch_inequality
  )

pg_decomp_changes
write.csv(
  pg_decomp_changes,
  paste(outpath, "/Outputs/Main/Tables/Prosperity gap decomposition changes only official vs imputed.csv", sep = ""),
  row.names = FALSE
)

# ====================================================
# Export to Excel
# ====================================================

output_file <- paste(outpath, "/Outputs/Main/Tables/prosperity_gap_outputs.xlsx", sep = "")

wb <- createWorkbook()

addWorksheet(wb, "PG 2016")
writeData(wb, "PG 2016", pg_2016_table)

addWorksheet(wb, "PG decomposition full")
writeData(wb, "PG decomposition full", pg_decomp_table)

addWorksheet(wb, "PG decomposition changes")
writeData(wb, "PG decomposition changes", pg_decomp_changes)

saveWorkbook(wb, output_file, overwrite = TRUE)


# ====================================================
# Optional version: 2016 PG level by National / Urban / Rural
# ====================================================

pg_2016_plot_df_all <- pg_2016_table %>%
  mutate(
    group = factor(group, levels = c("National", "Urban", "Rural")),
    source_short = case_when(
      source == "Official HIES 2016" ~ "Official HIES 2016",
      source == "Imputed LFS 2016" ~ "Imputed LFS 2016",
      TRUE ~ source
    )
  )

pg_2016_level_plot_all <- ggplot(
  pg_2016_plot_df_all,
  aes(
    x = prosperity_gap,
    y = group,
    color = source_short
  )
) +
  geom_line(
    aes(group = group),
    color = "gray60",
    linewidth = 0.8
  ) +
  geom_point(size = 4) +
  geom_text(
    aes(label = round(prosperity_gap, 2)),
    nudge_y = 0.3,
    size = 3.8,
    show.legend = FALSE
  ) +
  theme_minimal() +
  labs(
    title = "Prosperity Gap in 2016",
    subtitle = "Official HIES 2016 vs imputed LFS 2016",
    x = "Prosperity gap",
    y = NULL,
    color = NULL
  ) +
  scale_color_manual(
    values = c(
      "Official HIES 2016" = "steelblue",
      "Imputed LFS 2016" = "darkorange"
    )
  ) +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

pg_2016_level_plot_all

ggsave(
  paste(outpath, "/Outputs/Main/Figures/Prosperity gap 2016 official vs imputed by area.png", sep = ""),
  plot = pg_2016_level_plot_all,
  width = 12,
  height = 6,
  units = "cm"
)


# ====================================================
# 2) Decomposition contribution plot
# ====================================================

pg_decomp_plot_df <- pg_decomp_changes %>%
  mutate(
    comparison_short = case_when(
      comparison == "Official HIES" ~ "Official HIES\n2016–2019",
      comparison == "Imputed LFS 2016 + Official HIES 2019" ~ "Imputed LFS 2016 +\nOfficial HIES 2019",
      TRUE ~ comparison
    )
  ) %>%
  rename(
    total_change = `Annualized change in prosperity gap (%)`,
    mean_contribution = `Contribution of mean welfare growth (%)`,
    inequality_contribution = `Contribution of inequality change (%)`
  )

pg_decomp_components_df <- pg_decomp_plot_df %>%
  select(
    comparison_short,
    mean_contribution,
    inequality_contribution
  ) %>%
  pivot_longer(
    cols = c(mean_contribution, inequality_contribution),
    names_to = "component",
    values_to = "contribution"
  ) %>%
  mutate(
    component = case_when(
      component == "mean_contribution" ~ "Mean welfare growth",
      component == "inequality_contribution" ~ "Inequality change"
    ),
    component = factor(
      component,
      levels = c("Mean welfare growth", "Inequality change")
    )
  )

pg_decomp_total_df <- pg_decomp_plot_df %>%
  select(comparison_short, total_change)

pg_decomp_plot <- ggplot(
  pg_decomp_components_df,
  aes(
    x = comparison_short,
    y = contribution,
    fill = component
  )
) +
  geom_col(
    width = 0.55,
    position = "stack"
  ) +
  geom_point(
    data = pg_decomp_total_df,
    aes(
      x = comparison_short,
      y = total_change
    ),
    inherit.aes = FALSE,
    size = 4,
    color = "black"
  ) +
  geom_text(
    data = pg_decomp_total_df,
    aes(
      x = comparison_short,
      y = total_change,
      label = paste0("Total: ", round(total_change, 2), "%")
    ),
    inherit.aes = FALSE,
    nudge_y = 0.25,
    size = 3.8
  ) +
  geom_hline(yintercept = 0, linewidth = 0.6) +
  theme_minimal() +
  labs(
    title = "Annualized Change in the Prosperity Gap, 2016–2019",
    subtitle = "Decomposition into mean welfare growth and inequality change",
    x = NULL,
    y = "Annualized contribution, percentage points",
    fill = NULL
  ) +
  scale_fill_manual(
    values = c(
      "Mean welfare growth" = "darkgreen",
      "Inequality change" = "goldenrod"
    )
  ) +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

pg_decomp_plot

ggsave(
  paste(outpath, "/Outputs/Main/Figures/Prosperity gap decomposition 2016 2019.png", sep = ""),
  plot = pg_decomp_plot,
  width = 20,
  height = 12,
  units = "cm"
)