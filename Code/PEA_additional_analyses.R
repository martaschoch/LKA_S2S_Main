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
ggplot(plot_data |> filter(survey %in% c("HIES_16", "LFS_16_imp"),
                           variable %in% c("$3.0 PPP21","$4.2 PPP21")),
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
  xlim(c(3,5)) +
  geom_line(
    aes(group = group),
    color = "gray60",
    linewidth = 0.8
  ) +
  geom_point(size = 4) +
    geom_text(
    data = pg_2016_plot_df_all |> filter(source_short == "Official HIES 2016"),
    aes(label = round(prosperity_gap, 2)),
    nudge_x = -0.08,
    nudge_y = 0.3,
    hjust = 1,
    size = 3.8,
    show.legend = FALSE
  ) +
  geom_text(
    data = pg_2016_plot_df_all |> filter(source_short == "Imputed LFS 2016"),
    aes(label = round(prosperity_gap, 2)),
    nudge_x = 0.08,
    nudge_y = 0.3,
    hjust = 0,
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




###############
#SUBNATIONAL COMPARISON
###############

#LFS 2016
lfs16.orig=read_dta(paste(datapath,
                          "/cleaned/lfs2016_clean.dta",
                          sep="")) 
lfs16.orig=subset(lfs16.orig,select=c(hhid,district))
lfs16=read_dta(paste(dataout,
                          "/lfs2016_imputed.dta",
                          sep="")) 
lfs16$district=NULL
lfs16=merge(lfs16.orig,lfs16,by="hhid",all.x=TRUE)
lfs16$welfare=lfs16$welfare*(12/365)/cpi21/icp21

#hies 2016
hies16=read_dta(paste(datapath,"hies16ppp.dta",sep=""))

hies16.prov <- hies16 |>
  group_by(subnatid, district) |>
  summarise(
    pop = sum(weight)/1000000,
    .groups = "drop"
  ) |>
  #select(subnatid, district) |>
  rename(province = subnatid) |>
  mutate(province = sub("^\\d+\\s*-\\s*", "", province))

hies16 <- hies16 |>
  rename(popwt = weight) |>
  mutate(
    urban = factor(
      as.numeric(urban),
      levels = c(0, 1),
      labels = c("Rural", "Urban")
    ),
    survey = "HIES_16"
  ) |>
  rename(province = subnatid) |>
  mutate(province = sub("^\\d+\\s*-\\s*", "", province)) |>
  select(urban, popwt, welfare, district, province, survey)

lfs16 <- lfs16 |>
  left_join(hies16.prov, by = "district") |>
  mutate(
    urban = factor(
      urban,
      levels = c(0, 1),
      labels = c("Rural", "Urban")
    ),
    survey = "LFS_16"
  ) |>
  select(urban, popwt, welfare, district,province, survey)

df16 <- bind_rows(hies16, lfs16)

df16$pov30 = ifelse(df16$welfare<3,1,0)
df16$pov42 = ifelse(df16$welfare<4.2,1,0)
df16$pov83 = ifelse(df16$welfare<8.3,1,0)

svydf <- svydesign(ids = ~1, data = df16, 
                   weights = ~popwt)

tab3=svyby(~pov30+pov42+pov83, ~survey+district, design=svydf, 
           svymean,na.rm=TRUE,keep.var=FALSE)

# 4.2 line


tab3_wide_42 <- tab3 %>%
  select(survey,district,statistic.pov42) %>%
  pivot_wider(names_from = survey, values_from =statistic.pov42)

tab3_wide_42 = tab3_wide_42 %>%
  left_join(hies16.prov %>% select(pop, district), by = "district") 

tab3_wide_42$HIES_16=100*tab3_wide_42$HIES_16
tab3_wide_42$LFS_16=100*tab3_wide_42$LFS_16
tab3_wide_42$Diff=with(tab3_wide_42,LFS_16-HIES_16)

write.csv(tab3_wide_42,paste(outpath, 
  "/Outputs/Main/Tables/district_pov_42.csv",sep=""))

#ranking plot
tab3_wide_42 <- tab3_wide_42 %>%
  mutate(
    hies_rank = rank(-HIES_16, ties.method = "first"),
    lfs_rank = rank(-LFS_16, ties.method = "first")
  )

#rank correlation
cat("Rank correlation is: ",cor(tab3_wide_42$hies_rank,tab3_wide_42$lfs_rank))

ggplot(tab3_wide_42, aes(x = hies_rank, y = lfs_rank)) +
  geom_point(aes(size = pop)) +
  geom_text(aes(label = district), vjust = -0.5, check_overlap = TRUE) +
  scale_x_continuous(breaks = 1:nrow(tab3_wide_42)) +
  scale_y_continuous(breaks = 1:nrow(tab3_wide_42)) +
  geom_abline(slope = 1, intercept = 0, size=1.3,
              linetype = "dashed", color = "gray") +
  coord_fixed() +
  labs(
    x = "HIES Ranking (1 = Highest Poverty Rate)",
    y = "LFS Ranking (1 = Highest Poverty Rate)",
    size = "Population (millions)",
    title = "District Poverty Rankings ($4.2 PPP21)"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = .5)   # Reduce axis text size
  )
ggsave(
  paste(outpath, "/Outputs/Main/Figures/District Ranking 2016 @4.2.png", sep = ""),
  width = 20,
  height = 12,
  units = "cm"
)


#barplot
tab3=svyby(~pov30+pov42+pov83, ~survey+province, design=svydf, 
           svymean,na.rm=TRUE,keep.var=FALSE)
tab3$statistic.pov42=100*tab3$statistic.pov42
ggplot(tab3, aes(
  y     = as.factor(province),
  x     = statistic.pov42,
  fill  = survey
)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(
    y     = "Province",
    x     = "Intl. Poverty Rate at \n$4.2 (2021 PPP) (%)",
    fill  = "Survey",
    title = "Actual and Imputed Poverty Rates by Province, 2016"
  ) +
  xlim(c(0,75))+
  theme_minimal()
ggsave(
  paste(outpath, "/Outputs/Main/Figures/Province Barplot 2016 @4.2.png", sep = ""),
  width = 20,
  height = 12,
  units = "cm"
)

#Descriptives LFS 2019-2024

#lfs 2019
lfs19=read_dta(paste(dataout,
                      "lfs2019_imputed.dta",
                      sep="")) 
#lfs19$hhid=NULL
#lfs19=subset(lfs19,select=c(urban,sector,popwt,welfare,ln_rpcinc1))
lfs19$survey="LFS_19_imp"
lfs19$urban=factor(lfs19$urban, levels=c(0,1),labels=c("Rural","Urban"))
lfs19$loginc=log(lfs19$rpcinc_tot)
lfs19$year=2019
lfs19$sector=factor(lfs19$sector, levels=c(1,2,3),labels=c("Urban","Rural","Estate"))

#lfs 2020-2024
lfs_imp_list <- lapply(2020:2024, function(year) {
  read_dta(file.path(dataout, paste0("lfs", year, "_imputed.dta"))) |>
    #subset(select = c(urban, sector, popwt, welfare,ln_rpcinc1)) |>
    mutate(
      loginc = log(rpcinc_tot),
      year = year,
      survey = paste0("LFS_", substr(year, 3, 4), "_imp"),
      urban = factor(urban, levels = c(0, 1), labels = c("Rural", "Urban")),
      sector = factor(sector, levels = c(1, 2, 3), labels = c("Urban", "Rural", "Estate"))
    )
})
names(lfs_imp_list) <- paste0("lfs", substr(2020:2024, 3, 4))
list2env(lfs_imp_list, envir = .GlobalEnv)

survey_list <- list(
  "2019" = lfs19,
  "2020" = lfs20,
  "2021" = lfs21,
  "2022" = lfs22,
  "2023" = lfs23,
  "2024" = lfs24
)

Reduce(intersect, lapply(survey_list, names))

vars_to_keep <- c(
  "urban", "sector", "popwt", "welfare", "hhsize", "female_hhh", 
  "age_hhh", "num_deps", "num_kids" , "edu_hhh_none"  ,"edu_hhh_prim"  ,  
    "edu_hhh_sec", "loginc", "hh_main_agri", "hh_main_ind"  ,  "year",        
   "hh_main_serv" ,"sh_employee"  ,"sh_selfempl", "sh_ecactive", "rpcinc_tot",
   "sh_wages"
)

# Subset each data frame to keep only the desired columns
subset_list <- lapply(survey_list, function(df) {
  df[, intersect(vars_to_keep, names(df)), drop = FALSE]
})

# Append (row-bind) all the data frames into one
combined_data <- do.call(rbind, subset_list)
combined_data$year = fct_rev(as.factor(combined_data$year))

stats_data <- combined_data %>%
  filter(!is.na(popwt)) %>%
  group_by(year, urban) %>%
  summarise(
    median_val = as.numeric(Hmisc::wtd.quantile(
      loginc,
      weights = popwt, probs = 0.5
    )),
    .groups = "drop"
  ) %>%
  mutate(
    year_factor = as.factor(year),
    year_numeric = as.numeric(as.factor(year))
      )

stats_data_levels <- combined_data %>%
  filter(!is.na(popwt)) %>%
  group_by(year, urban) %>%
  summarise(
    median_val = as.numeric(Hmisc::wtd.quantile(
      rpcinc_tot,
      weights = popwt, probs = 0.5
    )),
    .groups = "drop"
  ) %>%
  mutate(
    year_factor = as.factor(year),
    year_numeric = as.numeric(as.factor(year))
      )

write.csv(stats_data_levels, 
  paste(outpath, "/Outputs/Main/Tables/Median income by year and urban rural.csv", sep = ""), row.names = FALSE)

# Ridge plot with median lines colored by urban/rural
ggplot(combined_data |> dplyr::filter(is.finite(loginc)),
  aes(
  x = loginc,
  y = year,
  fill = as.factor(year)
)) +
  geom_density_ridges(scale = 1.5, alpha = 0.7, color = "black") +
  geom_segment(
    data = stats_data,
    aes(
      x = median_val, xend = median_val,
      y = year_numeric, yend = year_numeric + 1,
      color = urban
    ),
    linetype = "dotted",
    size = 0.8
  ) +
  scale_color_manual(
    name = "Sector",
    values = c("Rural" = "#ab7126", "Urban" = "#1E90FF")  # green & blue
  ) +
  scale_fill_viridis_d(guide = "none") +
  labs(
    x = "Log Labor Income, 2019 prices",
    y = "Year",
    title = "Labor Income Density and Median by Sector and Year"
  ) +
  scale_x_continuous(limits = c(7.5, 10.5), breaks = seq(7.5, 10.5, by = 0.5)) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "right"
  )
ggsave(
  paste(outpath, "/Outputs/Main/Figures/Ridgeplot_income.png", sep = ""),
  width = 20,
  height = 12,
  units = "cm"
)




####DESCRIPTIVES####

####Figure 6####
exclude_vars <- c("urban", "sector", "popwt", "welfare", "year","loginc")

weighted_means_long <- combined_data %>%
  group_by(year) %>%
  summarise(across(
    .cols = setdiff(names(combined_data), exclude_vars),
    .fns = ~ {
      valid <- !is.na(.x) & !is.na(popwt) & popwt > 0
      if (any(valid)) weighted.mean(.x[valid], popwt[valid]) else NA_real_
    },
    .names = "{.col}"
  ))  %>%
  # Convert from wide to long
  pivot_longer(
    cols = -year,
    names_to = "variable",
    values_to = "weighted_mean"
  )


var_labels <- c(
  hhsize       = "HH size",
  female_hhh   = "Female HH head",
  age_hhh      = "Age of HH head",
  num_deps     = "No. of dependents",
  num_kids     = "No. of children",
  edu_hhh_none = "HH head: no schooling",
  edu_hhh_prim = "HH head: primary edu",
  edu_hhh_sec  = "HH head: secondary edu",
  hh_main_agri = "HH main sector: agriculture",
  hh_main_ind  = "HH main sector: industry",
  hh_main_serv = "HH main sector: services",
  sh_employee  = "Sh. employees in HH",
  sh_selfempl  = "Sh. self-employed in HH",
  sh_ecactive  = "Sh. ec. active in HH",
  rpcinc_tot   = "Real per-capita labor income",
  sh_wages     = "Wage income share"
)

weighted_means_plot <- weighted_means_long |>
  mutate(
    variable_label = recode(variable, !!!var_labels, .default = variable)
  )

ggplot(
  weighted_means_plot,
  aes(
    x = as.integer(as.character(year)),
    y = weighted_mean,
    group = 1,
    color = variable
  )
) +
  geom_line(linewidth = 0.7, show.legend = FALSE) +
  facet_wrap(~ variable_label, ncol = 4, scales = "free_y") +
  scale_color_viridis_d(option = "D") +
  theme_minimal(base_size = 11) +
  labs(x = "Year", y = NULL) +
  theme(
    strip.background = element_rect(fill = "grey95", colour = NA),
    strip.text       = element_text(face = "bold", size = 8, colour = "#444444"),
    panel.grid.major = element_line(colour = "grey85"),
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(angle = 45, hjust = 1, colour = "#333333"),
    axis.text.y      = element_text(colour = "#333333"),
    plot.background  = element_rect(fill = "white", colour = NA)
  )

ggsave(
  paste(outpath, "/Outputs/Main/Figures/Descriptives LFS.png", sep = ""),
  width = 20,
  height = 15,
  units = "cm"
)

rm(combined_data,data2017,data2018,data2019,
   data2020,data2021,data2022)


###############
#POVERTY PROFILE COMPARISON
###############

#LFS 2016
lfs16.orig=read_dta(paste(datapath,
                          "/cleaned/lfs2016_clean.dta",
                          sep="")) 
lfs16.orig=subset(lfs16.orig,select=c(hhid,district))
lfs16=read_dta(paste(dataout,
                          "/lfs2016_imputed.dta",
                          sep="")) 
lfs16$district=NULL
lfs16=merge(lfs16.orig,lfs16,by="hhid",all.x=TRUE)
lfs16$welfare=lfs16$welfare*(12/365)/cpi21/icp21

#hies 2016
hies16_welf=read_dta(paste(datapath,"hies16ppp.dta",sep=""))
hies16_welf = hies16_welf |>
  select(hhid, welfare)

#HIES 2016
hies16=read_dta(paste(datapath,"hies2016_clean.dta",sep="")) 
hies16$welfare=NULL
hies16=merge(hies16,hies16_welf,by="hhid",all.x=TRUE)
hies16$survey="HIES_16"
#hies16$urban=factor(hies16$urban, levels=c(0,1),labels=c("Rural","Urban"))
hies16$sector=factor(hies16$sector, levels=c(1,2,3),labels=c("Urban","Rural","Estate"))


lfs16 <- lfs16 |>
    mutate(
    #urban = factor(
    #  urban,
    #  levels = c(0, 1),
    #  labels = c("Rural", "Urban")
    #),
    survey = "LFS_16",
    sector = factor(
      sector,
      levels = c(1, 2, 3),
      labels = c("Urban", "Rural", "Estate")
    ),
    psu=as.numeric(psu)
  ) 

df16 <- bind_rows(hies16, lfs16)

df16$pov30 = ifelse(df16$welfare<3,1,0)
df16$pov42 = ifelse(df16$welfare<4.2,1,0)
df16$pov83 = ifelse(df16$welfare<8.3,1,0)

svydf <- svydesign(ids = ~1, data = df16, 
                   weights = ~popwt)

svyby(~pov30+pov42+pov83, ~survey, design=svydf, 
           svymean,na.rm=TRUE,keep.var=FALSE)


lfs_names <- names(lfs16)
hies_names <- names(hies16)

exact_common <- intersect(lfs_names, hies_names) |> sort()

comp_vars = c("age_avg", "age_hhh", "buddhist_hhh","sinhala_hhh",
 "edu_hhh_none","edu_sh_1564_none", "sector", "district", "hhsize",
 "edu_hhh_prim", "edu_hhh_sec", "female_hhh", "have_agri_emp"  ,
    "have_constr_emp"  , "have_ind_emp"  , "have_serv_emp"  ,
     "hh_main_agri"  , "hh_main_ind"       , "hh_main_serv",
    "married_hhh" ,"num_deps" ,"num_kids", "sh_mem_fem", 
    "welfare","popwt","survey", "share_dep", "has_in_school", "num_old",
    "employer_hhh","urban"
  )

# Overall profile comparison: LFS 2024 vs. BRIGHT 2024-25

prof_vars_perc = setdiff(comp_vars, c("welfare","popwt","survey",
"age_avg","age_hhh","hhsize","num_deps","num_kids","num_old","district","sector"))
prof_vars_num = c("age_avg","age_hhh","hhsize","num_deps","num_kids","num_old")


# Weighted means by survey (fixes summarize/across + factor issue) ----
radar_wide <- df16 |>
  group_by(survey) |>
  dplyr::summarize(
    dplyr::across(
      dplyr::all_of(prof_vars_perc),
      ~ stats::weighted.mean(as.numeric(.x), w = popwt, na.rm = TRUE)
    ),
    .groups = "drop"
  )

# ---- 2) Long format + safe scaling (fixes if_else length error) ----
radar_long <- radar_wide |>
  tidyr::pivot_longer(
    cols = dplyr::all_of(prof_vars_perc),
    names_to = "variable",
    values_to = "value"
  )

scale_factor <- if (max(radar_long$value, na.rm = TRUE) <= 1) 100 else 1

radar_long <- radar_long |>
  mutate(
    value = value * scale_factor,
    variable = factor(variable, levels = prof_vars_perc)
  )

# ---- 3) Build radar coordinates ----
n_vars <- length(prof_vars_perc)

radar_plot_data <- radar_long |>
  group_by(survey) |>
  arrange(variable, .by_group = TRUE) |>
  mutate(
    angle = 2 * pi * (row_number() - 1) / n_vars,
    x = value * sin(angle),
    y = value * cos(angle)
  ) |>
  group_modify(\(.x, .y) bind_rows(.x, .x[1, ])) |>
  ungroup()

max_val <- max(radar_long$value, na.rm = TRUE)
outer_lim <- max_val * 1.20
label_radius <- max_val * 1.10

# short, informative labels for radar axes
radar_labels <- c(
  buddhist_hhh    = "Buddhist HH head",
  sinhala_hhh     = "Sinhala HH head",
  edu_hhh_none    = "HH head: no schooling",
  edu_sh_1564_none = "Any member 15-64 no schooling",
  edu_hhh_prim    = "HH head: primary edu",
  edu_hhh_sec    = "HH head: secondary edu",
  has_in_school   = "Any member in school",
  #employee_hhh    = "HH head is employee",
  employer_hhh    = "HH head is employer",
  #self_employed_hhh = "HH head is self-employed",
  female_hhh      = "Female HH head",
  have_agri_emp   = "Any agri worker",
  have_constr_emp = "Any construction worker",
  have_ind_emp    = "Any industry worker",
  have_serv_emp   = "Any services worker",
  hh_main_agri    = "Main sector: agri",
  hh_main_ind     = "Main sector: industry",
  hh_main_serv    = "Main sector: services",
  married_hhh     = "HH head married",
  sh_mem_fem      = "Female member share",
  share_dep        = "Dependent member share",
  urban        = "Urban residence"
)

axis_data <- tibble(
  variable = factor(prof_vars_perc, levels = prof_vars_perc),
  angle = 2 * pi * (seq_len(n_vars) - 1) / n_vars,
  x = label_radius * sin(angle),
  y = label_radius * cos(angle)
) |>
  mutate(
    var_label = dplyr::recode(as.character(variable), !!!radar_labels, .default = as.character(variable))
  )


# --- radar guides (spokes + circular grids) ---
grid_breaks <- pretty(c(0, max_val), n = 5)
grid_breaks <- grid_breaks[grid_breaks >= 0]

theta <- seq(0, 2 * pi, length.out = 360)

grid_circles <- tibble(r = rep(grid_breaks, each = length(theta)),
                       t = rep(theta, times = length(grid_breaks))) |>
  mutate(
    x = r * sin(t),
    y = r * cos(t)
  )

spokes <- axis_data |>
  transmute(
    x = 0, y = 0,
    xend = max_val * sin(angle),
    yend = max_val * cos(angle)
  )

grid_labels <- tibble(
  x = 0,
  y = grid_breaks,
  lab = paste0(round(grid_breaks), "%")
)

radar_plot_data$survey <- factor(radar_plot_data$survey, levels = c("HIES_16", "LFS_16"),
                                labels = c("HIES 2016", "LFS 2016"))

# --- plot ---
p_radar <- ggplot(radar_plot_data, aes(x = x, y = y, group = survey)) +
  geom_path(
    data = grid_circles,
    aes(x = x, y = y, group = r),
    inherit.aes = FALSE,
    color = "grey85",
    linewidth = 0.4
  ) +
  geom_segment(
    data = spokes,
    aes(x = x, y = y, xend = xend, yend = yend),
    inherit.aes = FALSE,
    color = "grey80",
    linewidth = 0.4
  ) +
  geom_text(
    data = grid_labels,
    aes(x = x, y = y, label = lab),
    inherit.aes = FALSE,
    color = "grey40",
    size = 3,
    vjust = -0.2
  ) +
  geom_polygon(aes(fill = survey, color = survey), alpha = 0.20, linewidth = 0.8) +
  geom_path(aes(color = survey), linewidth = 0.9) +
  geom_point(aes(color = survey), size = 1.6) +
  geom_text(
    data = axis_data,
    aes(x = x, y = y, label = var_label),
    inherit.aes = FALSE,
    size = 3.5
  ) +
  coord_equal() +
  scale_x_continuous(limits = c(-outer_lim, outer_lim)) +
  scale_y_continuous(limits = c(-outer_lim, outer_lim)) +
  labs(
    title = "Overall profile comparison: HIES 2016 vs. LFS 2016",
    subtitle = "",
    fill = "Survey",
    color = "Survey"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom"
  )

p_radar

ggsave(paste(outpath,
             "/Outputs/Main/Figures/radar_plot_lfs_hies_16.png",sep=""),
       width = 24, height = 20, units = "cm")

#Numerical comparison of means by survey

# Short labels for numeric profile variables
num_var_labels <- c(
  age_avg  = "Average age",
  age_hhh  = "Age of HH head",
  hhsize   = "Household size",
  num_deps = "No. of dependents",
  num_kids = "No. of children",
  num_old  = "No. of elderly"

)

tab_prof_num <- df16 |>
  dplyr::group_by(survey) |>
  dplyr::summarize(
    dplyr::across(
      dplyr::all_of(prof_vars_num),
      ~ stats::weighted.mean(.x, w = popwt, na.rm = TRUE)
    ),
    .groups = "drop"
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::all_of(prof_vars_num),
    names_to = "variable",
    values_to = "weighted_avg"
  ) |>
  dplyr::mutate(
    variable = factor(variable, levels = prof_vars_num),
    label = dplyr::recode(as.character(variable), !!!num_var_labels, .default = as.character(variable)),
    weighted_avg = round(weighted_avg, 2)
  ) |>
  dplyr::arrange(variable, survey) |>
  dplyr::select(variable, label, survey, weighted_avg) |>
  tidyr::pivot_wider(
    names_from = survey,
    values_from = weighted_avg
  )

tab_prof_num

write.csv(tab_prof_num, paste(outpath,
             "/Outputs/Main/Tables/numeric_profile_comparison_lfs_hies_16.csv",sep=""), 
             row.names = FALSE)


# Comparison of percentage profile variables overall: LFS 2016 vs. HIES 2016
tab_prof_perc <- df16 |>
  dplyr::group_by(survey) |>
  dplyr::summarize(
    dplyr::across(
      dplyr::all_of(prof_vars_perc),
      ~ stats::weighted.mean(.x, w = popwt, na.rm = TRUE)
    ),
    .groups = "drop"
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::all_of(prof_vars_perc),
    names_to = "variable",
    values_to = "weighted_avg"
  ) |>
  dplyr::mutate(
    variable = factor(variable, levels = prof_vars_perc),
    label = dplyr::recode(as.character(variable), !!!radar_labels, .default = as.character(variable)),
    weighted_avg = round(weighted_avg, 3)
  ) |>
  dplyr::arrange(variable, survey) |>
  dplyr::select(variable, label, survey, weighted_avg) |>
  tidyr::pivot_wider(
    names_from = survey,
    values_from = weighted_avg
  )

tab_prof_perc

write.csv(tab_prof_perc, paste(outpath,
             "/Outputs/Main/Tables/percentage_profile_comparison_lfs_hies_16_all.csv",sep=""), 
             row.names = FALSE)


#Profile of the poor: LFS 2016 vs. HIES 2016

# Weighted means by survey (fixes summarize/across + factor issue) ----
radar_wide <- df16 |>
  filter(pov42 == 1) |>
  group_by(survey) |>
  dplyr::summarize(
    dplyr::across(
      dplyr::all_of(prof_vars_perc),
      ~ stats::weighted.mean(as.numeric(.x), w = popwt, na.rm = TRUE)
    ),
    .groups = "drop"
  )

# ---- 2) Long format + safe scaling (fixes if_else length error) ----
radar_long <- radar_wide |>
  tidyr::pivot_longer(
    cols = dplyr::all_of(prof_vars_perc),
    names_to = "variable",
    values_to = "value"
  )

scale_factor <- if (max(radar_long$value, na.rm = TRUE) <= 1) 100 else 1

radar_long <- radar_long |>
  mutate(
    value = value * scale_factor,
    variable = factor(variable, levels = prof_vars_perc)
  )

# ---- 3) Build radar coordinates ----
n_vars <- length(prof_vars_perc)

radar_plot_data <- radar_long |>
  group_by(survey) |>
  arrange(variable, .by_group = TRUE) |>
  mutate(
    angle = 2 * pi * (row_number() - 1) / n_vars,
    x = value * sin(angle),
    y = value * cos(angle)
  ) |>
  group_modify(\(.x, .y) bind_rows(.x, .x[1, ])) |>
  ungroup()

max_val <- max(radar_long$value, na.rm = TRUE)
outer_lim <- max_val * 1.20
label_radius <- max_val * 1.10


axis_data <- tibble(
  variable = factor(prof_vars_perc, levels = prof_vars_perc),
  angle = 2 * pi * (seq_len(n_vars) - 1) / n_vars,
  x = label_radius * sin(angle),
  y = label_radius * cos(angle)
) |>
  mutate(
    var_label = dplyr::recode(as.character(variable), !!!radar_labels, .default = as.character(variable))
  )


# --- radar guides (spokes + circular grids) ---
grid_breaks <- pretty(c(0, max_val), n = 5)
grid_breaks <- grid_breaks[grid_breaks >= 0]

theta <- seq(0, 2 * pi, length.out = 360)

grid_circles <- tibble(r = rep(grid_breaks, each = length(theta)),
                       t = rep(theta, times = length(grid_breaks))) |>
  mutate(
    x = r * sin(t),
    y = r * cos(t)
  )

spokes <- axis_data |>
  transmute(
    x = 0, y = 0,
    xend = max_val * sin(angle),
    yend = max_val * cos(angle)
  )

grid_labels <- tibble(
  x = 0,
  y = grid_breaks,
  lab = paste0(round(grid_breaks), "%")
)

radar_plot_data$survey <- factor(radar_plot_data$survey, levels = c("HIES_16", "LFS_16"),
                                labels = c("HIES 2016", "LFS 2016"))

# --- plot ---
p_radar <- ggplot(radar_plot_data, aes(x = x, y = y, group = survey)) +
  geom_path(
    data = grid_circles,
    aes(x = x, y = y, group = r),
    inherit.aes = FALSE,
    color = "grey85",
    linewidth = 0.4
  ) +
  geom_segment(
    data = spokes,
    aes(x = x, y = y, xend = xend, yend = yend),
    inherit.aes = FALSE,
    color = "grey80",
    linewidth = 0.4
  ) +
  geom_text(
    data = grid_labels,
    aes(x = x, y = y, label = lab),
    inherit.aes = FALSE,
    color = "grey40",
    size = 3,
    vjust = -0.2
  ) +
  geom_polygon(aes(fill = survey, color = survey), alpha = 0.20, linewidth = 0.8) +
  geom_path(aes(color = survey), linewidth = 0.9) +
  geom_point(aes(color = survey), size = 1.6) +
  geom_text(
    data = axis_data,
    aes(x = x, y = y, label = var_label),
    inherit.aes = FALSE,
    size = 3.5
  ) +
  coord_equal() +
  scale_x_continuous(limits = c(-outer_lim, outer_lim)) +
  scale_y_continuous(limits = c(-outer_lim, outer_lim)) +
  labs(
    title = "Profile of the poor: LFS 2024 vs. BRIGHT 2024-25",
    subtitle = "",
    fill = "Survey",
    color = "Survey"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom"
  )

p_radar

ggsave(paste(outpath,
             "/Outputs/Main/Figures/radar_plot_lfs_hies_16_poor.png",sep=""),
       width = 24, height = 20, units = "cm")

#Numerical comparison of means by survey


tab_prof_num <- df16 |>
  filter(pov42 == 1) |>
  dplyr::group_by(survey) |>
  dplyr::summarize(
    dplyr::across(
      dplyr::all_of(prof_vars_num),
      ~ stats::weighted.mean(.x, w = popwt, na.rm = TRUE)
    ),
    .groups = "drop"
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::all_of(prof_vars_num),
    names_to = "variable",
    values_to = "weighted_avg"
  ) |>
  dplyr::mutate(
    variable = factor(variable, levels = prof_vars_num),
    label = dplyr::recode(as.character(variable), !!!num_var_labels, .default = as.character(variable)),
    weighted_avg = round(weighted_avg, 2)
  ) |>
  dplyr::arrange(variable, survey) |>
  dplyr::select(variable, label, survey, weighted_avg) |>
  tidyr::pivot_wider(
    names_from = survey,
    values_from = weighted_avg
  )

tab_prof_num

write.csv(tab_prof_num, paste(outpath,
             "/Outputs/Main/Tables/numeric_profile_comparison_lfs_hies_16_poor.csv",sep=""), 
             row.names = FALSE)

# Comparison of percentage profile variables for the poor: LFS 2016 vs. HIES 2016
tab_prof_perc <- df16 |>
  filter(pov42 == 1) |>
  dplyr::group_by(survey) |>
  dplyr::summarize(
    dplyr::across(
      dplyr::all_of(prof_vars_perc),
      ~ stats::weighted.mean(.x, w = popwt, na.rm = TRUE)
    ),
    .groups = "drop"
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::all_of(prof_vars_perc),
    names_to = "variable",
    values_to = "weighted_avg"
  ) |>
  dplyr::mutate(
    variable = factor(variable, levels = prof_vars_perc),
    label = dplyr::recode(as.character(variable), !!!radar_labels, .default = as.character(variable)),
    weighted_avg = round(weighted_avg, 3)
  ) |>
  dplyr::arrange(variable, survey) |>
  dplyr::select(variable, label, survey, weighted_avg) |>
  tidyr::pivot_wider(
    names_from = survey,
    values_from = weighted_avg
  )

tab_prof_perc

write.csv(tab_prof_perc, paste(outpath,
             "/Outputs/Main/Tables/percentage_profile_comparison_lfs_hies_16_poor.csv",sep=""), 
             row.names = FALSE)
