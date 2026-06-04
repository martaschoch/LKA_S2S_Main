#This script creates a poverty profile in the imputed LFs 2024 and
# imputed BRIGHT 2024-25, and compares them

#lfs 2024
lfs=read_dta(paste(dataout,
                      "lfs2024_imputed.dta",
                      sep="")) 
#lfs19$hhid=NULL
#lfs19=subset(lfs19,select=c(urban,sector,popwt,welfare,ln_rpcinc1))
lfs$survey="LFS_24"
#lfs$urban=factor(lfs$urban, levels=c(0,1),labels=c("Rural","Urban"))
lfs$loginc=log(lfs$rpcinc_tot)
lfs$year=2024
#lfs$sector=factor(lfs$sector, levels=c(1,2,3),labels=c("Urban","Rural","Estate"))

#bright 2024-25
bright=read_dta("C:\\Users\\wb553773\\WBG\\Marta Schoch - Analysis\\Data\\IFPRI\\World Bank BRIGHT\\Output\\Imputed_Bright_match.dta")
bright$survey="BRIGHT_24-25"
#bright$sector=factor(bright$sector, levels=c(1,2,3),labels=c("Urban","Rural","Estate"))

bright = bright |>
  rename(popwt=popweight,
        hhsize=hhmem,
        edu_hhh_sec=edu_hhh_OL) |>
  mutate(urban = sector,
    urban = case_when(
    urban == 1 ~ 1,
    urban == 2 ~ 0,
    urban == 3 ~ 0,
    TRUE ~ as.numeric(urban)
  ))

# Compare variable names
lfs_names <- names(lfs)
bright_names <- names(bright)

exact_common <- intersect(lfs_names, bright_names) |> sort()

comp_vars = c("age_avg", "age_hhh", "buddhist_hhh","sinhala_hhh",
 "edu_hhh_none","edu_sh_1564_none", "sector", "district", "hhsize",
 "edu_hhh_prim", "edu_hhh_sec", "female_hhh", "have_agri_emp"  ,
    "have_constr_emp"  , "have_ind_emp"  , "have_serv_emp"  ,
     "hh_main_agri"  , "hh_main_ind"       , "hh_main_serv",
    "married_hhh" ,"num_deps" ,"num_kids", "sh_mem_fem", "sh_wages",
    "welfare","popwt","survey", "share_dep", "has_in_school", "num_old",
    "employer_hhh","urban"
  )


lfs=lfs |> select(all_of(comp_vars))
bright=bright |> select(all_of(comp_vars))

df=bind_rows(lfs,bright)
df$urban=factor(df$urban, levels=c(0,1),labels=c("Rural","Urban"))
df$welfare=df$welfare*(12/365)/cpi21/icp21

df$pov30 = ifelse(df$welfare<3,1,0)
df$pov42 = ifelse(df$welfare<4.2,1,0)
df$pov83 = ifelse(df$welfare<8.3,1,0)
svydf <- svydesign(ids = ~1, data = df, 
                   weights = ~popwt)

tab1=svyby(~pov30+pov42+pov83, ~survey, design=svydf, svymean,
           na.rm=TRUE,vartype = "ci")
tab1$urban="National"

#Poverty by sector
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

# Merge the long data frames by survey, sector, and variable
plot_data <- means_long %>%
  left_join(ci_lower_long, by = c("survey", "urban", "variable")) %>%
  left_join(ci_upper_long, by = c("survey", "urban", "variable"))

# Correct labels in poverty lines
plot_data$variable=factor(plot_data$variable,
                          levels=c("pov30","pov42","pov83"),
                          labels=c("$3.0 PPP21","$4.2 PPP21","$8.3 PPP21"))

plot_data$survey=factor(plot_data$survey,
                        levels=c("LFS_24","BRIGHT_24-25"),
                        labels=c("LFS 24","BRIGHT 24-25"))

plot_data$urban=factor(plot_data$urban,
                         levels=c("Urban","Rural","National"),
                         labels=c("Urban","Rural","National"))

# Create the bar plot with error bars and facet by variable (rows) and area (columns)
ggplot(plot_data |> filter(urban %in% c("Urban", "Rural","National"),
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
             "/Outputs/Main/Figures/poverty rates lfs bright 24 CI barplot.png",sep=""),
       width = 15, height = 10, units = "cm")


# Overall profile comparison: LFS 2024 vs. BRIGHT 2024-25

prof_vars_perc = setdiff(comp_vars, c("welfare","popwt","survey",
"age_avg","age_hhh","hhsize","num_deps","num_kids","num_old","district","sector"))
prof_vars_num = c("age_avg","age_hhh","hhsize","num_deps","num_kids","num_old")

#Back to dummy for urban for profile comparison
df$urban=as.numeric(df$urban=="Urban")

# Weighted means by survey (fixes summarize/across + factor issue) ----
radar_wide <- df |>
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
  urban           = "Urban residence",
  employer_hhh    = "HH head is employer",
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
  sh_wages        = "Wage income share"
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

radar_plot_data$survey <- factor(radar_plot_data$survey, levels = c("LFS_24", "BRIGHT_24-25"),
                                labels = c("LFS 2024", "BRIGHT 2024-25"))

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
    title = "Overall profile comparison: LFS 2024 vs. BRIGHT 2024-25",
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
             "/Outputs/Main/Figures/radar_plot_lfs_bright_24.png",sep=""),
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

tab_prof_num <- df |>
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
             "/Outputs/Main/Tables/numeric_profile_comparison_lfs_bright_24.csv",sep=""), 
             row.names = FALSE)


# Comparison of percentage profile variables overall: LFS 2024 vs. BRIGHT 2024-25
tab_prof_perc <- df |>
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
             "/Outputs/Main/Tables/percentage_profile_comparison_lfs_bright_24_all.csv",sep=""), 
             row.names = FALSE)


#Profile of the poor: LFS 2024 vs. BRIGHT 2024-25

# Weighted means by survey (fixes summarize/across + factor issue) ----
radar_wide <- df |>
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

radar_plot_data$survey <- factor(radar_plot_data$survey, levels = c("LFS_24", "BRIGHT_24-25"),
                                labels = c("LFS 2024", "BRIGHT 2024-25"))

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
             "/Outputs/Main/Figures/radar_plot_lfs_bright_24_poor.png",sep=""),
       width = 24, height = 20, units = "cm")

#Numerical comparison of means by survey


tab_prof_num <- df |>
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
             "/Outputs/Main/Tables/numeric_profile_comparison_lfs_bright_24_poor.csv",sep=""), 
             row.names = FALSE)

# Comparison of percentage profile variables for the poor: LFS 2024 vs. BRIGHT 2024-25
tab_prof_perc <- df |>
  filter(pov42 == 0) |>
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
             "/Outputs/Main/Tables/percentage_profile_comparison_lfs_bright_24_non_poor.csv",sep=""), 
             row.names = FALSE)
