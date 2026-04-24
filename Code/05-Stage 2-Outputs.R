#lfs 2019
lfs19==read_dta(paste(datapath,
                      "/lfs2019_imputed.dta",
                      sep="")) 
lfs19$hhid=NULL
lfs19=subset(lfs19,select=c(urban,popwt,welfare))
lfs19$survey="LFS_19_imp"
lfs19$urban=factor(lfs19$urban, levels=c(0,1),labels=c("Rural","Urban"))

#HIES 2019
hies.don=read_dta(paste(datapath,"cleaned/hies2019_clean.dta",sep="")) 
hies19 = subset(hies.don,select=c(urban,sector,popwt,welfare,ln_rpcinc1))
hies19$survey="HIES_19"
hies19$urban=factor(hies19$urban, levels=c(0,1),labels=c("Rural","Urban"))
hies19$sector=factor(hies19$sector, levels=c(1,2,3),labels=c("Urban","Rural","Estate"))

#lfs 2016
lfs16.orig=read_dta(paste(datapath,
                          "/lfs2016_imputed.dta",
                          sep="")) 
lfs16.orig=subset(lfs16.orig,select=c(urban,popwt,welfare))
lfs16.imp=lfs16.orig
lfs16.imp$survey="LFS_16_imp"
lfs16.imp$urban=factor(lfs16.imp$urban, levels=c(0,1),labels=c("Rural","Urban"))

#hies 2016
hies16=read_dta(paste(datapath,"hies16ppp.dta",sep=""))
hies16$survey="HIES_16"
hies16$district=NULL
hies16=hies16 %>%
  rename(popwt=weight) %>%
  mutate(urban=factor(urban, levels=c(0,1),labels=c("Rural","Urban")))


#lfs 2020-2024
lfs_imp_list <- lapply(2020:2024, function(year) {
  read_dta(file.path(dataout, paste0("lfs", year, "_imputed.dta"))) |>
    subset(select = c(urban, sector, popwt, welfare,ln_rpcinc1)) |>
    mutate(
      survey = paste0("LFS_", substr(year, 3, 4), "_imp"),
      urban = factor(urban, levels = c(0, 1), labels = c("Rural", "Urban")),
      sector = factor(sector, levels = c(1, 2, 3), labels = c("Urban", "Rural", "Estate"))
    )
})
names(lfs_imp_list) <- paste0("lfs", substr(2020:2024, 3, 4))
list2env(lfs_imp_list, envir = .GlobalEnv)


###APPEND THREE ROUNDS 

lfs.all=bind_rows(lfs20,lfs21,lfs22,lfs23,lfs24)
lfs.all$welfare=lfs.all$welfare*(12/365)/cpi21/icp21 #convert to 2021 PPP

hies19$welfare=hies19$welfare*(12/365)/cpi21/icp21 #convert to 2021 PPP
df=bind_rows(lfs.all,hies19)
df=na.omit(df)

df$pov30 = ifelse(df$welfare<3,1,0)
df$pov42 = ifelse(df$welfare<4.2,1,0)
df$pov83 = ifelse(df$welfare<8.3,1,0)

#Set as survey
svydf <- svydesign(ids = ~1, data = df, 
                   weights = ~popwt)

tab1=svyby(~pov30+pov42+pov83, ~survey, design=svydf, svymean,
           na.rm=TRUE,vartype = "ci")
tab1$sector="National"
write.csv(tab1,paste(outpath,
                     "/Outputs/Main/Tables/Poverty 2019 2024 national.csv",sep=""),
          row.names = FALSE)

#Poverty by sector
tab2=svyby(~pov30+pov42+pov83, ~survey+sector, design=svydf, 
           svymean,na.rm=TRUE,vartype = "ci")

write.csv(tab2,paste(outpath,
                     "/Outputs/Main/Tables/Poverty 2019 2024.csv",sep=""),row.names = FALSE)

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
  left_join(ci_lower_long, by = c("survey", "sector", "variable")) %>%
  left_join(ci_upper_long, by = c("survey", "sector", "variable"))

# Correct labels in poverty lines
plot_data$variable=factor(plot_data$variable,
                          levels=c("pov30","pov42","pov83"),
                          labels=c("$3.0 PPP21","$4.2 PPP21","$8.3 PPP21"))

plot_data$survey=factor(plot_data$survey,
                        levels=c("HIES_19","LFS_20_imp","LFS_21_imp","LFS_22_imp","LFS_23_imp","LFS_24_imp"),
                        labels=c("HIES_19","LFS_20_imp","LFS_21_imp","LFS_22_imp","LFS_23_imp","LFS_24_imp"))

plot_data$sector=factor(plot_data$sector,
                         levels=c("Urban","Rural","Estate","National"),
                         labels=c("Urban","Rural","Estate","National"))

# Create the bar plot with error bars and facet by variable (rows) and area (columns)
ggplot(plot_data, 
       aes(x = survey, y = mean, fill = survey)) +
  geom_bar(stat = "identity", width = 0.7, position = position_dodge()) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                width = 0.2, 
                position = position_dodge(width = 0.7)) +
  geom_text(aes(label = percent(mean, accuracy = 0.1), y = mean/2), 
            position = position_dodge(width = 0.7),
            color = "black", size = 3) +
  facet_grid(variable ~ sector, scales = "free_y") +
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
             "/Outputs/Main/Figures/poverty rates hies lfs 19 - 24 CI barplot.png",sep=""),
       width = 30, height = 20, units = "cm")

#Line plot with ribbons
plot_data_line <- plot_data |>
  mutate(survey_num = as.numeric(survey))

p_line <- ggplot(
  plot_data_line,
  aes(x = survey_num, y = mean, color = variable, fill = variable, group = variable)
) +
  geom_ribbon(
    aes(ymin = ci_lower, ymax = ci_upper),
    alpha = 0.18,
    linewidth = 0,
    color = NA
  ) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  facet_wrap(~sector, ncol = 2) +
  scale_x_continuous(
    breaks = seq_along(levels(plot_data$survey)),
    labels = levels(plot_data$survey)
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Survey",
    y = "Poverty Rate (%)",
    color = "Poverty Line",
    fill = "Poverty Line",
    title = "Original and Imputed Poverty Rates (95% CI)"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

p_line
ggsave(
  filename = file.path(
    outpath,
    "Outputs/Main/Figures/poverty rates hies lfs 19 - 24 CI lineplot.png"
  ),
  plot = p_line,
  width = 30,
  height = 20,
  units = "cm"
)

#ECDF
line300 = log(3.0)
line420 = log(4.2)
line830 = log(8.3)

df_ecdf <- subset(df,survey=="HIES_16" | survey=="LFS_16_imp" ) %>%
  group_by(survey) %>%
  arrange(log_consumption = log(welfare)) %>%
  mutate(cum_weight = cumsum(popwt),
         total_weight = sum(popwt),
         ecdf = cum_weight / total_weight)


ggplot(df_ecdf, aes(x = log(welfare), y = ecdf, color = survey)) +
  geom_step() +
  labs(x = "Log Consumption",
       y = "Density",
       title = "ECDF of Official and Imputed Log Consumption (2016)")+
  geom_vline(xintercept = line300,linetype="dashed",size=0.5)+
  geom_vline(xintercept = line420,linetype="dashed",size=0.5)+
  geom_vline(xintercept = line830,linetype="dashed",size=0.5)+
  annotate("text", x = line300, y = 0, label = "3.0", vjust = 1.5,size=2.5)+
  annotate("text", x = line420, y = 0, label = "4.2", vjust = 1.5,size=2.5)+
  annotate("text", x = line830, y = 0, label = "8.3", vjust = 1.5,size=2.5)

ggsave(paste(path,
             "/Outputs/Main/Figures/ecdf_hies lfs 19.png",sep=""),
       width = 30, height = 20, units = "cm")

# ====================================================
# GICs
# ====================================================

dftemp <- df %>%
  filter(survey %in% c("HIES_19", "LFS_24_imp"))

dftemp_nat <- dftemp %>%
  group_by(survey) %>%
  mutate(
    pctile_nat = xtile(welfare, n = 100, w = popwt)
  ) %>%
  ungroup()

mean_nat <- dftemp_nat %>%
  group_by(survey, pctile_nat) %>%
  summarise(
    welfare_avg = weighted.mean(welfare, popwt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = survey,
    values_from = welfare_avg
  ) %>%
  mutate(
    group = "National",
    growth_rate = (`LFS_24_imp` / `HIES_19`)^(1/4) - 1,
    pctile = pctile_nat
  ) %>%
  select(group, pctile, growth_rate)


# ====================================================
# URBAN / RURAL PERCENTILES (WITHIN SURVEY × URBAN)
# ====================================================

dftemp_urb <- dftemp %>%
  group_by(survey, sector) %>%
  mutate(
    pctile_urb = xtile(welfare, n = 100, w = popwt)
  ) %>%
  ungroup()

mean_urb <- dftemp_urb %>%
  group_by(survey, sector, pctile_urb) %>%
  summarise(
    welfare_avg = weighted.mean(welfare, popwt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = survey,
    values_from = welfare_avg
  ) %>%
  mutate(
    growth_rate = (`LFS_24_imp` / `HIES_19`)^(1/4) - 1,
    group = sector,
    pctile = pctile_urb
  ) %>%
  select(group, pctile, growth_rate)

# ====================================================
# COMBINE NATIONAL + URBAN + RURAL
# ====================================================

final_plot_df <- bind_rows(mean_nat, mean_urb) %>%
  filter(pctile > 2, pctile < 98)

# ====================================================
# PLOT
# ====================================================

ggplot(final_plot_df, aes(x = pctile, y = growth_rate, color = group)) +
  geom_line(linewidth = 1) +
  theme_minimal() +
  labs(
    title = "Growth Incidence Curve - Consumption (2019–2024)",
    x = "Consumption Percentile",
    y = "Annualized Growth Rate",
    color = "Population"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))+
  scale_color_manual(values = c("National" = "orange", "Urban" = "darkgreen", 
                                "Rural" = "blue", "Estate" = "steelblue"))+
  geom_hline(yintercept = 0)


ggsave(paste(outpath,
             "/Outputs/Main/Figures/GIC 19 24.png",sep=""),
       width = 30, height = 20, units = "cm")


# ====================================================
# GIC of labor income
# ====================================================

dftemp_nat2 <- dftemp %>%
  group_by(survey) %>%
  mutate(
    pctile_nat = xtile(ln_rpcinc1, n = 100, w = popwt)
  ) %>%
  ungroup()

mean_nat2 <- dftemp_nat2 %>%
  group_by(survey, pctile_nat) %>%
  summarise(
    inc_avg = weighted.mean(ln_rpcinc1, popwt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = survey,
    values_from = inc_avg
  ) %>%
  mutate(
    group = "National",
    growth_rate = (`LFS_24_imp` / `HIES_19`)^(1/4) - 1,
    pctile = pctile_nat
  ) %>%
  select(group, pctile, growth_rate)


# ====================================================
# URBAN / RURAL PERCENTILES (WITHIN SURVEY × URBAN)
# ====================================================

dftemp_urb2 <- dftemp %>%
  group_by(survey, sector) %>%
  mutate(
    pctile_urb = xtile(ln_rpcinc1, n = 100, w = popwt)
  ) %>%
  ungroup()

mean_urb2 <- dftemp_urb2 %>%
  group_by(survey, sector, pctile_urb) %>%
  summarise(
    inc_avg = weighted.mean(ln_rpcinc1, popwt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = survey,
    values_from = inc_avg
  ) %>%
  mutate(
    growth_rate = (`LFS_24_imp` / `HIES_19`)^(1/4) - 1,
    group = sector,
    pctile = pctile_urb
  ) %>%
  select(group, pctile, growth_rate)

# ====================================================
# COMBINE NATIONAL + URBAN + RURAL
# ====================================================

final_plot_df2 <- bind_rows(mean_nat2, mean_urb2) %>%
  filter(pctile > 2, pctile < 98)

# ====================================================
# PLOT
# ====================================================

ggplot(final_plot_df2, aes(x = pctile, y = growth_rate, color = group)) +
  geom_line(linewidth = 1) +
  theme_minimal() +
  labs(
    title = "Growth Incidence Curve - Labor Income (2019–2024)",
    x = "Labor Income Percentile",
    y = "Annualized Growth Rate",
    color = "Population"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))+
  scale_color_manual(values = c("National" = "orange", "Urban" = "darkgreen", 
                                "Rural" = "blue", "Estate" = "steelblue"))+
  geom_hline(yintercept = 0)


ggsave(paste(outpath,
             "/Outputs/Main/Figures/GIC 19 24.png",sep=""),
       width = 30, height = 20, units = "cm")

