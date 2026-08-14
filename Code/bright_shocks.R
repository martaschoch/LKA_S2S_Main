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

bright$welfare=bright$welfare*(12/365)/cpi21/icp21

bright$poor = ifelse(bright$welfare<4.2,1,0)

svydf <- svydesign(ids = ~1, data = bright, 
                   weights = ~popwt)

svyby(~poor, ~survey, design=svydf, svymean,
           na.rm=TRUE,vartype = "ci")

bright = bright |>
  mutate(province = case_when(
    province == 1 ~ "Western",
    province == 2 ~ "Central",
    province == 3 ~ "Southern",
    province == 4 ~ "Northern",
    province == 5 ~ "Eastern",
    province == 6 ~ "North Western",
    province == 7 ~ "North Central",
    province == 8 ~ "Uva",
    province == 9 ~ "Sabaragamuwa",
    TRUE ~ as.character(province)
  ))


bright = bright |>
  group_by(survey) |>
  mutate(quintile=xtile(welfare,n=5,wt=popwt),
         quintile=as.factor(quintile)) |>
  ungroup()

bright = bright |>
  mutate(debt_formal = ifelse(debt_banks == 1 | debt_financecomp == 1, 1, 0),
    debt_rest = ifelse(debt_pawn_moneylender == 1 | debt_employer == 1 |
      debt_retail == 1 | debt_other == 1, 1, 0)) |>
  mutate(debt_type = case_when(
    debt_formal == 1 & debt_rest == 0 ~ "Financial system",
    debt_rest == 1 & debt_formal == 0 ~ "Other",
    debt_formal == 1 & debt_rest == 1 ~ "Both",
    TRUE ~ "No debt"
  ))

barplot = bright |>
  mutate(debt_type = factor(debt_type, levels = c("Both", "Other", "Financial system"))) |>
  group_by(quintile, debt_type) |>
  summarise(n = sum(popwt, na.rm = TRUE), .groups = "drop") |>
  group_by(quintile) |>
  mutate(share = n / sum(n)) |>
  filter(debt_type != "No debt") |>
  ggplot(aes(x = quintile, y = share, fill = debt_type)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x    = "Consumption Quintile",
    y    = "Share of Households",
    fill = "Debt Type"
  ) +
  theme_wb %+replace%
  theme(
    text = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 15)
  )

barplot
ggsave(paste(outpath,
             "/Outputs/Main/Figures/barplot_indebtedness_quintile.png",sep=""),
       width = 15, height = 10, units = "cm")

barplot = bright |>
  mutate(debt_type = factor(debt_type, levels = c("Both", "Other", "Financial system"))) |>
  group_by(province, debt_type) |>
  summarise(n = sum(popwt, na.rm = TRUE), .groups = "drop") |>
  group_by(province) |>
  mutate(share = n / sum(n)) |>
  filter(debt_type != "No debt") |>
  ggplot(aes(x = province, y = share, fill = debt_type)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x    = "Province",
    y    = "Share of Households",
    fill = "Debt Type"
  ) +
  theme_wb %+replace%
  theme(
    text = element_text(size = 16),
    axis.text = element_text(size = 14),
    axis.text.x = element_text(size = 14,angle=45),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 15)
  )

barplot
ggsave(paste(outpath,
             "/Outputs/Main/Figures/barplot_indebtedness_province.png",sep=""),
       width = 20, height = 13, units = "cm")

b.shocks=read_dta("C:\\Users\\wb553773\\WBG\\Marta Schoch - Analysis\\Data\\IFPRI\\World Bank BRIGHT\\Data\\Wide\\mod_o1_shocks.dta")

b.shocks = b.shocks |>
  rename(hhid=hhcode,
  s_food_price=o1_01_1,
  s_fuel_price=o1_01_2,
  s_financial=o1_01_3,
  s_empl_inc=o1_01_4,
  s_electricity=o1_01_5,
  s_flood=o1_01_9,
  s_landslide=o1_01_10) |>
  select(hhid,s_financial,s_empl_inc,s_food_price,s_fuel_price,
    s_electricity,s_flood,s_landslide)

b.shocks <- b.shocks |>
  mutate(across(-hhid, ~ factor(., levels = c(1, 2, 3), labels = c("Severe", "Moderate", "No shock"))))

bright = bright |>
   left_join(b.shocks, by="hhid")


plot_shocks = bright |>
  group_by(quintile) |>
  summarise(across(starts_with("s_"), ~ weighted.mean(. == "Severe", w = popwt, na.rm = TRUE))) |>
  pivot_longer(cols = c("s_financial","s_empl_inc","s_flood","s_landslide"), names_to = "shock", values_to = "share_severe") |>
  ggplot(aes(x = quintile, y = share_severe, fill = shock)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(
    labels = c(
      s_empl_inc  = "Employment/Income loss",
      s_financial = "Financial shock",
      s_flood     = "Flood",
      s_landslide = "Landslide"
    )
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Consumption Quintile", y = "Share of Households with Severe Shock", fill = "Shock Type") +
  theme_wb %+replace%
  theme(
    text = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 14)
  )

plot_shocks

ggsave(paste(outpath,
             "/Outputs/Main/Figures/barplot_shocks_quintile.png",sep=""),
       width = 20, height = 14, units = "cm")

plot_shocks = bright |>
  group_by(province) |>
  summarise(across(starts_with("s_"), ~ weighted.mean(. == "Severe", w = popwt, na.rm = TRUE))) |>
  pivot_longer(cols = c("s_financial","s_empl_inc","s_flood","s_landslide"), names_to = "shock", values_to = "share_severe") |>
  ggplot(aes(x = province, y = share_severe, fill = shock)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(
    labels = c(
      s_empl_inc  = "Employment/Income loss",
      s_financial = "Financial shock",
      s_flood     = "Flood",
      s_landslide = "Landslide"
    )
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Province", y = "Share of Households with Severe Shock", fill = "Shock Type") +
  theme_wb %+replace%
   theme(
    text = element_text(size = 16),
    axis.text = element_text(size = 14),
    axis.text.x = element_text(size = 14,angle=45),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 15)
  )

plot_shocks

ggsave(paste(outpath,
             "/Outputs/Main/Figures/barplot_shocks_province.png",sep=""),
       width = 22, height = 16, units = "cm")


b.cope=read_dta("C:\\Users\\wb553773\\WBG\\Marta Schoch - Analysis\\Data\\IFPRI\\World Bank BRIGHT\\Data\\Wide\\mod_o2_coping.dta")
b.cope = b.cope |>
  mutate(c_school=case_when(
    o2_15 == 1 ~ "Yes",
    o2_15 == 2 | o2_15 == 3 ~ "No",
    TRUE ~ NA_character_ ),
    c_health=case_when(
    o2_09 == 1 ~ "Yes",
    o2_09 == 2 | o2_09 == 3 ~ "No",
    TRUE ~ NA_character_ ),
  c_assets=case_when(
    o2_01 == 1 ~ "Yes",
    o2_01 == 2 | o2_01 == 3 ~ "No",
    TRUE ~ NA_character_ ) ) |>
  rename(hhid=hhcode) |>
  select(hhid,c_school,c_health,c_assets)

bright = bright |>
  left_join(b.cope, by="hhid")

plot_cope = bright |>
  group_by(quintile) |>
  summarise(across(starts_with("c_"), ~ weighted.mean(. == "Yes", w = popwt, na.rm = TRUE))) |>
  pivot_longer(cols = c("c_school","c_health","c_assets"), names_to = "cope", values_to = "share_yes") |>
  ggplot(aes(x = quintile, y = share_yes, fill = cope)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(
    labels = c(
      c_school = "Reduce school attendance",
      c_health = "Cut back on health expenses",
      c_assets = "Sell assets"
    )
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Consumption Quintile", y = "Share of Households with Coping Strategy", fill = "Coping Strategy") +
  theme_wb %+replace%
  theme(
    text = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 15)
  )

plot_cope
ggsave(paste(outpath,
             "/Outputs/Main/Figures/barplot_cope_quintile.png",sep=""),
       width = 25, height = 15, units = "cm")

plot_cope = bright |>
  group_by(province) |>
  summarise(across(starts_with("c_"), ~ weighted.mean(. == "Yes", w = popwt, na.rm = TRUE))) |>
  pivot_longer(cols = c("c_school","c_health","c_assets"), names_to = "cope", values_to = "share_yes") |>
  ggplot(aes(x = province, y = share_yes, fill = cope)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(
    labels = c(
      c_school = "Reduce school attendance",
      c_health = "Cut back on health expenses",
      c_assets = "Sell assets"
    )
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Province", y = "Share of Households with Coping Strategy", fill = "Coping Strategy") +
  theme_wb %+replace%
   theme(
    text = element_text(size = 16),
    axis.text = element_text(size = 14),
    axis.text.x = element_text(size = 14,angle=45),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 15)
  )

plot_cope

ggsave(paste(outpath,
             "/Outputs/Main/Figures/barplot_cope_province.png",sep=""),
       width = 25, height = 17, units = "cm")


#plot coefficient of variation of labor income by province and quintile