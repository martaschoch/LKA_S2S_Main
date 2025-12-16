
df.test=read_dta(paste(datapath,
     "/Data/Stage 2/Raw/IND_2022_PLFS/IND_2022_PLFS_v01_M/Data/Stata/IND_2022_PLFS-V1_v01_M.dta",
    sep="")) 

d <- df.test %>%
    filter(is.finite(hh_usual_cons_rs) & relationtohead==1) %>%
    mutate(
        round_cat = case_when(
            hh_usual_cons_rs %% 1000 == 0 ~ "Multiple of 1000",
            hh_usual_cons_rs %%  500 == 0 ~ "Multiple of 500 (not 1000)",
            TRUE                           ~ "Neither 1000 nor 500"
        ),
        decile = ntile(hh_usual_cons_rs, 10)
    )

# 1) Overall proportions (unweighted)
overall_props <- d %>%
    count(round_cat) %>%
    mutate(prop = n / sum(n))

overall_props


decile_shares <- d %>%
    count(decile, round_cat) %>%
    group_by(decile) %>%
    mutate(pct_in_decile = n / sum(n)) %>%
    ungroup()

decile_shares <- decile_shares %>%
    mutate(round_cat = fct_rev(factor(round_cat)))


# stacked bar: each decile sums to 100%
ggplot(decile_shares, aes(x = factor(decile), y = pct_in_decile, fill = round_cat)) +
    geom_col() +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(
        x = "Consumption decile (based on abbreviated consumption)",
        y = "Percent of households (within decile)",
        fill = "",
        title = "Rounding/heaping categories by consumption decile (2022)"
    ) +
    theme_minimal()+
    theme(legend.position = "bottom")


ggsave(paste(path,
             "/Outputs/Annex/Figures/Rounding plot 2022.png",sep=""),
       width = 20, height = 20, units = "cm")

# Now let's reproduce the heaping in the HCES and recalculate poverty and inequality

targets <- decile_shares %>%
    select(decile, round_cat, pct_in_decile) %>%
    pivot_wider(names_from = round_cat, values_from = pct_in_decile) %>%
    transmute(
        decile,
        p1000 = `Multiple of 1000`,
        p500  = `Multiple of 500 (not 1000)`,
        pnone = `Neither 1000 nor 500`
    )
#load HCES 2022

df3=read_dta(paste(datapath,
                        "/Data/Stage 1/Cleaned/HCES22_s2s.dta",sep="")) 

set.seed(1729)  # for reproducible tie-breaking

# helper: largest remainder allocation so counts sum exactly to group size
alloc_counts <- function(n, p1000, p500, pnone) {
    p <- c(p1000, p500, pnone)
    raw <- p * n
    base <- floor(raw)
    rem <- n - sum(base)
    if (rem > 0) {
        frac <- raw - base
        add_idx <- order(frac, decreasing = TRUE)[seq_len(rem)]
        base[add_idx] <- base[add_idx] + 1
    }
    names(base) <- c("n1000", "n500", "nnone")
    base
}

# nearest multiple of 1000 (ties at .5 go up with this formula for positive x)
nearest_1000 <- function(x) 1000 * floor(x/1000 + 0.5)

# nearest multiple of 500 that is NOT a multiple of 1000:
# i.e., nearest value of form 500 + 1000*k
nearest_500_not1000 <- function(x) 500 + 1000 * floor((x - 500)/1000 + 0.5)

df3_heaped <- df3 %>%
    filter(is.finite(mpce_sp_def_ind)) %>%
    mutate(decile = ntile(mpce_sp_def_ind, 10)) %>%
    left_join(targets, by = "decile") %>%
    group_by(decile) %>%
    group_modify(~{
        g <- .x
        n <- nrow(g)
        
        # decile-specific target counts
        cc <- alloc_counts(n, unique(g$p1000), unique(g$p500), unique(g$pnone))
        n1000 <- cc["n1000"]; n500 <- cc["n500"]
        
        x <- g$mpce_sp_def_ind
        
        # distances to candidate heap points
        t1000 <- nearest_1000(x)
        d1000 <- abs(x - t1000)
        
        t500  <- nearest_500_not1000(x)
        d500  <- abs(x - t500)
        
        # pick who becomes "Multiple of 1000" first (most restrictive)
        ord1000 <- order(d1000, runif(n))  # random tie-break
        idx1000 <- ord1000[seq_len(n1000)]
        
        # from remaining, pick who becomes "Multiple of 500 (not 1000)"
        remaining <- setdiff(seq_len(n), idx1000)
        ord500 <- remaining[order(d500[remaining], runif(length(remaining)))]
        idx500 <- ord500[seq_len(n500)]
        
        # assign
        g$round_cat <- "Neither 1000 nor 500"
        g$mpce_sp_def_ind_heaped <- x
        
        g$round_cat[idx500] <- "Multiple of 500 (not 1000)"
        g$mpce_sp_def_ind_heaped[idx500] <- t500[idx500]
        
        g$round_cat[idx1000] <- "Multiple of 1000"
        g$mpce_sp_def_ind_heaped[idx1000] <- t1000[idx1000]
        
        g
    }) %>%
    ungroup()

decile_shares_df3 <- df3_heaped %>%
    count(decile, round_cat) %>%
    group_by(decile) %>%
    mutate(pct_in_decile = n / sum(n)) %>%
    ungroup() %>%
    mutate(round_cat = fct_rev(factor(round_cat)))  # reverse fill order

ggplot(decile_shares_df3, aes(x = factor(decile), y = pct_in_decile, fill = round_cat)) +
    geom_col() +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(x = "Decile (MMRP)", 
         y = "Percent within decile", fill = "Category",
         title= "Rounding/heaping categories by consumption decile ( HCES 2022)"
         ) +
    theme_minimal() +
    theme(legend.position = "bottom")


ggsave(paste(path,
             "/Outputs/Annex/Figures/Rounding plot 2022 HCES.png",sep=""),
       width = 20, height = 20, units = "cm")

# Now plot and calculate poverty

hces.orig <- df3 %>%
    select(urb,mpce_sp_def_ind, pop_wgt) %>%
    mutate(survey = "HCES original") %>%
    rename(welfare=mpce_sp_def_ind)

hces.heaped <- df3_heaped %>%
    select(urb,mpce_sp_def_ind_heaped, pop_wgt) %>%
    mutate(survey = "HCES rounded")%>%
    rename(welfare=mpce_sp_def_ind_heaped)
# Ensure any labelled columns to plain numeric.


# Append
df <- bind_rows(hces.orig, hces.heaped)

# Remove NAs
df=na.omit(df)

df$povlic = ifelse(df$welfare*(12/365)/cpi21/icp21<lic,1,0)
df$povlmic = ifelse(df$welfare*(12/365)/cpi21/icp21<lmic,1,0)
df$povumic = ifelse(df$welfare*(12/365)/cpi21/icp21<umic,1,0)

#Set as survey
svydf <- svydesign(ids = ~1, data = df, 
                   weights = ~pop_wgt)

#Tables and graphs

#Density of predicted consumption
ggplot(df, aes(x = log(welfare), weight = pop_wgt,
               fill = survey)) +
    geom_density(alpha = 0.4, adjust=1.5) +
    labs(x = "Log Consumption",
         y = "Density",
         title = "Original and Heaped Log Consumption HCES (2022)")+
    xlim(c(6,11))

ggsave(paste(path,
             "/Outputs/Annex/Figures/Heaped HCES 2022 distribution.png",sep=""),
       width = 30, height = 20, units = "cm")


### Table 2####

#Gini coefficient
gini1= gini.wtd(df[df$survey=="HCES original",]$welfare,
                df[df$survey=="HCES original",]$pop_wgt)

gini2 = gini.wtd(df[df$survey=="HCES heaped",]$welfare,
                 df[df$survey=="HCES heaped",]$pop_wgt)

tab = data.frame(survey=c("HCES original","HCES heaped"),gini=c(gini1,gini2))

#Overall Poverty
tab1=svyby(~povlic+povlmic+povumic, ~survey, design=svydf, svymean,
           na.rm=TRUE,vartype = "ci")

write.csv(tab1,paste(path,
                     "/Outputs/Annex/Tables/Poverty HCES 2022 heaping.csv",sep=""))

tab2=svyby(~povlic+povlmic+povumic, ~survey+urb, design=svydf, 
           svymean,na.rm=TRUE,vartype = "ci")
tab2$urb=factor(tab2$urb, levels=c(0,1),labels=c("Rural","Urban"))
tab2 = tab2 %>% rename(Sector=urb)
tab2


write.csv(tab2,paste(path,
                     "/Outputs/Annex/Tables/Poverty HCES 2022 heaping by sector.csv",sep=""))
