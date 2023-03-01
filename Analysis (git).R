## Regression Analysis 

rm(list = ls())

dat <- read.csv(file.choose())

dat$newid <- as.factor(dat$newid)
dat$sex <- as.factor(dat$sex)
dat$school_year_key <- as.factor(dat$school_year_key)
dat$prev_concussion <- as.factor(dat$prev_concussion)

## Data Clean 
library(tidyverse)

dat <- dat %>% 
  mutate( 
    status = case_when(
      injury_type != 10 ~ 1,         # Censoring status, old analysis
      injury_type == 10 ~ 0
    ),
    contact_status = case_when(
      contact_status == 1 ~"Collision",
      contact_status == 2 ~ "Contact",
      contact_status == 3 ~ "Limited Contact"
    ),
    injury_surfaces = case_when(
      injury_surfaces == 1 ~ "Constructed",
      injury_surfaces == 2 ~ "Data_Not_Provided",
      injury_surfaces == 3 ~ "Multiple",
      injury_surfaces == 5 ~ "Organic",
      injury_surfaces == 6 ~ "Other",
      injury_surfaces == 7 ~ "Synthetic"
    ),
    sex = case_when(
      sex == 1 ~ "Female",
      sex == 2 ~ "Male"
    )
  ) %>% 
  filter(
    injury_surfaces != "Data_Not_Provided"  # Filters out non-useful predictor info
  ) %>% 
  mutate_if(
    is.character, as.factor
  )

### Model Libraries 
library(ggfortify)
library(lme4)
library(lmerTest)
library(car)
library(lsmeans)

#### All Years Analysis 

dat_all <- dat %>%                  # Filter based on conv. with PRA
  filter(concussion17 == 0) %>% 
  filter(concussion18 == 0) %>% 
  filter(concussion19 == 0) %>% 
  filter(concussion20 == 0) %>% 
  filter(dup <= 2) %>% 
  mutate_if(is.character, as.factor)
  
# write.csv(dat_all, "LEMSK Data/dat_sum1.csv")

# Model 1
mod_all <- lmer(
  formula = failure_time ~ sex + prev_concussion + injury_surfaces + contact_status + 
    (1|newid) + (1|school_year_key), 
  data = dat_all
)

mod_all.sum <- anova(mod_all)

mod_all.sum <- as.data.frame(round(anova(mod_all), 3))

rownames(mod_all.sum) <- c(
  "Sex", 
  "Had Previous Concussion",
  "Injury Surface", 
  "Contact Status"
  )

names(mod_all.sum) <- c(names(mod_all.sum[1:3]),"df", "F-value", "p-value")

mod_all.sum <- mod_all.sum %>% 
  mutate(
    `p-value` = case_when(
      `p-value` <= 0.001 ~ "p < 0.001",
      TRUE ~ as.character(`p-value`)
    )
  )

# Contrasts 

ls_sex <- as.data.frame(lsmeans(mod_all, pairwise ~ sex)$contrast)

names(ls_sex) <- c("Contrast", "Difference", "SE", "df", "T-ratio", "p-value")

ls_is <- as.data.frame(lsmeans(mod_all, pairwise ~ injury_surfaces, adjust = "Tukey")$contrast) %>% 
  mutate_if(is.numeric, round, digits = 3)
names(ls_is) <- c("Contrast", "Difference", "SE", "df", "T-ratio", "p-value")

ls_is <- ls_is %>% 
  mutate(
    `p-value` = case_when(
      `p-value` <= 0.001 ~ "p < 0.001",
      TRUE ~ as.character(`p-value`)
    )
  )

ls_cs <- as.data.frame(lsmeans(mod_all, pairwise ~ contact_status)$contrast) %>% 
  mutate_if(is.numeric, round, digits = 3)
names(ls_cs) <- c("Contrast", "Difference", "SE", "df", "T-ratio", "p-value")

ls_cs <- ls_cs %>% 
  mutate(
    `p-value` = case_when(
      `p-value` <= 0.001 ~ "p < 0.001",
      TRUE ~ as.character(`p-value`)
    )
  )

ls_pc <- as.data.frame(lsmeans(mod_all, pairwise ~ prev_concussion)$contrast)
names(ls_pc) <- c("Contrast", "Difference", "SE", "df", "T-ratio", "p-value")

## Plots, failure_time ~ injury_surfaces + contact_status

dat_p <- dat_all

ggplot(
  data = dat_p,
  aes(
    x = injury_surfaces,
    y = failure_time
  )
) + 
  stat_boxplot(
    geom = "errorbar"
  ) + 
  geom_boxplot(
    
  ) + 
  theme_bw(
    
  ) + 
  labs(
    x = "Playing Surface",
    y = "Time to LEMSKI"
  ) + 
  scale_x_discrete(
    limits = c("Constructed", "Organic", "Synthetic", "Multiple", "Other")
  )

ggplot(
  data = dat_p,
  aes(
    x = contact_status,
    y = failure_time
  )
) + 
  stat_boxplot(
    geom = "errorbar"
  ) + 
  geom_boxplot(
    
  ) + 
  theme_bw(
    
  ) + 
  labs(
    x = "Sport Contact Status",
    y = "Time to LEMSKI"
  )

## Summary Tables 

library(table1)

dat.num <- dat_all %>% 
  group_by(
    newid
  ) %>%
  summarise(across(.cols = failure_time, mean))

dat.cat <- dat_all %>% 
  select(
    sex, prev_concussion, injury_surfaces, contact_status, newid, school_year_key
  ) %>% 
  mutate(
    prev_concussion = case_when(
      prev_concussion == 0 ~ "No Previous Concussion",
      prev_concussion == 1 ~ "Previous Concussion"
    )
  ) %>% 
  group_by(
    newid
  ) %>%
  slice(1)
  
dat.sum <- merge(dat.num, dat.cat, by = "newid") %>% 
  na.omit()


render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits = 3, round.integers = F, digits.pct = 2), 
       c("", "Mean (SD)" = sprintf("%s (&plusmn;%s)", MEAN, SD)))
}

render.cat <- function(x) {
  c("", 
    sapply(stats.default(x), 
           function(y) with(y, sprintf("%d (%0.0f%%)", FREQ, PCT))))
}


pvalue <- function(x, ...) {
  y <- unlist(x)
  g <- factor(rep(1:length(x), times = sapply(x, length)))
  if (is.numeric(y)) {
    p <- t.test(y ~ g)$p.value
  } else {
    p <- chisq.test(table(y, g))$p.value
  }
  c("", sub("<", "&lt;", format.pval(p, digits = 3, eps = 0.001)))
}

label(dat.sum$failure_time) <- "Mean Time to LEMSKI (Days)"
label(dat.sum$sex) <- "Sex"
label(dat.sum$prev_concussion) <- "Previous Concussion"
label(dat.sum$injury_surfaces) <- "Injury Surface"
label(dat.sum$contact_status) <- "Contact Type"

write.csv(dat.sum, "LEMSKI_summary_dat.csv", row.names = F)

tab1 <- table1(
  ~ failure_time + sex + prev_concussion + injury_surfaces + contact_status,
  data = dat.sum,
  render.continuous = render.cont,
  render.categorical = render.cat)

tab1

tab2 <- table1(
  ~ failure_time + sex + injury_surfaces + contact_status | prev_concussion,
  data = dat.sum,
  render.continuous = render.cont,
  render.categorical = render.cat,
  extra.col=list(`P-value`= pvalue),
  render.missing = NULL,
  overall = F
)

tab2

tab3 <- table1(
  ~ failure_time + prev_concussion + injury_surfaces + contact_status | sex,
  data = dat.sum,
  render.continuous = render.cont,
  render.categorical = render.cat,
  extra.col=list(`P-value`= pvalue),
  render.missing = NULL,
  overall = F
)

tab3
