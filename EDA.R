library(knitr)
library(tidyverse)
library(mice)
library(GGally)
library(UpSetR)
library(naniar)
library(tableone)

# Load data
df_raw <- read.csv("project1.csv") # raw df
df_tobacco <- df_raw # this will be our processed df
# look at each variable

# Covariate ----

## Child ----

# child age
summary(df_tobacco$tage)

# child sex - 
summary(df_tobacco$tsex)

# child language - 
summary(df_tobacco$language)

# child ethic - 
table(df_tobacco$tethnic, useNA = "always") # miss 12

# child race - 

summary(df_tobacco$twhite)

# check if someone didn't answer at all

df_tobacco %>% 
  mutate(num_trace = taian + tasian + tnhpi + tblack + twhite + trace_other) %>% 
  group_by(num_trace) %>% 
  summarise(n = n())

df_tobacco <- df_tobacco %>% 
  mutate(num_trace = taian + tasian + tnhpi + tblack + twhite + trace_other)

# we see co-occurrence pattern in missing values

md.pattern(df_tobacco %>%
             select(tage, tsex, language, num_race) %>% 
             filter(num_race == 0))

md.pattern(df_tobacco %>%
             select(tage, tsex, language, num_race) %>% 
             filter(num_race != 0))

# child cigarette - 
summary(df_tobacco$cig_ever)
sum(df_tobacco$cig_ever == 1, na.rm = TRUE) 

# child # of cigarette - 
summary(df_tobacco$num_cigs_30)
sum(df_tobacco$num_cigs_30 == 0, na.rm = TRUE) 

# child e-cigarette - 
summary(df_tobacco$e_cig_ever)
sum(df_tobacco$e_cig_ever == 1, na.rm = TRUE) # 3

# child # of e-cigarette - 
summary(df_tobacco$num_e_cigs_30)
table(df_tobacco$num_e_cigs_30, useNA = "always") 

# child marijuana - 
summary(df_tobacco$mj_ever)
sum(df_tobacco$mj_ever == 1, na.rm = TRUE) # 3

# child # of marijuana
summary(df_tobacco$num_mj_30)
table(df_tobacco$num_mj_30, useNA = "always") 

# child alcohol - 
summary(df_tobacco$alc_ever)
table(df_tobacco$alc_ever, useNA = "always") # 5

# child # of alcohol
summary(df_tobacco$num_alc_30)
table(df_tobacco$num_alc_30, useNA = "always") 

# child parental knowledge
summary(df_tobacco$pmq_parental_knowledge) 
summary(df_tobacco$pmq_child_disclosure) 
summary(df_tobacco$pmq_parental_solicitation) 
summary(df_tobacco$pmq_child_disclosure) 

md.pattern(df_tobacco %>%
             select(tage, tsex, language, num_trace, 
                    cig_ever, e_cig_ever, mj_ever, alc_ever,
                    pmq_parental_knowledge, pmq_child_disclosure,
                    pmq_parental_solicitation, pmq_child_disclosure) %>% 
             filter(num_trace == 0)) # co-occurrence, except for one child

md.pattern(df_tobacco %>%
             select(tage, tsex, language, num_trace, 
                    cig_ever, e_cig_ever, mj_ever, alc_ever,
                    pmq_parental_knowledge, pmq_child_disclosure,
                    pmq_parental_solicitation, pmq_child_disclosure) %>% 
             filter(num_trace != 0)) # missing is almost random

## Parent ----

# parent age
summary(df_tobacco$page) # miss 8

# parent sex - 
table(df_tobacco$psex, useNA = "always") # miss 8

# parent language - 
table(df_tobacco$plang, useNA = "always") # miss 8

# parent ethic - 
table(df_tobacco$pethnic, useNA = "always") # miss 8

# parent race
summary(df_tobacco$pwhite)

# check if someone didn't answer at all

df_tobacco %>% 
  mutate(num_prace = paian + pasian + pnhpi + pblack + pwhite + prace_other) %>% 
  group_by(num_prace) %>% 
  summarise(n = n()) # 8

df_tobacco <- df_tobacco %>% 
  mutate(num_prace = paian + pasian + pnhpi + pblack + pwhite + prace_other)

# parent employ - 
table(df_tobacco$employ, useNA = "always") 

# parent education - 
table(df_tobacco$pedu, useNA = "always") 

# parent income - change values
table(df_tobacco$income, useNA = "always") 

unique(df_tobacco$income)

# change value
df_tobacco$income_new <- df_tobacco$income
df_tobacco$income_new[df_tobacco$income_new == ""] <- NA
df_tobacco$income_new[df_tobacco$income_new == "250, 000"] <- 250000
df_tobacco$income_new <- as.numeric(df_tobacco$income_new)

summary(df_tobacco$income_new)

# parent alcohol - 
table(df_tobacco$nidaalc, useNA = "always") 

# parent tobacco - 
table(df_tobacco$nidatob, useNA = "always") 

# parent pres. drugs - 
table(df_tobacco$nidapres, useNA = "always") 

# parent illegal drugs - 
table(df_tobacco$nidaill, useNA = "always") 

# patterns between these 4 variables? - substance use excluding cigarettes

parent_habit <- c("nidaalc", "nidatob", "nidapres", "nidaill")

df_tobacco %>%
  select(all_of(parent_habit)) %>% 
  cor(use = "complete.obs") # cor nah

# stratified into never and ever in past 6 months
table(df_tobacco$nidaalc == 0, df_tobacco$nidatob == 0)

table(df_tobacco$nidapres == 0, df_tobacco$nidaill == 0) # exclude
# but the sample size is too small

# calculate the number of substance
df_tobacco %>%
  select(parent_id, all_of(parent_habit)) %>% 
  na.omit() %>% 
  group_by(parent_id) %>% 
  mutate(num_sub = sum(nidaalc>0, na.rm = TRUE) + 
           sum(nidatob>0, na.rm = TRUE) + 
           sum(nidapres>0, na.rm = TRUE) + 
           sum(nidaill>0, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(nidaalc) %>% 
  summarize(mean_num = mean(num_sub),
            n = n())

df_tobacco %>%
  select(parent_id, all_of(parent_habit)) %>% 
  group_by(parent_id) %>% 
  filter(!( is.na(nidaalc) & is.na(nidatob) &
              is.na(nidapres) & is.na(nidaill))) %>% 
  mutate(num_sub = sum(nidaalc>0, na.rm = TRUE) + 
           sum(nidatob>0, na.rm = TRUE) + 
           sum(nidapres>0, na.rm = TRUE) + 
           sum(nidaill>0, na.rm = TRUE)) %>% 
  filter(num_sub == 0)
# 12 patients not using all substances (one has one NA, but I still count)

parent_wo_substance <- df_tobacco %>%
  select(parent_id, all_of(parent_habit)) %>% 
  group_by(parent_id) %>% 
  filter(!( is.na(nidaalc) & is.na(nidatob) &
              is.na(nidapres) & is.na(nidaill))) %>% 
  mutate(num_sub = sum(nidaalc>0, na.rm = TRUE) + 
           sum(nidatob>0, na.rm = TRUE) + 
           sum(nidapres>0, na.rm = TRUE) + 
           sum(nidaill>0, na.rm = TRUE)) %>% 
  filter(num_sub == 0) %>% 
  select(parent_id)

### cigarettes ----

# parent # days
table(df_tobacco$momcig, useNA = "always")

# record the patient id
outlier_momcig <- (df_raw %>%
  select(parent_id, momcig, mom_numcig) %>% 
  filter(momcig > 30))$parent_id

# change anything above 30 into 30
df_tobacco$momcig[df_tobacco$momcig > 30] <- 30


# parent # cigarettes
table(df_tobacco$mom_numcig, useNA = "always") # messy
unique(df_tobacco$mom_numcig)

# 2 black and miles a day - cigar
# we first deal with the values we are more sure
df_tobacco$mom_numcig[df_tobacco$mom_numcig == ""] <- NA
df_tobacco$mom_numcig[df_tobacco$mom_numcig == "None"] <- 0

# use the maximum observed values
df_tobacco$mom_numcig[df_tobacco$mom_numcig == "2 black and miles a day"] <- 25
df_tobacco$mom_numcig[df_tobacco$mom_numcig == "20-25"] <- 25
df_tobacco$mom_numcig[df_tobacco$mom_numcig == "44989"] <- 25

df_tobacco$mom_numcig <- as.numeric(df_tobacco$mom_numcig)

# check the correspondence between these answers
table(df_tobacco$nidatob == 0, df_tobacco$momcig > 0)

# look at the parents who said they didn't use tobacco but cigarettes
df_tobacco %>% 
  select(parent_id, nidatob, momcig) %>% 
  filter(nidatob == 0, momcig > 0)
# the num of days were 30
# and this parent claimed no substance use

# how often and # days in the past 30 days
df_tobacco %>% 
  select(parent_id, nidatob, momcig) %>% 
  ggplot()+
  geom_jitter(aes(x = nidatob, y = momcig),
              width = 0.2, height = 0.1)

# can use a table to show the confusion about 0 and 1
# but maybe into gtsummary?
# like show the number and the proportion in parenthesis
tapply(df_tobacco$momcig > 20, df_tobacco$nidatob, sum)
tapply(df_tobacco$momcig, df_tobacco$nidatob, length)

# how often and # cigarettes 
table(df_tobacco$nidatob == 0, df_tobacco$mom_numcig == 0)
# this one seems more consistent

# also if missing momcig => miss all substance related (from md.pattern)

#### previous study smoking ----

# one is at the previous study, one is retrospective (reflection)
# maybe use them together - if one of them is 1 then it is 1 (combined) 

## mom 16 weeks preg -  this and the following
table(df_tobacco$mom_smoke_16wk, useNA = "always")
unique(df_tobacco$mom_smoke_16wk)
df_tobacco$mom_smoke_16wk[df_tobacco$mom_smoke_16wk == ""] <- NA

# 22 weeks
table(df_tobacco$mom_smoke_22wk, useNA = "always")
df_tobacco$mom_smoke_22wk[df_tobacco$mom_smoke_22wk == ""] <- NA

# 32 weeks
table(df_tobacco$mom_smoke_32wk, useNA = "always")
df_tobacco$mom_smoke_32wk[df_tobacco$mom_smoke_32wk == ""] <- NA

# first postpartum
table(df_tobacco$mom_smoke_pp1, useNA = "always")
df_tobacco$mom_smoke_pp1[df_tobacco$mom_smoke_pp1 == ""] <- NA

# second postpartum
table(df_tobacco$mom_smoke_pp2, useNA = "always")
df_tobacco$mom_smoke_pp2[df_tobacco$mom_smoke_pp2 == ""] <- NA

# 12 week postpartum
table(df_tobacco$mom_smoke_pp12wk, useNA = "always")
df_tobacco$mom_smoke_pp12wk[df_tobacco$mom_smoke_pp12wk == ""] <- NA

# 6 mo postpartum
table(df_tobacco$mom_smoke_pp6mo, useNA = "always")
df_tobacco$mom_smoke_pp6mo[df_tobacco$mom_smoke_pp6mo == ""] <- NA

# urine 24 week - numeric
summary(df_tobacco$cotimean_34wk)

# urine postpartum 6 month - mom
summary(df_tobacco$cotimean_pp6mo)

# urine postpartum 6 month - mom
summary(df_tobacco$cotimean_pp6mo_baby)

# correlation between mom and baby

df_tobacco %>% 
  select(cotimean_pp6mo, cotimean_pp6mo_baby) %>% 
  cor(use = "complete.obs")

plot(df_tobacco$cotimean_pp6mo, df_tobacco$cotimean_pp6mo_baby)
# one outlier

# if we remove the outlier - the correlation disappears
df_tobacco %>% 
  filter(cotimean_pp6mo_baby < 40) %>% 
  select(cotimean_pp6mo, cotimean_pp6mo_baby) %>% 
  cor(use = "complete.obs")

summary(lm(cotimean_pp6mo_baby ~ cotimean_pp6mo,
           data = df_tobacco[df_tobacco$cotimean_pp6mo_baby < 40, ]))

### merge them into a new variable - first post til 6 mo
# as the post. exposure indicator

previous_pp <- c("mom_smoke_pp1", "mom_smoke_pp2", "mom_smoke_pp12wk",
                 "mom_smoke_pp6mo")

df_tobacco$mom_smoke_pp1 <- ifelse(df_tobacco$mom_smoke_pp1 == "1=Yes", 1, 0)
df_tobacco$mom_smoke_pp2 <- ifelse(df_tobacco$mom_smoke_pp2 == "1=Yes", 1, 0)
df_tobacco$mom_smoke_pp12wk <- ifelse(df_tobacco$mom_smoke_pp12wk == "1=Yes", 1, 0)
df_tobacco$mom_smoke_pp6mo <- ifelse(df_tobacco$mom_smoke_pp6mo == "1=Yes", 1, 0)

df_tobacco$previous_pp6mo <- 
  ifelse(rowSums(!is.na(df_tobacco[, previous_pp])) == 0, NA,
       rowSums(df_tobacco[, previous_pp], na.rm = TRUE))

df_tobacco$previous_pp6mo_num <- rowSums(!is.na(df_tobacco[, previous_pp]))
df_tobacco$previous_pp6mo_prop <- df_tobacco$previous_pp6mo/
  df_tobacco$previous_pp6mo_num

# also merge the ones when pregnant
previous_preg <- c("mom_smoke_16wk", "mom_smoke_22wk", "mom_smoke_32wk")

df_tobacco$mom_smoke_16wk <- ifelse(df_tobacco$mom_smoke_16wk == "1=Yes", 1, 0)
df_tobacco$mom_smoke_22wk <- ifelse(df_tobacco$mom_smoke_22wk == "1=Yes", 1, 0)
df_tobacco$mom_smoke_32wk <- ifelse(df_tobacco$mom_smoke_32wk == "1=Yes", 1, 0)
df_tobacco$previous_32wk <- 
  ifelse(rowSums(!is.na(df_tobacco[, previous_preg])) == 0, NA,
         rowSums(df_tobacco[, previous_preg], na.rm = TRUE))




# plot the cotimean again
df_tobacco %>% 
  select(cotimean_pp6mo_baby, cotimean_pp6mo, previous_pp6mo) %>% 
  mutate(previous_pp6mo_dummy = ifelse(previous_pp6mo != 0, "Ever", "Never")) %>% 
  ggplot()+
  geom_point(aes(x = cotimean_pp6mo, y = cotimean_pp6mo_baby,
                 color = previous_pp6mo_dummy),
                 alpha = 0.5)

  # scale_alpha_manual(values = c("TRUE" = 0.9, "FALSE" = 0.1))

# this one is more informative
# checked the outlier - unfortunately, it doesn't have more info (missing)

df_tobacco %>% 
  ggplot()+
  geom_point(aes(x = cotimean_34wk, y = cotimean_pp6mo,
                 color = (previous_pp6mo != 0),
                 alpha = (previous_pp6mo != 0)))

# df_tobacco %>% 
#   ggplot()+
#   geom_jitter(aes(x = previous_pp6mo_prop, y = cotimean_pp6mo_baby,
#                   alpha = previous_pp6mo_num), 
#              width = 0.05)

ggplot()+
  geom_jitter(data = df_tobacco,
              aes(x = as.factor(combined_pp6mo), y = cotimean_pp6mo),
              width = 0.2, height = 0.1)

# create the severity for prenatal - previous_32wk > 1

df_tobacco %>% 
  ggplot()+
  geom_jitter(aes(x = previous_32wk, y = cotimean_34wk))

df_tobacco$SDP <- ifelse(df_tobacco$previous_32wk > 1, 1, 0)


ggplot()+
  geom_point(data = df_tobacco,
              aes(x = cotimean_34wk, y = cotimean_pp6mo_baby,
             color = previous_32wk + previous_pp6mo))

ggplot()+
  geom_point(data = df_tobacco,
             aes(x = cotimean_pp6mo, y = cotimean_pp6mo_baby,
                 color = smoke_exposure_6mo))

ggplot()+
  geom_jitter(data = df_tobacco,
              aes(x = previous_32wk + previous_pp6mo, y = cotimean_pp6mo_baby),
              width = 0.2, height = 0.1)

#### current study smoking (reflection) ----

# 6mo
table(df_tobacco$smoke_exposure_6mo, useNA = "always")

# 12mo
table(df_tobacco$smoke_exposure_12mo, useNA = "always")

# 2yr
table(df_tobacco$smoke_exposure_2yr, useNA = "always")

# 3yr
table(df_tobacco$smoke_exposure_3yr, useNA = "always") # more missing

# 4yr
table(df_tobacco$smoke_exposure_4yr, useNA = "always") # more missing

# 5yr
table(df_tobacco$smoke_exposure_5yr, useNA = "always")

# check cor with previous and the current
table(df_tobacco$previous_pp6mo!=0, df_tobacco$smoke_exposure_6mo,
      useNA = "ifany")

df_tobacco$combined_pp6mo <- ifelse(df_tobacco$previous_pp6mo!=0 | 
                                      df_tobacco$smoke_exposure_6mo == 1,
                                    1,0)

# merge the ones after 
pp_all <- c("smoke_exposure_12mo", "smoke_exposure_2yr",
            "smoke_exposure_3yr", "smoke_exposure_4yr",
            "smoke_exposure_5yr")

# use both cotimean and combined exposure

df_tobacco$final_pp6mo <- ifelse(df_tobacco$combined_pp6mo == 1 |
                                   df_tobacco$cotimean_pp6mo_baby > 5, 1, 0)

# create the severity for postnatal

df_tobacco$pp_5yr <- ifelse(rowSums(!is.na(df_tobacco[, c("final_pp6mo",
                                                          pp_all)])) == 0, NA,
                            rowSums(df_tobacco[, c("final_pp6mo",
                                                   pp_all)], na.rm = TRUE))


# parent cognitive
summary(df_tobacco$erq_cog_a)

# parent expressive
summary(df_tobacco$erq_exp_a)
# these missing numbers are consistent

# parental knowledge
summary(df_tobacco$ppmq_parental_knowledge)
summary(df_tobacco$ppmq_child_disclosure)
summary(df_tobacco$ppmq_parental_solicitation)
summary(df_tobacco$ppmq_parental_control)

#### missing ----

# need to separate the smoking from previous study - the pattern is different

md.pattern(df_tobacco %>%
             select(# page, psex, plang,
                    num_prace,
                    employ, pedu, income_new,
                    nidaalc, nidatob, nidapres, nidaill,
                    momcig, mom_numcig,
                    mom_smoke_16wk, mom_smoke_22wk,
                    smoke_exposure_6mo) %>% 
             filter(num_prace == 0)) # co-occurrence

md.pattern(df_tobacco %>%
             select(# page, psex, plang, 
                    num_prace,
                    employ, pedu,
                    nidaalc, nidatob, nidapres, nidaill,
                    momcig, mom_numcig,
                    mom_smoke_16wk) %>% 
             filter(num_prace != 0)) 

##### missing within the previous study ----

###### survey from 16 preg to urine at 6 months post. ----

# known these miss all current survey parent data 
md.pattern(df_tobacco %>%
             select(# page, psex, plang,
               num_prace, page,
               mom_smoke_16wk:cotimean_pp6mo_baby) %>% 
             filter(num_prace == 0)) # some but not a strong pattern

# known they have some current survey information 
md.pattern(df_tobacco %>%
             select(# page, psex, plang,
               num_prace, page,
               mom_smoke_16wk:cotimean_pp6mo_baby,
               nidaalc) %>% 
             filter(num_prace != 0)) 
# one parent doesn't have all information in the previous study
# also the one who didn't response substance.use info in the current study

###### missing within urine samples ----
md.pattern(df_tobacco %>%
             select(# page, psex, plang,
               #num_prace, page,
               cotimean_pp6mo, cotimean_34wk, cotimean_pp6mo_baby))
# mom and baby miss together at 6 months - makes sense


##### missing within the current study smoking (reflection) ----
md.pattern(df_tobacco %>%
             select(# page, psex, plang,
               num_prace, page,
               smoke_exposure_6mo:smoke_exposure_5yr,
               nidaalc) %>% 
             filter(num_prace == 0)) # occurrence - these parents miss all current study
df_tobacco %>% filter(num_prace ==0) %>% View()

md.pattern(df_tobacco %>%
             select(# page, psex, plang,
               num_prace, page,
               smoke_exposure_6mo:smoke_exposure_5yr,
               nidaalc) %>% 
             filter(num_prace != 0))

df_tobacco %>% filter(num_prace != 0, is.na(smoke_exposure_6mo)) %>% View()
# one parent misses almost all - as the child
# the other misses substance use info in the current study


# if it misses with child together
md.pattern(df_tobacco %>%
             select(tage, tsex, language,
                    page, psex, plang, num_prace, num_trace) %>% 
             filter(num_trace == 0)) # 7 occurrence, 5 only child missing

# maybe they haven't completed the interview

md.pattern(df_tobacco %>%
             select(tage, tsex, language,
                    page, psex, plang, num_prace, num_trace) %>% 
             filter(num_trace != 0)) # 1 only parent missing



# ------------------------------------------------------------------- #

# Outcome ----

## Child ----
## child attention

child_brief <- c("bpm_att", "bpm_ext", "bpm_int")
child_brief_approx <- c("bpm_att_approx", "bpm_ext_approx", "bpm_int_approx")

summary(df_tobacco$bpm_att) # mean 3, miss 12

## child externalizing
summary(df_tobacco$bpm_ext) # mean 2.81, miss 12

## child internalizing
summary(df_tobacco$bpm_int) # mean 2.71, miss 14

## child cognitive
summary(df_tobacco$erq_cog) # mean 3.19, miss 13

## child expressive
summary(df_tobacco$erq_exp) # mean 2.75, miss 13

# recalculate outcome
child_df <- read.csv("Project 1 Pre-Processing//K01BB.csv")

child_df <- child_df %>%
  select(c(participant_id:su_interview_complete))  %>%
  filter(redcap_event_name == "child_baseline_arm_1")

child_df <- child_df %>%
  mutate(bpm_att_approx = rowMeans(dplyr::select(., c(bpm1,bpm3,bpm4,bpm5,bpm10)),
                           na.rm = TRUE)*5,
         bpm_ext_approx = rowMeans(dplyr::select(., c(bpm2,bpm6,bpm7,bpm8,bpm15,
                                              bpm16,bpm17)),
                           na.rm = TRUE)*7,
         bpm_int_approx = rowMeans(dplyr::select(., c(bpm9,bpm11,bpm12,bpm13,bpm18,
                                              bpm19)),
                           na.rm = TRUE)*6,
         erq_cog_approx = rowMeans(dplyr::select(., c(erq1,erq3,erq5,erq7,
                                               erq8,erq10)),
                            na.rm = TRUE),
         erq_exp_approx = rowMeans(dplyr::select(., c(erq2,erq4,erq6,
                                               erq9)),
                            na.rm = TRUE))

df_tobacco$bpm_att_approx <- 
  child_df$bpm_att_approx[match(df_tobacco$parent_id, child_df$parent_id)]

df_tobacco$bpm_ext_approx <- 
  child_df$bpm_ext_approx[match(df_tobacco$parent_id, child_df$parent_id)]

df_tobacco$bpm_int_approx <- 
  child_df$bpm_int_approx[match(df_tobacco$parent_id, child_df$parent_id)]

df_tobacco$erq_cog_approx <- 
  child_df$erq_cog_approx[match(df_tobacco$parent_id, child_df$parent_id)]

df_tobacco$erq_exp_approx <- 
  child_df$erq_exp_approx[match(df_tobacco$parent_id, child_df$parent_id)]

# corr
df_tobacco %>% 
  select(all_of(child_brief)) %>% 
  cor(use = "pairwise.complete.obs")

df_tobacco %>% 
  select(all_of(child_brief_approx)) %>% 
  cor(use = "pairwise.complete.obs")
# the correlation increases within brief

df_tobacco %>% 
  select(all_of(child_brief), all_of(child_brief_approx)) %>% 
  filter(!is.na(bpm_int_approx)) %>% 
  ggplot() +
  geom_jitter(aes(x = bpm_att_approx, y = bpm_int_approx,
             color = is.na(bpm_int)), width = 0.1,
             height = 0.1, alpha = 0.5) +
  geom_smooth(aes(x = bpm_att_approx, y = bpm_int_approx),
              method = "lm")

ggplot() +
  geom_jitter(data = df_tobacco,
              aes(x = bpm_att_approx, y = bpm_int_approx,
                  color = is.na(bpm_int)), width = 0.1,
              height = 0.1, alpha = 0.5) +
  geom_smooth(data = df_tobacco,
              aes(x = bpm_att_approx, y = bpm_int_approx),
              method = "lm", alpha = 0.1,
              color = "red") +
  geom_smooth(data = df_tobacco,
              aes(x = bpm_att, y = bpm_int),
              method = "lm", alpha = 0.1,
              color = "blue")

# have this plot

# missing pattern
md.pattern(df_tobacco %>%
             select(tage, num_trace,
                    bpm_att, bpm_ext, bpm_int, erq_cog, erq_exp) %>% 
             filter(num_trace == 0)) # 

md.pattern(df_tobacco %>%
             select(tage, num_trace,
                    bpm_att, bpm_ext, bpm_int, erq_cog, erq_exp) %>% 
             filter(num_trace != 0))

## Parent ----
## child autism

table(df_tobacco$childasd, useNA = "always") # miss 28, 1 suspect, 1 diag.

# attention, externalizing, internalizing
# potential problem - use rowSums without rm NA - causes more NA
summary(df_tobacco$bpm_att_p, useNA = "always")
summary(df_tobacco$bpm_ext_p, useNA = "always")
summary(df_tobacco$bpm_int_p, useNA = "always")



parent_df <- read.csv("Project 1 Pre-Processing/K01BB.csv") %>%
  filter(redcap_event_name == "parent_baseline_arm_2") %>%
  select(c(parent_id, page:chart23)) 

# recreate the outcome variables - saved as new
# use the mean of the non-missing values * the number of questions - approximate
parent_df <- parent_df %>% 
  mutate(
    erq_cog_a_approx = rowMeans(dplyr::select(., c(perq1,perq3,perq5,perq7,
                                            perq8,perq10)), na.rm=TRUE),
    erq_exp_a_approx = rowMeans(dplyr::select(., c(perq2,perq4,perq6,
                                            perq9)),  na.rm=TRUE),
    bpm_att_p_approx = rowMeans(dplyr::select(., c(pbpm1,pbpm3,pbpm4,pbpm5,pbpm10)),
                             na.rm = TRUE)*5,
    bpm_ext_p_approx = rowMeans(dplyr::select(., c(pbpm2,pbpm6,pbpm7,pbpm8,pbpm15,
                                       pbpm16,pbpm17)),
                               na.rm = TRUE)*7,
    bpm_int_p_approx = rowMeans(dplyr::select(., c(pbpm9,pbpm11,pbpm12,pbpm13,pbpm18,
                                       pbpm19)),
                               na.rm = TRUE)*6,
    bpm_att_a_approx = rowMeans(dplyr::select(., c(abpm1,abpm6,abpm7,abpm8,abpm9,
                                           abpm12)),
                               na.rm = TRUE)*6,
    bpm_ext_a_approx = rowMeans(dplyr::select(., c(abpm3,abpm13,abpm14,abpm17,
                                           abpm18)),
                               na.rm = TRUE)*5,
    bpm_int_a_approx = rowMeans(dplyr::select(., c(abpm2,abpm4,abpm5,abpm10,abpm15,
                                           abpm16)),
                               na.rm = TRUE)*6) %>% 
  mutate(new_ppmq9 = 5-ppmq9,
         new_ppmqcd3 = 5-ppmqcd3,
         new_ppmqcd4 = 5-ppmqcd4) %>% 
  mutate(
    ppmq_parental_knowledge = rowMeans(dplyr::select(., c(ppmq1,ppmq2,ppmq3,ppmq4,
                                                          ppmq5,ppmq6,ppmq7,ppmq8,
                                                          new_ppmq9)),
                                       na.rm = TRUE),
    ppmq_child_disclosure = rowMeans(dplyr::select(., c(ppmqcd1,ppmqcd2,
                                                        new_ppmqcd3,new_ppmqcd4,
                                                        ppmqcd5)),
                                     na.rm = TRUE),
    ppmq_parental_solicitation = rowMeans(dplyr::select(., c(ppmqps1:ppmqps5)),
                                          na.rm = TRUE),
    ppmq_parental_control = rowMeans(dplyr::select(., ppmqpc1:ppmqpc5),
                                     na.rm=TRUE))

df_tobacco$erq_cog_a_approx <- 
  parent_df$erq_cog_a_approx[match(df_tobacco$parent_id,  parent_df$parent_id)]
df_tobacco$erq_exp_a_approx <- 
  parent_df$erq_exp_a_approx[match(df_tobacco$parent_id,  parent_df$parent_id)]

df_tobacco$bpm_att_p_approx <- 
  parent_df$bpm_att_p_approx[match(df_tobacco$parent_id,  parent_df$parent_id)]
df_tobacco$bpm_ext_p_approx <- 
  parent_df$bpm_ext_p_approx[match(df_tobacco$parent_id,  parent_df$parent_id)]
df_tobacco$bpm_int_p_approx <- 
  parent_df$bpm_int_p_approx[match(df_tobacco$parent_id,  parent_df$parent_id)]

df_tobacco$bpm_att_a_approx <- 
  parent_df$bpm_att_a_approx[match(df_tobacco$parent_id,  parent_df$parent_id)]
df_tobacco$bpm_ext_a_approx <- 
  parent_df$bpm_ext_a_approx[match(df_tobacco$parent_id,  parent_df$parent_id)]
df_tobacco$bpm_int_a_approx <- 
  parent_df$bpm_int_a_approx[match(df_tobacco$parent_id,  parent_df$parent_id)]

df_tobacco$ppmq_parental_knowledge_approx <- 
  parent_df$ppmq_parental_knowledge[match(df_tobacco$parent_id,  parent_df$parent_id)]
df_tobacco$ppmq_child_disclosure_approx <- 
  parent_df$ppmq_child_disclosure[match(df_tobacco$parent_id,  parent_df$parent_id)]
df_tobacco$ppmq_parental_solicitation_approx <- 
  parent_df$ppmq_parental_solicitation[match(df_tobacco$parent_id,  parent_df$parent_id)]
df_tobacco$ppmq_parental_control_approx <- 
  parent_df$ppmq_parental_control[match(df_tobacco$parent_id,  parent_df$parent_id)]


# swan hyperactive and inattentive - need to go back to the original study
# change some 0 values to NA if they were all NA


parent_df$swan_inattentive <-
  ifelse(rowSums(!is.na(parent_df %>% select(swan1:swan9))) == 0, NA,
         rowSums(parent_df %>% select(swan1:swan9), na.rm = TRUE))

parent_df$swan_hyperactive <-
  ifelse(rowSums(!is.na(parent_df %>% select(swan10:swan18))) == 0, NA,
         rowSums(parent_df %>% select(swan10:swan18), na.rm = TRUE))

df_tobacco$swan_inattentive <- parent_df$swan_inattentive[match(df_tobacco$parent_id,  parent_df$parent_id)]
df_tobacco$swan_hyperactive <- parent_df$swan_hyperactive[match(df_tobacco$parent_id,  parent_df$parent_id)]

# check associations

# substance use

# cig ever and cotimean

df_tobacco %>%
  select(cig_ever, e_cig_ever, mj_ever, alc_ever,
         #SDP, pp_5yr, combined_pp6mo, 
         cotimean_34wk, cotimean_pp6mo_baby) %>% 
  ggpairs(lower = list(continuous = "smooth"))

# within parent answers - not high
df_tobacco %>%
  select(bpm_att_p_approx, bpm_att_a_approx,
         bpm_ext_p_approx, bpm_ext_a_approx,
         bpm_int_p_approx, bpm_int_a_approx) %>% 
  cor(use = "pairwise.complete.obs")

df_tobacco %>%
  select(bpm_att_p_approx, bpm_att_approx,
         bpm_ext_p_approx, bpm_ext_approx,
         bpm_int_p_approx, bpm_int_approx) %>% 
  cor(use = "pairwise.complete.obs")

plot(df_tobacco$bpm_att_p_approx, df_tobacco$bpm_att_approx)





# swan with the adhd status
plot(df_tobacco$childasd, df_tobacco$swan_hyperactive)
plot(df_tobacco$childasd, df_tobacco$swan_inattentive)

# smoking with swan
plot(df_tobacco$SDP, df_tobacco$swan_hyperactive)
plot(df_tobacco$SDP, df_tobacco$swan_inattentive)

# cotimean with swan
plot(df_tobacco$cotimean_34wk, df_tobacco$swan_hyperactive)
plot(df_tobacco$cotimean_34wk, df_tobacco$swan_inattentive)

plot(df_tobacco$cotimean_pp6mo_baby, df_tobacco$swan_hyperactive)
plot(df_tobacco$cotimean_pp6mo_baby, df_tobacco$swan_inattentive)


df_tobacco %>% 
  select(bpm_att_p_approx, bpm_ext_p_approx, bpm_int_p_approx,
         SDP, pp_5yr, combined_pp6mo, cotimean_34wk, cotimean_pp6mo_baby) %>% 
  ggpairs(lower = list(continuous = "smooth"))

# combined_pp6mo with att

df_tobacco %>% 
  select(bpm_att_approx, bpm_ext_approx, bpm_int_approx,
         SDP, pp_5yr, combined_pp6mo, cotimean_pp6mo_baby) %>% 
  ggpairs(lower = list(continuous = "smooth"))

df_tobacco %>% 
  select(bpm_att_approx, bpm_ext_approx, bpm_int_approx,
         ) %>% 
  ggpairs(lower = list(continuous = "smooth"))

df_tobacco %>% 
  ggplot() +
  geom_jitter(aes(x = SDP, y = bpm_att_approx), width = 0.1) # not differ much

df_tobacco %>% 
  select(swan_hyperactive, swan_inattentive,
         SDP, pp_5yr, combined_pp6mo, cotimean_pp6mo_baby) %>% 
  ggpairs(lower = list(continuous = "smooth"))

## pp 5yrs with swan inattentive

df_tobacco %>% 
  ggplot() +
  geom_jitter(aes(x = combined_pp6mo, y = swan_inattentive,
                  col = swan_inattentive >= 6 ), width = 0.1, height = 0) +
  geom_hline(yintercept = 6, linetype = "dashed") +
  scale_x_continuous(breaks = c(0,1))

# Plots ----
df_tobacco %>% 
  mutate(previous = mom_smoke_16wk,
         parent = psex,
         children = tsex) %>% 
  select(previous, parent, children) %>% 
  gg_miss_upset()

df_tobacco %>% 
  filter(num_prace > 0) %>% 
  select(nidaalc, nidatob, nidapres, nidaill) %>% 
  gg_miss_upset()

# how often and # days in the past 30 days
df_tobacco %>% 
  select(parent_id, nidatob, momcig) %>% 
  filter(!is.na(nidatob)) %>% 
  ggplot()+
  geom_jitter(aes(x = factor(nidatob, levels = c(0, 1, 2, 3, 4),
                             labels = c("Never", "Once or Twice",
                                        "Month",
                                        "Weekly", "Daily or almost daily")),
                  y = momcig),
              width = 0.15, height = 0.2, alpha = 0.6) +
  theme_minimal() +
  labs(x = "Tobacco Usage", y = "Number of Days Smoking")

df_tobacco %>% 
  ggplot()+
  geom_jitter(aes(x = previous_32wk, y = cotimean_34wk),
              width = 0.15, height = 0.2) +
  geom_hline(yintercept = 40, linetype = "dashed", color = "red") +
  theme_minimal()+
  labs(x = "Number of Smoking Exposure", y = "Urine Cotinine")

df_tobacco %>% 
  select(cotimean_pp6mo_baby, cotimean_pp6mo, combined_pp6mo) %>% 
  mutate(previous_pp6mo_dummy = ifelse(combined_pp6mo != 0, "Ever", "Never")) %>% 
  ggplot()+
  theme_minimal()+
  geom_point(aes(x = cotimean_pp6mo, y = cotimean_pp6mo_baby,
                 color = as.factor(combined_pp6mo)),
             alpha = 0.5) +
  scale_color_manual(values = c("#F8766D", "#00BFC4", "grey"),
                     labels = c("Not exposed", "Exposed", "NA"))+
  labs(y = "Urine Cotinine of Babies", x = "Urine Cotinine of Mothers",
       color = "Exposure") +
  theme(legend.title = element_text(size = 10),
        legend.text  = element_text(size = 8))


# correlation

df_tobacco %>% 
  ggplot()+
  theme_minimal()+
  geom_jitter(aes(x = bpm_att_approx, y = bpm_ext_approx),
             alpha = 0.5, width = 0.1, height = 0.1) +
  geom_smooth(aes(x = bpm_att_approx, y = bpm_ext_approx),
              alpha = 0.1, method = "lm") +
  labs(x = "Attention Problem", y = "Externalizing Problem") 

df_tobacco %>% 
  ggplot()+
  theme_minimal()+
  geom_jitter(aes(x = swan_hyperactive, y = swan_inattentive,
                  color = as.factor(childasd)),
              alpha = 0.5, width = 0.1, height = 0.1,) +
  geom_smooth(aes(x = swan_hyperactive, y = swan_inattentive),
              alpha = 0.1, method = "lm") +
  geom_vline(xintercept = 6, linetype = "dashed",
             col = "black")+
  geom_hline(yintercept = 6, linetype = "dashed",
             col = "black")+
  scale_color_manual(values = c("#F8766D", "#00BFC4", "#619CFF",
                                "grey"),
                     labels = c("No", "Diagnosed", "Suspected",
                                "NA"))+
  labs(x = "ADHD Hyperactive", y = "ADHD Inattentive",
       color = "ASD") +
  theme(legend.title = element_text(size = 10),
        legend.text  = element_text(size = 8))

df_tobacco %>% 
  ggplot() +
  geom_jitter(aes(x = combined_pp6mo, y = bpm_att_approx), width = 0.1, height = 0) +
  scale_x_continuous(breaks = c(0,1)) +
  theme_minimal()+
  labs(x = "Exposed", y = "Attention Problem")

df_tobacco %>% 
  ggplot() +
  geom_jitter(aes(x = combined_pp6mo, y = swan_inattentive,
                  col = swan_inattentive >= 6 ), width = 0.1, height = 0) +
  geom_hline(yintercept = 6, linetype = "dashed") +
  scale_x_continuous(breaks = c(0,1))+
  theme_minimal()+
  labs(x = "Exposed", y = "SWAN inattentive",
       color = "ADHD likely")

df_tobacco$ETS <- factor(ifelse(df_tobacco$pp_5yr>0, 1, 0))

df_tobacco %>% 
  ggplot() +
  geom_jitter(aes(x = alc_ever, y = cotimean_34wk, color = ETS), width = 0.1, height = 0.1,
              alpha = 0.5) +
  scale_x_continuous(breaks = c(0,1))+
  theme_minimal()+
  labs(x = "Alcohol Ever", y = "Urine Cotinine at 34 Weeks")

# ----
# Look at SBP versus ETS
set.seed(1)
df_tobacco %>% 
  ggplot() +
  geom_jitter(aes(x = previous_32wk, y = pp_5yr), width = 0.1, height = 0.1,
              alpha = 0.6) +
  stat_smooth(aes(x = previous_32wk, y = pp_5yr),
              method = "lm", 
              formula = y ~ x, 
              geom = "smooth") +
  theme_minimal() +
  labs(x = "SDP Intensity", y = "ETS Intensity")
  
df_tobacco <- df_tobacco %>% 
  mutate(composite = case_when(SDP == 1 & ETS == 1 ~ "Both",
                               SDP == 1 ~ "SDP",
                               ETS == 1 ~ "ETS",
                               ETS == 0 ~ "Neither",
                               TRUE ~ NA))
tb_composite <- CreateTableOne(vars = c("bpm_att_p_approx", "bpm_ext_p_approx", "bpm_int_p_approx"),
               strata = c("composite"),
               data = df_tobacco) 
tb_composite <- print(tb_composite)
rownames(tb_composite) <- c("n", "BPM Attention", "BPM Externalizing", 
                            "BPM Internalizing")
saveRDS(tb_composite, "tb_composite.RDS")

