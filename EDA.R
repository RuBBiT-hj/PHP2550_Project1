library(knitr)
library(tidyverse)
library(mice)

# Load data
df_tobacco <- read.csv("project1.csv")

# look at each variable

# Covariate ----

## Child ----

# child age
summary(df_tobacco$tage)

# child sex - factorize
summary(df_tobacco$tsex)

# child language - factorize
summary(df_tobacco$language)

# child ethic - factorize
table(df_tobacco$tethnic, useNA = "always") # miss 12

# child race - factorize

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

# child cigarette - factorize
summary(df_tobacco$cig_ever)
sum(df_tobacco$cig_ever == 1, na.rm = TRUE) # only 1 person

# child # of cigarette - factorize
summary(df_tobacco$num_cigs_30)
sum(df_tobacco$num_cigs_30 == 0, na.rm = TRUE) # that person didn't smoke in the past 30 days

# child e-cigarette - factorize
summary(df_tobacco$e_cig_ever)
sum(df_tobacco$e_cig_ever == 1, na.rm = TRUE) # 3

# child # of e-cigarette - factorize
summary(df_tobacco$num_e_cigs_30)
table(df_tobacco$num_e_cigs_30, useNA = "always") # one person - 0, one person - 2, one person NA

# child marijuana - factorize
summary(df_tobacco$mj_ever)
sum(df_tobacco$mj_ever == 1, na.rm = TRUE) # 3

# child # of marijuana
summary(df_tobacco$num_mj_30)
table(df_tobacco$num_mj_30, useNA = "always") # one person - 3, one person - 12, one person 18

# child alcohol - factorize
summary(df_tobacco$alc_ever)
table(df_tobacco$alc_ever, useNA = "always") # 5

# child # of alcohol
summary(df_tobacco$num_alc_30)
table(df_tobacco$num_alc_30, useNA = "always") # 2 people - 0, one person - 1, one person 10

# child parental knowledge
summary(df_tobacco$pmq_parental_knowledge) # mean 3.99, miss 14
summary(df_tobacco$pmq_child_disclosure) # mean 3.43, miss 13
summary(df_tobacco$pmq_parental_solicitation) # mean 2.97, miss 14
summary(df_tobacco$pmq_child_disclosure) # mean 3.43, miss 13

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

# parent sex - factorize
table(df_tobacco$psex, useNA = "always") # miss 8

# parent language - factorize
table(df_tobacco$plang, useNA = "always") # miss 8

# parent ethic - factorize
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

# parent employ - factorize
table(df_tobacco$employ, useNA = "always") 

# parent education - factorize
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

# parent alcohol - factorize
table(df_tobacco$nidaalc, useNA = "always") 

md.pattern(df_tobacco %>%
             select(page, psex, plang, num_prace,
                    employ, pedu, income_new) %>% 
             filter(num_prace == 0)) # co-occurrence

md.pattern(df_tobacco %>%
             select(page, psex, plang, num_prace,
                    employ, pedu, income_new) %>% 
             filter(num_prace != 0)) 

# if it misses with child together
md.pattern(df_tobacco %>%
             select(tage, page, psex, plang, num_prace, num_trace) %>% 
             filter(num_trace == 0)) # 7 occurrence, 5 only child missing

# maybe they haven't completed the interview

md.pattern(df_tobacco %>%
             select(tage, page, psex, plang, num_prace, num_trace) %>% 
             filter(num_trace != 0)) # 1 only parent missing

# Outcome ----

## Child ----
## child attention

child_brief <- c("bpm_att", "bpm_ext", "bpm_int", 
                 "erq_cog", "erq_exp")

summary(df_tobacco$bpm_att) # mean 3, miss 12

## child externalizing
summary(df_tobacco$bpm_ext) # mean 2.81, miss 12

## child internalizing
summary(df_tobacco$bpm_int) # mean 2.71, miss 14

## child cognitive
summary(df_tobacco$erq_cog) # mean 3.19, miss 13

## child expressive
summary(df_tobacco$erq_exp) # mean 2.75, miss 13

# corr
df_tobacco %>% 
  select(all_of(child_brief)) %>% 
  cor(use = "pairwise.complete.obs")

df_tobacco %>% 
  filter(num_race != 0) %>% 
  select(all_of(child_brief)) %>% 
  cor(use = "pairwise.complete.obs")

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
## child autism - factorize

table(df_tobacco$childasd, useNA = "always") # miss 28


# 56 is at the previous study, 60 is retrospective (reflection)
# maybe use them together - if one of them is 1 then it is 1 (combined) 
