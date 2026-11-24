# Code and calculations
#
# Prevalence and Sources of Abuse Among Male Elite Football Players: A Cross Sectional Study 
#
########################

library(dplyr)
library(broom)      
library(haven)
library(kableExtra)
library(janitor)


## Define high-profile club (variable "storklubb")

sef <- sef %>% 
  mutate(storklubb = if_else(
    as.numeric(club) %in% c(34, 32, 1, 3, 7, 5, 10),
    1,
    0
  )
  )

# rename variables
sef <- sef %>% rename(mobbing_ever_socialmedia = exposed_to_1a, mobbing_ever_sup_irl = exposed_to_1b, mobbing_ever_sother_player = exposed_to_1c, mobbing_ever_coach = exposed_to_1d, mobbing_ever_no = exposed_to_1e, mobbing_ever_do_not_want_answer = exposed_to_1f,
                      diskriminering_ever_socialmedia = exposed_to_2a, diskriminering_ever_sup_irl = exposed_to_2b, diskriminering_ever_sother_player = exposed_to_2c, diskriminering_ever_coach = exposed_to_2d, diskriminering_ever_no = exposed_to_2e, diskriminering_ever_do_not_want_answer = exposed_to_2f,
                      hot_ever_socialmedia = exposed_to_3a, hot_ever_sup_irl = exposed_to_3b, hot_ever_sother_player = exposed_to_3c, hot_ever_coach = exposed_to_3d, hot_ever_no = exposed_to_3e, hot_ever_do_not_want_answer = exposed_to_3f,
                      vald_ever_socialmedia = exposed_to_4a, vald_ever_sup_irl = exposed_to_4b, vald_ever_sother_player = exposed_to_4c, vald_ever_coach = exposed_to_4d, vald_ever_no = exposed_to_4e, vald_ever_do_not_want_answer = exposed_to_4f)

sef <- sef %>% rename(mobbing_last_year_socialmedia = exposed_to_2_1a, mobbing_last_year_sup_irl = exposed_to_2_1b, mobbing_last_year_sother_player = exposed_to_2_1c, mobbing_last_year_coach = exposed_to_2_1d, mobbing_last_year_no = exposed_to_2_1e, mobbing_last_year_do_not_want_answer = exposed_to_2_1f,
                      diskriminering_last_year_socialmedia = exposed_to_2_2a, diskriminering_last_year_sup_irl = exposed_to_2_2b, diskriminering_last_year_sother_player = exposed_to_2_2c, diskriminering_last_year_coach = exposed_to_2_2d, diskriminering_last_year_no = exposed_to_2_2e, diskriminering_last_year_do_not_want_answer = exposed_to_2_2f,
                      hot_last_year_socialmedia = exposed_to_2_3a, hot_last_year_sup_irl = exposed_to_2_3b, hot_last_year_sother_player = exposed_to_2_3c, hot_last_year_coach = exposed_to_2_3d, hot_last_year_no = exposed_to_2_3e, hot_last_year_do_not_want_answer = exposed_to_2_3f,
                      vald_last_year_socialmedia = exposed_to_2_4a, vald_last_year_sup_irl = exposed_to_2_4b, vald_last_year_sother_player = exposed_to_2_4c, vald_last_year_coach = exposed_to_2_4d, vald_last_year_no = exposed_to_2_4e, vald_last_year_do_not_want_answer = exposed_to_2_4f)

#Proportions ever exposed
sef %>% summarise(across(mobbing_ever_socialmedia:mobbing_ever_do_not_want_answer, ~ mean(.x==1, na.rm = T)*100)) %>%  kable(caption = " ") %>% kable_styling()
sef %>% summarise(across(diskriminering_ever_socialmedia:diskriminering_ever_do_not_want_answer, ~ mean(.x==1, na.rm = T)*100)) %>%  kable(caption = " ") %>% kable_styling()
sef %>% summarise(across(hot_ever_socialmedia:hot_ever_do_not_want_answer, ~ mean(.x==1, na.rm = T)*100)) %>%  kable(caption = " ") %>% kable_styling()
sef %>% summarise(across(vald_ever_socialmedia:vald_ever_do_not_want_answer, ~ mean(.x==1, na.rm = T)*100)) %>%  kable(caption = " ") %>% kable_styling()

# Proportions exposed last year
sef %>% summarise(across(mobbing_last_year_socialmedia:mobbing_last_year_do_not_want_answer, ~ mean(.x==1, na.rm = T)*100)) %>%  kable(caption = " ") %>% kable_styling()
sef %>% summarise(across(diskriminering_last_year_socialmedia:diskriminering_last_year_do_not_want_answer, ~ mean(.x==1, na.rm = T)*100)) %>%  kable(caption = " ") %>% kable_styling()
sef %>% summarise(across(hot_last_year_socialmedia:hot_last_year_do_not_want_answer, ~ mean(.x==1, na.rm = T)*100)) %>%  kable(caption = " ") %>% kable_styling()
sef %>% summarise(across(vald_last_year_socialmedia:vald_last_year_do_not_want_answer, ~ mean(.x==1, na.rm = T)*100)) %>%  kable(caption = " ") %>% kable_styling()

#create variables any bullying last year, any discrimination last  year and threats last year

sef <- sef %>% rowwise() %>% mutate(mobbing_last_year = sum(across(mobbing_last_year_socialmedia:mobbing_last_year_coach))) %>% ungroup()
sef <- sef %>% rowwise() %>% mutate(diskriminering_last_year = sum(across(diskriminering_last_year_socialmedia:diskriminering_last_year_coach))) %>% ungroup()
sef <- sef %>% rowwise() %>% mutate(hot_last_year = sum(across(hot_last_year_socialmedia:hot_last_year_coach))) %>% ungroup()

## create variable any type of abuse last year
sef <- sef %>% 
  rowwise() %>% 
  mutate(
    person_directed_abuse = {
      vals <- c_across(mobbing_last_year:hot_last_year)
      
      if (all(is.na(vals))) {
        NA_real_        # If all is NA → return NA
      } else if (sum(vals, na.rm = TRUE) >= 1) {
        1               # If sum ≥ 1 --> 1 
      } else {
        0               # Else (sum = 0) --> 0
      }
    }
  ) %>% ungroup()

## make numeric variable into categorical/factor
sef <- sef %>%
  mutate(
    level = factor(as.numeric(level),
                   levels = c(1, 2),
                   labels = c("Allsvenskan", "Superettan"))
  )



## person directed abuse by level
tab_level <- table(sef$person_directed_abuse, sef$level)
tab_level

prop.table(tab_level, margin = 2) * 100

sef %>% filter(!is.na(person_directed_abuse)) %>% 
  tabyl(level, person_directed_abuse) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting()

## test and check
chisq.test(tab_level)

test_level <- chisq.test(tab_level)

test_level$expected

## relevel variable high profile club

sef <- sef %>%
  mutate(
    storklubb = factor(
      storklubb,
      levels = c(0, 1),
      labels = c("Övriga", "Storklubb")
    )
  )

tab_bigclub <- table(sef$person_directed_abuse, sef$storklubb)

tab_bigclub

prop.table(tab_bigclub, margin = 2) * 100

## test and check
test_bigclub <- chisq.test(tab_bigclub)

test_bigclub$expected

#Manual calculations
#Competition level (Allsvenskan vs. Superettan) 
# χ²  = 6.0434
# N = 29 + 42 + 56 + 35 = 162
V  <- sqrt(6.0435/162) 
#V = 0.193
# Cramér's V = 0.19

#Club type (Other clubs vs. Major clubs)

# χ² = 9.5138
# N = 62 + 9 + 59 + 32 = 162
V <- sqrt(9.5138/162) 
# V =0.242
# Cramér's V = 0.24

