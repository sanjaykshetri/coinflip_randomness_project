library(readxl)
library(tidyverse)
library(janitor)

#this dataset is derived from the raw dataset by removing some columns 
#and by renaming some colums 
df1 <- read_excel("coinflip_crt_survey_data_edited.xlsx") %>% 
  clean_names()

head(df1)

#let's replace "Head" with "H" and Tail with "H"
df2 <- df1 %>% 
  mutate(across(starts_with("flip_"), 
                ~case_when(
                  .x == "HEAD"~ "H", 
                  .x == "TAIL" ~ "T", 
                  TRUE ~ NA_character_
                )))
head(df2)

#now, combine these 12 flip columns into one column with "HTHT..." form

df3 <- df2 %>% 
  mutate( coinflips = pmap_chr(select(., starts_with("flip_")), str_c)) %>% 
  select(-starts_with("flip_"), -timestamp)
head(df3)

# now turning to the crt items
#if correct answers, 1, else 0. 

correct_answers <- c(
  crt_q1 = "5 cents",
  crt_q2 = "5 minutes",
  crt_q3 = "47 days",
  crt_q4 = "4 days",
  crt_q5 = "29 students",
  crt_q6 = "20",
  crt_q7 = "simon has lost money."
)

df4 <- df3 %>% mutate(across(
  .cols = names(correct_answers), 
  .fns = ~as.integer(str_trim(.x) == correct_answers[cur_column()]),
  .names = "{.col}"))
))

head(df4)



#now combine these individual crt scores into one score 
df5 <-df4 %>% 
  mutate( total_crt_score = rowSums(select(., starts_with("crt_")), na.rm = TRUE)) %>% 
  select(-starts_with("crt_"))
head(df5)



#now the datafrma df5 has four columns: grade, sleep hours, coinflips, total_crt_score

#Now, for each participant's coinflips, we come up with a 'randomness score' based on
#the existing knowledge of how these sequences are distributed 

#feature 1: alternation rate 
calc_alternation_rate <- function(seq) {
  flips <- unlist(strsplit(seq, ""))
  sum(head(flips, -1) != tail(flips, -1)) / (length(flips) - 1)
}


#feature 2: max run length 
calc_max_run <- function(seq) {
  flips <- unlist(strsplit(seq, ""))
  max(rle(flips)$lengths)
}


# feature 3: deviation from 6 heads 
calc_head_deviation <- function(seq) {
  abs(str_count(seq, "H") - 6)
}


#apply to the dataset 
df6 <- df5 %>% 
  mutate(
    alternation_rate = map_dbl(coinflips, calc_alternation_rate),
    max_run = map_int(coinflips, calc_max_run),
    head_deviation = map_dbl(coinflips, calc_head_deviation)
  )
head(df6)

#now, turning these three features into one. 
#alternation_rate: ideal about 0.5 
#max_run: ideal about 3 to 5 for 12 flips 
#head_deviation: ideal 0 for 6 H and 6 T

df7 <- df6 %>%
  mutate(
  alt_penalty = abs(alternation_rate - 0.5), 
  run_penalty = abs(max_run - 3.5)/3.5, 
  head_penalty = head_deviation/6) %>% 
#aggregate into a single score:
  mutate(randomness_score = 1 - (alt_penalty + run_penalty + head_penalty)/3)

head(df7 %>% arrange(desc(randomness_score)), 10)

df7 %>% filter(if_any(everything(), is.na)) %>% View()
#looks like there are 7 rows with NA. let's drop them

df8 <- df7 %>% drop_na(coinflips, randomness_score, total_crt_score) %>% 
  select(crt_score = total_crt_score, randomness_score, grade, sleep_hrs)

head(df8)
summary(df8)

#now, some visualizations
#scatterplot: CRT vs Randomness 
ggplot(df8, aes(x = crt_score, y = randomness_score))+
  geom_jitter(width = 0.1, height = 0.01, alpha = 0.6)+
  geom_smooth(method = "lm", se = F, color = "blue") +
  labs(title = "CRT Score vs Randomness Score", 
       x = "CRT Score", y = "Randomness Score") + 
  theme_minimal()


#boxplot: randomness by grade
ggplot(df8, aes(x = grade, y = randomness_score))+
  geom_boxplot()+
  labs(title = "Ramndomness Score by Grade Level")+
  theme_minimal()

cor.test(df8$crt_score, df8$randomness_score)
#t = -1.72, df = 81, p-value = 0.09 (not significant), 
#correlation = -0.18 

# No significant correlation found 


