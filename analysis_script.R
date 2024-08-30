library(tidyverse)
library(sf)
library(MetBrewer)
library(patchwork)
library(tidytext)

## ---- Output directory, data input, and stats ----
results <- read.csv(paste0(wd, "archived_data.csv"))

results %>% filter(Q3.Intro == "No") %>% nrow # 805 California Residents ONLY

#### ---- FUNCTIONS ---------
## Function to calculate the number of respondents 
calc_n <- function(Q) {
  results %>%
    select({{Q}}, Q3.Intro) %>%
    filter({{Q}} != "" & Q3.Intro == "No") %>% 
    nrow()
}

## Function to format data for figures
# n1 = number of respondents in service area
# n2 = number of respondents in other counties
count_Q <- function(Q, tot_n) {
  results %>%
    select({{Q}}, Q3.Intro) %>%
    filter({{Q}} != "" & Q3.Intro == "No") %>% 
    separate_rows({{Q}}, sep = ",") %>%
    count({{Q}}) %>%
    rename(answer = {{Q}}) %>%
    mutate(percent = round(n/{{tot_n}}*100, digits = 1))
}

## Function to create bar plot 
# n_col = number of possible answers
# w = width of the plot needed
bar_plot <- function(b, n_col, w) {
  {{b}} %>%
    mutate(answer = str_wrap(answer, width=35),
           answer = fct_reorder(answer, n, .desc = FALSE)) %>%
    ggplot(aes(x=percent, y=answer, fill = answer)) +
    geom_bar(position=position_dodge(), stat="identity", color = "white") +
    geom_text(aes(label = paste0(percent, "%")), position = position_dodge(width = 0.9), hjust=-0.05, color="black") +
    scale_fill_manual(values=met.brewer("Demuth", {{n_col}})) +
    xlab("Percent of respondents") + ylab("") +
    theme_minimal() +
    theme(legend.position = "none", legend.title = element_blank()) +
    xlim(0,{{w}})
}

## Function to create vertical bar plot
# w = width of the plot needed
v_bar_plot <- function(b, w) {
  {{b}} %>%
    mutate(answer = str_wrap(answer, width=10),
           answer = fct_reorder(answer, n, .desc = T)) %>%
    ggplot(aes(y=percent, x=answer, fill = answer)) +
    geom_bar(position=position_dodge(), stat="identity", color = "white") +
    geom_text(aes(label = paste0(percent, "%")), position = position_dodge(width = 0.9), vjust=-0.15, color="black") +
    ylab("Percent of respondents") + xlab("") +
    theme_minimal() +
    theme(legend.position = "none", legend.title = element_blank()) +
    ylim(0,{{w}})
}

## Function to create pie chart
# filt = region name
# n_col = number of possible answers
pie_chart <- function(b, n_col) {
  {{b}} %>%
    mutate(answer = str_wrap(answer, width=35),
           ypos = cumsum(percent) - 0.5 * percent, 
           answer = fct_reorder(answer, n, .desc = FALSE)) %>%
    ggplot(aes(x = "", y = percent, fill = answer)) +
    geom_bar(width = 1, stat="identity", color = "white") +
    coord_polar("y", start = 0) +
    geom_text(aes(x=1.75, label = paste0(answer, "\n(", percent, "%)" )), position = position_stack(vjust = 0.5), color = "black", size = 4) +
    scale_fill_manual(values=met.brewer("Demuth", {{n_col}})) +
    theme_void() +
    theme(legend.position = "none")
}


#### -------- THE BUILT ENVIRONMENT --------
## ---- Q14 Have you completed home retrofitting or remodeling to make your home more resistant to wildfire? ----
tot_n = calc_n(Q14) #788
a = count_Q(Q14, tot_n) %>%  
  filter(answer == " I have not completed home retrofitting or remodeling" | 
           answer == "Purchased an air filtration system for home to use during wildfire smoke events" |
           answer == "Purchased a backup power source (e.g." | answer == "Put up non-combustible" |
           answer == "Removed vegetation and other combustible material within the first 0-5 feet of my home " |
           answer == "Retrofitted home with ignition and/or ember resistant materials" |
           answer == "No" | answer == "Other (please specify):") %>%
  mutate(answer = case_when(answer == " I have not completed home retrofitting or remodeling" ~ "I am considering retrofitting or remodeling, but am unsure of what actions to take",
                            answer == "Purchased a backup power source (e.g." ~ "Purchased a backup power source",
                            answer == "Put up non-combustible" ~ "Put up non-combustible, reflective signs/addressing on house",
                            answer == "No" ~ "No, I have not completed home retrofitting or remodeling",
                            answer == "Other (please specify):" ~ "Other", TRUE ~ answer))
w = max(a$percent) + 5
bar_plot(a, 9, w)



## ---- Q15 What are your three biggest considerations in home retrofitting or remodeling to improve your home’s fire-resistance? ----  
n_Q = calc_n(Q15)
a = count_Q(Q15, n_Q) %>%
  filter(answer != " information" & answer != " or resources") %>%
  mutate(answer = case_when(answer == "Lack of knowledge" ~ "Lack of knowledge, information, or resources", 
                            answer == "Other (please specify)" ~ "Other", TRUE ~ answer))
w = max(a$percent) + 5
bar_plot(a, 9, w)


#### -------- FUELS REDUCTION --------
## ---- Q17 Have you completed any fuels reduction work on your property? ----
n_Q = calc_n(Q17)
a = count_Q(Q17, n_Q)
pie_chart(a, 2)

## ---- Q18 Which treatment types have you used to reduce hazardous fuels? ----
n_Q = calc_n(Q18) #654
a = count_Q(Q18, n_Q) %>%
  filter(answer != " mastication or chipping)") %>%
  mutate(answer = case_when(answer == "Other (please specify):" ~ "Other", 
                            answer == "Prescribed fire (a controlled fire set intentionally to meet management objectives under applicable laws and regulations)" ~ "Prescribed fire",
                            answer == "Removal of small trees and brush with heavy equipment (e.g." ~ "Removal of small trees and brush with heavy equipment", 
                            TRUE ~ answer))
w = max(a$percent) + 5
bar_plot(a, 9, w)

## ---- Q19 How did you accomplish your fuels reduction work? ----
n_Q = calc_n(Q19) 
count_Q(Q19,n_Q) %>%
  mutate(answer = case_when(answer =="Other (please specify)" ~ "Other", 
                            answer == "Did some myself and hired a contractor" ~ "Did some myself &\nhired a contractor",
                            answer == "Worked with neighbors or community group" ~ "Worked with neighbors\nor community group", 
                            TRUE ~ answer),
         tot_n = sum(n), percent = round(n/n_Q * 100, digits = 1)) %>%
  ggplot(aes(x=answer, y = percent, fill = answer)) +
  geom_bar(position=position_dodge(), stat="identity", color = "white") +
  geom_text(aes(label = paste0(percent, "%")), position = position_dodge(width = 0.9), vjust=-0.2, color="black") +
  scale_fill_manual(values=met.brewer("Demuth", 5)) +
  ylab("Percent of respondents") + xlab("") +
  theme_classic() +
  theme(legend.position = "none")

## ---- Q20 How did you pay for your fuels reduction work? ----
n_Q = calc_n(Q20)
a = count_Q(Q20, n_Q) %>%
  filter(answer != " and family)" & answer != " friends") %>%
  mutate(answer = ifelse(answer == "My only cost was personal labor (myself", "My only cost was personal labor", answer))
pie_chart(a, 4)

## ---- Q21 What organization provided the grant and/or cost-share agreement? ----
n_Q = calc_n(Q21)
a = count_Q(Q21, n_Q) %>%
  mutate(answer = case_when(answer == "I don‚Äôt know who provided the grant" ~ "I don't know who provided the grant",
                            answer == "Other (please specify)" ~ "Other", TRUE ~ answer))
w = max(a$percent) + 5
bar_plot(a, 10, w)  

## ---- Q24 Are you interested in conducting a prescribed fire on your property? ----
n_Q = calc_n(Q24)
a = count_Q(Q24, n_Q)
pie_chart(a, 3)

## ---- Q25 What resources would make you more likely to conduct a prescribed fire? ----
n_Q = calc_n(Q25)
a = count_Q(Q25, n_Q) %>%
  mutate(answer = ifelse(answer == "Other (please explain)", "Other", answer))
w = max(a$percent) + 5
bar_plot(a, 10, w)


#### -------- COMMUNITY ENGAGEMENT --------
## ---- Q32 Please select three topics you are most interested in learning about ---- 
tot_n = calc_n(Q32)
a = count_Q(Q32, tot_n) %>%
  filter(answer != " chipping" & answer != " mastication" & answer != " etc)") %>%
  mutate(answer = case_when(answer == "Other (please specify)" ~ "Other", 
                            answer == "Mechanical fuels reduction treatments (forest thinning" ~ "Mechanical fuels reduction treatments",
                            TRUE ~ answer))
w = max(a$percent) + 5
bar_plot(a, 12, w)

## ---- Q33 What activities related to the topics above would you be most interested in participating in? ----
tot_n = calc_n2(Q33)
a = count_Q2(Q33) %>%
  filter(Q3.Intro == "CA Resident")
w = max(a$percent) + 5
p1 = v_bar_plot(a, w)

## ---- Q34 How do you prefer to receive information? ----
tot_n = calc_n2(Q34)
a = count_Q2(Q34) %>% mutate(answer = ifelse(answer == "Other (please specify)", "Other", answer)) %>% filter(Q3.Intro == "CA Resident")
w = max(a$percent) + 5
p2 = v_bar_plot(a, w)

p1 + p2


#### -------- DEMOGRAPHICS --------
## ---- Q37 Would you consider the community where you live to be urban, suburban, or rural? ----
n_Q = calc_n(Q37)
a = count_Q(Q37, n_Q)
pie_chart(a, 3)

## ---- Q38 What size is your residential property? ----
tot_n = calc_n(Q38)
count_Q(Q38, tot_n)

## ---- Q39 Not including residential use, what would you describe as the primary use(s) for your property? ----
n_Q = calc_n(Q39)
a = count_Q(Q39, n_Q) %>%
  filter(answer != " my property is only used for residential use") %>%
  mutate(answer = case_when(answer == "Not applicable" ~ "N/A, my property is only used for residential use", 
                            answer == "Other (please specify)" ~ "Other", TRUE ~ answer))
w = max(a$percent) +5
bar_plot(a, 11, w)

## ---- Q40 Do you have a conservation easement on your property? ----
n_Q = calc_n(Q40)
count_Q(Q40, n_Q)

## ---- Q41 What is your age? ----
n_Q = calc_n(Q41)
count_Q(Q41, n_Q) %>%
  ggplot(aes(x=answer, y = percent, fill = answer)) +
  geom_bar(position=position_dodge(), stat="identity", color = "white") +
  geom_text(aes(label = paste0(percent, "%")), position = position_dodge(width = 0.9), vjust=-0.05, color="black") +
  scale_fill_manual(values=met.brewer("Demuth", 7)) +
  ylab("Percent of respondents") + xlab("Age") +
  theme_classic() +
  theme(legend.position = "none", legend.title = element_blank()) 

## ---- Q42 What is your gender? ---- 
tot_n = calc_n(Q42)
count_Q(Q42, tot_n)

## ---- Q43 What is your race? ----
tot_n = calc_n(Q43)
count_Q(Q43, tot_n) %>%
  filter(answer != " Latino/a/x" & answer != " or other Spanish origin") %>%
  mutate(answer = case_when(answer == "Hispanic" ~ "Hispanic, Latino/a/x, or other Spanish origin",
                            answer == "Other (please specify)" ~ "Other", TRUE ~ answer))

## ---- Q44 What is/are your preferred language(s)? ----
tot_n = calc_n(Q44)
count_Q(Q44, tot_n)

## ---- Q45 Please select your household's approximate annual income ----
n_Q = calc_n(Q45)
results %>%
  select(Q45) %>%
  filter(Q45 != "") %>%
  count(Q45) %>%
  mutate(percent = round(n/n_Q * 100, digits = 1),
         Q45 = case_when(Q45=="Less than $50,000 per year" ~ "< $50k",
                         Q45=="$50,000 - $100,000 per year" ~ "$50-100k",
                         Q45=="$100,001 - $150,000 per year" ~ "$100-150k",
                         Q45=="Greater than $150,000 per year" ~ "> $150k", TRUE ~ Q45)) %>%
  ggplot(aes(x=factor(Q45, level=c("< $50k", "$50-100k", "$100-150k", "> $150k", "Prefer not to state")), y = percent, fill = Q45)) +
  geom_bar(position=position_dodge(), stat="identity", color = "white") +
  geom_text(aes(label = paste0(percent, "%")), position = position_dodge(width = 0.9), vjust=-0.05, color="black") +
  scale_fill_manual(values=met.brewer("Demuth", 5)) +
  ylab("Percent of respondents") + xlab("Annual income") +
  theme_classic() +
  theme(legend.position = "none", legend.title = element_blank())
