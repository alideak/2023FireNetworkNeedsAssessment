library(tidyverse)
library(sf)
library(MetBrewer)


## JOINING RESULTS TO REGION BY ZIP CODE ## ------
regs = st_read(paste0(wd, "gis_data/taskforce_regions.shp"))

zips = st_read(paste0(wd, "gis_data/zipcodes.shp"))

dat = read.csv(paste0(wd, "archived_data.csv")) %>%
  na.omit(Q36) %>%
  mutate(Q36 = as.numeric(Q36)) # %>% nrow 713 

z = st_join(zips, regs, sparse = T, largest = T) %>%
  mutate(ZIP_CODE = as.numeric(ZIP_CODE))

df = z %>% select(ZIP_CODE, Name) %>% 
  right_join(dat, by=c(ZIP_CODE = "Q36"))

nrow(zips) # 1721
nrow(z) # 1721
nrow(dat) # 713
nrow(df) # 713 

# joined = z %>% 
#   as.data.frame() %>% 
#   select(ZIP_CODE, Name) %>% 
#   distinct() %>% 
#   right_join(dat, by = join_by(ZIP_CODE == Q36)) %>% 
#   rename("REGION" = "Name")

# write.csv(joined, archived_data_with_region.csv"))
results = read.csv(paste0(wd,"archived_data_with_region.csv"))

## FUNCTIONS ----
n <- function(Q) {
  results %>%
    select({{Q}}, REGION, Q3.Intro) %>%
    filter({{Q}} != "" & REGION != "" & Q3.Intro == "No") %>% 
    group_by(REGION) %>%
    count() %>%
    rename("tot_n" = n)
}

count_Q <- function(Q) {
  results %>%
    select({{Q}}, REGION) %>%
    filter({{Q}} != "" & REGION != "") %>% 
    separate_rows({{Q}}, sep = ",") %>%
    group_by(REGION) %>%
    count({{Q}}) %>%
    rename(answer = {{Q}}) %>%
    right_join(tot_n) %>%
    mutate(answer = ifelse(answer=="Other (please specify)", "Other", answer),
           percent = round(n/tot_n*100, digits = 1),
           REGION = case_when(REGION == "CentralCoast" ~ paste0("Central Coast\n(n=", tot_n, ")"),
                              REGION == "NorthCoast" ~ paste0("North Coast\n(n=", tot_n, ")"),
                              REGION == "SierraNevada" ~ paste0("Sierra Nevada\n(n=", tot_n, ")"),
                              REGION == "SouthernCalifornia" ~ paste0("Southern California\n(n=", tot_n, ")")))
}

heat_map = function(a) {
  {{a}} %>%
    mutate(answer = str_wrap(answer, width=35),
           answer = fct_reorder(answer, percent, .desc = T)) %>%
    ggplot(aes(x = REGION, y = answer, fill = REGION)) +
    geom_tile(color = "white", aes(alpha = percent)) +
    geom_text(aes(label = paste0(percent, "%")), size=4.5) +
    scale_fill_manual(values=met.brewer("Demuth", 4)) +    
    theme_minimal() + theme(legend.position = "none") +
    xlab("") + ylab("") +
    scale_y_discrete(limits=rev) +
    ggtitle(Q)
}

grouped_bar = function(a, w) {
  {{a}} %>%
    mutate(answer = str_wrap(answer, width=35),
           answer = fct_reorder(answer, percent, .desc = T)) %>%
    ggplot(aes(x = reorder(answer, percent), y = percent, fill = REGION)) +
    geom_bar(position=position_dodge(), stat="identity", color = "white") +
    coord_flip() +
    geom_text(aes(label = paste0(percent, "%")), position = position_dodge(width = 0.9), hjust=-0.1, color="black") +
    scale_fill_manual(values=met.brewer("Demuth", 4)) +
    ylab("Percent of respondents") + xlab("") +
    theme_minimal() +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    guides(fill = guide_legend(reverse = TRUE)) + ylim(0, {{w}}) +
    ggtitle(Q)
}

## Q14 ----
Q = "Have you completed home retrofitting or remodeling to make your home more resistant to wildfire?"
tot_n = n(Q14)
a = count_Q(Q14) %>%
  filter(answer == " I have not completed home retrofitting or remodeling" | 
           answer == "Purchased an air filtration system for home to use during wildfire smoke events" |
           answer == "Purchased a backup power source (e.g." | answer == "Put up non-combustible" |
           answer == "Removed vegetation and other combustible material within the first 0-5 feet of my home " |
           answer == "Retrofitted home with ignition and/or ember resistant materials" |
           answer == "No" | answer == "Other") %>%
  mutate(answer = case_when(answer == " I have not completed home retrofitting or remodeling" ~ "I am considering retrofitting or remodeling, but am unsure of what actions to take",
                            answer == "Purchased a backup power source (e.g." ~ "Purchased a backup power source",
                            answer == "Put up non-combustible" ~ "Put up non-combustible, reflective signs/addressing on house",
                            answer == "No" ~ "No, I have not completed home retrofitting or remodeling",
                            answer == "Other (please specify):" ~ "Other", TRUE ~ answer),
         answer = str_wrap(answer, width=35))
heat_map(a)


## Q15 ----
Q = "What are your three biggest considerations in home retrofitting or remodeling to improve your home’s fire-resistance?"
tot_n = n(Q15)
a = count_Q(Q15) %>%
  filter(answer != " information" & answer != " or resources") %>%
  mutate(answer = case_when(answer == "Lack of knowledge" ~ "Lack of knowledge, information, or resources", 
                            answer == "Other (please specify)" ~ "Other", TRUE ~ answer),
         answer = str_wrap(answer, width=35))

ggplot(a, aes(x = percent, y = reorder(answer, percent), color = REGION)) +
  geom_point(size = 5, alpha = .75) +  # Use a larger dot
  scale_color_manual(values=met.brewer("Demuth", 4)) +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey60", linetype = "dashed"),
    legend.position = "bottom", axis.title.y = element_blank())

## Q18 ----
Q = "Which treatment types have you used to reduce hazardous fuels?"
tot_n = n(Q18) # CC = 172, NorCo = 79, SN = 240 SoCal = 199
a = count_Q(Q18) %>%
  filter(answer != " mastication or chipping)") %>%
  mutate(answer = case_when(answer == "Other (please specify):" ~ "Other", 
                            answer == "Prescribed fire (a controlled fire set intentionally to meet management objectives under applicable laws and regulations)" ~ "Prescribed fire",
                            answer == "Removal of small trees and brush with heavy equipment (e.g." ~ "Removal of small trees and brush with heavy equipment", 
                            TRUE ~ answer),
         percent = round(percent, 0))
grouped_bar(a, 105) + facet_grid(cols = vars(REGION))

## Q24 ----
Q = "Are you interested in conducting a prescribed fire on your property?"
tot_n = n(Q24)
a = count_Q(Q24)
grouped_bar(a, 70)

## Q32 ----
Q = "Please select three topics you are most interested in learning about"
tot_n = n(Q32)
a = count_Q(Q32) %>%
  filter(answer != " chipping" & answer != " mastication" & answer != " etc)") %>%
  mutate(answer = case_when(answer == "Mechanical fuels reduction treatments (forest thinning" ~ "Mechanical fuels reduction treatments",
                            TRUE ~ answer),
         answer = str_wrap(answer, width=40))

ggplot(a, aes(x = percent, y = reorder(answer, percent), color = REGION)) +
  geom_point(size = 5, alpha = .75) +  # Use a larger dot
  scale_color_manual(values=met.brewer("Demuth", 4)) +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey60", linetype = "dashed"),
    legend.position = c(0.8, 0.2), axis.title.y = element_blank())
