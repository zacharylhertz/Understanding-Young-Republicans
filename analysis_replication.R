# analysis_replication.R
# Created by Pia Deshpande, Zachary Hertz, and Brian Schaffner


# Loading libraries ####
library(dplyr)
library(haven)
library(tidyverse)
library(cem)
library(dataverse)
library(survey)
library(ggplot2)
library(ggalt)
library(magrittr)


# Bring in data ####
dat <- get_dataframe_by_name(
  filename = "CES20_Common_OUTPUT_vv.dta",
  dataset = "10.7910/DVN/E9N6PH",
  original = TRUE,
  .f = haven::read_dta,
  server = "dataverse.harvard.edu"
)

#### DATA CLEANING + MATCHING ####
# Variable re-codes ####

# Create a variable identifying partisans (and leaners)
dat$party[dat$pid7<4] <- "Democrats"
dat$party[dat$pid7>4 & dat$pid7<8] <- "Republicans"
dat$party <- as.factor(dat$party)
dat$party3 <- NA
dat$party3[dat$pid7<4] <- "Democrats"
dat$party3[dat$pid7>4 & dat$pid7<8] <- "Republicans"
dat$party3[dat$pid7==4] <- "Independents"
dat$party3 <- as.factor(dat$party3)

# Create age variable and a second age variable that combines 18 and 19
dat$age <- 2020-dat$birthyr
dat$age2 <- dat$age
dat$age2[dat$age2==18] <- 19
dat$age3 <- NA
dat$age3[dat$age<20] <- 0
dat$age3[dat$age==22 | dat$age==23] <- 1

dat <- dat %>%
  mutate(race5 = 
           case_when(race== 1 ~ 1, # White
                     race == 2 ~ 2, # Black
                     race == 4 ~ 4, # Asian
                     race > 4 ~ 5, # Other
                     race==3 | hispanic==1 ~ 3), # Hispanic
         Biden = 
           case_when(CC20_410 == 1 ~ 1, # Voted for Biden
                     CC20_410 == 2 ~ 0, # Voted for Trump
                     CC20_410 == 4 ~ 0), # Other vote
         Trump = # Binary Trump Vote Variable
           case_when(CC20_410 == 2 ~ 1, # Voted for Trump
                     CC20_410 == 1 ~ 0, # Voted for Biden
                     CC20_410 == 4 ~ 0), # Other vote
         racism_a = # White people in the U.S. have certain advantages because of the color of their skin.
           case_when(CC20_440a < 3 ~ 1, # Agree
                     CC20_440a == 3 ~ 0, # Neither Agree nor Disagree
                     CC20_440a > 3 ~ 0), # Disagree 
         racism_b = # Racial problems in the U.S. are rare, isolated situations.
           case_when(CC20_440b < 3 ~ 0, # Agree
                     CC20_440b == 3 ~ 0, # Neither Agree nor Disagree
                     CC20_440b > 3 ~ 1), # Disagree 
         workup = # Irish, Italians, Jewish and many other minorities overcame prejudice and worked their way up. Blacks should do the same without any special favors.
           case_when(CC20_441a < 3 ~ 1, # Agree
                     CC20_441a == 3 ~ 0, # Neither Agree nor Disagree
                     CC20_441a > 3 ~ 0), # Disagree 
         structural = # Generations of slavery and discrimination have created conditions that make it difficult for blacks to work their way out of the lower class.
           case_when(CC20_441b < 3 ~ 1, # Agree
                     CC20_441b == 3 ~ 0, # Neither Agree nor Disagree
                     CC20_441b > 3 ~ 0), # Disagree
         epa = # Give the EPA power to regulate CO_2 emissions
           case_when(CC20_333a == 1 ~ 1, # Support
                     CC20_333a == 2 ~ 0), # Oppose
         mw = # $15 Federal Minimum Wage
           case_when(CC20_350b == 1 ~ 1, # Support
                     CC20_350b == 2 ~ 0), # Oppose
         paris = # Withdrawing from the Paris Climate Agreement
           case_when(CC20_355a == 1 ~ 0,  # Support
                     CC20_355a == 2 ~ 1), # Oppose 
         gunban = # Banning Assault Rifles
           case_when(CC20_330b == 1 ~ 1, # Support ban on assault rifles
                     CC20_330b == 2 ~ 0), # Oppose ban on assault rifles 
         discrim = # Support for anti-discrimination laws
           case_when(CC20_350a == 1 ~ 1, # Favor laws against discrimination
                     CC20_350a == 2 ~ 0, # Oppose laws against discrimination
                     CC20_350a >= 8 ~ NA_real_), # 8 = skipped, 9 = not asked
         # Require equal pay for women and men
         equalpay = 
           case_when(CC20_350d == 1 ~ 1,  # Favor equal pay for men and women doing similar jobs
                     CC20_350d == 2 ~ 0, # Oppose 
                     CC20_350d >= 8 ~ NA_real_ ), 
         # 8 = skipped, 9 = not asked
         
         # Ideology (ideo5)
         ideology = 
           case_when(ideo5 == 1 ~ 1, # Very Liberal
                     ideo5 == 2 ~ 2, # Liberal
                     ideo5 == 3 ~ 3, # Moderate
                     ideo5 == 4 ~ 4, # Conservative
                     ideo5 == 5 ~ 5, # Very Conservative
                     ideo5 >= 6 ~ NA_real_),  # 6 is not sure, 8 is skipped, 9 is not asked 
         # Recoded student variable
         # Want to categorize someone as a student if they say they are in the
         # post-elec wave, OR if they list themselves as a full or part-time student
         # in the employment question (pre-elec)
         
         student_recode = 
           case_when(student < 3   ~  1,
                     is.na(student) & employ == 8 ~ 1,
                     TRUE ~ 0)) 

# Subset data to correct age group ####

dat <- subset(dat, age<26)

# Create variable for whether respondent is in college OR graduated from college ####
dat$college <- 0
dat$college[dat$educ>3] <- 1
dat$college[dat$student_recode == 1] <- 1

# Separating data into college and non-college groups
dat_college <- dat %>% filter(college == 1)
dat_college_R <- dat_college %>% filter(party == "Republicans")
dat_college_D <- dat_college %>% filter(party == "Democrats")

dat_notcollege <- dat %>% filter(college == 0)
dat_notcollege_R <- dat_notcollege %>% filter(party == "Republicans")
dat_notcollege_D <- dat_notcollege %>% filter(party == "Democrats")
# Creating survey objects####
survey_college <- svydesign(ids = ~0, data = dat_college, weights = ~commonweight)
survey_notcollege <- svydesign(ids = ~0, data = dat_notcollege, weights = ~commonweight)

##### DEFINING FUNCTIONS ####
college_label <- function(college, notcollege){college %>% 
    mutate(Type = "College") %>% 
    bind_rows(notcollege %>%
                mutate(Type = "Not College"))}


# Party defection plot ####
voteshares2.rep.col <- svytable(~Trump, design=subset(survey_college, party=="Republicans")) %>%
  prop.table() %>%
  multiply_by(100) %>%
  round(digits=0) %>%
  as.data.frame()
voteshares2.rep.col$Type <- "College"
voteshares2.rep.col$party <- "Republicans"
voteshares2.rep.col$vote[voteshares2.rep.col$Trump==0] <- 0
voteshares2.rep.col$vote[voteshares2.rep.col$Trump==1] <- 1
voteshares2.rep.col = subset(voteshares2.rep.col, select = -c(Trump))


voteshares2.rep.ncol <- svytable(~Trump, design=subset(survey_notcollege, party=="Republicans")) %>%
  prop.table() %>%
  multiply_by(100) %>%
  round(digits=0) %>%
  as.data.frame()
voteshares2.rep.ncol$Type <- "Not College"
voteshares2.rep.ncol$party <- "Republicans"
voteshares2.rep.ncol$vote[voteshares2.rep.ncol$Trump==0] <- 0
voteshares2.rep.ncol$vote[voteshares2.rep.ncol$Trump==1] <- 1
voteshares2.rep.ncol = subset(voteshares2.rep.ncol, select = -c(Trump))

voteshares2.dem.col <- svytable(~Biden, design=subset(survey_college, party=="Democrats")) %>%
  prop.table() %>%
  multiply_by(100) %>%
  round(digits=0) %>%
  as.data.frame()
voteshares2.dem.col$Type <- "College"
voteshares2.dem.col$party <- "Democrats"
voteshares2.dem.col$vote[voteshares2.dem.col$Biden==0] <- 0
voteshares2.dem.col$vote[voteshares2.dem.col$Biden==1] <- 1
voteshares2.dem.col = subset(voteshares2.dem.col, select = -c(Biden))

voteshares2.dem.ncol <- svytable(~Biden, design=subset(survey_notcollege, party=="Democrats")) %>%
  prop.table() %>%
  multiply_by(100) %>%
  round(digits=0) %>%
  as.data.frame()
voteshares2.dem.ncol$Type <- "Not College"
voteshares2.dem.ncol$party <- "Democrats"
voteshares2.dem.ncol$vote[voteshares2.dem.ncol$Biden==0] <- 0
voteshares2.dem.ncol$vote[voteshares2.dem.ncol$Biden==1] <- 1
voteshares2.dem.ncol = subset(voteshares2.dem.ncol, select = -c(Biden))


voteshares2 <- rbind(voteshares2.dem.col, voteshares2.dem.ncol, voteshares2.rep.col, voteshares2.rep.ncol)

voteshares2$group[voteshares2$party=="Democrats" &
                    voteshares2$Type=="College"] <- "College-educated Democrats"
voteshares2$group[voteshares2$party=="Republicans" &
                    voteshares2$Type=="College"] <- "College-educated Republicans"
voteshares2$group[voteshares2$party=="Democrats" &
                    voteshares2$Type=="Not College"] <- "Democrats without college education"
voteshares2$group[voteshares2$party=="Republicans" &
                    voteshares2$Type=="Not College"] <- "Republicans without college education"

voteshares2$group <- factor(voteshares2$group, 
                            levels=c("College-educated Republicans", 
                                     "Republicans without college education",
                                     "College-educated Democrats",
                                     "Democrats without college education"))
voteshares2 = subset(voteshares2, vote==1)
voteshares2 <- voteshares2 %>% 
  arrange(group, vote) %>% 
  mutate('label_yposa'=unlist(by(data = Freq, INDICES = group, FUN = cumsum)))

voteshares2$label_ypos <- voteshares2$label_yposa - 0.5*voteshares2$Freq

voteshares2$label_ypos[voteshares2$label_ypos<5] <- NA
voteshares2$label_yposa[voteshares2$label_yposa<5] <- NA
voteshares2$label_ypos[voteshares2$label_ypos>95] <- NA

voteshares2$vote <- factor(voteshares2$vote)

party.new <- ggplot(voteshares2, aes(x=group, y=Freq,fill=party)) + 
  geom_bar(stat="identity", position = position_stack(reverse = TRUE)) +
  coord_flip() + theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5, 
                                  size = 16), 
        plot.caption = element_text(hjust = 1,
                                    face = "italic", size=8),
        axis.title.x = element_text(size=10), 
        axis.title.y = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        axis.text.y = element_text(size=10, 
                                   color = "black")) + 
  scale_fill_manual(values = c("#113e88", "#cf142b")) +
  ylab("Percent of 18-25 year olds who voted for party's presidential nominee") +
  labs(caption = "Plot: Zachary L. Hertz, Pia Deshpande, and Brian Schaffner \nData: CES") +
  ggtitle("") +
  geom_text(aes(y=label_ypos, label=Freq), 
            color="white", size=3.5)



##### ISSUES PLOT ####

# Equal Pay for Equal Work
equalpay1.party <- svyby(~equalpay, ~party3, survey_college, svymean, na.rm = T)
equalpay2.party <- svyby(~equalpay, ~party3, survey_notcollege, svymean, na.rm = T)

equalpay.dat <- college_label(equalpay1.party, equalpay2.party) %>%
  select(equalpay, se_equalpay = se)
# Paris Climate Accords
paris1.party <- svyby(~paris, ~party3, survey_college, svymean, na.rm = T)
paris2.party <- svyby(~paris, ~party3, survey_notcollege, svymean, na.rm = T)

paris.dat <- college_label(paris1.party, paris2.party) %>%
  select(paris, se_paris = se)
# Paris Climate Accords
# Assault Rifle Bans
guns1.party <- svyby(~gunban, ~party3, survey_college, svymean, na.rm = T)
guns2.party <- svyby(~gunban, ~party3, survey_notcollege, svymean, na.rm = T)

guns.dat <- college_label(guns1.party, guns2.party)  %>%
  select(gunban, se_gunban = se)
# Support for anti-discrim legislation
discrim1.party <- svyby(~discrim, ~party3, survey_college, svymean, na.rm = T)
discrim2.party <- svyby(~discrim, ~party3, survey_notcollege, svymean, na.rm = T)


discrim.dat <- college_label(discrim1.party, discrim2.party) %>%
  select(discrim, se_discrim = se)
# % of Whites have advantages
pid.denial.col <- svyby(~racism_a, ~party3, survey_college, svymean, na.rm = T)
pid.denial.ncol <- svyby(~racism_a, ~party3, survey_notcollege, svymean, na.rm = T)

denial.dat <- college_label(pid.denial.col, pid.denial.ncol) %>%
  select(racism_a, se_racism_a = se)

pid.isolate.col <- svyby(~racism_b, ~party3, survey_college, svymean, na.rm = T)
pid.isolate.ncol <- svyby(~racism_b, ~party3, survey_notcollege, svymean, na.rm = T)

isolate.dat <- college_label(pid.isolate.col, pid.isolate.ncol) %>%
  select(racism_b, se_racism_b = se)

pid.workup.col <- svyby(~workup, ~party3, survey_college, svymean, na.rm = T)
pid.workup.ncol <- svyby(~workup, ~party3, survey_notcollege, svymean, na.rm = T)

workup.dat <- college_label(pid.workup.col, pid.workup.ncol) %>%
  select(workup, se_workup = se)

pid.structural.col <- svyby(~structural, ~party3, survey_college, svymean, na.rm = T)
pid.structural.ncol <- svyby(~structural, ~party3, survey_notcollege, svymean, na.rm = T)

structural.dat <- college_label(pid.structural.col, pid.structural.ncol) %>%
  select(structural, se_structural = se)
# EPA approval by party
gen_env1.party <- svyby(~epa, ~party3, survey_college, svymean, na.rm = T)
gen_env2.party <- svyby(~epa, ~party3, survey_notcollege, svymean, na.rm = T)

env.dat <- college_label(gen_env1.party, gen_env2.party) %>%
  select(epa, se_epa = se)

# Minimum wage
gen_mw1.party <- svyby(~mw, ~party3, survey_college, svymean, na.rm = T)
gen_mw2.party <- svyby(~mw, ~party3, survey_notcollege, svymean, na.rm = T)

gen_mw.party<- college_label(gen_mw1.party, gen_mw2.party) %>%
  filter(party3 != "Independents")

mw.dat <- college_label(gen_mw1.party, gen_mw2.party) %>%
  rename(se_mw = se)

issue_wide <- cbind(mw.dat, env.dat, denial.dat, discrim.dat, guns.dat, paris.dat, equalpay.dat, isolate.dat, workup.dat, structural.dat) %>%
  filter(party3 == "Republicans") %>%
  
  # dropping SE columns for graphing
  select(-contains("se_")) %>%
  unite(rep_type, c("party3", "Type")) %>%
  pivot_longer(cols = c(mw:structural),
               names_to = "issue", 
               values_to = "value") %>%
  pivot_wider(names_from = rep_type, values_from = value) %>%
  mutate(issue = case_when(
    issue == "mw" ~ "Support increasing min. wage",
    issue == "epa" ~ "Give EPA the power to regulate carbon emissions",
    issue == "discrim" ~ "Support anti-discrimination Laws",
    issue == "gunban" ~ "Support ban on Assault Rifles",
    issue == "paris" ~ "Oppose withdrawal from Paris Climate Accords",
    issue == "equalpay" ~ "Support equal pay for equal work",
    issue == "racism_a" ~ "Believe that Whites have advantages",
    issue == "racism_b" ~ "Racial problems in the U.S. are NOT rare, isolated situations",
    issue == "workup" ~ "Black people should NOT be expected to overcome prejudice without any special favors",
    issue == "structural" ~ "Historical conditions make it difficult for Black people to escape the lower class",
    TRUE ~ issue
  ))  %>%
  arrange(Republicans_College) %>%
  mutate(issue == factor(issue, levels = 
                           c ("Support equal pay for equal work",
                              "Suport ban on assault rifles",
                              "Support anti-discrimination laws",
                              "Oppose withdrawal from Paris Climate Accords",
                              "Give EPA the power to regulate carbon emissions",
                              "Racial problems in the U.S. are NOT rare, isolated situations",
                              "Black people should NOT be expected to overcome prejudice without any special favors",
                              "Historical conditions make it difficult for Black people to escape the lower class",
                              "Believe that Whites have advantages",
                              "Support increasing min. wage"
                             )))

issue_wide$issue <- factor(issue_wide$issue,
                        levels = c("Believe that Whites have advantages", 
                                   "Support increasing min. wage",
                                   "Black people should NOT be expected to overcome prejudice without any special favors", 
                                   "Historical conditions make it difficult for Black people to escape the lower class",
                                   "Support ban on Assault Rifles",
                                   "Support anti-discrimination Laws",
                                   "Support equal pay for equal work",
                                   "Racial problems in the U.S. are NOT rare, isolated situations",
                                   "Oppose withdrawal from Paris Climate Accords",
                                   "Give EPA the power to regulate carbon emissions"))

issue_wide <- issue_wide %>% 
  mutate(statsig = case_when(issue=="Believe that Whites have advantages" | 
                issue == "Black people should NOT be expected to overcome prejudice without any special favors" |
                issue == "Historical conditions make it difficult for Black people to escape the lower class" |
                issue == "Support increasing min. wage" ~ 0,
                issue!="Believe that Whites have advantages" & 
                issue != "Black people should NOT be expected to overcome prejudice without any special favors" &
                issue != "Historical conditions make it difficult for Black people to escape the lower class" & 
                issue != "Support increasing min. wage" ~ 1))

statsig <- issue_wide %>%
  filter(issue=="Believe that Whites have advantages" | 
           issue == "Black people should NOT be expected to overcome prejudice without any special favors" |
           issue == "Historical conditions make it difficult for Black people to escape the lower class" |
           issue == "Support increasing min. wage") 
insig <- issue_wide %>%
  filter(issue!="Believe that Whites have advantages" & 
           issue != "Black people should NOT be expected to overcome prejudice without any special favors" &
           issue != "Historical conditions make it difficult for Black people to escape the lower class" & 
           issue != "Support increasing min. wage") 

issues_plot <- ggplot() +
  geom_dumbbell(data=insig, aes(y=issue, x=`Republicans_Not College`, xend=Republicans_College),
                size=1.5, color="#DADFF7", size_x=3, size_xend = 3, colour_x = "#320D6D", colour_xend = "#119DA4") +
  geom_dumbbell(data=statsig, aes(y=issue, x=`Republicans_Not College`, xend=Republicans_College),
                size=1.5, color="#5F758E", size_x=3, size_xend = 3, colour_x = "#320D6D", colour_xend = "#119DA4") +
  geom_text(data=filter(issue_wide, issue=="Believe that Whites have advantages"),
            aes(x=Republicans_College, y=issue, label="College"),
            color="#119DA4", size=3, vjust=-1.5, fontface="bold") +
  geom_text(data=filter(issue_wide, issue=="Believe that Whites have advantages"),
            aes(x=`Republicans_Not College`, y=issue, label="Not College"),
            color="#320D6D", size=3, vjust=-1.5, fontface="bold") +
  geom_text(data=issue_wide, aes(x=Republicans_College, y=issue, label= round(100*(Republicans_College),0)),
            color="#119DA4", size=2.75, vjust=2.5) +
  geom_text(data=issue_wide, color="#320D6D", size=2.75, vjust=2.5,
            aes(x=`Republicans_Not College`, y=issue, label= round(100*(`Republicans_Not College`),0))) +
  scale_x_continuous(labels = scales::percent) +
  geom_errorbar() + xlab("Percent") + 
  ylab("") +
  ggtitle("Issue Positions Among Republicans With and Without a College Education") +
  theme_minimal()
  
