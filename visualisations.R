# UPF map visualisations
library(dplyr)
library(ggplot2)
library(tidyr)

# load in data
upf_data <- read.csv("~/Library/CloudStorage/GoogleDrive-nealhaddaway@gmail.com/My Drive/contracts/Zalf/UPF review/upf_data.csv", check.names=FALSE)

# 1. General overview
#### Time ####
#keep only unique articles
articles <- upf_data[!duplicated(upf_data[ , c("label")]),]

#summarise by year
pub_year <- articles %>% 
  group_by(year) %>% 
  summarise(n = n())
#plot
ggplot(data=pub_year, aes(x=year, y=n)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() +
  xlab('Publication year') +
  ylab('Number of studies') + 
  scale_x_continuous(breaks = seq(1985, 2025, by = 5))

#### Countries ####
countries <- upf_data %>% 
  group_by(country) %>% 
  summarise(n = n())
write.csv(countries, 'countries.csv', row.names = FALSE)

#### Language-Country ####
lang_cntry <- upf_data %>% 
  group_by(language, country) %>% 
  summarise(n = n())
#plot
ggplot(lang_cntry, aes(x = language, y = country, fill = n)) +
  geom_tile(color = "white",
            lwd = .1,
            linetype = 1) +
  scale_y_discrete(limits=rev) +
  geom_text(aes(label = n), color = "white", size = 2) +
  xlab('Language') +
  ylab('Country') +
  scale_fill_continuous(name = 'Studies')

#### Type-Country ####
type_cntry <- articles %>% 
  group_by(type, country) %>% 
  summarise(n = n())
#plot
ggplot(type_cntry, aes(x = type, y = country, fill = n)) +
  geom_tile(color = "white",
            lwd = .1,
            linetype = 1) +
  scale_y_discrete(limits=rev) +
  geom_text(aes(label = n), color = "white", size = 2) +
  xlab('Type') +
  ylab('Country') +
  scale_fill_continuous(name = 'Studies')

#### Type-Language ####
type_lang <- articles %>% 
  group_by(type, language) %>% 
  summarise(n = n())
#plot
ggplot(type_lang, aes(x = type, y = language, fill = n)) +
  geom_tile(color = "white",
            lwd = .1,
            linetype = 1) +
  scale_y_discrete(limits=rev) +
  geom_text(aes(label = n), color = "white", size = 2) +
  xlab('Type') +
  ylab('Language') +
  scale_fill_continuous(name = 'Studies')

#### Country-UPF typology ####
country_system <- upf_data %>% 
  group_by(country, system) %>% 
  summarise(n = n())
#plot
ggplot(country_system, aes(x = system, y = country, fill = n)) +
  geom_tile(color = "white",
            lwd = .1,
            linetype = 1) +
  scale_y_discrete(limits=rev) +
  geom_text(aes(label = n), color = "white", size = 2) +
  xlab('System') +
  ylab('Country') +
  scale_fill_continuous(name = 'Studies') + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), text = element_text(size = 9.5))

#### Outcome-UPF typology ####
syst_outcome <- data.frame(system=upf_data$system, upf_data[,10:35], check.names=FALSE)
syst_outcome[syst_outcome == ""] <- NA

syst_outcome_long <- syst_outcome %>%
  pivot_longer(!system, names_to = "outcome", values_to = "count", values_drop_na = TRUE)

syst_outcome_sum <- syst_outcome_long %>% 
  group_by(system, outcome) %>% 
  summarise(n = n())
syst_outcome_sum$outcome <- gsub('X', '', syst_outcome_sum$outcome)
#plot
ggplot(syst_outcome_sum, aes(x = system, y = outcome, fill = n)) +
  geom_tile(color = "white",
            lwd = .1,
            linetype = 1) +
  scale_y_discrete(limits=rev) +
  geom_text(aes(label = n), color = "white", size = 2) +
  xlab('System') +
  ylab('Outcome') +
  scale_fill_continuous(name = 'Studies') + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), text = element_text(size = 9.5))

