# Load necessary libraries
#for data manipulation
library(dplyr)
library(tidyverse)

#For graphs
library(ggplot2)
library(gridExtra)

#for checkinig correlation
library(corrgram)

#Set current working directory
setwd("<Your current working folder here>")

#Load data from current working directory and give them the custom column names
#data <- read.csv("Conbined GDP per capita and life expectency by birth.csv")
data <- read.csv("Econmical_Factors.csv")

colnames(data) <- c("Country","Code","Region","Year","GDP_Per_Capita","Life_Expectency_by_birth","Inflation_percent","GNI")



#have a galnce of data
head(data)
str(data)

#count wors before data cleaning
count(data)

#Replace empty striings with N/A values
data <- data %>% mutate_if(is.character, ~na_if(., ''))

#Source: https://jenslaufer.com/data/analysis/visualize_missing_values_with_ggplot.html
#You can use the gather function from tidyr to collapse the columns into key-value pairs.
# Then you create a new logical feature which is true in case of a missing value.
# You group on the key and the new logical feature to do a count.
# Then you filter on the logical feature to get the count where the value is missing.
# You skip the rows which are not needed and sort by the number of missing values.
missing_values <- data %>% gather(key = "key", value = "val") %>%
  mutate(is.missing = is.na(val)) %>%
  group_by(key, is.missing) %>%
  summarise(num.missing = n()) %>%
  filter(is.missing==T) %>%
  select(-is.missing) %>%
  arrange(desc(num.missing))

#data <- data %>% filter(Region != "" | Code != "" | Country != "" | is.na(Year),
#                        is.na(GDP_Per_Capita) | is.na(Life_Expectency_by_birth))


missing_values %>%
  ggplot() +
  geom_bar(aes(x=key, y=num.missing), stat = 'identity') +
  labs(x='variable', y="number of missing values", title='Number of missing values') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Another method for missing values
missing_values <- data %>%
  gather(key = "key", value = "val") %>%
  mutate(isna = is.na(val)) %>%
  group_by(key) %>%
  mutate(total = n()) %>%
  group_by(key, total, isna) %>%
  summarise(num.isna = n()) %>%
  mutate(pct = num.isna / total * 100)

levels <-
  (missing_values  %>% filter(isna == T) %>% arrange(desc(pct)))$key

percentage.plot <- missing_values %>%
  ggplot() +
  geom_bar(aes(x = reorder(key, desc(pct)),
               y = pct, fill=isna),
           stat = 'identity', alpha=0.8) +
  scale_x_discrete(limits = levels) +
  scale_fill_manual(name = "",
                    values = c('steelblue', 'tomato3'), labels = c("Present", "Missing")) +
  coord_flip() +
  labs(title = "Percentage of missing values", x =
    'Variable', y = "% of missing values")

percentage.plot

# Total count of missing values
colSums(is.na(data))
sum(is.na(data))

data <- data %>% drop_na()
data <- data %>% filter(Year %in% 2000:2017)

#Count data after dropping na's
count(data)

#STORE AND CHECK REGION WISE COUNT
Sub_Saharan_Africa <- data %>% filter(Region == "Sub-Saharan Africa") %>%
  group_by(Country) %>% summarise(Count = n()) %>% arrange(desc(Count))
Europe_Central_Asia <- data %>% filter(Region == "Europe & Central Asia") %>%
  group_by(Country) %>% summarise(Count = n()) %>% arrange(desc(Count))
Latin_America_Caribbean<- data %>% filter(Region == "Latin America & Caribbean") %>%
  group_by(Country) %>% summarise(Count = n()) %>% arrange(desc(Count))
East_Asia_Pacific <- data %>% filter(Region == "East Asia & Pacific") %>%
  group_by(Country) %>% summarise(Count = n()) %>% arrange(desc(Count))
Middle_East_North_Africa <- data %>%
  filter(Region == "Middle East & North Africa") %>% group_by(Country) %>%
  summarise(Count = n()) %>% arrange(desc(Count))
South_Asia <- data %>% filter(Region == "South Asia") %>%
  group_by(Country) %>% summarise(Count = n()) %>% arrange(desc(Count))
North_America  <- data %>% filter(Region == "North America") %>%
  group_by(Country) %>% summarise(Count = n()) %>% arrange(desc(Count))


head(Sub_Saharan_Africa)
head(Europe_Central_Asia)
head(Latin_America_Caribbean)
head(East_Asia_Pacific)
head(Middle_East_North_Africa)
head(South_Asia)
head(North_America)

#PLOT THE GRAPHS FOR COUNTRIES WITH MOST DATA IN A REGION
p1 <- data %>% filter(Country == "Nigeria") %>% ggplot(aes(Life_Expectency_by_birth,log(GDP_Per_Capita)))+
  geom_point(aes(col = Year),size = 2)+
  labs(title = "Nigeria")+
  theme(legend.position = "none")

p2 <- data %>% filter(Country == "Austria") %>% ggplot(aes(Life_Expectency_by_birth,log(GDP_Per_Capita)))+
  geom_point(aes(col = Year),size = 2)+
  labs(title = "Austria")+
  theme(legend.position = "none")

p3<- data %>% filter(Country == "Colombia") %>% ggplot(aes(Life_Expectency_by_birth,log(GDP_Per_Capita)))+
  geom_point(aes(col = Year),size = 2)+
  labs(title = "Colombia")+
  theme(legend.position = "none")

p4<- data %>% filter(Country == "Australia") %>% ggplot(aes(Life_Expectency_by_birth,log(GDP_Per_Capita)))+
  geom_point(aes(col = Year),size = 2)+
  labs(title = "Australia")+
  theme(legend.position = "none")

p5<- data %>% filter(Country == "Morocco") %>% ggplot(aes(Life_Expectency_by_birth,log(GDP_Per_Capita)))+
  geom_point(aes(col = Year),size = 2)+
  labs(title = "Morocco")+
  theme(legend.position = "none")

p6<- data %>% filter(Country == "India") %>% ggplot(aes(Life_Expectency_by_birth,log(GDP_Per_Capita)))+
  geom_point(aes(col = Year),size = 2)+
  labs(title = "India")+
  theme(legend.position = "none")

p7<- data %>% filter(Country == "United States") %>% ggplot(aes(Life_Expectency_by_birth,log(GDP_Per_Capita)))+
  geom_point(aes(col = Year),size = 2)+
  labs(title = "United States")+
  theme(legend.position = "none")

grid.arrange(p1, p2, p3, p4, p5, p6, p7, nrow = 4)

#PLOT FOR QUESTION-1
# Is the relationship between GDP per capita and infant mortality between countries stable over time
all_country_plot <- ggplot(data = data, aes(GDP_Per_Capita,Life_Expectency_by_birth))+
  geom_point(color = "#0000CD",size = 2,alpha = 0.5)+
  xlab("GDP PER CAPITA (Current US$)")+
  ylab("Life Expectancy at Birth")+
  ggtitle("GDP per Capita VS Life Expectancy 2000-2017")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),axis.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(face = "bold"), axis.text.y = element_text(face = "bold"))
all_country_plot

# data %>% filter(Year == 2000) %>%ggplot(aes(Life_Expectency_by_birth,log(GDP_Per_Capita)))+
#  geom_point(aes(col = Country),size = 2)+
#  facet_wrap(~Year, ncol=1) +
#  theme(legend.position = "none")

# library(plm)
#
# new_data <- data
# new_data$Year <- as.Date(paste0(new_data$Year, "-01-01"))
# pdata <- pdata.frame(new_data, index = c("Country", "Year"))
# head(pdata)
# fe_model <- plm(Life_Expectency_by_birth ~ GDP_Per_Capita, data = pdata, model = "within")
# summary(fe_model)
#
#
# library(lme4)
#
# # Rescale predictor variables
# new_data$Year_scaled <- scale(new_data$Year)
# new_data$GDP_Per_Capita_scaled <- scale(new_data$GDP_Per_Capita)
# new_data$Life_Expectency_by_birth_scaled <- scale(new_data$Life_Expectency_by_birth)
# # Fit linear mixed effects model
# model <- lmer(Life_Expectency_by_birth ~ GDP_Per_Capita_scaled + (1|Country), data = new_data)
#
# # View summary of the model
# summary(model)
#
# boxplot(new_data)


#FOR QUESTION-2
#Is the relationship between countries (cross-sectional) the same as within a single country over time (longitudinal)

#lOAD UK DATA
gbp_data <- data %>% filter(Code == "GBR")

#COMPARE USING VISUALZING THE UK AND WORLD DATA
gbp_plot <- gbp_data %>% ggplot(aes(GDP_Per_Capita,Life_Expectency_by_birth,col = as.factor(Year)))+
  geom_point(size = 3)+
  xlab("GDP PER CAPITA (Current US$)")+
  ylab("Life Expectancy by Birth")+
  ggtitle("United Kingdom 2000-2017")+
  labs(color = "Years")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),axis.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(face = "bold"), axis.text.y = element_text(face = "bold"))
  #scale_color_gradient(low = "blue", high = "red")
grid.arrange(all_country_plot,gbp_plot, nrow = 1)

#SOME EXTRA STUFF......
gbp_data %>% ggplot(aes(Life_Expectency_by_birth,log(GDP_Per_Capita)))+
  geom_point(aes(col = Year),size = 2)+
  theme(legend.position = "none")

gbp_data %>% ggplot(aes(log(GDP_Per_Capita),Year))+
  geom_point(aes(col = Year),size = 2)+
  theme(legend.position = "none")

#FOR QUESTIONS 3
#Is the relationship different in different countries. Can you spot times when it breaks down, and relate this to historical events

#lOAD DATA WITHOUT THE YEARS WITH FINANCIAL CRISIS AND PLOT
all_country_plot_non_crisis <- data %>%
  filter(Year != 1997 | Year != 1998 | Year != 2007 | Year != 2008 | Year != 2009) %>%
  ggplot(aes(Life_Expectency_by_birth,log(GDP_Per_Capita)))+
  geom_point(aes(col = Country),size = 2,alpha = 0.5)+
  theme(legend.position = "none")
all_country_plot_non_crisis

data %>% ggplot(aes(Life_Expectency_by_birth,log(GDP_Per_Capita)))+
 geom_point(aes(col = Country),size = 2)+
 facet_wrap(~Region, ncol=1) +
 theme(legend.position = "none")

data %>% ggplot(aes(Life_Expectency_by_birth,log(GDP_Per_Capita)))+
  geom_point(aes(col = Country),size = 2)+
  facet_wrap(~Region, ncol=1) +
  theme(legend.position = "none")

####################################
#TRENDS IN MIDDLE EAST & NORTH AFRICA

# data %>% filter(Region == "Middle East & North Africa") %>% ggplot(aes(log(GDP_Per_Capita),Life_Expectency_by_birth))+
#   geom_point(aes(col = Country),size = 2)+
#   facet_wrap(~Region, ncol=1)
#
# data %>% filter(Region == "Middle East & North Africa") %>% ggplot(aes(Year,Life_Expectency_by_birth))+
#   geom_point(aes(col = Country),size = 2)+
#   facet_wrap(~Region, ncol=1)

# Plot relationship between gdp_per_capita and life_expectancy over different periods of time
# YearRange_1 <- data %>% filter(Year %in% 2000:2008) %>%ggplot(aes(x = log(GDP_Per_Capita), y = Life_Expectency_by_birth, color = as.factor(Year))) +
#   geom_line()+
#   scale_color_brewer(type = "qual", palette = "Set1") +
#   labs(title = "Relationship between GDP per capita and Life Expectancy over Time",
#        x = "GDP per capita",
#        y = "Life Expectancy",
#        color = "Year")
#
# YearRange_2 <- data %>% filter(Year %in% 2009:2015) %>%ggplot(aes(x = log(GDP_Per_Capita), y = Life_Expectency_by_birth, color = as.factor(Year))) +
#   geom_line()+
#   scale_color_brewer(type = "qual", palette = "Set1") +
#   labs(title = "Relationship between GDP per capita and Life Expectancy over Time",
#        x = "GDP per capita",
#        y = "Life Expectancy",
#        color = "Year")
#
# grid.arrange(YearRange_1, YearRange_2, nrow = 2)

# data %>% filter(Year %in% 2000:2005 & Region == "Middle East & North Africa") %>%ggplot(aes(x = log(GDP_Per_Capita), y = Life_Expectency_by_birth, shape = as.factor(Year), color = Country)) +
#   geom_point() +
#   labs(title = "Relationship between GDP per capita and Life Expectancy over Time",
#        x = "GDP per capita",
#        y = "Life Expectancy",
#        color = "Country",
#        shape = "Year")

c1 <- data %>% filter(Year %in% 2000:2017 & Region == "Middle East & North Africa") %>%
  group_by(Year) %>% arrange(desc(GDP_Per_Capita),.by_group = TRUE) %>%
  top_n(7,GDP_Per_Capita) %>%
  ggplot(aes(x = Year, y = Life_Expectency_by_birth, color = Country)) +
  geom_line(lwd = 1.5) +
  labs(title = "Relationship between GDP per capita Years",
       x = "Year",
       y = "Life Expectancy",
       color = "Country")+
  scale_color_brewer(type = "qual", palette = "Paired")

c2 <- data %>% filter(Year %in% 2000:2017 & Region == "Middle East & North Africa") %>%
  group_by(Year) %>% arrange(desc(GDP_Per_Capita),.by_group = TRUE) %>%
  top_n(7,GDP_Per_Capita) %>%
  ggplot(aes(x = Year, y = GDP_Per_Capita, color = Country)) +
  geom_line(lwd = 1.5) +
  labs(title = "Relationship between GDP per capita and Years",
       x = "Year",
       y = "GDP per capita",
       color = "Country")+
  scale_color_brewer(type = "qual", palette = "Paired")

grid.arrange(c1, c2, nrow = 2)

####################################
#TRENDS IN EUROPE AND CENTRAL ASIA
c1 <- data %>% filter(Year %in% 2000:2020 & Region == "Europe & Central Asia") %>%
  group_by(Year) %>% arrange(desc(GDP_Per_Capita),.by_group = TRUE) %>%
  top_n(7,GDP_Per_Capita) %>%
  ggplot(aes(x = Year, y = Life_Expectency_by_birth, color = Country)) +
  geom_line(lwd = 1.5) +
  labs(title = "Relationship between GDP per capita Years",
       x = "Year",
       y = "Life Expectancy",
       color = "Country")

c2 <- data %>% filter(Year %in% 2000:2020 & Region == "Europe & Central Asia") %>%
  group_by(Year) %>% arrange(desc(GDP_Per_Capita),.by_group = TRUE) %>%
  top_n(7,GDP_Per_Capita) %>%
  ggplot(aes(x = Year, y = GDP_Per_Capita, color = Country)) +
  geom_line(lwd = 1.5) +
  labs(title = "Relationship between GDP per capita and Years",
       x = "Year",
       y = "GDP per capita",
       color = "Country")

grid.arrange(c1, c2, nrow = 2)

####################################
#TRENDS IN Sub_Saharan_Africa
c1 <- data %>% filter(Year %in% 2000:2020 & Region == "Sub-Saharan Africa") %>%
  group_by(Year) %>% arrange(GDP_Per_Capita,.by_group = TRUE) %>%
  top_n(7,GDP_Per_Capita) %>%
  ggplot(aes(x = Year, y = Life_Expectency_by_birth, color = Country)) +
  geom_line(lwd = 1.5) +
  labs(title = "Relationship between GDP per capita Years",
       x = "Year",
       y = "Life Expectancy",
       color = "Country")+
  scale_color_brewer(type = "qual", palette = "Paired")

c2 <- data %>% filter(Year %in% 2000:2020 & Region == "Sub-Saharan Africa") %>%
  group_by(Year) %>% arrange(GDP_Per_Capita,.by_group = TRUE) %>%
  top_n(7,GDP_Per_Capita) %>%
  ggplot(aes(x = Year, y = GDP_Per_Capita, color = Country)) +
  geom_line(lwd = 1.5) +
  labs(title = "Relationship between GDP per capita and Years",
       x = "Year",
       y = "GDP per capita",
       color = "Country")+
  scale_color_brewer(type = "qual", palette = "Paired")

grid.arrange(c1, c2, nrow = 2)

data %>%
  filter(Year %in% 2000:2020 & Country=="Liberia" | Country=="Sierra Leone"|Country=="Guinea") %>%
  group_by(Year) %>% arrange(GDP_Per_Capita,.by_group = TRUE) %>%
  top_n(7,GDP_Per_Capita) %>%
  ggplot(aes(x = Year, y = GDP_Per_Capita, color = Country)) +
  geom_line(lwd = 1.5) +
  labs(title = "Relationship between GDP per capita and Years",
       x = "Year",
       y = "GDP per capita",
       color = "Country")+
  scale_color_brewer(type = "qual", palette = "Paired")

data %>%
  filter(Year %in% 2000:2020 & Country=="Liberia" | Country=="Sierra Leone"|Country=="Guinea") %>%
  group_by(Year) %>% arrange(GDP_Per_Capita,.by_group = TRUE) %>%
  top_n(7,GDP_Per_Capita) %>%
  ggplot(aes(x = Year, y = Life_Expectency_by_birth, color = Country)) +
  geom_line(lwd = 1.5) +
  labs(title = "Relationship between GDP per capita and Years",
       x = "Year",
       y = "GDP per capita",
       color = "Country")+
  scale_color_brewer(type = "qual", palette = "Paired")

####################################
#TRENDS IN EAST ASIA PACIFIC
c1 <- data %>% filter(Year %in% 2000:2020 & Region == "East Asia & Pacific") %>%
  group_by(Year) %>% arrange(desc(GDP_Per_Capita),.by_group = TRUE) %>%
  top_n(7,GDP_Per_Capita) %>%
  ggplot(aes(x = Year, y = Life_Expectency_by_birth, color = Country)) +
  geom_line(lwd = 1.5) +
  labs(title = "Relationship between GDP per capita Years",
       x = "Year",
       y = "Life Expectancy",
       color = "Country")+
  scale_color_brewer(type = "qual", palette = "Paired")

c2 <- data %>% filter(Year %in% 2000:2020 & Region == "East Asia & Pacific") %>%
  group_by(Year) %>% arrange(desc(GDP_Per_Capita),.by_group = TRUE) %>%
  top_n(7,GDP_Per_Capita) %>%
  ggplot(aes(x = Year, y = GDP_Per_Capita, color = Country)) +
  geom_line(lwd = 1.5) +
  labs(title = "Relationship between GDP per capita and Years",
       x = "Year",
       y = "GDP per capita",
       color = "Country")+
  scale_color_brewer(type = "qual", palette = "Paired")

grid.arrange(c1, c2, nrow = 2)


####################################
#TRENDS IN EAST SOUTH ASIA
c1 <- data %>% filter(Year %in% 2000:2020 & Region == "South Asia") %>%
  group_by(Year) %>% arrange(desc(GDP_Per_Capita),.by_group = TRUE) %>%
  top_n(7,GDP_Per_Capita) %>%
  ggplot(aes(x = Year, y = Life_Expectency_by_birth, color = Country)) +
  geom_line(lwd = 1.5) +
  labs(title = "Relationship between GDP per capita Years",
       x = "Year",
       y = "Life Expectancy",
       color = "Country")+
  scale_color_brewer(type = "qual", palette = "Paired")

c2 <- data %>% filter(Year %in% 2000:2020 & Region == "South Asia") %>%
  group_by(Year) %>% arrange(desc(GDP_Per_Capita),.by_group = TRUE) %>%
  top_n(7,GDP_Per_Capita) %>%
  ggplot(aes(x = Year, y = GDP_Per_Capita, color = Country)) +
  geom_line(lwd = 1.5) +
  labs(title = "Relationship between GDP per capita and Years",
       x = "Year",
       y = "GDP per capita",
       color = "Country")+
  scale_color_brewer(type = "qual", palette = "Paired")

grid.arrange(c1, c2, nrow = 2)

####################################
#TRENDS IN LATIN AMERICA AND CARIBBEAN
c1 <- data %>% filter(Year %in% 2000:2020 & Region == "Latin America & Caribbean") %>%
  group_by(Year) %>% arrange(desc(GDP_Per_Capita),.by_group = TRUE) %>%
  top_n(6,GDP_Per_Capita) %>%
  ggplot(aes(x = Year, y = Life_Expectency_by_birth, color = Country)) +
  geom_line(lwd = 1.5) +
  labs(title = "Relationship between GDP per capita Years",
       x = "Year",
       y = "Life Expectancy",
       color = "Country")+
  scale_color_brewer(type = "qual", palette = "Paired")

c2 <- data %>% filter(Year %in% 2000:2020 & Region == "Latin America & Caribbean") %>%
  group_by(Year) %>% arrange(desc(GDP_Per_Capita),.by_group = TRUE) %>%
  top_n(6,GDP_Per_Capita) %>%
  ggplot(aes(x = Year, y = GDP_Per_Capita, color = Country)) +
  geom_line(lwd = 1.5) +
  labs(title = "Relationship between GDP per capita and Years",
       x = "Year",
       y = "GDP per capita",
       color = "Country")+
  scale_color_brewer(type = "qual", palette = "Paired")

grid.arrange(c1, c2, nrow = 2)

data %>% filter(Year %in% 2000:2020 & Country == 'China')

####################################
#TRENDS IN NORTH AMERICA
c1 <- data %>% filter(Year %in% 2000:2020 & Region == "North America") %>%
  group_by(Year) %>% arrange(desc(GDP_Per_Capita),.by_group = TRUE) %>%
  top_n(6,GDP_Per_Capita) %>%
  ggplot(aes(x = Year, y = Life_Expectency_by_birth, color = Country)) +
  geom_line(lwd = 1.5) +
  labs(title = "Relationship between GDP per capita Years",
       x = "Year",
       y = "Life Expectancy",
       color = "Country")+
  scale_color_brewer(type = "qual", palette = "Paired")

c2 <- data %>% filter(Year %in% 2000:2020 & Region == "North America") %>%
  group_by(Year) %>% arrange(desc(GDP_Per_Capita),.by_group = TRUE) %>%
  top_n(6,GDP_Per_Capita) %>%
  ggplot(aes(x = Year, y = GDP_Per_Capita, color = Country)) +
  geom_line(lwd = 1.5) +
  labs(title = "Relationship between GDP per capita and Years",
       x = "Year",
       y = "GDP per capita",
       color = "Country")+
  scale_color_brewer(type = "qual", palette = "Paired")

grid.arrange(c1, c2, nrow = 2)

####################################
#TRENDS IN MIDDLE EAST AND NORTH AFRICA
c1 <- data %>% filter(Year %in% 2000:2020 & Region == "Middle East & North Africa") %>%
  group_by(Year) %>% arrange(desc(GDP_Per_Capita),.by_group = TRUE) %>%
  top_n(6,GDP_Per_Capita) %>%
  ggplot(aes(x = Year, y = Life_Expectency_by_birth, color = Country)) +
  geom_line(lwd = 1.5) +
  labs(title = "Relationship between GDP per capita Years",
       x = "Year",
       y = "Life Expectancy",
       color = "Country")+
  scale_color_brewer(type = "qual", palette = "Paired")

c2 <- data %>% filter(Year %in% 2000:2020 & Region == "Middle East & North Africa") %>%
  group_by(Year) %>% arrange(desc(GDP_Per_Capita),.by_group = TRUE) %>%
  top_n(6,GDP_Per_Capita) %>%
  ggplot(aes(x = Year, y = GDP_Per_Capita, color = Country)) +
  geom_line(lwd = 1.5) +
  labs(title = "Relationship between GDP per capita and Years",
       x = "Year",
       y = "GDP per capita",
       color = "Country")+
  scale_color_brewer(type = "qual", palette = "Paired")

grid.arrange(c1, c2, nrow = 2)

data %>% filter(Year %in% 2000:2020 & Country=="Liberia" | Country=="Sierra Leone"|Country=="Guinea") %>%
  group_by(Year) %>% arrange(desc(GDP_Per_Capita),.by_group = TRUE) %>%
  top_n(6,GDP_Per_Capita) %>%
  ggplot(aes(x = Year, y = Life_Expectency_by_birth, color = Country)) +
  geom_line(lwd = 1.5) +
  labs(title = "Relationship between GDP per capita and Years",
       x = "Year",
       y = "GDP per capita",
       color = "Country")+
  scale_color_brewer(type = "qual", palette = "Paired")

#############
library(ggthemes)
library(directlabels)
c1 <- data %>% filter(Country == "Syrian Arab Republic" | Country == "Venezuela, RB"
                        | Country == "Ukraine" | Country == "Yemen, Rep."
                        | Country == "Libya"| Country == "United States"
                        | Country == "India"| Country == "Japan"
                        | Country == "China"| Country == "United Kingdom"
                        | Country == "Lesotho"
                        | Country == "Korea, Rep."
                        | Country == "Brazil"| Country == "South Africa"
                        | Country == "Mexico") %>%
  group_by(Year) %>% arrange(desc(GDP_Per_Capita),.by_group = TRUE) %>%
  top_n(16,GDP_Per_Capita) %>%
  ggplot(aes(x = Year, y = Life_Expectency_by_birth, color = Country)) +
  geom_line() +
  geom_dl(aes(label = Country), method = list(dl.combine("last.points")), cex = 0.8)+
  coord_cartesian(clip = 'off') +
  labs(title = "Life Expectency over years",
       x = "Year",
       y = "Life Expectancy",
       color = "Country")+
  geom_rect(data = data,aes(xmin = 2010.5, xmax = 2012),
            ymin = -Inf, ymax = Inf, fill = '#fffd8d',
            alpha = 0.02,color = NA) +
  theme(legend.position = 'none',
        plot.margin = margin(0.1, 2.6, 0.1, 0.1, "cm"),
        plot.title = element_text(hjust = 0.5))

c2 <- data %>% filter(Country == "Syrian Arab Republic" | Country == "Venezuela, RB"
                        | Country == "Ukraine" | Country == "Yemen, Rep."
                        | Country == "Libya"| Country == "United States"
                        | Country == "India"| Country == "Japan"
                        | Country == "China"| Country == "United Kingdom"
                        | Country == "Lesotho"
                        | Country == "Korea, Rep."
                        | Country == "Brazil"| Country == "South Africa"
                        | Country == "Mexico") %>%
  group_by(Year) %>% arrange(desc(GDP_Per_Capita),.by_group = TRUE) %>%
  top_n(16,GDP_Per_Capita) %>%
  ggplot(aes(x = Year, y = GDP_Per_Capita, color = Country)) +
  geom_line() +
  scale_colour_discrete(guide = 'none') +
  scale_x_discrete(expand=c(0, 1)) +
  geom_dl(aes(label = Country), method = list(dl.combine("last.points")), cex = 0.8)+
  coord_cartesian(clip = 'off') +
  labs(title = "GDP per Capita over years",
       x = "Year",
       y = "GDP per capita",
       color = "Country")+
  geom_rect(data = data,aes(xmin = 2010.5, xmax = 2012),
            ymin = -Inf, ymax = Inf, fill = '#fffd8d',
            alpha = 0.02,color = NA) +
  theme(legend.position = 'none',
        plot.margin = margin(0.1, 2.6, 0.1, 0.1, "cm"),
        plot.title = element_text(hjust = 0.5))

grid.arrange(c1, c2, nrow = 2)
?geom_rect
ggsave("q3_plot",last_plot(),device = "png")

data %>% filter(Year %in% 2000:2020 & Country=="Liberia" | Country=="Sierra Leone"|Country=="Guinea")
max(data['Year'])
#QUESTION-5
#How can you assess causality? Does wealth cause health

#WEALTH CAUSES HEALTH INDICATORS :

#1. GDP PER CAPITA - GDP per capita is gross domestic product divided by midyear population.
# GDP is the sum of gross value added by all resident producers in the economy plus any product taxes and minus any subsidies not included in the value of the products.
# It is calculated without making deductions for depreciation of fabricated assets or for depletion and degradation of natural resources.
# Data are in current U.S. dollars.

#2. GNI - GNI (formerly GNP) is the sum of value added by all resident producers plus any product taxes (less subsidies) not included in the valuation of output plus net receipts of primary income (compensation of employees and property income) from abroad.
# Data are in current U.S. dollars.

#3. Inflation, consumer prices(annual %) - Inflation as measured by the consumer price index reflects the annual percentage change in the cost to the average consumer of acquiring a basket of goods and services that may be fixed or changed at specified intervals, such as yearly.
# The Laspeyres formula is generally used.

#Read new file
new_data <- read.csv("Econmical_Factors_V1.csv")

#V1_FILE
colnames(new_data) <- c("Country","Code","Region","Year","GDP_Per_Capita","Life_Expectency_by_birth","Inflation_percent","GNI","HDI")

#Handle NA and select data between 2000-2017
new_data <- new_data %>% drop_na()
new_data <- new_data %>% filter(Year %in% 2000:2017)

# Normalize GDP per capita
new_data$GDP_per_capita_norm <- (new_data$GDP_Per_Capita - min(new_data$GDP_Per_Capita)) / (max(new_data$GDP_Per_Capita) - min(new_data$GDP_Per_Capita))

# Normalize GNI
new_data$GNI_norm <- (new_data$GNI - min(new_data$GNI)) / (max(new_data$GNI) - min(new_data$GNI))

# Normalize inflation percentage with negative values
new_data$inflation_percent_norm <- (new_data$Inflation_percent + abs(min(new_data$Inflation_percent))) / (max(new_data$Inflation_percent) + abs(min(new_data$Inflation_percent)))
new_data$inflation_percent_norm <- (new_data$Inflation_percent - min(new_data$Inflation_percent)) / (max(new_data$Inflation_percent) - min(new_data$Inflation_percent))

# Normalize GDP per capita
data$GDP_per_capita_norm <- (data$GDP_Per_Capita - min(data$GDP_Per_Capita)) / (max(data$GDP_Per_Capita) - min(data$GDP_Per_Capita))

# Normalize GNI
data$GNI_norm <- (data$GNI - min(data$GNI)) / (max(data$GNI) - min(data$GNI))

# Normalize inflation percentage with negative values
data$inflation_percent_norm <- (data$Inflation_percent + abs(min(data$Inflation_percent))) / (max(data$Inflation_percent) + abs(min(data$Inflation_percent)))
data$inflation_percent_norm <- (data$Inflation_percent - min(data$Inflation_percent)) / (max(data$Inflation_percent) - min(data$Inflation_percent))

#OUTLIER DETECTION AND FIXING IT
# boxplot(data[9:11])
# #Function to calculate count of outliers
# outliers <- lapply(data[9:11], function(x) {
#   bp <- boxplot.stats(x)
#   bp$out
# })

#V1 DATA FILE
boxplot(data[9:12])
#Function to calculate count of outliers
outliers <- lapply(data[9:11], function(x) {
  bp <- boxplot.stats(x)
  bp$out
})

# Count number of outliers for each column
sapply(outliers, length)
# Replace outlier values with mean of non-outlier values
#REPLACING IS WRONG AS THE DATA IS NOT WRONG/MISTYPED IT IS ACCURATE OBSERVERED VALUE
# new_data <- data[9:11]
# boxplot(new_data)
# for (i in seq_along(new_data)) {
#   col <- new_data[, i]
#   outliers_i <- outliers[[i]]
#   if (length(outliers_i) > 0) {
#     col[col %in% outliers_i] <- mean(col[!col %in% outliers_i])
#     new_data[, i] <- col
#   }
# }
boxplot(new_data)
#Function to calculate count of outliers
outliers <- lapply(new_data, function(x) {
  bp <- boxplot.stats(x)
  bp$out
})
# Count number of outliers for each column
sapply(outliers, length)

new_data <- data[9:12]
new_data <- cbind.data.frame(new_data,Life_Expectency_by_birth = data$Life_Expectency_by_birth)


#check correlation between data
corrgram(new_data,upper.panel=panel.cor, main="Econmical Factors")
#install.packages("psych")
library(psych)

data <- data[,c(1,2,3,4,5,7,8,6,9,10,11,12)]

pairs.panels(new_data,
             smooth = TRUE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = TRUE,    # If TRUE, draws ellipses
             method = "pearson", # Correlation method (also "spearman" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals

eigen(cor(new_data[,1:4]))

#linear_model <- glm(Life_Expectency_by_birth ~ GDP_per_capita_norm + GNI_norm + inflation_percent_norm,data = new_data,family = gaussian)

linear_model_HDI <- lm(Life_Expectency_by_birth ~ GDP_per_capita_norm + GNI_norm + inflation_percent_norm + HDI,data = new_data)
linear_model <- lm(Life_Expectency_by_birth ~ GDP_per_capita_norm + GNI_norm + inflation_percent_norm,data = data)


#linear_model_1 <- lm(Life_Expectency_by_birth ~ GDP_per_capita_norm + GNI_norm + inflation_percent_norm,data = data)

summary(linear_model)
summary(linear_model_HDI)

cor(data$HDI,data$Life_Expectency_by_birth)

vif(linear_model)

?vif

library(lmtest)
grangertest(Life_Expectency_by_birth ~ GDP_per_capita_norm, order = 3, data = data)


# Plot Model 1
plot1 <- ggplot(data, aes(x = GDP_per_capita_norm, y = Life_Expectency_by_birth)) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "blue") +
  labs(title = "Model 1: Life Expectancy vs. GDP per Capita and HDI", x = "GDP per Capita", y = "Life Expectancy") +
  geom_text(aes(x = 50000, y = 85, label = paste("R-squared: ", round(summary(linear_model)$r.squared, 2), "\n p-value: ", format.pval(summary(linear_model)$coefficients[2, 4], digits = 2, eps = 0.0001))), size = 4)

# Plot Model 2
plot2 <- ggplot(data, aes(x = GDP_per_capita_norm, y = Life_Expectency_by_birth)) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "green") +
  labs(title = "Model 2: Life Expectancy vs. GDP per Capita, HDI, and Additional Variable", x = "GDP per Capita", y = "Life Expectancy") +
  geom_text(aes(x = 50000, y = 85, label = paste("R-squared: ", round(summary(linear_model_HDI)$r.squared, 2), "\n p-value: ", format.pval(summary(linear_model_HDI)$coefficients[2, 4], digits = 2, eps = 0.0001))), size = 4)

# Arrange plots side by side
grid.arrange(plot1, plot2, nrow=2)


model_data <- data.frame(
  model = c("Without HDI", "With HDI", "Without External Factor", "With External factor"),
  r_squared = c(0.38, 0.8)
)

# Creating the plot
bar_1<-ggplot(model_data, aes(x = model, y = r_squared, fill = model)) +
  geom_col() +
  labs(
    title = "Comparison of Model with and without Additional Variable",
    y = "R-squared Value",
    x = "Model"
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 12, color = "black", angle = 0),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title.y = element_text(size = 14, color = "black"),
    axis.title.x = element_text(size = 14, color = "black"),
    plot.title = element_text(size = 18, color = "black", face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()
  )

model_data2 <- data.frame(
  model = c("With External Balance","Without External Balance"),
  r_squared = c(0.8, 0.7)
)

bar_2 <- # Creating the plot
  ggplot(model_data2, aes(x = model, y = r_squared, fill = model)) +
    geom_col() +
    labs(
      title = "Comparison of Model with and without Additional Variable",
      y = "R-squared Value",
      x = "Model"
    ) +
    theme_bw() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(size = 12, color = "black", angle = 0),
      axis.text.y = element_text(size = 12, color = "black"),
      axis.title.y = element_text(size = 14, color = "black"),
      axis.title.x = element_text(size = 14, color = "black"),
      plot.title = element_text(size = 18, color = "black", face = "bold"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank()
    )

grid.arrange(bar_1,bar_2,nrow = 2)