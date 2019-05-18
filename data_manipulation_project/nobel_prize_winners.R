# Loading in required libraries
install.packages("tidyverse")
library(tidyverse)

# Reading in the Nobel Prize data
nobel <- read_csv("nobel.csv")

# Taking a look at the first couple of winners
head(nobel)

# Counting the number of (possibly shared) Nobel Prizes handed
# out between 1901 and 2016
nobel %>% count

# Counting the number of prizes won by male and female recipients.
nobel %>% group_by(Sex) %>% count

# Counting the number of prizes won by different nationalities.
names(nobel)<-str_replace_all(names(nobel), c(" " = "_"))
nobel %>% group_by(Birth_Country) %>% count %>% arrange(desc(n)) %>% head(20)

# Calculating the proportion of USA born winners per decade
nobel$USA_Born_Winner <- nobel$Birth_Country == "United States of America"
nobel$Decade <- nobel$Year - (nobel$Year %% 10)
prop_usa_winners <- nobel %>% group_by(Decade) %>% summarize(Proportion = mean(USA_Born_Winner, na.rm = TRUE))

# Display the proportions of USA born winners per decade
prop_usa_winners

# Plotting USA born winners
ggplot(data = prop_usa_winners, aes(x = Decade, y = Proportion)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1), expand = waiver())

# Calculating the proportion of female laureates per decade
nobel$Female_Winner <- nobel$Sex == "Female"
prop_female_winners <- nobel %>% group_by(Decade, Category) %>% summarize(Prop.Female = mean(Female_Winner, na.rm = TRUE))

# Plotting the proportion of female laureates per decade
ggplot(data = prop_female_winners, aes(x = Decade, y = Prop.Female, color = Category)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1), expand = waiver())

# Picking out the first woman to win a Nobel Prize
nobel %>% filter(Sex == "Female") %>% top_n(1, desc(Year))

# Selecting the laureates that have received 2 or more prizes.
nobel %>% group_by(Full_Name) %>% count %>% filter(n>1) %>% arrange(desc(n))

# Loading the lubridate package
library(lubridate)

# Calculating the age of Nobel Prize winners
nobel_age <- mutate(nobel, Age = Year - year(as.Date(nobel$Birth_Date)))

# Plotting the age of Nobel Prize winners
ggplot(nobel_age, aes(x = Year, y = Age)) +
  geom_point() +
  geom_smooth()

# Same plot as above, but faceted by the category of the Nobel Prize
ggplot(nobel_age, aes(x = Year, y = Age)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_wrap(~ Category)

# The oldest winner of a Nobel Prize as of 2016
nobel %>% top_n(1, nobel_age$Age)

# The youngest winner of a Nobel Prize as of 2016
nobel %>% top_n(1, desc(nobel_age$Age))

# Exporting variables as csv files for vizualization from other tool.
write.csv(nobel, file = "nobelforviz.csv")
write.csv(prop_usa_winners, file = "prop_usa_winners.csv")
write.csv(prop_female_winners, file = "prop_female_winners.csv")
