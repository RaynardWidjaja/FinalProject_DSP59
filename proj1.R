library(tidyverse)
library(lubridate)

unicorn <- read_csv("final/unicorns.csv", show_col_types = FALSE) %>%
  select(-1) %>%
  mutate(`Valuation ($B)` = as.numeric(substr(`Valuation ($B)`, 2, nchar(`Valuation ($B)`)))) %>%
  rename(Valuation = `Valuation ($B)`) %>%
  mutate(`Date Joined` = as.Date(`Date Joined`, format = "%m/%d/%Y")) %>%
  mutate(year = year(`Date Joined`)) %>%
  mutate(month = month(`Date Joined`))
  
country <- read_csv("final/Country_Data.csv", show_col_types = FALSE)

num_country <- unicorn %>% 
  group_by(Country) %>% 
  summarize(num_unicorn = n(), valuation=sum(Valuation))

top_num_unicorn <- country %>%
  left_join(num_country, by=join_by(country==Country)) %>%
  arrange(desc(num_unicorn)) %>%
  slice(1:10)

uni_country <- unicorn %>%
  group_by(Country, Industry) %>%
  summarize(num_industry = n()) %>%
  left_join(country, by=join_by(Country==country)) %>%
  left_join(num_country, by=join_by(Country==Country)) %>%
  filter(Country %in% c("Brazil", "Canada", "China", "France", "Germany", "India", "Israel", "Singapore", "United Kingdom", "United States"))

# Total valuation of Unicorn per country (Top 10)
ggplot(data=top_num_unicorn) + 
  geom_col(aes(x=valuation, y=country)) +
  labs(x = "Total Valuation ($B)", y = "Country")

# Average valuation of Unicorn per country (Top 10)
ggplot(data=top_num_unicorn) + 
  geom_col(aes(x=valuation/num_unicorn, y=country)) +
  labs(x = "Average Valuation ($B)", y = "Country")

# Correlation of GDP and Number of Unicorn
ggplot(data=top_num_unicorn) + 
  geom_line(aes(x=gdpp, y=num_unicorn)) +
  geom_point(aes(x=gdpp, y=num_unicorn, color=country)) +
  labs(x = "GDP per Capita", y = "Number of Unicorn")

#
ggplot(data=uni_country) + 
  geom_point(aes(x=gdpp, y=num_industry/num_unicorn, color=Industry)) + 
  labs(x = "GDP per Capita", y = "Average Number of Unicorn")
