library(readr)
library(magrittr)
library(dplyr)
library(ggplot2)
library(plotly)


# download data -----------------------------------------------------------


urlfile="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
confirmed_glob <- read_csv(url(urlfile))

urlfile="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
confirmed_us <- read_csv(url(urlfile)) %>% filter(code3==840)

urlfile="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
deaths_glob <- read_csv(url(urlfile))

urlfile="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
deaths_us <- read_csv(url(urlfile))

urlfile="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
recovered <- read_csv(url(urlfile))



# process data ------------------------------------------------------------

df_population <- 
  tribble(
    ~country, ~population, # population in million
    "US", 330.758784,
    "France", 65.255646,
    "Germany", 83.750665,
    "Italy", 60.472650,
    "Spain", 46.752556,
    "Sweden", 10.091340,
    "UK", 67.841324,
    "Austria", 9.021247
  )

countries_lst <- df_population$country

df_confirmed <- 
confirmed_glob %>% 
  rename(province=`Province/State`,country=`Country/Region` ) %>% 
  select(-Lat, -Long) %>%
  mutate(country=if_else(country=="United Kingdom","UK",country)) %>% 
  filter(country %in% countries_lst) %>% 
  filter(country != "US") %>% # exclude US to be taken from separate report
  filter(is.na(province)) %>% 
  select(-province) %>% 
  tidyr::gather(key="date",value="confirmed",-country) %>% 
  mutate(date=as.Date(date,"%m/%d/%y")) %>% 
  mutate(confirmed=as.numeric(confirmed)) %>% 
  group_by(country,date) %>% 
  summarise(confirmed=sum(confirmed)) %>% 
  ungroup()

df_confirmed <-
confirmed_us %>%
  select(-UID ,-iso2  ,-iso3  ,-code3 , -FIPS ,-Admin2, -Country_Region ,-Lat,-Long_, -Combined_Key) %>% 
  select(-Province_State) %>% 
  mutate(country="US") %>% 
  tidyr::gather(key="date",value="confirmed",-country) %>% 
  mutate(date=as.Date(date,"%m/%d/%y")) %>% 
  group_by(country,date) %>% 
  summarise(confirmed=sum(confirmed)) %>% # View()
  ungroup() %>% 
  bind_rows(df_confirmed)

df_deaths <- 
deaths_glob %>% 
  rename(province=`Province/State`,country=`Country/Region` ) %>% 
  select(-Lat, -Long) %>%
  mutate(country=if_else(country=="United Kingdom","UK",country)) %>%   filter(is.na(province)) %>% 
  filter(country %in% countries_lst) %>% 
  filter(country != "US") %>% # exclude US to be taken from separate report
  select(-province) %>% 
  tidyr::gather(key="date",value="deaths",-country) %>% 
  mutate(date=as.Date(date,"%m/%d/%y")) %>% 
  mutate(deaths=as.numeric(deaths)) %>% 
  group_by(country,date) %>% 
  summarise(deaths=sum(deaths)) %>% 
  ungroup()

df_deaths <- 
deaths_us %>% 
  select(-UID ,-iso2  ,-iso3  ,-code3 , -FIPS ,-Admin2, -Country_Region ,-Lat,-Long_, -Combined_Key) %>% 
  select(-Province_State) %>% 
  mutate(country="US") %>% 
  tidyr::gather(key="date",value="deaths",-country) %>% 
  mutate(date=as.Date(date,"%m/%d/%y")) %>% 
  group_by(country,date) %>% 
  summarise(deaths=sum(deaths)) %>% 
  ungroup() %>%
  bind_rows(df_deaths)


df_confirmed <- 
inner_join(
  df_population,
  df_confirmed,
  by="country"
) #%>% 
  # mutate(confirmed=confirmed/population)

df_deaths <- 
inner_join(
  df_deaths,
  df_confirmed,
  by=c("country","date")
)

# plot --------------------------------------------------------------------


gg <- 
df_confirmed %>% 
  ggplot(aes(x=date,y=confirmed,group=country, color=as.factor(country))) +
  geom_line()+
  scale_y_log10()

gg
ggplotly(gg)

gg <- 
  df_confirmed %>% 
  ggplot(aes(x=date,y=confirmed/population,group=country, color=as.factor(country))) +
  geom_line() +
  scale_x_date(breaks = "1 month")
# scale_y_log10()
gg
ggplotly(gg)

gg <- 
df_deaths %>% 
  ggplot(aes(x=date,y=deaths/confirmed,group=country, color=as.factor(country) )) +
  geom_line()+
  geom_hline(yintercept=852/181105)+ # influenza mortality rate
annotate(geom="text", label="influenza all cases", x=min(df_deaths$date), y=852/181105, vjust=-0.5,hjust = -0.0, size=3)+
  geom_hline(yintercept=852/39686) + # hospitalized mortality rate
  annotate(geom="text", label="influenza hospitalized", x=min(df_deaths$date), y=852/39686, vjust=-0.5,hjust = -0.0, size=3)
gg

ggplotly(gg)

gg <-
df_confirmed %>% 
  group_by(country) %>% 
  arrange(country,date) %>% 
  mutate(new_confirmed=confirmed-lag(confirmed)) %>% 
  mutate(new_confirmed=pmax(0,new_confirmed)) %>% 
  ggplot(aes(x=date,y=new_confirmed/population, color=as.factor(country))) +
  geom_line() #+geom_smooth(method="loess",se=F,span=0.2)
gg

ggplotly(gg)


