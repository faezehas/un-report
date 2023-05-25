library(tidyverse) 

#%>%  (cmd+shif+M) Pipe operator 

#----------------
#Read in data
#----------------
gapminder_data <- read_csv("Data/gapminder_data.csv")

#----------------
#what is the mean life expectancy
#----------------

summarize(gapminder_data, averageLifeExp= mean (lifeExp)) #typical function use

?summarize

gapminder_data %>% 
  summarise(averagelifeExp=mean(lifeExp))   # use %>%  (cmd+shif+M) Pipe operator 

gapminder_data_summarized <- gapminder_data %>% 
  summarise(averagelifeExp=mean(lifeExp))      # write output in a valiable


#what is the mean population 


summarize(gapminder_data, averageLifeExp= mean (pop))

gapminder_data %>% 
  summarize(averagepop=mean(pop))

gapreminder_data_summarized_pop <- gapminder_data %>% 
  summarize(averagepop=mean(pop))


#what is the mean population  and life Exp


gapminder_data %>% 
  summarize(averagepop=mean(pop),averagelifeExp = mean(lifeExp))

#----------------
#what is the mean life expectancy for the most recent year
#max()
#filter
#----------------

#max()
gapminder_data %>% 
  summarize(maxYear=max(year))

#Filter
# == ask if it is equal ( also we have < > !=) , for multiple input use & for example
#filter ( a == "dog" & a == "cat")
#filter ( id %in% c("id1",id2","id3")). 
#The %in% operator checks if the values in the "id" column are present in the provided set of values, in this case, "id1", "id2", and "id3".
#Rows that match any of these values are kept, while others are filtered out.

gapminder_data %>% 
  filter(year == 2007) %>% 
  summarize (meanlifeExp = mean(lifeExp))


gapminder_data %>% 
  filter(year == max(year)) %>% 
  summarize (meanlifeExp = mean(lifeExp))


#what is the mean GDP per capital for teh first/earliest year


gapminder_data %>% 
  filter(year == min(year)) %>% 
  summarize (meanlifeExp = mean(gdpPercap))


#----------------
#what is the mean lifeExp for each year
#group_by()
#----------------

gapminder_data %>% 
  group_by(year) %>%  #name of the colume
  summarise(meanlifeExp=mean(lifeExp))
  

#what is the mean life expectancy for the each continent


gapminder_data %>% 
  group_by(continent) %>%  #name of the colume
  summarise(meanlifeExp=mean(lifeExp))


#what is the mean life expectancy and mean GPD per capital for the each continent in a single result tibble

gapminder_data %>% 
  group_by(continent) %>%  #name of the colume
  summarise(meanlifeExp=mean(lifeExp), meangdp=mean(gdpPercap))


#-------------------
#what is the GPD ( not per capital)
#mutate() add new column to our data
#-------------------

gapminder_data %>% 
  mutate(gdp = gdpPercap * pop)


#make a new column for population in million

gapminder_data %>% 
  mutate (popm = pop/1000000 )

# both together
#overwrite
gapminder_data <- gapminder_data %>% 
  mutate (popm = pop/1000000 , gdp = gdpPercap * pop )
#othervariable
PopGdp <- gapminder_data %>% 
  mutate (popm = pop/1000000 , gdp = gdpPercap * pop )


#--------------------------------------
# my practice 
#Add an index column
gapminder_data <- gapminder_data %>%
  mutate(index = 1:n())
#remove the column
gapminder_data$index <- NULL


gapminder_data %>% 
  group_by(group = ceiling(row_number() / 5)) %>% 
  mutate(mean_value = mean(pop))

#get mean in this way and transfer data a to b like this:   mean a(1:5) --> b(1), mean a(2:6) --> b(2), ...

window_size <- 5

# Initialize an empty vector for storing the means
b <- numeric(length(gapminder_data) - window_size + 1)

# Calculate the means and transfer to vector b using a loop
for (i in 1:(length(a) - window_size + 1)) {
  b[i] <- mean(a[i:(i + window_size - 1)])
}
#not complete
#--------------------------------------


#------------------------
#select() chooses a subset of columns from a dataset
#------------------------

gapminder_data %>% 
  select (year, pop)

#select every column exepct continent
gapminder_data %>% 
  select(-continent)


#Create a tibble with only country, continent, year, lifeExp
gapminder_data %>% 
  select(-pop, -gdpPercap)
#or - better understand for others
gapminder_data %>% 
  select(country, continent,year,lifeExp)


#select help function: starts_with(), ends_with(), contains()
gapminder_data %>% 
  select(year,starts_with("c")) # select column with name start with C means country and continent

#------------------------
#Vector
#c()
#------------------------

my_vec <- c("dog","cat","horse","duck")
num_vec <- c(1,3,5,7,9,11,13)
view(num_vec)

#extract one column to a vector
proof <- gapminder_data %>% 
  pull(year)

gapminder_data %>% 
  select(contains("e"))


#------------------------
#reshape functions
#pivot_longer(). pivot_wider()
#------------------------


gapminder_data %>% 
  select(country, continent, year, lifeExp) %>% 
  pivot_wider(names_from = year , values_from = lifeExp)

#pivot_linger, but populate values with gdpPercap
gapminder_data %>% 
  select(country, continent, year, gdpPercap) %>% 
  #pivot_wider(names_from = pop, values_from = gdpPercap)
  #pivot_wider(names_from = gdpPercap, values_from = pop)
  pivot_wider(names_from = year, values_from = gdpPercap)


#pivot_longer
a<-gapminder_data %>% 
  pivot_longer(cols= c(pop,lifeExp,gdpPercap),
               names_to= "measurement_type",
               values_to = "measurement")
#my practice
a %>% 
  pivot_wider(names_from= "measurement_type",values_from = measurement )

#------------------------------------------------
#is there a relationship between GDP and CO2 emission
#gapminder_data_2007
#fillter for year 2007 and continent americas
#remove (using select) the year and continent columns


# & and ---  | or
gapminder_Data_2007 <- gapminder_data %>% 
  filter(year == 2007 & continent == "Americas") %>% # it can be & or ,
  select(-year,-continent)

#read in the CO2 data
co2_emission_dirty <- read_csv("Data/co2-un-data.csv", 
                skip=2,  # line 1 of file is some description and it is not column names
                col_names = c("region","country","year", "series", "value", "footnotes","source"))


co2_emission <- co2_emission_dirty %>% 
  select(country, year, series, value) %>% 
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emission",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capital_emission")) %>%  
  pivot_wider(names_from = series, values_from = value ) %>% 
  filter(year == 2005) %>% 
  select(-year)


#---------------------
#inner_join
#---------------------

inner_join(gapminder_Data_2007, co2_emission, by="country")


