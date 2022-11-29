library(dplyr)
library(ggplot2)

# The functions might be useful for A4

data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

state_abre_to_name <- read.csv("https://raw.githubusercontent.com/jasonong/List-of-US-States/master/states.csv")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
data <- left_join(state_abre_to_name, data, by = c("Abbreviation" = "state")) #add a column with full state name

data_2018 <- filter(data, year == 2018)#filter data from only 2018
male_prisoner_prop_2018 <- summarize(
  data_2018,
  total_jail = sum(total_jail_pop,na.rm = TRUE), #calculate the proportion of male prisoner population out of total prisoner population
  total_male_prisoner = sum(male_jail_pop, na.rm = TRUE),
  prop_male_of_total_2018 = total_male_prisoner/total_jail
)
colnames(male_prisoner_prop_2018)[3] = "3"         #find the proportion in 2018
answer_1 <- pull(male_prisoner_prop_2018, "3")



data_1970 <- filter(data, year == 1970)
crime_rate_1970 <- summarize(
  data_1970,
  total_pop = sum(total_pop, na.rm = TRUE),
  total_jail_pop = sum(total_jail_pop, na.rm = TRUE), #filter data from only 1970 and calculate crime rate
  prop_crime_rate = total_jail_pop/total_pop
)
crimerate_1970 <- pull(crime_rate_1970, prop_crime_rate)
crime_rate_2018 <- summarize(
  data_2018,
  total_pop = sum(total_pop, na.rm = TRUE),
  total_jail_pop = sum(total_jail_pop, na.rm = TRUE),  #same thing but with data only from 2018
  prop_crime_rate = total_jail_pop/total_pop
)
crimerate_2018 <- pull(crime_rate_2018, prop_crime_rate)



jail_pop_state <- summarize(data%>%
      group_by(State),                                  #group by state and find the total jail pop
      total_pop <- sum(total_jail_pop, na.rm = TRUE),
)
colnames(jail_pop_state)[2] = "number"
biggest_jail_pop_state <- jail_pop_state%>%              #filter and find the state with max pop
filter(number == max(number))%>%
pull(State)

#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function ... <todo:  update comment>
get_year_jail_pop <- function() {

  data_by_year <- group_by(data, year)
  jail_pop_by_year <- summarize(
    data_by_year,
    total_jail_pop = sum(total_jail_pop, na.rm = TRUE),
  )  
return(jail_pop_by_year)   
}

# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function()  {
  plot_1 <- ggplot(data = jail_pop_by_year) +
    geom_col(mapping = aes(x = year, y = total_jail_pop))+
    ggtitle("Graph of Total Jail Population from 1970 to 2018.")
  return(plot_1)   
} 

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#








## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

male_female_prop <- summarize(data%>%
  group_by(year),
  total_male_jail_pop = sum(male_jail_pop, na.rm = TRUE),
  total_female_jail_pop = sum(female_jail_pop, na.rm = TRUE),
  prop_male_female = total_male_jail_pop/total_female_jail_pop
)


 ggplot(data = male_female_prop) +
  geom_col(mapping = aes(x = year, y = prop_male_female))+
  ggtitle("Graph of ratio of male prisoner to female prisoner from 1970 to 2018.")



## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

jail_pop_state <- summarize(data%>%
group_by(State),                                  
total_pop = sum(total_jail_pop, na.rm = TRUE),
 )
 jail_pop_state$State = tolower(jail_pop_state$State)
state_shape <- map_data("state")%>%
rename(State = region) %>% 
left_join(jail_pop_state, by="State")

ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = total_pop),
    color = "white", 
    size = .5        
  ) +
  coord_map() + 
  scale_fill_continuous(low = "#132B43", high = "Red") +
  labs(fill = "Total Jail Population")+
  ggtitle("Map of Total Jail Population")
  
 
 
 
 
 
 
 
## Load data frame ---- 


