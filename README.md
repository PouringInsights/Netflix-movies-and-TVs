 # **Netflix-movies-and-TVs**
## 1. Exploratory and Visualization 

In the pandemic time, as a student, I spent so much time in watching movies/ TVs from Netflix. Because of that, I have an idea that what if I could analyze the data from Netflix to see how Netflix is doing. I wasnt sure that I could find the dataset, but I was lucky that this is a famous dataset in Kaggle. In that way, I could try my best to practice what I have learned with this. 

This dataset contains more than 8,500 Netflix movies and TV shows, including cast members, duration, and genre. It contains titles added as recently as late September 2021. 

### * *Calling the library* * 
First of all, I will call all the library that I could use in this dataset. 

<details><summary>library</summary>
<p>

```ruby
library(ggplot2)
library(tidyverse)
library(lubridate) 
library(dplyr)
library(tibble)  
library(purrr)
library(tidyr)
library(forcats)
```
</p>
</details>

<details><summary>import file</summary>
<p>
     
```{r} 
data <- readr::read_csv('D:/Giang/studying/project/Netflix/Netflix movies and TVs/netflix_titles.csv')
head(data,5)
```
</p>
</details>

## Data Dictionary

     |:--------------|:----------|:--------------------------------------------------------------|
     | type          | character | Either 'TV Show' or 'Movie'                                   |
     | title         | character | The title of the movie or TV show                             |
     | director      | character | The director of the movie or TV show                          |
     | cast          | character | The actors playing in the movie or TV show                    |
     | country       | character | The country in which the movie or TV show was directed        |
     | date_added    | character | The date on which the movie or TV show was added to Netflix   |
     | release_year  | character | The year the movie or TV show was released                    |
     | rating        | character | The kid-friendly rating the movie or TV show received         |
     | duration      | character | The length of the movie or TV show                            |
     | listed_in     | character | The genre of the movie or TV show                             |
     | description   | character | The description/short summary of the movie or TV show         | 
  
#  [Source of dataset](https://www.kaggle.com/shivamb/netflix-shows)

There is some quick summary of the dataset

```ruby
summary(data)
data%>%
  group_by(show_id)%>%
  count()%>%
  filter(n>1)
glimpse(data)
```
The following code is just about the theme and size that I want. it is more like the personal reference. 

<details><summary>theme and size</summary>
<p>

```ruby
fill_theme <- theme(axis.text.x = element_text(size = 16, color = "#1B4F72"),
           axis.text.y = element_text(size = 16, color = "#34495E"),
           axis.title.x = element_text(size = 16),
           axis.title.y = element_text(size = 16,color = "#34495E"))+
  theme(legend.key.size = unit(x = 2, units = 'line'),
        legend.text = element_text(size = 14, color = "#1B4F72"),
        legend.title = element_text(size = 14, color = "#34495E"))
```
```ruby
fig <- function(width, heigth){
  options(repr.plot.width = width, repr.plot.height = heigth)}
```
</p>
</details>

**Drop NA value**
```ruby
countries<-data%>%
  select(country, type, title, listed_in)
sum(is.na(countries$country))/nrow(countries)
countries<-countries%>%
  filter(!is.na(country))
  ```
  
## 2. Show types
The following part is the bar chart about the comparation between number of movies and TVs show in Netflix, the number of movies existed in Netlfix is double the number of TVs show

```ruby
countries %>%
  count(type) %>%
  ggplot() + geom_col(aes(x = type, y = n, fill = type)) +
  labs(title = "Show Types",
       subtitle = "Netflix Data",
       caption = 'Data Source: Kaggle') +
  theme_minimal()
```
![image](https://user-images.githubusercontent.com/100246099/155381141-3bfc234e-8ec7-4cc9-bb69-83c7b4637b8e.png)

## 3. Which countries have produced the most movies in Netflix
This part is about the origin countries of the movies and TV shows in Netflix
In the hidden part is about the preparation that extract a dataframe that include only countries name and the number of titles for each country 

<details><summary>preparation code</summary>
<p>

 ```ruby
### number title of each country
max(str_count(countries$country, ','))
#max = 11 ',' => maximum = 12 countries

### split the combined countries into single one
ctr<-countries%>%
  separate(country, into = c('a','b','c','d','e','f','g','h','i','j','k','l')
           ,", ", convert = TRUE)

ctr<-ctr[,1:12]

ctr_list<-ctr%>%
  unlist()

ctr_tibble<-tibble(country_name=ctr_list)
#Which country has the most movies
ctr<-ctr_tibble%>%
  group_by(country_name)%>%
  count()%>%
  filter(!is.na(country_name))
```
</p>
</details>

**And there is the code for chart**

<details><summary>Top countries</summary>
<p>
 
 ```ruby
fig(6,20)
ctr%>%
  filter(n>100 && country_name != '')%>%
  ggplot(aes(reorder(country_name, FUN=median, n),n, fill= n>800)) +
  geom_bar(stat='identity', show.legend = F) +
  labs(
    y="Numbers of movies on Netflix",
    x= "Country name",
    title="The outstanding number of movies in US and India") +coord_flip() +fill_theme
 ```
 </p>
</details>

![image](https://user-images.githubusercontent.com/100246099/155432345-8df9bd05-1cf2-43d6-9cd5-22697aa9efcb.png)

# To understand the categories from Netflix 

ctr<-ctr[-1,-2]
max(str_count(countries$listed_in, ','))

List_in<-countries%>%
  select(listed_in)%>%
  separate(listed_in, into = c('a','b','c'),", ", convert = TRUE)

List_in<-List_in%>%unlist()

list_in<-tibble(
  list_in=List_in
)

list_in%>%
  group_by(list_in)%>%
  count()%>%
  filter(!is.na(list_in) && n>=100)%>%
  ggplot(aes(reorder(list_in, fun=median, n),n, fill = n>1000))+ 
  geom_histogram(stat = 'identity', show.legend = F)+
  labs(
    y='Numbers of type in movies on Netflix',
    x='Types',
    title='Interational movies and the Dramas are the most movie types on Netflix') + coord_flip() + fill_theme


# show rating

colorset = c("#105738","#407442","#6e914c","#a1ad57",
              "#dac767","#ca9b43","#b77028","#a04417","#850b10")

```ruby
data %>%
  count(rating) %>%
  group_by(rating) %>%
  filter(n > 100) %>%
  ggplot(aes(rating, n, fill = rating))+ scale_fill_manual(values = colorset)+
  geom_bar(stat = 'identity') +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank())+
  scale_y_continuous(breaks = seq(from = 0, to = 3000, by = 200))+
  labs(x = '', y = '') + fill_theme
```
![image](https://user-images.githubusercontent.com/100246099/155435456-f32023b4-b8e0-45a0-90df-340bdcf00f47.png)

# To figure out how many shows have been added into Netflix, the data will be visualized quarterly
fig(17,20)
data$date_added <- as.Date(data$date_added, format = '%B %d, %Y') 
data %>%
  filter(date_added > '2015-01-01' & date_added < '2021-12-31') %>%
  mutate(date_added = as.Date(floor_date(date_added, unit = 'quarter'))) %>%
  count(date_added) %>%
  ggplot(aes(date_added, n))+
  geom_line(size = 1.3, alpha = 1, color = "#CD5C5C") +
  scale_x_date(breaks = '3 month', date_labels = '%b %y')+
  scale_y_continuous(breaks = seq(from = 0, to = 800, by = 100))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        panel.grid.major.x = element_blank())+
  labs(y = 'shows added per quarter', color = "#0B5345",x = '') + fill_theme

# Figure out the top TVs/ movies types in UK

list_in<-list_in%>%
  group_by(list_in)%>%
  filter(!is.na(list_in))%>%
  count()

list_in<-list_in[,-2]

UK.movie<-countries[str_which(countries$country, 'United Kingdom'), ]
max(str_count(UK.movie$listed_in, ','))

UK.movie<-UK.movie%>%
  separate(
    listed_in, into= c('type1', 'type2', 'type3'), ', ', convert = T)


UK.list<-UK.movie%>%
  select(type1, type2, type3)%>%
  unlist()

UK.list<-tibble(
  type = UK.list,
)

UK.list%>%
  group_by(type)%>%
  count()%>%
  ungroup()%>%
  filter( !is.na(type))%>%
  mutate(proportion = n / sum(n))%>%
  filter( rank(n) >= 28 )%>% #choose top 10 types
  ggplot(aes('' , n ,fill=type))+
  geom_histogram( position = 'stack', stat = 'identity', color='white', show.legend = F)+
  geom_text(aes(label = paste(type,'\n',round(proportion,2))), 
            position = position_stack(vjust = 0.5), size=2.8)+
  coord_polar('y', start = 0)+
  theme_bw()+
  labs(
    x='',
    y='',
    title='NetFlix: Top 10 movie types in UK',
    subtitle='Dramas amd Comedies are the most types of movies in UK'
  )

