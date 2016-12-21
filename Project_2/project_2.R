# Team 7 Project 2

setwd("C:/Users/Andrew Pomykalski/Desktop/R/Project 2")
library(readr)
library(stringr)
library(dplyr)
library(scales)
library(ggplot2)

genres <- read_lines('genres.txt')
reviews <- read_lines('reviews.txt')
reviewers <- read_lines('reviewers.txt')
zipcodes <- read_lines('zipcodes.txt')

# Used the delim and delim2 functions to read each text file into their own data frame

df1 <- read.delim2('genres.txt', header=FALSE, sep = "|", quote = "", stringsAsFactors = FALSE)
df2 <- read.delim2('reviews.txt', header=FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
df3 <- read.delim2('reviewers.txt', header=FALSE, sep = "|", quote = "", stringsAsFactors = FALSE)
df4 <- read.delim2('zipcodes.txt', header=TRUE, sep = ",", quote = "\" \"", stringsAsFactors = FALSE, colClasses = "character")

colnames(df1) <- c("movie id", "movie title", "release date", "video release date", "IMDb URL",
                   "unknown",  "Action", "Adventure", "Animation" , "Children's",  "Comedy",
                   "Crime", "Documentary", "Drama",  "Fantasy", "Film-Noir" , "Horror", "Musical",
                   "Mystery",  "Romance", "Sci-Fi", "Thriller", "War",  "Western")
colnames(df2) <- c("reviewer id",  "movie id",  "rating",  "timestamp")
colnames(df3) <- c("reviewer id",  "age",  "gender",  "occupation",  "Zipcode")

df4$Zipcode <- as.character(df4$Zipcode)


# Question 1: 
# Merged two data fames by the ID to get a more robust data frame

movies <- merge(df1, df2, by="movie id")
modeling <- merge(movies, df3, by="reviewer id")

# Very tediously set variables to zero

action = 0
action_cum = 0
adventure = 0
adventure_cum = 0
animation = 0
animation_cum =0
child = 0
child_cum = 0
com = 0
com_cum = 0
crime = 0
crime_cum =0
doc = 0
doc_cum =0
drama = 0
drama_cum =0
fan = 0
fan_cum = 0
film = 0
film_cum =0
horror = 0
horror_cum =0
Western = 0
western_cum = 0
War = 0
war_cum = 0
Musical = 0
musical_cum = 0
Mystery = 0
mystery_cum =0
Romance = 0
romance_cum =0
Sci_fi = 0
sci_fi_cum = 0
Thriller = 0
thriller_cum = 0

# Created a for loop to pull out the sum of total genre and the sum of the ratings

for (i in 1:length(movies$`movie id`)){
  if (movies$Action[i] == 1){
    action = as.integer(movies$rating[i]) + action
    action_cum = action_cum + 1
  }
  if (movies$Animation[i] == 1){
    animation = as.integer(movies$rating[i]) + animation
    animation_cum = animation_cum + 1
  }
  if (movies$Adventure[i] == 1){
    adventure = as.integer(movies$rating[i]) + adventure
    adventure_cum = adventure_cum +1
  }
  if (movies$`Children's`[i] == 1){
    child = as.integer(movies$rating[i]) + child
    child_cum = child_cum+1
  }
  if (movies$Comedy[i] == 1){
    com = as.integer(movies$rating[i]) + com
    com_cum = com_cum+1
  }
  if (movies$Crime[i] == 1){
    crime = as.integer(movies$rating[i]) + crime
    crime_cum = crime_cum+1
  }
  if (movies$Documentary[i] == 1){
    doc = as.integer(movies$rating[i]) + doc
    doc_cum = doc_cum+1
  }
  if (movies$Drama[i] == 1){
    drama = as.integer(movies$rating[i]) + drama
    drama_cum = drama_cum+1
  }
  if (movies$Fantasy[i] == 1){
    fan = as.integer(movies$rating[i]) + fan
    fan_cum = fan_cum+1
  }
  if (movies$`Film-Noir`[i] == 1){
    film = as.integer(movies$rating[i]) + film
    film_cum=film_cum+1
  }
  if (movies$Horror[i] == 1){
    horror = as.integer(movies$rating[i]) + horror
    horror_cum=horror_cum+1
  }
  if (movies$Western[i] == 1){
    Western = as.integer(movies$rating[i]) + Western
    western_cum = western_cum+1
  }
  if (movies$War[i] == 1){
    War = as.integer(movies$rating[i]) + War
    war_cum = war_cum+1
  }
  if (movies$Thriller[i] == 1){
    Thriller = as.integer(movies$rating[i]) + Thriller
    thriller_cum = thriller_cum+1
  }
  if (movies$`Sci-Fi`[i] == 1){
    Sci_fi = as.integer(movies$rating[i]) + Sci_fi
    sci_fi_cum = sci_fi_cum+1
  }
  if (movies$Mystery[i] == 1){
    Mystery = as.integer(movies$rating[i]) + Mystery
    mystery_cum= mystery_cum+1
  }
  if (movies$Musical[i] == 1){
    Musical = as.integer(movies$rating[i]) + Musical
    musical_cum=musical_cum+1
  }
  if (movies$Romance[i] == 1){
    Romance = as.integer(movies$rating[i]) + Romance
    romance_cum=romance_cum+1
  }
}

# Divided sum of ratings by the total number of ratings to get averages of every genre. 

action_rate = action/action_cum
animation_rate = animation/animation_cum
adventure_rate = adventure/adventure_cum
childrens_rate = child/child_cum
comedy_rate = com / com_cum
crime_rate = crime/crime_cum
documentary_rate = doc/doc_cum
drama_rate = drama / drama_cum
fantasy_rate = fan/fan_cum
film_noir_rate = film/film_cum
horror_rate = horror/horror_cum
musical_rate = Musical/musical_cum
mystery_rate = Mystery/mystery_cum
romance_rate = Romance/romance_cum
sci_fi_rate = Sci_fi/sci_fi_cum
thriller_rate = Thriller/thriller_cum
war_rate = War / war_cum
western_rate = Western/western_cum

# Put all the averages into a data frame so we plot this information

genre <- data.frame(Genre = c("Action", "Adventure", "Animation", "Children's", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "Film-Noir", "Horror", "Musical", "Mystery", "Romance", "Sci-Fi", "Thriller", "War",  "Western"), Mean = c(action_rate, adventure_rate, animation_rate, childrens_rate, comedy_rate, crime_rate, documentary_rate, drama_rate, fantasy_rate, film_noir_rate, horror_rate, musical_rate, mystery_rate, romance_rate, sci_fi_rate, thriller_rate, war_rate, western_rate))

genre$Mean <- as.numeric(genre$Mean)

gen <- ggplot(genre, aes(Genre, Mean)) + labs(x ="Genre", y="Average Rating", title="Average Rating for Each Genre")

gen + geom_bar(fill = "blue", stat="identity")


mean <- density(genre$Mean) 

plot(mean, main="Density of Average Movie Rating", xlab = "Average Rating", col = "blue")


# Analysis of decades and their average ratings

#Format the date
format_date<-gsub("Jan","01",movies[["release date"]])
format_date<-gsub("Feb","02",format_date)
format_date<-gsub("Mar","03",format_date)
format_date<-gsub("Apr","04",format_date)
format_date<-gsub("May","05",format_date)
format_date<-gsub("Jun","06",format_date)
format_date<-gsub("Jul","07",format_date)
format_date<-gsub("Aug","08",format_date)
format_date<-gsub("Sep","09",format_date)
format_date<-gsub("Oct","10",format_date)
format_date<-gsub("Nov","11",format_date)
format_date<-gsub("Dec","12",format_date)

format_date=as.Date(format_date,"%d-%m-%Y")

format_date

movies_date_format<-cbind(movies, format_date)

##Set-up the decades
begin_20s<-as.Date("1920-01-01")
end_20s<-as.Date("1929-12-31")

begin_30s<-as.Date("1930-01-01")
end_30s<-as.Date("1939-12-31")

begin_40s<-as.Date("1940-01-01")
end_40s<-as.Date("1949-12-31")

begin_50s<-as.Date("1950-01-01")
end_50s<-as.Date("1959-12-31")

begin_60s<-as.Date("1960-01-01")
end_60s<-as.Date("1969-12-31")

begin_70s<-as.Date("1970-01-01")
end_70s<-as.Date("1979-12-31")

begin_80s<-as.Date("1980-01-01")
end_80s<-as.Date("1989-12-31")

begin_90s<-as.Date("1990-01-01")
end_90s<-as.Date("1999-12-31")

movies_date_format<-movies_date_format[complete.cases(movies_date_format$format_date), ]
time_20s<-movies_date_format[(movies_date_format$format_date>= begin_20s & movies_date_format$format_date<=end_20s),]
time_20s_mean<-mean(time_20s$rating)
time_20s_length<-length(time_20s$format_date)

time_30s<-movies_date_format[(movies_date_format$format_date>= begin_30s & movies_date_format$format_date<=end_30s),]
time_30s_mean<-mean(time_30s$rating)
time_30s_length<-length(time_30s$format_date)

time_40s<-movies_date_format[(movies_date_format$format_date>= begin_40s & movies_date_format$format_date<=end_40s),]
time_40s_mean<-mean(time_40s$rating)
time_40s_length<-length(time_40s$format_date)

time_50s<-movies_date_format[(movies_date_format$format_date>= begin_50s & movies_date_format$format_date<=end_50s),]
time_50s_mean<-mean(time_50s$rating)
time_50s_length<- length(time_50s$format_date)

time_60s<-movies_date_format[(movies_date_format$format_date>= begin_60s & movies_date_format$format_date<=end_60s),]
time_60s_mean<-mean(time_60s$rating)
time_60s_length<-length(time_60s$format_date)

time_70s<-movies_date_format[(movies_date_format$format_date>= begin_70s & movies_date_format$format_date<=end_70s),]
time_70s_mean<-mean(time_70s$rating)
time_70s_length<-length(time_70s$format_date)

time_80s<-movies_date_format[(movies_date_format$format_date>= begin_80s & movies_date_format$format_date<=end_80s),]
time_80s_mean<-mean(time_80s$rating)
time_80s_length<-length(time_80s$format_date)

time_90s<-movies_date_format[(movies_date_format$format_date>= begin_90s & movies_date_format$format_date<=end_90s),]
time_90s_mean<-mean(time_90s$rating)
time_90s_length<-length(time_90s$format_date)

decade_df=data.frame(Decades=c("20s","30s","40s","50s","60s","70s","80s","90s"),Mean=c(time_20s_mean,time_30s_mean,time_40s_mean,time_50s_mean,time_60s_mean,time_70s_mean,time_80s_mean,time_90s_mean) )
decade_df$Mean<-as.numeric(decade_df$Mean)
decade_df_gg<-ggplot(decade_df, aes(Decades, Mean))
decade_df_gg+geom_bar(stat="identity",fill="steel blue")+ylim(c(0,8))

decade_df_length<-data.frame(Decades=c("20s","30s","40s","50s","60s","70s","80s","90s"), Number_of_Reviews=c(time_20s_length,time_30s_length,time_40s_length,time_50s_length,time_60s_length,time_70s_length,time_80s_length,time_90s_length))
decade_df_length$Number_of_Reviews<-as.numeric(decade_df_length$Number_of_Reviews)
gg_decade_df_length<-ggplot(decade_df_length, aes(Decades, Number_of_Reviews))+ggtitle("Number of Reviews Each Decade")+ labs(y="Number of Reviews")
gg_decade_df_length+geom_bar(stat='identity', fill="steel blue")

# Film Noir further research
# find the Film Noir films in the 40s data frame
# pull out and analyze
filmNoir = 0
for (i in 1:length(time_40s$`movie id`)){
  if (time_40s$`Film-Noir`[i] == 1){
    filmNoir = filmNoir + 1
  }
}
filmNoir 

filmnoir = filmNoir/2249
# Only 14% #

filmNoir = 0
for (i in 1:length(time_40s$`movie id`)){
  if (time_50s$`Film-Noir`[i] == 1){
    filmNoir = filmNoir + 1
  }
}
filmNoir 

filmnoir = filmNoir/3506
# 1.8% #

filmNoir = 0
for (i in 1:length(time_90s$`movie id`)){
  if (time_90s$`Film-Noir`[i] == 1){
    filmNoir = filmNoir + 1
  }
}
filmNoir 

filmnoir = filmNoir/3506
# 1.8% #

# Question 2. 

###############################################
###############################################



# Appendix Question 1:
groupby_df2<-group_by(df2,`reviewer id`)
total <- summarise(groupby_df2, count=n())
total <- total[order(-total$count),]

# Reviewer ID = 405, 737 movies reviewed

# Question 2 
zip_reviews <- merge(df2, df3, by='reviewer id')
zip_reviews$Zipcode <- as.integer(gsub('[A-Z][0-9][A-Z][0-9][A-Z]', '999999', zip_reviews$`zip code`))
df4_clean <- data.frame(df4$Zipcode, df4$City, df4$State, df4$Country)
colnames(df4_clean) <- c('Zipcode', 'City', 'State', 'Country')
df4_clean_matrix <- as.matrix(df4_clean)
df4_clean_matrix <- rbind(df4_clean_matrix, c(999999, '', 'Canada', 'Canada'))
reviews_loc <- merge(df4_clean_matrix, zip_reviews, by='Zipcode')
reviews_by_state <- data.frame(sort(table(reviews_loc$State), decreasing = TRUE))
reviews_by_state[1:5,]


# Question 3 
df1$genres <- rowSums(df1[,7:13])
movie_genres <- merge(df1, df2, by='movie id')
two <- movie_genres[movie_genres$genres>=2,]
nrow(two)/nrow(movie_genres)

# Question 4
movie_freq <- data.frame(sort(table(df2$`movie id`), decreasing = FALSE))
colnames(movie_freq) <- c('movie id', 'reviews')
movie_freq_perc <- data.frame(table(movie_freq$reviews))
colnames(movie_freq_perc) <- c('reviews', 'number of movies')
movie_freq_perc$percentage <- percent(movie_freq_perc$`number of movies`/nrow(df1))

write.csv(movie_freq, file = "Frequencey_team_7.csv")
