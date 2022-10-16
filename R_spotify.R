#import data

Spotify <- read.csv("~/Desktop/Spotify.csv")
View(Spotify)

#Scatter Plot
plot(Spotify$danceability ~ Spotify$popularity,xlab="Popularity", ylab="Danceability")
plot(Spotify$acousticness ~ Spotify$popularity,xlab="Popularity", ylab="Acousticness")
plot(Spotify$duration_ms ~ Spotify$popularity,xlab="Popularity", ylab="Duration_ms")
plot(Spotify$energy ~ Spotify$popularity,xlab="Popularity", ylab="Energy")
plot(Spotify$instrumentalness ~ Spotify$popularity,xlab="Popularity", ylab="Instrumentalness")
plot(Spotify$liveness~ Spotify$popularity,xlab="Popularity", ylab="Liveness")
plot(Spotify$loudness ~ Spotify$popularity,xlab="Popularity", ylab="Loudness")
plot(Spotify$speechiness ~ Spotify$popularity,xlab="Popularity", ylab="Speechiness")
plot(Spotify$tempo ~ Spotify$popularity,xlab="Popularity", ylab="Tempo")
plot(Spotify$valence ~ Spotify$popularity,xlab="Popularity", ylab="Valence")

#clean artists and name
Spotify <- subset(Spotify, grepl('[a-zA-Z]', artists))
Spotify <- subset(Spotify, grepl('[a-zA-Z]', name))
nrow(Spotify)

#remove duplicate
library(dplyr)
Spotify %>% distinct()
nrow(Spotify)

#verify no duplicated ID
dim(Spotify[duplicated(Spotify$id),])[1]

# drop release date 
Spotify_complete <- na.omit(Spotify)
nrow(Spotify_complete)
View(Spotify_complete)
Spotify_complete$release_date <- NULL
Spotify_complete$duration_ms <- Spotify_complete$duration_ms/60000
View(Spotify_complete)

# Check 0 popularity distribution
zero_popularity = Spotify_complete[Spotify_complete$popularity == 0,]
hist(zero_popularity$year, col = "yellow")
# Filter songs only after 1960
Spotify_complete <- filter(Spotify_complete, year >1960)
nrow(Spotify_complete)

# add a new column popularity_cat to equally bin data set into
# low, medium, high popularity
num_row <- nrow(Spotify_complete)
popularity <- Spotify_complete$popularity
popularity <- sort(popularity)
low_value = popularity[num_row%/%3]
low_value
mid_value = popularity[2*num_row%/%3]
mid_value

Spotify_complete$popularity_cat = "m"
Spotify_complete$popularity_cat[Spotify_complete$popularity <= low_value] <- "l"
Spotify_complete$popularity_cat[Spotify_complete$popularity > mid_value] <- "h"
Spotify_complete$popularity_cat = factor(Spotify_complete$popularity_cat)

#Verify that the 3 bins are approximately the same size
nrow(Spotify_complete[Spotify_complete$popularity_cat == "l",])
nrow(Spotify_complete[Spotify_complete$popularity_cat == "m",])
nrow(Spotify_complete[Spotify_complete$popularity_cat == "h",])

#export to excel to check
write.csv(Spotify_complete, "~/Desktop/No/Cleaned_Spotify.csv")

#Generate HeatMap
generate_heatmap <- function(Spotify_complete) {
  library(reshape2)
  library(ggplot2)
  
  columns <- c('acousticness', 'danceability', 'duration_ms', 'energy',
               'instrumentalness', 'liveness', 'loudness', 'popularity',
               'speechiness', 'tempo', 'valence', 'mode',
               'key', 'explicit')
  data <- Spotify_complete[columns]
  cormat <- round(cor(data),2)
  head(cormat)
  reorder_cormat <- function(cormat){
    # Use correlation between variables as distance
    dd <- as.dist((1-cormat)/2)
    hc <- hclust(dd)
    cormat <-cormat[hc$order, hc$order]
  }
  cormat <- reorder_cormat(cormat)
  cormat[lower.tri(cormat)]<- NA
  melted_cormat <- melt(cormat)
  
  head(melted_cormat)
  
  reorder_cormat <- function(cormat){
    # Use correlation between variables as distance
    dd <- as.dist((1-cormat)/2)
    hc <- hclust(dd)
    cormat <-cormat[hc$order, hc$order]
  }
  
  get_upper_tri <- function(cormat){
    cormat[lower.tri(cormat)]<- NA
    return(cormat)
  }
  ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "yellow", mid = "pink", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Heatmap Correlation") +
    theme_minimal()+ # minimal theme
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 12, hjust = 1))+
    coord_fixed()
  final <- ggheatmap + 
    geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      legend.justification = c(1, 0),
      legend.position = c(0.6, 0.7),
      legend.direction = "horizontal")+
    guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                 title.position = "top", title.hjust = 0.5))
  print(final)
}

generate_heatmap(Spotify_complete)


############################################################
#Training Models
############################################################
library(rpart) 
library(rpart.plot)
library(caret)
library(e1071)
#drop these columns
columns <- c('year', 'artists', 'id', 'name', 'release_date', 'speechiness', 'energy', 'valence', 'instrumentalness','popularity')
data = Spotify_complete[,!(names(Spotify_complete) %in% columns)]
View(data)



numberOfRows <- nrow(data)
set.seed(1)
train.index <- sample(numberOfRows, numberOfRows*0.6)
train.df <- data[train.index, ]
valid.df <- data[-train.index, ]
View(train.df)
View(valid.df)

#Linear Regression
lm(formula=popularity ~ ., data=train.df)


.ct <- rpart(popularity_cat ~ ., data = train.df, method = "class", cp = 0, maxdepth = 4, minsplit = 20)
printcp(.ct)
prp(.ct, type = 1, extra = 1, under = FALSE, split.font = 1, varlen = -10)
ct.pred <- predict(.ct, valid.df, type = "class")
confusionMatrix(ct.pred, as.factor(valid.df$popularity_cat))



# build a deeper classification tree
max.ct <- rpart(popularity_cat ~ ., data = train.df, method = "class", cp = 0, minsplit = 1, maxdepth = 10)

# count number of leaves
length(max.ct$frame$var[max.ct$frame$var == "<leaf>"])

# plot tree
prp(max.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(max.ct$frame$var == "<leaf>", 'gray', 'white'))  

# classify records in the training data to show that the tree prefectly fits the training data.
# this is an example of overfitting
# set argument type = "class" in predict() to generate predicted class membership.
max.pred <- predict(max.ct, train.df, type = "class")

# generate confusion matrix for training data
confusionMatrix(max.pred, as.factor(train.df$popularity_cat))

max.pred <- predict(max.ct, valid.df, type = "class")

# generate confusion matrix for training data
confusionMatrix(max.pred, as.factor(valid.df$popularity_cat))
