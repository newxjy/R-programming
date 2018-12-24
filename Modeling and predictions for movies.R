##title: "Modeling and prediction for movies"
##highlight: pygments
##theme: spacelab
---
## Setup
  ## This project aims to develop a multiple linear regression model that will explain what makes movies popular given variables in a dataset. 
  ## The dataset contains information from Rotten Tomatoes,IMDB, television programs and video games. In this project, we wil also use the model to predict a movie released in 2016 that is not included in the dataset.
  
  ### Load packages
  
library(ggplot2)
library(dplyr)

### Load data
load("/Users/newxjy/Desktop/movies.Rdata")

  
## Part 1: Data
## The data were obtained from IMDB and Rotten Tomatoes. The data represents 651 randomly sampled movies produced and released before 2016. There are 32 variables about the movies including: genre of the movie (genre), year the movie is released (year), audience score on Rotten Tomatoes(audience_score), critics score on Rotten Tomatoes(critics_score), and many other interesting variables.



## Part 2: Research question
  
  ## The research question addressed in this analysis is this: is it possible to predict the popularity of a feature film prior to its release based upon certain characteristic information about it? 
  ## More specifically, do variables such as movie genre, MPAA rating, run length, etc. work as reasonable predictors of a movie’s popularity?  


  
## Part 3: Exploratory data analysis

  ## Removing unrelated data from the dataset. 
f1 <- ggplot(data = movies, aes(x = genre)) + geom_bar(fill = "skyblue") + xlab("Movie Genre") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0))
f2 <- ggplot(data = movies, aes(x = title_type)) + geom_bar(fill = "yellow") + xlab("Movie Type") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0))
f3 <- ggplot(data = movies, aes(x = mpaa_rating)) + geom_bar(fill = "pink") + xlab("Movie MPAA Rating") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0))
f4 <- ggplot(data = movies, aes(x = runtime)) + geom_histogram(binwidth = 10, fill = "purple") + xlab("Movie Runtime")
fmovies <- movies %>% filter(title_type == "Feature Film") %>% filter(!(mpaa_rating %in% c("NC-17", "Unrated")))

  ## Correlations between different rating scores. 
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

pairs(~ imdb_rating + critics_score + audience_score, 
      data=fmovies, lower.panel=panel.smooth, upper.panel=panel.cor)

  ## Given the correlations above, using the analysis below to choose only one rating to be the response variable. 
p1 <- ggplot(data = fmovies, aes(x = imdb_rating)) + geom_histogram(color = "black", binwidth = 0.5, fill = "yellow") + xlab("IMDB Scores")
p2 <- ggplot(data = fmovies, aes(x = critics_score)) + geom_histogram(color = "black", binwidth = 5, fill = "skyblue") + xlab("Critics Scores")
p3 <- ggplot(data = fmovies, aes(x = audience_score)) + geom_histogram(color = "black", binwidth = 5, fill = "pink") + xlab("Audience Scores")

d1 <- ggplot(fmovies, aes(x = critics_score, y = imdb_rating, color = critics_rating)) + geom_point(alpha = 0.4) 
d2 <- ggplot(fmovies, aes(x = audience_score, y = imdb_rating, color = audience_rating)) + geom_point(alpha = 0.4) 


  
## Part 4: Modeling

  ## The initial model fitting results. 
fullMod <- lm(imdb_rating ~ genre + runtime + mpaa_rating + best_dir_win + best_pic_nom + audience_score + critics_score, data = fmovies)
summary(fullMod)
anova(fullMod)

  ## The final model fitting results.
finalMod <- lm(imdb_rating ~ genre + runtime + audience_score + critics_score, data = fmovies)
summary(finalMod)
anova(finalMod)

  ## Model diagnostic. 
    ## Linear relationships between each (numerical) predictor and response. 
t1 <-  ggplot(data = fmovies, aes(x = audience_score, y = resid(finalMod))) + geom_hline(yintercept = 0, size = 1)  + xlab("Audience Score") + ylab("Residual") + geom_point()
t2 <-  ggplot(data = fmovies, aes(x = critics_score, y = resid(finalMod))) + geom_hline(yintercept = 0, size = 1)  + xlab("Critics Score") + ylab("Residual") + geom_point() 
    ## Nearly normal residuals with mean 0 and independent residuals.
       ## Histograms of residuals.
hist(finalMod$residuals, breaks = 25, main = "Histogram of Residuals", col = "blue", border = "black", prob = TRUE)
curve(dnorm(x, mean = mean(finalMod$residuals), sd = sd(finalMod$residuals)), col="red", add=T, lwd = 2)   
      ## Normal Q-Q plot.
qqnorm(finalMod$residuals)
qqline(finalMod$residuals)
      ## Plot of residuals vs. Order of observations.
plot(finalMod$residuals, main = "Plot of Residuals VS order of Observations")
  ## Constant variability of residuals.
t3 <- ggplot(data.frame(x = finalMod$fitted.values, y = resid(finalMod)), aes(x=x, y=y)) + geom_hline(yintercept = 0, size = 1)  + xlab("Fitted Values") + ylab("Residual") + geom_point()
t4 <- ggplot(data.frame(x =finalMod$fitted.values, y = abs(resid(finalMod))), aes(x=x, y=y)) + geom_hline(yintercept = 0, size = 1)  + xlab("Fitted Values") + ylab("Residual") + geom_point()


## Part 5: Prediction
  
  ## Use the final model to generate rating predictions for Ameélie released in November 2011
  ## and for Crouching Tiger, Hidden Dragon released in December 2000.
data1 <- data.frame(genre="Comedy", runtime=122, audience_score=95, critics_score=89)
pred1 <- predict(finalMod, data1, interval = "predict", level = 0.95, se.fit = TRUE)

data2 <- data.frame(genre="Action & Adventure", runtime=120, audience_score=86, critics_score=97)
pred2 <- predict(finalMod, data2, interval = "predict", level = 0.95, se.fit = TRUE)

  
## Part 6: Conclusion

  ## The model diagnostics has shown that the final model in this project has met the requirements for the linear conditions to be valid. This model has also been proven to be able to predict with a certain amount of accuracy the popularity of movies using imdb_rating as a measure of popularity. However, the predictive power of this model is limited. Variables such as audience_scores and critics_scores are subjective measures unavoidably prone to bias. 
  ## Moreover, the following is a list of problems that could be further addressed. 
  ## 1. Find representatives scores from audiences and critics for movies that haven't been released in movie theatres.
  ## 2. Find additional movie characteristics data to add onto the model to increase the value of adjusted r^2. 
  ## 3. Create individual models for each movie genre to improve the predictive power of the model.