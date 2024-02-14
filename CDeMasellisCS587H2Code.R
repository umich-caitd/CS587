x=c(1,7,4) # vector1
y=c(2,9,3) # vector2


ManhattanDistance=function(x,y){
  dist=abs(x-y)
  dist=sum(dist)
  return(dist)
} #function to solve Manhattan Distance

ManhattanDistance(x,y) #will result in Manhattan Distance



EuclideanDistance=function(x,y)sqrt(sum((x-y)^2)) #vectors are already set, this is the function to solve Euclidean Distance

EuclideanDistance(x,y) #will result in Euclidean Distance



data(mtcars) #accessing the mtcars dataframe

cor(mtcars$mpg, mtcars$wt) #finding correlation between mpg and wt attributes

library(ggplot2)

ggplot(mtcars, aes(x=mtcars$wt, y=mtcars$mpg)) + geom_point() #creating the scatter plot


setwd("C:/Users/Caitlin/Downloads")
MetaboliteData=read.csv("metabolite.csv") #access homework data

print(MetaboliteData) #view data

library(tidyverse) #use tidyverse

MetaboliteDataNoMissing=MetaboliteData %>%
  select(where(~sum(is.na(.))/length(.)<=0.75)) #remove columns that have more than 75% missing values

ReplaceMedian=function(MetaboliteDataNoMissing) {
  MetaboliteData %>% 
    mutate_at(vars(-Label), ~if_else(is.na(.), median(., na.rm = TRUE), .))
} 

MetaboliteDataNoMissingUpdated=ReplaceMedian(MetaboliteDataNoMissing) #update with median values

print(MetaboliteDataNoMissingUpdated)

library(ggplot2) #library used
library(tidyverse) #library used

MetabolitePCA=prcomp(MetaboliteDataNoMissingUpdated %>% 
                       select(-Label) %>% select(where(~ !all(is.na(.)))), scale. = TRUE) #apply PCA

PCA1=MetabolitePCA$x[,1]
PCA2=MetabolitePCA$x[,2]


PCA1and2=data.frame(PCA1=PCA1, PCA2=PCA2, Label=MetaboliteDataNoMissingUpdated$Label)

ggplot(PCA1and2, aes(x=PCA1, y=PCA2, color=Label)) + geom_point()

