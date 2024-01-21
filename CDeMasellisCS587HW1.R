#Question 1

setwd("C:\\Users\\Caitlin\\Downloads\\Rscripts\\data")
su = read.delim("su_raw_matrix.txt", header = TRUE, sep = "\t")
Liver2 = su$Liver_2.CEL #This is grabbing just the Liver2 column
Liver2Mean = mean(su$Liver_2.CEL) #This is the mean of Liver2
Liver2SD = sd(su$Liver_2.CEL) #This is the standard deviation of Liver2
print(Liver2)
means = colMeans(su,na.rm = TRUE) #This is getting the means for all of my columns
sums = colSums(su, na.rm = TRUE) #This is getting the sums of all of my columns


#Question 2

rnorm(10000, mean = 0, sd = 0.2)
rnorm_a = rnorm(10000, mean = 0, sd = 0.2) #This is setting up the 10000 numbers for a
rnorm_b = rnorm(10000, mean = 0, sd = 0.5) #This is setting up the b
hist_a = hist(rnorm_a, xlim = c(-5,5), main = "Histogram of rnorm_a", xlab = "range") #I used the histogram fucntion to create a histogram with the results of rnorm_a
hist_b = hist(rnorm_b, xlim = c(-5,5), main = "Histogram of rnorm_b", xlab = "range") #I used the histogram fucntion to create a histogram with the results of rnorm_b


#Question 3

library(ggplot2)
dat <- data.frame(cond = factor(rep(c("A","B"), each=200)),
                  rating = c(rnorm(200),rnorm(200, mean=.8)))
# Overlaid histograms
ggplot(dat, aes(x=rating, fill=cond)) +
  geom_histogram(binwidth=.5, alpha=.5, position="identity")
# Interleaved histograms
ggplot(dat, aes(x=rating, fill=cond)) + geom_histogram(binwidth=.5, position="dodge")
# Density plots
ggplot(dat, aes(x=rating, colour=cond)) + geom_density()
# Density plots with semitransparent fill
ggplot(dat, aes(x=rating, fill=cond)) + geom_density(alpha=.3)
diabetes = read.delim("diabetes_train.csv", header = TRUE, sep = ",") # This is needed to find the data
# Overlaid histograms
ggplot(diabetes, aes(x=mass, fill=class)) +
  geom_histogram(binwidth=.5, alpha=.5, position="identity")
# Interleaved histograms
ggplot(diabetes, aes(x=mass, fill=class)) + geom_histogram(binwidth=.5, position="dodge")
# Density plots
ggplot(diabetes, aes(x=mass, colour=class)) + geom_density()
# Density plots with semitransparent fill
ggplot(diabetes, aes(x=mass, fill=class)) + geom_density(alpha=.3)

#Question 4

passengers = read.delim("titanic.csv", header = TRUE, sep = ",")
passengers %>% drop_na() %>% summary()
passengers %>% filter(Sex == "male")
passengers %>% arrange(desc(Fare))
passengers %>% mutate(FamSize = Parch + SibSp)
passengers %>% group_by(Sex) %>% summarise(meanFare = mean(Fare), numSurv = sum(Survived))


#Question 5

Diabetes_Percentiles = quantile(diabetes$skin, probs = c(0.1, 0.3, 0.5, 0.6))

