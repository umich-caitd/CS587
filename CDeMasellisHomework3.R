age=c(13, 15, 16, 16, 19, 20, 20, 21, 22, 22, 25, 25, 25, 25, 30, 33, 33, 35, 35, 35, 35, 36, 40, 45, 46, 52, 70) ## age data

smoothed_age=tapply(age, gl(length(age) %/% 3, 3), mean) ## smoothed age

smoothed_age ## output


Q1=quantile(age, 0.25)
Q3=quantile(age, 0.75)
IQR_value=Q3-Q1 ## use IQR to determine outliers

lower_bound=Q1-.5*IQR_value
upper_bound=Q3+1.5*IQR_value

outliers=age[age<lower_bound|age>upper_bound]

outliers

min_age=min(age)
max_age=max(age)

normalized_age_min_max=(35-min_age)/(max_age - min_age) ## min-max normalization

normalized_age_min_max

mean_age=mean(age)
sd_age=sd(age)

normalized_age_z_score=(35-mean_age)/sd_age ## z-score

normalized_age_z_score

max_abs_value=max(abs(age)) 
num_decimal_places=nchar(trunc(max_abs_value))

normalized_age_decimal_scaling=35/(10^num_decimal_places) ## normalization by decimal scaling


entropy=function(p) {if(p==0|p==1){return(0)}else{return(-p*log2(p)-(1- p)*log2(1-p))}}


total_junior=40+40+3+6  ## count of junior staff
total_senior=30+5+3+10+4+4  ## count of senior staff
total_count=total_junior+total_senior  ## count of all staff

p_junior=total_junior/total_count  ## probability of being junior
p_senior=total_senior/total_count  ## probability of being senior

entropy_status=entropy(p_junior)*(total_junior/total_count)+entropy(p_senior)*(total_senior/total_count)

entropy_sales=entropy(120/184)*(120/184)+entropy(64/184)*(64/184)
information_gain_sales=entropy_status-entropy_sales

entropy_age=entropy(96/184)*(96/184)+entropy(88/184)*(88/184) information_gain_age=entropy_status-entropy_age

entropy_salary=entropy(86/184)*(86/184)+entropy(98/184)*(98/184)
information_gain_salary=entropy_status-entropy_salary