setwd("H:\\V SEM\\ST 3010 –Introduction to Health Statistics\\assignment")
data1=read.csv("heart_failure_clinical_records_dataset.csv")
attach(data1)
#to get overview
summary(data1)
str(data1)
#to get total observations(since there are no any missing values)
l=length(age)
l

#install.packages("descriptr")
library(descriptr)
sex_count=table(sex)
sex_count
dim(sex_count)
death_count=table(DEATH_EVENT)
death_count

#charts

pie(death_count,main = "Death & Alive", col = c("yellow","red"),labels = c("Alive","Dead"))
barplot(sex_count,xlab = "Gender",ylab = "Frequencies",col = c("pink","blue"),names.arg = c("Female","Male"),main = "Barplot of Gender")
boxplot(creatinine_phosphokinase~DEATH_EVENT)
t.test(new_data2$creatinine_phosphokinase,new_data$creatinine_phosphokinase,alternative = "two.sided",var.equal = FALSE)

#Dead vs smoking
table_Smoking_Dead=table(smoking=factor(smoking,levels=c("1","0")),DEATH_EVENT=factor(DEATH_EVENT ,levels=c("1","0")))
table_Smoking_Dead
colnames(table_Smoking_Dead)=c("Dead","Alive")
rownames(table_Smoking_Dead)=c("Yes","No")
table_Smoking_Dead

#install.packages("epiR")
library(epiR)

epi.2by2(table_Smoking_Dead,method="cohort.count",conf.level = 0.95)
percentage_relativerisk=(0.961-1)*100
percentage_relativerisk

#Dead vs diabetes
table(diabetes,DEATH_EVENT)
table_diabetes_Dead=table(diabetes =factor(diabetes ,levels=c("1","0")),DEATH_EVENT=factor(DEATH_EVENT ,levels=c("1","0")))
table_diabetes_Dead
colnames(table_diabetes_Dead)=c("Dead","Alive")
rownames(table_diabetes_Dead)=c("Yes","No")
table_diabetes_Dead

epi.2by2(table_diabetes_Dead,method="cohort.count",conf.level = 0.95)
percentage_relativerisk1=(0.99-1)*100
percentage_relativerisk1


#new_data = data1[which(DEATH_EVENT=="1")]
new_data=subset(data1,DEATH_EVENT=="1")
new_count=table(new_data$sex)
new_count
qqnorm(new_data$platelets)
qqline(new_data$platelets)
shapiro.test(new_data$platelets)
hist(new_data$platelets,col = "grey",main = "Histogram of Platelets Of People Who Died")
sactterplot=plot(age,platelets,xlab = "Age",ylab = "Platelets",col=c("black"),main = "Platelets vs Age")

#ttest
t_test_platelets=t.test(new_data$platelets,alternative = "less",conf.level = 0.95,mu=100000)
t_test_platelets$statistic
t_test_platelets
qt(0.05,95)

#creatinine level (should between 10-120)
t_test_creatinine=t.test(new_data$creatinine_phosphokinase,alternative = "two.sided",conf.level = 0.95,mu=65)
t_test_creatinine
t_test_creatinine1=t.test(new_data$creatinine_phosphokinase,alternative = "greater",conf.level = 0.95,mu=120)
t_test_creatinine1
t_test_creatinine2=t.test(new_data$creatinine_phosphokinase,alternative = "less",conf.level = 0.95,mu=10)
t_test_creatinine2

#serum sodium level with dead
new_data2=subset(data1,DEATH_EVENT=="0")
new_data2_count=table(new_data2$sex)
new_data2_count

#t test for two independent samples
#serum sodium
t_test_sodium = t.test(new_data2$serum_sodium,new_data$serum_sodium,alternative = "two.sided",var.equal = FALSE)
t_test_sodium
df=((var(new_data2$serum_sodium)/203 + var(new_data$serum_sodium)/96)^2)/(((var(new_data2$serum_sodium)/203)^2/202)+((var(new_data$serum_sodium)/96)^2 /95))
df
qt(0.975,df)
t_test_sodium$statistic
(1-pt(3.1645,df=df))*2

#serum_creatinine
t_test_serum_creatinine = t.test(new_data2$serum_creatinine,new_data$serum_creatinine,alternative = "two.sided",var.equal = FALSE)
t_test_sodium
df=((var(new_data2$serum_creatinine)/203 + var(new_data$serum_creatinine)/96)^2)/(((var(new_data2$serum_creatinine)/203)^2/202)+((var(new_data$serum_creatinine)/96)^2 /95))
df
qt(0.025,df)
t_test_sodium$statistic
(1-pt(3.1645,df=df))*2

#time and serum_creatinine
sactterplot1=plot(new_data$time,new_data$serum_creatinine,xlab = "Time until dead",ylab = "Serum Creatinine",col=c("black"),main = "Serum Creatinine vs Time (Dead)")
sactterplot1=plot(new_data2$time,new_data2$serum_creatinine,xlab = "Time until dead",ylab = "Serum Creatinine",col=c("black"),main = "Serum Creatinine vs Time(Alive)")

#Dead vs anaemia
table_anaemia_Dead=table(anaemia=factor(anaemia,levels=c("1","0")),DEATH_EVENT=factor(DEATH_EVENT ,levels=c("1","0")))
table_anaemia_Dead
colnames(table_anaemia_Dead)=c("Dead","Alive")
rownames(table_anaemia_Dead)=c("Low","Normal")
table_anaemia_Dead


epi.2by2(table_anaemia_Dead,method="cohort.count",conf.level = 0.95)
percentage_relativerisk=(1.21-1)*100
percentage_relativerisk


