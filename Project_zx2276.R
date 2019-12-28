#view data and relationships
library(tidyverse)
library(modelr)
###Clean the tibble and analyse the data
countries<-read_csv("D:/Proj/countries.csv")%>%
  drop_na()
vars1<-countries%>%
  select(-c(1,2,22:28))%>%
  mutate(region=factor(region),
         continent=factor(continent))
vars2<-countries%>%
  select(22)
vars3<-bind_cols(vars1,vars2)

##Analyse response variables
summary(vars3$democracy_index)
m<-mean(vars3$democracy_index)
v<-var(vars3$democracy_index)
ks.test((vars3$democracy_index-m)/sqrt(v),pnorm,alternative = c("two.sided"))

#QQplot
qq_plot1_0<-ggplot(data=vars3,
                   aes(sample=democracy_index))+
  stat_qq()+
  stat_qq_line()
qq_plot1_0
#Boxplot
box_plot1_0<-ggplot(data=vars3,
                    aes(y=democracy_index))+
  geom_boxplot()
box_plot1_0

###Problem1
##Choose variables using multiple linear regression
#Forward selection with AIC
min.model<-lm(democracy_index~1,data=vars3)
max.model<-formula(lm(democracy_index~.-democracy_index,data=vars3))
step(min.model,scope=max.model,direction = c("forward"))

model1_1<-lm(formula = democracy_index ~ region + infant_mortality_rate + 
     health_spend_pct_gdp + gdpPPP_percap + land_area + coastline + 
     death_rate + roadways + refined_petrol_consumption + birth_rate + 
     airports , data = vars3)

#Analyse the model
summary(model1_1)
anova(model1_1)

sample_with_resid1_1<-vars3%>%
  add_predictions(model1_1)%>%
  add_residuals(model1_1)%>%
  mutate(SDResid=rstudent(model1_1))
sample_with_resid1_1

#Scatterplot of predictions and residuals
resid_plot1_1<-ggplot(data=sample_with_resid1_1,
                      aes(x=pred,y=resid))+
  geom_ref_line(h=0,colour="red")+
  geom_point()+
  geom_smooth(se=F)+
  labs(title = "Scatterplot of predictions and residuals")
resid_plot1_1
#QQplot of the studentized deleted residuals
qq_plot1_1<-ggplot(data=sample_with_resid1_1,
                   aes(sample=SDResid))+
  stat_qq()+
  stat_qq_line()+
  labs(title = "QQplot of the studentized deleted residuals")
qq_plot1_1
#Scatterplot of Predictions and SDresid
Prediction_and_SDresid_plot1_1<-ggplot(data=sample_with_resid1_1,
                                  aes(x=pred,y=SDResid))+
  geom_ref_line(h=0,colour="red")+
  geom_point()+
  geom_smooth(se=F)+
  labs(title = "Scatterplot of Predictions and SDresid")
Prediction_and_SDresid_plot1_1
#Boxplot
box_plot1_1<-ggplot(data=sample_with_resid1_1,
       aes(y=SDResid))+
  geom_boxplot()+
  labs(title = "Boxplot")
box_plot1_1
#From the plots, we can conclude that there are outliers and distributions of errors have different variance. 

#Try to modify the model
#Delete "airports"
model1_2<-lm(formula = democracy_index ~ region + infant_mortality_rate + 
               health_spend_pct_gdp + gdpPPP_percap + land_area + coastline + 
               death_rate + roadways + refined_petrol_consumption + birth_rate , data =vars3)
summary(model1_2)
anova(model1_2)
#Delete the variables but mse and r squared do not change a lot.

#Check multicolinearity of variables in the model
correlation<-as_tibble(cor(data_no_outlier%>%dplyr::select(infant_mortality_rate,health_spend_pct_gdp,gdpPPP_percap ,land_area , coastline ,
                                          death_rate ,roadways , refined_petrol_consumption, birth_rate)))
view(correlation)
#roadways and refined_petrol_consumption are highly correlated and birth rate and infant mortality rate are highly correlated.

#To check significance of infant mortality rate, we need to delete birth rate because of their high correlation.
model1_3<-lm(formula = democracy_index ~ region + infant_mortality_rate + 
               health_spend_pct_gdp + gdpPPP_percap + land_area + coastline + 
               death_rate + roadways + refined_petrol_consumption  , data =vars3)
summary(model1_3)
anova(model1_3)

#Research if there is an interaction between roadways and refined_petrol_consumption.
#Delete refined_petrol_consumption
model1_4<-lm(formula = democracy_index ~ region + infant_mortality_rate + 
               health_spend_pct_gdp + gdpPPP_percap + land_area + coastline + 
               death_rate + roadways, data = vars3)
summary(model1_4)
anova(model1_4)

#Delete roadways.
model1_5<-lm(formula = democracy_index ~ region + infant_mortality_rate + 
               health_spend_pct_gdp + gdpPPP_percap + land_area + coastline + 
               death_rate + refined_petrol_consumption, data = vars3)
summary(model1_5)
anova(model1_5)
#Find refined_petrol_consumption becomes unsignificant.

#Try an interaction.
model1_6<-lm(formula = democracy_index ~ region + infant_mortality_rate + 
               health_spend_pct_gdp + gdpPPP_percap + land_area + coastline + 
               death_rate + roadways +refined_petrol_consumption+ I(roadways*refined_petrol_consumption), data = vars3)
summary(model1_6)
anova(model1_6)
#Interaction and refined_petrol_consumption are not significant but roadways becomes significant.

#delete both of them.
model1_7<-lm(formula = democracy_index ~ region  + 
               health_spend_pct_gdp + gdpPPP_percap + land_area + coastline + 
               death_rate + infant_mortality_rate, data = vars3)
summary(model1_7)
anova(model1_7)
#The performances of these models are almost same. Use model1_7.

#Visualization.
sample_with_resid1_7<-vars3%>%
  add_predictions(model1_7)%>%
  add_residuals(model1_7)%>%
  mutate(SDResid=rstudent(model1_7))
sample_with_resid1_7
#Scatterplot of predictions and residuals
resid_plot1_7<-ggplot(data= sample_with_resid1_7,
                      aes(x=pred,y=resid))+
  geom_ref_line(h=0,colour="red")+
  geom_point()+
  geom_smooth(se=F)
resid_plot1_7
#QQplot of the studentized deleted residuals
qq_plot1_7<-ggplot(data= sample_with_resid1_7,
                   aes(sample=SDResid))+
  stat_qq()+
  stat_qq_line()
qq_plot1_7
#Scatterplot of Predictions and SDresid
Prediction_and_SDresid_plot1_7<-ggplot(data= sample_with_resid1_7,
                                       aes(x=pred,y=SDResid))+
  geom_ref_line(h=0,colour="red")+
  geom_point()+
  geom_smooth(se=F)
Prediction_and_SDresid_plot1_7
#Boxplot
box_plot1_7<-ggplot(data= sample_with_resid1_7,
       aes(y=SDResid))+
  geom_boxplot()
box_plot1_7

#Weighted least squares
cal.weights <- 1/ lm(abs(sample_with_resid1_7$resid) ~ sample_with_resid1_7$pred)$fitted.values^2
model1_8<-lm(formula =  democracy_index ~ region  + 
               health_spend_pct_gdp + gdpPPP_percap + land_area + coastline + 
               death_rate + infant_mortality_rate,data = vars3,
             weights = cal.weights)
summary(model1_8)
anova(model1_8)
#r squared becomes a little bit higher, but death rate becomes totally unsiginificant. So delete the variable.

model1_9<-lm(formula =  democracy_index ~ region  + 
               health_spend_pct_gdp + gdpPPP_percap + land_area + coastline + 
                infant_mortality_rate,data = vars3,
             weights = cal.weights)
summary(model1_9)
anova(model1_9)

#Now, delete outliers using three measures
library(broom)
outliers1<-augment(model1_9)%>%
  mutate(key=row_number())%>%
  filter(.cooksd >= qf(0.5,df1 = 15,df2 = 149))%>%
  dplyr::select(key)
outliers1

outliers2<-which(abs(dffits(model1_9))>2*sqrt(15/164))
outliers2<-tibble(key=outliers2)
outliers2

remove_outliers<-function(df,outliers){
  df<-df%>%
    mutate(key=row_number())%>%
    anti_join(outliers,by='key')%>%
    dplyr::select(-key)
  return(df)
}

data_no_outlier<-remove_outliers(sample_with_resid1_9,outliers2)
data_no_outlier


#Visualization.
sample_with_resid1_9<-vars3%>%
  add_predictions(model1_9)%>%
  add_residuals(model1_9)%>%
  mutate(SDResid=rstudent(model1_9))
sample_with_resid1_9
#Scatterplot of predictions and residuals
resid_plot1_9<-ggplot(data= sample_with_resid1_9,
                      aes(x=pred,y=resid))+
  geom_ref_line(h=0,colour="red")+
  geom_point()+
  geom_smooth(se=F)+
  labs(title = "Scatterplot of predictions and residuals")
resid_plot1_9
#QQplot of the studentized deleted residuals
qq_plot1_9<-ggplot(data= sample_with_resid1_9,
                   aes(sample=SDResid))+
  stat_qq()+
  stat_qq_line()+
  labs(title = "QQplot of the studentized deleted residuals")
qq_plot1_9
#Scatterplot of Predictions and SDresid
Prediction_and_SDresid_plot1_9<-ggplot(data= sample_with_resid1_9,
                                       aes(x=pred,y=SDResid))+
  geom_ref_line(h=0,colour="red")+
  geom_point()+
  geom_smooth(se=F)+
  labs(title = "Scatterplot of Predictions and SDresid")
Prediction_and_SDresid_plot1_9
#Boxplot
box_plot1_9<-ggplot(data= sample_with_resid1_9,
                    aes(y=SDResid))+
  geom_boxplot()+
  labs(title = "Boxplot")
box_plot1_9


###Problem 2

##Training set and test set
training_set2_1<-vars3%>%
  select(-c(6,7))%>%
  sample_frac(0.8)
test_set2_1<-vars3%>%anti_join(training_set2_1)
vars3
training_set2_1
test_set2_1
##Choose variables using multiple linear regression

#Forward selection with AIC
min.model<-lm(life_exp_at_birth~1,data=training_set2_1)
max.model<-formula(lm(life_exp_at_birth~.-life_exp_at_birth,data=training_set2_1))
step(min.model,scope=max.model,direction = c("forward"))

#Analyse the model
model2_1<-lm(formula = life_exp_at_birth ~ infant_mortality_rate + gdpPPP_percap + 
     death_rate + region + urbanization + birth_rate + health_spend_pct_gdp + 
     continent + land_area + coastline, data = training_set2_1)
summary(model2_1)
anova(model2_1)
#There are too many variables, which might cause overfitting. Also, many of the variables are not so significant and covariate.
#By sampling training set randomly several times, we can get following conclusions:
#By F test, we find gdpPPP_percap, continent, land_area and coastline are not so significant and should be dropped therefore.
#By t test, we find some regions are not significant and can be supposed as "other regions" therefore. Also, it can be better to set "other regions" as baseline.

#Add predictions and residuals
training_set_with_resid2_1<-training_set2_1%>%
  add_predictions(model2_1)%>%
  add_residuals(model2_1)
training_set_with_resid2_1

#Draw some plots
#Compare y and y_hat
compare_y_yhat_plot_in_trainingset2_1<-ggplot(data=training_set_with_resid2_1,
                               aes(x=life_exp_at_birth,y=pred))+
  geom_point()+
  geom_abline(intercept=0, slope=1,color="red")
compare_y_yhat_plot_in_trainingset2_1
#It is not so bad, especially when y is large.
#Compare y_hat and residuals
resid_plot_in_trainingset2_1<-ggplot(data=training_set_with_resid2_1,
                      aes(x=pred,y=resid))+
  geom_ref_line(h=0,colour="red")+
  geom_point()+
  geom_smooth(se=F)
resid_plot_in_trainingset2_1
#So, not normality.
#QQplot
qqnorm(training_set_with_resid2_1$life_exp_at_birth, pch = 1, frame = FALSE)
qqline(training_set_with_resid2_1$life_exp_at_birth, col = "steelblue", lwd = 2)
#Short tail

#We can test the model in test set now and then compare the model with following models in test set.

#Test the model
test_set_with_resid2_1<-test_set2_1%>%
  add_predictions(model2_1)%>%
  add_residuals(model2_1)
test_set_with_resid2_1

test_mse2_1<-sum((test_set_with_resid2_1$resid)^2)/(14)
test_mse2_1
test_mse_biased2_1<-sum((test_set_with_resid2_1$resid)^2)/(33)
test_mse_biased2_1
#mse in test set is much larger than that of training set, which means overfitting.
#But the small size of the test set might cause the mse confusing, So we also consider the biased estimate of variance.

#Compare y and y_hat
compare_y_yhat_plot_in_testset2_1<-ggplot(data=test_set_with_resid2_1,
                               aes(x=life_exp_at_birth,y=pred))+
  geom_point()+
  geom_abline(intercept=0, slope=1,color="red")
compare_y_yhat_plot_in_testset2_1
#Compare y_hat and residuals
resid_plot_in_testset2_1<-ggplot(data=test_set_with_resid2_1,
                      aes(x=pred,y=resid))+
  geom_ref_line(h=0,colour="red")+
  geom_point()+
  geom_smooth(se=F)
resid_plot_in_testset2_1
#So, not normality
#QQplot
qqnorm(test_set_with_resid2_1$life_exp_at_birth, pch = 1, frame = FALSE)
qqline(test_set_with_resid2_1$life_exp_at_birth, col = "steelblue", lwd = 2)
#So,not normality, "s"-short tail


#Try to modify the mlr model by deleting variables
training_set2_2<-training_set2_1%>%
  mutate(region=fct_recode(region,
                           "Others"="Central America and the Caribbean",
                           "Others"="North America",
                           "Others"="South America",
                           "Others"="Middle East"))%>%
  mutate(region=fct_relevel(region,"Others"))
test_set2_2<-test_set2_1%>%  mutate(region=fct_recode(region,
                                                      "Others"="Central America and the Caribbean",
                                                      "Others"="North America",
                                                      "Others"="South America",
                                                      "Others"="Middle East"))%>%
  mutate(region=fct_relevel(region,"Others"))

model2_2<-lm(formula = life_exp_at_birth ~ infant_mortality_rate +
               death_rate + region + urbanization + birth_rate +
               health_spend_pct_gdp , data = training_set2_2)
summary(model2_2)
anova(model2_2)
#This time we find some regions are not significant and we will classify some regions as "Others" therefore.
#MSE becomes larger.

#Test the model
test_set_with_resid2_2<-test_set2_2%>%
  add_predictions(model2_2)%>%
  add_residuals(model2_2)
test_set_with_resid2_2

test_mse2_2<-sum((test_set_with_resid2_2$resid)^2)/(21)
test_mse2_2
test_mse_biased2_2<-sum((test_set_with_resid2_2$resid)^2)/(33)
test_mse_biased2_2
#Biased and unbiased mse of the model becomes smaller than those of original model.

#Try to modify the mlr model again by selecting variables
training_set2_3<-training_set2_2%>%
  mutate(region=fct_recode(region,
                           "Others"="Africa",
                           "Others"="Eurasia",
                           "Others"="Oceania",
                           "Others"="Southeast Asia",
                           "Others"="Asia"))
test_set2_3<-test_set2_2%>%  mutate(region=fct_recode(region,
                                                      "Others"="Africa",
                                                      "Others"="Eurasia",
                                                      "Others"="Oceania",
                                                      "Others"="Southeast Asia",
                                                      "Others"="Asia"))%>%
  mutate(region=fct_relevel(region,"Others"))

model2_3<-lm(formula = life_exp_at_birth ~ infant_mortality_rate +
               death_rate + region + urbanization + birth_rate +
               health_spend_pct_gdp , data = training_set2_3)
summary(model2_3)
anova(model2_3)
#Now as we can see from t and f test, every variable in this model is significant in training set.
#Let's see the performance of the model in test set.

#Test the model
test_set_with_resid2_3<-test_set2_3%>%
  add_predictions(model2_3)%>%
  add_residuals(model2_3)
test_set_with_resid2_3

test_mse2_3<-sum((test_set_with_resid2_3$resid)^2)/(26)
test_mse2_3
test_mse_biased2_3<-sum((test_set_with_resid2_3$resid)^2)/(33)
test_mse_biased2_3
#Unbiased mse becomes smaller and biased estimate of variance is almost same as that of the last model.
AIC(model2_3)

##Diagnostics: Multicolinearity
#View covariate matrix of the variates in the model
view(cor(vars2_2%>%select(infant_mortality_rate,death_rate,urbanization,birth_rate,health_spend_pct_gdp)))
#We find infant_mortality_rate and birth_rate are highly correlated.

#Compare the three models: with both two, without birth_rate, without infant_mortality_rate
#with both two
model2_4<-lm(formula = life_exp_at_birth ~ infant_mortality_rate +
               death_rate + region + urbanization + birth_rate+
               health_spend_pct_gdp  , data = training_set2_3)
summary(model2_4)
anova(model2_4)

test_set_with_resid2_4<-test_set2_3%>%
  add_predictions(model2_4)%>%
  add_residuals(model2_4)

test_mse2_4<-sum((test_set_with_resid2_4$resid)^2)/(26)
test_mse2_4
test_mse_biased2_4<-sum((test_set_with_resid2_4$resid)^2)/(33)
test_mse_biased2_4

#without birth_rate
model2_5<-lm(formula = life_exp_at_birth ~ infant_mortality_rate +
               death_rate + region + urbanization +
               health_spend_pct_gdp  , data = training_set2_3)
summary(model2_5)
anova(model2_5)

test_set_with_resid2_5<-test_set2_3%>%
  add_predictions(model2_5)%>%
  add_residuals(model2_5)

test_mse2_5<-sum((test_set_with_resid2_5$resid)^2)/(27)
test_mse2_5
test_mse_biased2_5<-sum((test_set_with_resid2_5$resid)^2)/(33)
test_mse_biased2_5

#without infant_mortality_rate
model2_6<-lm(formula = life_exp_at_birth ~ birth_rate +
               death_rate + region + urbanization +
               health_spend_pct_gdp  , data = training_set2_3)
summary(model2_6)
anova(model2_6)

test_set_with_resid2_6<-test_set2_3%>%
  add_predictions(model2_6)%>%
  add_residuals(model2_6)

test_mse2_6<-sum((test_set_with_resid2_6$resid)^2)/(27)
test_mse2_6
test_mse_biased2_6<-sum((test_set_with_resid2_6$resid)^2)/(33)
test_mse_biased2_6
#The performance of the first model is the best and that of last model is the worst. So we can't delete either of the variables.
#So,we still choose model2_3



##Diagnostics: Heteroscedasticity
#Try weighted Least Squares
training_set_with_resid2_3<-training_set2_3%>%
  add_predictions(model2_3)%>%
  add_residuals(model2_3)
training_set_with_resid2_3

cal.weights <- 1 / lm(abs(training_set_with_resid2_3$resid) ~ training_set_with_resid2_3$pred)$fitted.values^2
model2_7<-lm(formula = life_exp_at_birth ~ infant_mortality_rate +
               death_rate + region + urbanization + birth_rate +
               health_spend_pct_gdp , data = training_set2_3,
               weights = cal.weights)
summary(model2_7)
anova(model2_7)

test_set_with_resid2_7<-test_set2_3%>%
  add_predictions(model2_7)%>%
  add_residuals(model2_7)

test_mse2_7<-sum((test_set_with_resid2_7$resid)^2)/(26)
test_mse2_7
test_mse_biased2_7<-sum((test_set_with_resid2_7$resid)^2)/(33)
test_mse_biased2_7
#Weighted model performs a little bit worse than unweighted one. But this might be caused by other reasons.

#View the result
training_set_with_resid2_7<-training_set2_3%>%
  add_predictions(model2_7)%>%
  add_residuals(model2_7)

resid_plot_in_trainingset2_7<-ggplot(data=training_set_with_resid2_7,
                                     aes(x=pred,y=resid))+
  geom_ref_line(h=0,colour="red")+
  geom_point()+
  geom_smooth(se=F)
resid_plot_in_trainingset2_7

resid_plot_in_testset2_7<-ggplot(data=test_set_with_resid2_7,
                                 aes(x=pred,y=resid))+
  geom_ref_line(h=0,colour="red")+
  geom_point()+
  geom_smooth(se=F)
resid_plot_in_testset2_7
#Sample size of test set is too small to conclude which model is better. But variance of residuals is smaller in weighted model. Theoratically, the model with weight can perform better when sample size is large.
#The reason of bad performance of weighted model might be the exsitence of outliers.

#Try logY
cal.weights <- 1 / lm(abs(training_set_with_resid2_3$resid) ~ training_set_with_resid2_3$pred)$fitted.values^2
model2_8<-lm(formula = log(life_exp_at_birth) ~ infant_mortality_rate +
               death_rate + region + urbanization + birth_rate +
               health_spend_pct_gdp , data = training_set2_3,
             weights = cal.weights)
summary(model2_8)
anova(model2_8)

test_set_with_resid2_8<-test_set2_3%>%
  add_predictions(model2_8)%>%
  mutate(pred=exp(pred))%>%
  mutate(resid=life_exp_at_birth-pred)

test_mse2_8<-sum((test_set_with_resid2_8$resid)^2)/(26)
test_mse2_8
test_mse_biased2_8<-sum((test_set_with_resid2_8$resid)^2)/(33)
test_mse_biased2_8

training_set_with_resid2_8<-training_set2_3%>%
  add_predictions(model2_8)%>%
  mutate(pred=exp(pred))%>%
  mutate(resid=life_exp_at_birth-pred)

resid_plot_in_trainingset2_8<-ggplot(data=training_set_with_resid2_8,
                                     aes(x=pred,y=resid))+
  geom_ref_line(h=0,colour="red")+
  geom_point()+
  geom_smooth(se=F)
resid_plot_in_trainingset2_8

resid_plot_in_testset2_8<-ggplot(data=test_set_with_resid2_8,
                                 aes(x=pred,y=resid))+
  geom_ref_line(h=0,colour="red")+
  geom_point()+
  geom_smooth(se=F)
resid_plot_in_testset2_8
#logY performs very well in training set but not so well in test set. 

#Try Box-Cox transformation


##Diagnostics: Outliers
#Try robust regression
library(MASS)
model2_9<-rlm(formula = life_exp_at_birth ~ infant_mortality_rate +
               death_rate + region + urbanization + birth_rate +
               health_spend_pct_gdp , data = training_set2_3)
summary(model2_9)
anova(model2_9)

test_set_with_resid2_9<-test_set2_3%>%
  add_predictions(model2_9)%>%
  add_residuals(model2_9)

test_mse2_9<-sum((test_set_with_resid2_9$resid)^2)/(26)
test_mse2_9
test_mse_biased2_9<-sum((test_set_with_resid2_9$resid)^2)/(33)
test_mse_biased2_9

training_set_with_resid2_9<-training_set2_3%>%
  add_predictions(model2_9)%>%
  add_residuals(model2_9)

resid_plot_in_trainingset2_9<-ggplot(data=training_set_with_resid2_9,
                                     aes(x=pred,y=resid))+
  geom_ref_line(h=0,colour="red")+
  geom_point()+
  geom_smooth(se=F)
resid_plot_in_trainingset2_9

resid_plot_in_testset2_9<-ggplot(data=test_set_with_resid2_9,
                                 aes(x=pred,y=resid))+
  geom_ref_line(h=0,colour="red")+
  geom_point()+
  geom_smooth(se=F)
resid_plot_in_testset2_9
#Robust Regression performs almost the same as model2_3
