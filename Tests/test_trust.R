source('../model_test.R')

library(countrycode)

# load data
data<-read.csv('Final_COVIDiSTRESS_Vol2_cleaned.csv')
data$gender <- as.factor(data$gender)
data$education <- as.factor(data$education)

# related variable names
var.trust <- c('trust_1','trust_2','trust_3','trust_4','trust_5','trust_6',
               'trust_7')

# demo vars
var.demo <- c('age','gender','education')


# drop countries with < 100 for error prevention

n.cons <- table(data$residing_country)
list.cons <- labels(n.cons)[[1]]
cons.include <- list.cons[n.cons>=100]
c.include <- n.cons[n.cons>=100]

# extract data
for (i in 1:length(cons.include)){
  if (i == 1){
    data.mi <- data[data$residing_country == cons.include[i],]
  }else{
    current <- data[data$residing_country == cons.include[i],]
    data.mi <- rbind(data.mi,current)
  }
}

# change country code

data.mi$country <- countrycode::countryname(data.mi$residing_country,
                                            destination = 'iso3n')
data.mi$country<-as.factor(data.mi$country)

DATA <- data
data<-data.mi


## vacination intent?

f <- vaccine_midneutral~trust_1+trust_2+trust_3+trust_4+
  trust_5+trust_6+trust_7+age+gender+
  education

# test
start_100 <-Sys.time()
text.exp <- explore.models(data, f, group = 'residing_country',
                           slopes = var.trust,
                           must = c('age','gender','education'),cores = 4)
end_100<-Sys.time()
elapsed_100 <- end_100 - start_100

# sort
test.exp.aic <- sort.result(text.exp,'AIC')
test.exp.bic <- sort.result(text.exp,'BIC')

# test
library(brms)

# bic model
start_bic <-Sys.time()
prior.coef <- brms::prior(cauchy(0.,1),class='b')
test.bic <- brm(formula(test.exp.bic[1,1]),data=data,
                family = gaussian(),
                cores=4,chains=4, save_pars = save_pars(all = T),
                sample_prior ='yes', seed=1660415,
                prior=prior.coef)
end_bic<-Sys.time()
elapsed_bic <- end_bic - start_bic

# aic

start_aic <-Sys.time()
prior.coef <- brms::prior(cauchy(0.,1),class='b')
test.aic <- brm(formula(test.exp.aic[1,1]),data=data,
                family = gaussian(),
                cores=4,chains=4, save_pars = save_pars(all = T),
                sample_prior ='yes', seed=1660415,
                prior=prior.coef)
end_aic<-Sys.time()
elapsed_aic <- end_aic - start_aic

# null model
start_0 <-Sys.time()
prior.coef <- brms::prior(cauchy(0.,1),class='b')
test.0 <- brm( vaccine_midneutral ~ age +
                 gender + education,data=data,
                family = gaussian(),
                cores=4,chains=4, save_pars = save_pars(all = T),
                sample_prior ='yes', seed=1660415,
                prior=prior.coef)
end_0<-Sys.time()
elapsed_0 <- end_0 - start_0

# full model
start_f <-Sys.time()
prior.coef <- brms::prior(cauchy(0.,1),class='b')
test.f <- brm( formula (text.exp[nrow(text.exp),1]),data=data,
               family = gaussian(),
               cores=4,chains=4, save_pars = save_pars(all = T),
               sample_prior ='yes', seed=1660415,
               prior=prior.coef)
end_f<-Sys.time()
elapsed_f <- end_f - start_f

# bfs

bf.aic <- bayes_factor(test.aic,test.0,log=T)
bf.bic <- bayes_factor(test.bic,test.0,log=T)
bf.full <- bayes_factor(test.f,test.0,log=T)

save.image('test_trust.RData')
