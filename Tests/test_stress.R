source('../model_test.R')

# load stress data file
load('Stress_aligned.RData')


# standardize
data.filtered$primary_stressor_avg <- scale(data.filtered$primary_stressor_avg)
data.filtered$secondary <- scale(data.filtered$secondary)
data.filtered$pss <- scale(data.filtered$pss)
data.filtered$sps <- scale(data.filtered$sps)
data.filtered$resilience <- scale(data.filtered$resilience)
data.filtered$identity <- scale(data.filtered$identity)
data.filtered$age <- scale(data.filtered$age)
data.filtered$SSS_faml <- scale(data.filtered$SSS_faml)

# formula (full)

f <- pss ~ primary_stressor_avg + secondary + sps + identity +
  SSS_faml + gender

# test
start_100 <-Sys.time()
text.exp <- explore.models(data.filtered, f, group = 'residing_country',
                           slopes = c('primary_stressor_avg','secondary',
                                      'sps','identity'),
                           must = c('SSS_faml','gender'),cores = 4)
end_100<-Sys.time()
elapsed_100 <- end_100 - start_100

# sort
test.exp.aic <- sort.result(text.exp,'AIC')
test.exp.bic <- sort.result(text.exp,'BIC')

# null vs full vs aic vs bic

library(brms)

start_0 <-Sys.time()
prior.coef <- brms::prior(cauchy(0.,1),class='b')
test.0 <- brm(formula(text.exp[16,1]),data=data.filtered,
               family = gaussian(),
               cores=4,chains=4, save_pars = save_pars(all = T),
               sample_prior ='yes', seed=1660415, prior=prior.coef
              )
end_0<-Sys.time()
elapsed_0 <- end_0 - start_0

start_full <-Sys.time()
prior.coef <- brms::prior(cauchy(0.,1),class='b')
test.full <- brm(formula(text.exp[97,1]),data=data.filtered,
               family = gaussian(),
               cores=4,chains=4, save_pars = save_pars(all = T),
               sample_prior ='yes', seed=1660415,
               prior=prior.coef)
end_full<-Sys.time()
elapsed_full <- end_full - start_full


# best bic: 

start_bic <-Sys.time()
prior.coef <- brms::prior(cauchy(0.,1),class='b')
test.bic <- brm(formula(text.exp[83,1]),data=data.filtered,
               family = gaussian(),
               cores=4,chains=4, save_pars = save_pars(all = T),
               sample_prior ='yes', seed=1660415,
               prior=prior.coef)
end_bic<-Sys.time()
elapsed_bic <- end_bic - start_bic


# best aic: 

start_aic <-Sys.time()
prior.coef <- brms::prior(cauchy(0.,1),class='b')
test.aic <- brm(formula(test.exp.aic[1,1]),data=data.filtered,
                family = gaussian(),
                cores=4,chains=4, save_pars = save_pars(all = T),
                sample_prior ='yes', seed=1660415,
                prior=prior.coef)
end_aic<-Sys.time()
elapsed_aic <- end_aic - start_aic

bf.aic <- bayes_factor(test.aic,test.0,log=T)
bf.bic <- bayes_factor(test.bic,test.0,log=T)
bf.full <- bayes_factor(test.full,test.0,log=T)

save.image('stress_test.RData')
