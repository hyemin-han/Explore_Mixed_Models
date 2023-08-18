source('../model_test.R')

# load aligned data
load((file='Vaccine_aligned.RData'))

# standardize willingness and VAQB
data.filtered$vx <- scale(data.filtered$vx)
data.filtered$consp <- scale(data.filtered$consp)
data.filtered$anti <- scale(data.filtered$anti)
data.filtered$trust_6 <- scale(data.filtered$trust_6)
data.filtered$trust_7 <- scale(data.filtered$trust_7)


# formula (full)

f <- vx ~ trust_6+consp+gender + education + work_location + age+
  SSS_faml+ relationship_status

# test
start_100 <-Sys.time()
text.exp <- explore.models(data.filtered, f, group = 'residing_country',
                           slopes = c('trust_6','consp'),
                           must = c('SSS_faml','gender','education',
                                    'age','work_location','relationship_status'),
                           cores = 4)
end_100<-Sys.time()
elapsed_100 <- end_100 - start_100




# sort
test.exp.aic <- sort.result(text.exp,'AIC')
test.exp.bic <- sort.result(text.exp,'BIC')


# test
library(brms)

# bic model/full
start_bic <-Sys.time()
prior.coef <- brms::prior(cauchy(0.,1),class='b')
test.bic <- brm(formula(test.exp.bic[1,1]),data=data.filtered,
                family = gaussian(),
                cores=4,chains=4, save_pars = save_pars(all = T),
                sample_prior ='yes', seed=1660415,
                prior=prior.coef)
end_bic<-Sys.time()
elapsed_bic <- end_bic - start_bic

# null model
start_0 <-Sys.time()
prior.coef <- brms::prior(cauchy(0.,1),class='b')
test.0 <- brm( vx ~ age +
                 gender + education + work_location + age +
                 SSS_faml + relationship_status,data=data.filtered,
               family = gaussian(),
               cores=4,chains=4, save_pars = save_pars(all = T),
               sample_prior ='yes', seed=1660415,
               prior=prior.coef)
end_0<-Sys.time()
elapsed_0 <- end_0 - start_0

# bfs

bf.bic <- bayes_factor(test.bic,test.0,log=T)

save.image('test_vaccine.RData')

