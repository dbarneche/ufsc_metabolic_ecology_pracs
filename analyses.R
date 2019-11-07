resp  <-  read.csv('data/data.csv', header = TRUE, stringsAsFactors = FALSE)
names(resp)[3]  <-  'mass_specific_respiration_rate_gC_per_d_g'
resp$respiration_rate_gC_per_d  <-  resp$mass_specific_respiration_rate_gC_per_d_g * resp$individual_mass_g

plot(respiration_rate_gC_per_d ~ individual_mass_g, data = resp, log = "xy")
plot(log(respiration_rate_gC_per_d) ~ log(individual_mass_g), data = resp)

model  <-  lm(log(respiration_rate_gC_per_d) ~ log(individual_mass_g), data = resp)
summary(model)
plot(model)

library(lme4)
model_lmer  <-  lmer(log(respiration_rate_gC_per_d) ~ log(individual_mass_g) + (log(individual_mass_g) | family), data = resp)
summary(model_lmer)
desvios  <-  ranef(model_lmer)$family
hist(desvios[, 2])
valores  <-  coef(model_lmer)$family
hist(valores[, 2])
fixos   <-  fixef(model_lmer)
ranges  <-  range(log(resp$individual_mass_g))
preds   <-  fixos['(Intercept)'] + fixos['log(individual_mass_g)'] * ranges

plot(log(respiration_rate_gC_per_d) ~ log(individual_mass_g), data = resp)

for (i in 1:nrow(valores)) {
    ranges_i  <-  range(log(resp$individual_mass_g[resp$family == rownames(valores)[i]]))
    preds_i   <-  valores[i, '(Intercept)'] + valores[i, 'log(individual_mass_g)'] * ranges_i
    lines(ranges_i, preds_i, col = 'dodgerblue2', lwd = 0.8, lty = 2)
}
lines(ranges, preds, col = 'tomato', lwd = 3, lty = 2)


k                         <-  8.62e-5
resp$temperature_kelvin   <-  resp$temperature_celsius + 273.15
resp$inverse_temperature  <-  1 / (k * resp$temperature_kelvin)
resp$lnMass_g             <-  log(resp$individual_mass_g)
resp$lnResp_rate_gC_per_d <-  log(resp$respiration_rate_gC_per_d)

complete_model  <-  lm(lnResp_rate_gC_per_d ~ lnMass_g + inverse_temperature, data = resp)
summary(complete_model)

# estatistcamente a equação tem forma:
# lnResp_rate_gC_per_d ~ lnMass_g + inverse_temperature
# mas matematicamente, o equivalente é na verdade:
# lnResp_rate_gC_per_d = ln(c) + alpha * lnMass_g + Ea * inverse_temperature
# notando que os parâmetros ln(c), alpha e Ea correspondem respectivamente ao intercepto, slope do lnMass_g e slope da temperatura inversa.

parametros  <-  coef(complete_model)
b0     <-  exp(parametros['(Intercept)'])
alpha  <-  parametros['lnMass_g'] # alpha
Ea     <-  parametros['inverse_temperature'] # Ea


resp$mass_cor_resp_rate_gC_per_d_g_alpha  <-  resp$respiration_rate_gC_per_d / (resp$individual_mass_g ^ alpha)
resp$temp_cor_resp_rate_gC_per_d_g_alpha  <-  resp$respiration_rate_gC_per_d / exp(resp$inverse_temperature * Ea)

plot(respiration_rate_gC_per_d ~ inverse_temperature, data = resp, log = 'y')
plot(mass_cor_resp_rate_gC_per_d_g_alpha ~ inverse_temperature, data = resp, log = 'y')

plot(respiration_rate_gC_per_d ~ lnMass_g, data = resp, log = 'y')

plot(temp_cor_resp_rate_gC_per_d_g_alpha ~ lnMass_g, data = resp, log = 'y')
10^0.75
exp(1/k * (-Ea/303 + Ea/294))



set.seed(1)
av_survey  <-  data.frame(year = rep(1:5, each = 12), month = rep(1:12, 5))
av_survey$average_size_cm  <-  abs(rnorm(nrow(av_survey), mean = 5 + 1.5 * av_survey$year, sd = 0.5))
av_survey$density = 10 * av_survey$average_size_cm^-alpha + rnorm(nrow(av_survey), 1, 0.1)

av_survey$pop_resp  <-  av_survey$density * b0 * av_survey$average_size_cm^alpha * exp(Ea / (k * 293.15))
plot(av_survey$pop_resp)
plot(density ~ average_size_cm, data = av_survey)
plot(pop_resp ~ average_size_cm, data = av_survey)

