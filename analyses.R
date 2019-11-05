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
