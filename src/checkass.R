
ProjectTemplate::reload.project()

dataass <- mice::complete(imp, 3)



mod <- coxph(formula(paste0(
  "Surv(sos_outtime_hosphf, sos_out_deathcvhosphf == 'Yes') ~ ht + ",
  paste(modvars$var, collapse = " + ")
)), data = dataass %>% filter(shf_ef_cat == "HFrEF"))


print(testpat <- cox.zph(mod))
(sig <- testpat$table[testpat$table[, 3] < 0.05, ])

# check spec for ht, ok
plot(testpat[1], resid = F, ylim = c(-4, 4))

fit <- survfit(Surv(sos_outtime_hosphf, sos_out_deathcvhosphf=='Yes') ~ ht,
               data = pdata %>% filter(shf_ef_cat == "HFrEF"))
plot(fit)


mod <- coxph(formula(paste0(
  "Surv(sos_outtime_hosphf, sos_out_deathcvhosphf == 'Yes') ~ ht + ",
  paste(modvars$var, collapse = " + ")
)), data = dataass %>% filter(shf_ef_cat == "HFmrEF"))


print(testpat <- cox.zph(mod))
(sig <- testpat$table[testpat$table[, 3] < 0.05, ])

# check spec for ht, ok
plot(testpat[1], resid = F, ylim = c(-4, 4))


mod <- coxph(formula(paste0(
  "Surv(sos_outtime_hosphf, sos_out_deathcvhosphf == 'Yes') ~ ht + ",
  paste(modvars$var, collapse = " + ")
)), data = dataass %>% filter(shf_ef_cat == "HFpEF"))


print(testpat <- cox.zph(mod))
(sig <- testpat$table[testpat$table[, 3] < 0.05, ])

# check spec for ht, ok
plot(testpat[1], resid = F, ylim = c(-4, 4))



## 5 yr
mod <- coxph(formula(paste0(
  "Surv(sos_outtime_hosphf5yr, sos_out_deathcvhosphf5yr == 'Yes') ~ ht + ",
  paste(modvars$var, collapse = " + ")
)), data = dataass %>% filter(shf_ef_cat == "HFrEF"))


print(testpat <- cox.zph(mod))
(sig <- testpat$table[testpat$table[, 3] < 0.05, ])

plot(testpat[1], resid = F, ylim = c(-4, 4))

fit <- survfit(Surv(sos_outtime_hosphf3yr, sos_out_deathcvhosphf3yr=='Yes') ~ ht,
               data = pdata %>% filter(shf_ef_cat == "HFrEF"))
plot(fit)


mod <- coxph(formula(paste0(
  "Surv(sos_outtime_hosphf5yr, sos_out_deathcvhosphf5yr == 'Yes') ~ ht + ",
  paste(modvars$var, collapse = " + ")
)), data = dataass %>% filter(shf_ef_cat == "HFmrEF"))


print(testpat <- cox.zph(mod))
(sig <- testpat$table[testpat$table[, 3] < 0.05, ])

plot(testpat[1], resid = F, ylim = c(-4, 4))


mod <- coxph(formula(paste0(
  "Surv(sos_outtime_hosphf5yr, sos_out_deathcvhosphf5yr == 'Yes') ~ ht + ",
  paste(modvars$var, collapse = " + ")
)), data = dataass %>% filter(shf_ef_cat == "HFpEF"))


print(testpat <- cox.zph(mod))
(sig <- testpat$table[testpat$table[, 3] < 0.05, ])


plot(testpat[1], resid = F, ylim = c(-4, 4))
