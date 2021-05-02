# Precleaning 
library(RCT)
library(tidyverse)


load('Bases input/universo.Rdata')

glimpse(universo)

# Phone validated en lugar en lugar de curp 
universo<-
  universo %>% 
  rename(phone_validated = curp_verified)

summary(universo$age  )

table(universo$phone_validated, universo$never_trade)

universo$price<-NULL
universo$device<-NULL
universo$never_trade<-NULL

summary(universo$amount_deposited_all_time_usd)
summary(universo$amount_withdrawn_all_time_usd)

# Compras 
universo<-
  universo %>% 
  rename(total_purchases = amount_deposited_all_time_usd)

universo$amount_withdrawn_all_time_usd<-NULL
universo$balance_usd<-NULL

glimpse(universo)

save(universo, file = 'Bases input/universo.Rdata')


# Paso 2: cargamos universo_asignado 
universo_1 <-universo

load('Bases input/universo_asignado.Rdata')
universo_2<-universo
load('Bases input/base_evaluacion.Rdata')

universo<-
  universo %>% 
  filter(population !='New non organic')

universo$user_id<-NULL

universo<-
  universo %>% 
  select(all_of(intersect(names(universo), names(universo_2))), net_deposits_amount, transacted, population)


universo_2<-
  universo_2 %>% 
  arrange(strata, treat, missfit)

universo<-
  universo %>% 
  arrange(strata,treat, missfit)



universo_f<-
  bind_cols(universo_2 %>% select(-treat2), universo %>% select(treat2, transacted, net_deposits_amount, population))


rm(universo)

rm(universo_2)


universo_1<-
  left_join(universo_1, universo_f %>% select(user_id, population))

universo<-universo_1


universo_f<-
  universo_f %>% 
  select(user_id, transacted, net_deposits = net_deposits_amount, treat, treat2, 
         strata, missfit, device_value, age_median, curp_verified, age, population)

rm(universo_1)


universo_f<-
  left_join(universo_f, universo %>% select(user_id, total_purchases))


universo_f<-
  universo_f %>% 
  rename(total_purchases_after = net_deposits, phone_verified = curp_verified )

universo_f<-
  universo_f %>% 
  mutate(total_purchases_after= if_else(total_purchases_after<0, 0, total_purchases_after))



en_ambas<-intersect(names(universo), names(universo_f))


universo_f<-
  left_join(universo_f, universo %>% select(user_id, gender_F, months_since_register))


rm(universo)

glimpse(universo_f)

universo_f<-
  universo_f %>% 
  mutate(population = if_else(population == 'Existing - Unactive', "Inactive", "Never Active"))



universo<-
  universo_f %>% 
  select(-c(treat, treat2, strata, missfit, age_median, transacted, total_purchases_after))


save(universo, file = 'Bases input/universo.Rdata')
save(universo_f, file = 'Bases input/base_evaluacion.Rdata')

rm(list = ls())
