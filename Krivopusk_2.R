#Создайте модель множественной линейной регрессии дневных потоков паров воды за летний период 2013 года по данным измерений методом турбулентной пульсации

library("tidyr")
library("tibble")
library("tidyverse") 
library("stringr") 
library("dplyr") 
library("ggplot2") 
#считаем данные и удалим ненужные строчки
tbl = read_csv("eddypro.csv", skip = 1, na=c("","NA","-9999","-9999.0"), comment=c("["))

tbl = tbl[-1, ]
tbl = select(tbl, -(roll))
tbl = tbl %>% mutate_if(is.character, factor)

# назовем столбцы допустимыми символами
names(tbl) = names(tbl) %>% 
  str_replace_all("[!]", "_emph_") %>% 
  str_replace_all("[?]", "_quest_") %>% 
  str_replace_all("[*]", "_star_") %>% 
  str_replace_all("[+]", "_plus_") %>%
  str_replace_all("[-]", "_minus_") %>%
  str_replace_all("[@]", "_at_") %>%
  str_replace_all("[$]", "_dollar_") %>%
  str_replace_all("[#]", "_hash_") %>%
  str_replace_all("[/]", "_div_") %>%
  str_replace_all("[%]", "_perc_") %>%
  str_replace_all("[&]", "_amp_") %>%
  str_replace_all("[\\^]", "_power_") %>%
  str_replace_all("[()]", "_")

glimpse(tbl)
# отберем данные с 1 июня по 31 августа и по дневному времени
tbl = drop_na(tbl)
tbl = filter(tbl, DOY >= 153 & DOY < 243)
tbl = filter(tbl, daytime==TRUE)
tbl_numeric = tbl[,sapply(tbl,is.numeric) ]
tbl_non_numeric = tbl[,!sapply(tbl,is.numeric) ]

cor_td = cor(tbl_numeric)
cor_td
cor_td = cor(drop_na(tbl_numeric)) %>% as.data.frame %>% select(h2o_flux)
vars = row.names(cor_td)[cor_td$h2o_flux^2 > .1] %>% na.exclude
# создадим обучающую и тестирующую выборки
row_numbers = 1:length(tbl_numeric$h2o_flux)
teach = sample(row_numbers, floor(length(tbl_numeric$h2o_flux)*.7))
test = row_numbers[-teach]
teaching_tbl = tbl_numeric[teach,]
testing_tbl = tbl_numeric[test,]
#модель 1
mod1 = lm(h2o_flux~ (.) , data = teaching_tbl)

summary(mod1)
coef(mod1)
resid(mod1)
confint(mod1)
anova(mod1)
plot(mod1) 

#модель 2
mod2 = lm(h2o_flux~ DOY + Tau + qc_Tau + rand_err_Tau + H + qc_H + rand_err_H + LE + qc_LE + rand_err_LE + co2_flux
          + qc_co2_flux + rand_err_co2_flux +rand_err_h2o_flux + H_strg + co2_v_minus_adv + h2o_v_minus_adv + co2_molar_density
          + co2_mole_fraction + co2_mixing_ratio + h2o_molar_density + h2o_mole_fraction + h2o_mixing_ratio + h2o_time_lag 
          + sonic_temperature + air_temperature + air_pressure + air_density + air_heat_capacity + air_molar_volume
          + water_vapor_density + e + es + specific_humidity + RH + VPD + Tdew + u_unrot + v_unrot + w_unrot + u_rot
          + v_rot + w_rot + max_speed + wind_dir + yaw + pitch + u_star_ + TKE + L + `_z_minus_d__div_L` + bowen_ratio 
          + T_star_ + x_peak + x_offset + x_10_perc_ + x_30_perc_ + x_50_perc_ + x_70_perc_ + x_90_perc_ + un_Tau 
          + Tau_scf + un_H + H_scf + un_LE + LE_scf + un_co2_flux + un_h2o_flux + u_spikes + v_spikes 
          + w_spikes + ts_spikes + co2_spikes + h2o_spikes + mean_value + u_var + v_var + ts_var + co2_var 
          + h2o_var + w_div_ts_cov + w_div_co2_cov + w_div_h2o_cov + co2...125 + h2o...126 + co2...127 
          + h2o...128 + co2_signal_strength_7200 + h2o_signal_strength_7200 + flowrate, data = teaching_tbl)


names(teaching_tbl)
summary(mod2)
coef(mod2)
resid(mod2)
confint(mod2)
anova(mod2)
anova(mod2, mod1)
plot(mod2)


#модель 3
mod3 = lm(h2o_flux~ DOY + Tau + qc_Tau + rand_err_Tau + H + qc_H + rand_err_H + LE + qc_LE + rand_err_LE + co2_flux
          + qc_co2_flux + rand_err_co2_flux +rand_err_h2o_flux + H_strg + co2_v_minus_adv + h2o_v_minus_adv + co2_molar_density
          + co2_mole_fraction + co2_mixing_ratio + h2o_molar_density + h2o_mole_fraction + h2o_mixing_ratio + h2o_time_lag 
          + sonic_temperature + air_temperature + air_pressure + air_density + air_heat_capacity + air_molar_volume
          + water_vapor_density + e + es + specific_humidity + RH + VPD + Tdew + u_unrot + v_unrot + w_unrot + u_rot
          + v_rot + w_rot + max_speed + wind_dir + yaw + pitch + u_star_ + TKE + L + `_z_minus_d__div_L` + bowen_ratio 
          + T_star_ + x_peak + x_offset + x_10_perc_ + x_30_perc_ + x_50_perc_ + x_70_perc_ + x_90_perc_ + un_Tau 
          + Tau_scf + un_H + H_scf + un_LE + LE_scf + un_co2_flux + un_h2o_flux + u_spikes + v_spikes 
          + w_spikes + ts_spikes + co2_spikes + h2o_spikes + mean_value + u_var + v_var + ts_var + co2_var 
          + h2o_var + w_div_ts_cov + w_div_co2_cov + w_div_h2o_cov + co2...125 + h2o...126 + co2...127 
          + h2o...128 + co2_signal_strength_7200 + h2o_signal_strength_7200 + flowrate, data = teaching_tbl)


summary(mod3)
coef(mod3)
resid(mod3)
confint(mod3)
anova(mod3)
anova(mod3, mod2)
plot(mod3)

#корреляционный анализ

cor_teaching_tbl = select(teaching_tbl, h2o_flux, DOY , Tau , qc_Tau , rand_err_Tau , H , qc_H , rand_err_H , LE , 
                          qc_LE , rand_err_LE , co2_flux , qc_co2_flux , rand_err_co2_flux ,rand_err_h2o_flux ,
                          H_strg , co2_v_minus_adv , h2o_v_minus_adv , co2_molar_density , co2_mole_fraction ,
                          co2_mixing_ratio , h2o_molar_density , h2o_mole_fraction , h2o_mixing_ratio , h2o_time_lag,
                          sonic_temperature, air_temperature , air_pressure , air_density , air_heat_capacity , 
                          air_molar_volume , water_vapor_density , e , es , specific_humidity , RH , VPD , Tdew , 
                          u_unrot , v_unrot , w_unrot , u_rot , v_rot , w_rot , max_speed , wind_dir , yaw ,
                          pitch , u_star_ , TKE , L , `_z_minus_d__div_L` , bowen_ratio , T_star_ , x_peak , 
                          x_offset , x_10_perc_ , x_30_perc_ , x_50_perc_ , x_70_perc_ , x_90_perc_ , un_Tau ,
                          Tau_scf , un_H , H_scf , un_LE , LE_scf , un_co2_flux , un_h2o_flux , u_spikes ,
                          v_spikes , w_spikes , ts_spikes , co2_spikes , h2o_spikes , mean_value , u_var , 
                          v_var , ts_var , co2_var , h2o_var , w_div_ts_cov , w_div_co2_cov , w_div_h2o_cov , 
                          co2...125 , h2o...126 , co2...127 , h2o...128 , co2_signal_strength_7200 , h2o_signal_strength_7200 , flowrate) 

cor_td = cor(cor_teaching_tbl) %>% as.data.frame

qplot(h2o_flux , h2o_flux, data = teaching_tbl) + geom_line(aes(y = predict(mod3, teaching_tbl)))
qplot(h2o_flux , h2o_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))
# примеры графиков
qplot(DOY, h2o_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))
qplot(Tau, h2o_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))
qplot(h2o_flux, h2o_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))

