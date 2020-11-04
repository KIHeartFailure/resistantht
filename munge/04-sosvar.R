
patreg2 <- left_join(pdata %>% select(LopNr, shf_indexdtm), 
                     patreg, 
                     by = "LopNr") %>%
  mutate(sos_com_durationhypertension = as.numeric(difftime(shf_indexdtm, INDATUM, 
                          units = "days")),
         tmp_ht = stringr::str_detect(DIA_all, " I1[0-5]")) %>%
  dplyr::filter(sos_com_durationhypertension >= 0, 
                sos_com_durationhypertension <= 9 * 365.25, 
                tmp_ht)

patreg2 <- patreg2 %>%
  group_by(LopNr) %>%
  arrange(INDATUM) %>%
  slice(1) %>%
  ungroup() %>%
  select(LopNr, sos_com_durationhypertension)

pdata <- left_join(pdata, 
                   patreg2, 
                   by = "LopNr") %>%
  mutate(sos_com_durationhypertension_cat = factor(case_when(sos_com_durationhypertension > 5 * 365.25 ~ 4,
                                                      sos_com_durationhypertension > 2 * 365.25 ~ 3,
                                                      sos_com_durationhypertension >= 0 ~ 2,
                                                      TRUE ~ 1
                                                      ),
                                                   levels = 1:4, 
                                                   labels = c("No", "0-2yrs", "2-5yrs", ">5yrs")),
         sos_com_hypertension = case_when(sos_com_durationhypertension_cat == "No" ~ "No", 
                                          TRUE ~ "Yes") # overwrite existing ht variables since only 5 years back
         )

pdata <- pdata %>%
  mutate_if(is_character, factor)