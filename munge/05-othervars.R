

# Additional variables from mainly SHF ------------------------------------

pdata <- pdata %>%
  mutate(
    shf_indexyear_cat = case_when(
      shf_indexyear <= 2010 ~ "2006-2010",
      shf_indexyear <= 2015 ~ "2011-2015",
      shf_indexyear <= 2018 ~ "2016-2018"
    ),

    shf_ef_cat = factor(case_when(
      shf_ef == ">=50" ~ 3,
      shf_ef == "40-49" ~ 2,
      shf_ef %in% c("30-39", "<30") ~ 1
    ),
    labels = c("HFrEF", "HFmrEF", "HFpEF"),
    levels = 1:3
    ),

    shf_smoking_cat = factor(case_when(
      shf_smoking %in% c("Never") ~ 1,
      shf_smoking %in% c("Former", "Current") ~ 2
    ),
    labels = c("Never", "Former/Current"),
    levels = 1:2
    ),

    shf_device_cat = factor(case_when(
      is.na(shf_device) ~ NA_real_,
      shf_device %in% c("CRT", "CRT & ICD", "ICD") ~ 2,
      TRUE ~ 1
    ),
    labels = c("No", "CRT/ICD"),
    levels = 1:2
    ),

    shf_bmi_cat = case_when(
      is.na(shf_bmi) ~ NA_character_,
      shf_bmi < 30 ~ "<30",
      shf_bmi >= 30 ~ ">=30"
    ),

    shf_gfrckdepi_cat = factor(case_when(
      is.na(shf_gfrckdepi) ~ NA_real_,
      shf_gfrckdepi >= 60 ~ 1,
      shf_gfrckdepi < 60 ~ 2,
    ),
    labels = c(">=60", "<60"),
    levels = 1:2
    ),

    shf_sos_com_af = case_when(
      sos_com_af == "Yes" |
        shf_af == "Yes" |
        shf_ekg == "Atrial fibrillation" ~ "Yes",
      TRUE ~ "No"
    ),

    shf_sos_com_ihd = case_when(
      sos_com_ihd == "Yes" |
        shf_revasc == "Yes" |
        sos_com_pci == "Yes" |
        sos_com_cabg == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),

    shf_sos_com_hypertension = case_when(
      shf_hypertension == "Yes" |
        sos_com_hypertension == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),

    shf_sos_com_diabetes = case_when(
      shf_diabetes == "Yes" |
        sos_com_diabetes == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),

    ddr_rasarni = case_when(
      ddr_acei == "Yes" | ddr_arb == "Yes" | ddr_arni == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),

    ht = factor(case_when(
      (shf_bpsys >= 140 | shf_sos_com_diabetes == "Yes" & shf_bpsys >= 135) &
        ddr_ccbl == "Yes" & ddr_diuretic == "Yes" & ddr_rasarni == "Yes" ~ 3,
      shf_bpsys >= 140 | shf_sos_com_diabetes == "Yes" & shf_bpsys >= 135 ~ 2,
      TRUE ~ 1
    ),
    levels = 1:3,
    labels = c("Normal", "Non-resistant HT", "Resistant HT")
    ),
    
    htsens = factor(case_when(
      shf_bpsys >= 130 &
        ddr_ccbl == "Yes" & ddr_diuretic == "Yes" & ddr_rasarni == "Yes" ~ 3,
      shf_bpsys < 130 &
        ddr_ccbl == "Yes" & ddr_diuretic == "Yes" & ddr_rasarni == "Yes" & 
        (ddr_abl == "Yes" | ddr_mra == "Yes" | ddr_bbl == "Yes") ~ 3,
      shf_bpsys >= 130 ~ 2,
      TRUE ~ 1
    ),
    levels = 1:3,
    labels = c("Normal", "Non-resistant HT", "Resistant HT")
    ),

    # Outcomes

    # composite outcome
    sos_out_deathcvhosphf = case_when(
      sos_out_deathcv == "Yes" |
        sos_out_hosphf == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),

    # 3 yr
    sos_out_deathcvhosphf = ifelse(sos_outtime_hosphf <= 3 * 365, as.character(sos_out_deathcvhosphf), "No"),
    sos_out_hosphf = ifelse(sos_outtime_hosphf <= 3 * 365, as.character(sos_out_hosphf), "No"),
    sos_outtime_hosphf = pmin(sos_outtime_hosphf, 365 * 3),

    sos_out_deathcv = ifelse(sos_outtime_death <= 3 * 365, as.character(sos_out_deathcv), "No"),
    sos_out_death = ifelse(sos_outtime_death <= 3 * 365, as.character(sos_out_death), "No"),
    sos_outtime_death = pmin(sos_outtime_death, 365 * 3),

    # competing event outcome
    sos_out_deathcvhosphf_cr = create_crevent(sos_out_deathcvhosphf, sos_out_death),
    sos_out_deathcv_cr = create_crevent(sos_out_deathcv, sos_out_death),
    sos_out_hosphf_cr = create_crevent(sos_out_hosphf, sos_out_death)
  )


# income

inc <- pdata %>%
  group_by(shf_indexyear) %>%
  summarise(incmed = quantile(scb_dispincome,
    probs = 0.5,
    na.rm = TRUE
  ), .groups = "drop_last")

pdata <- left_join(
  pdata,
  inc,
  by = "shf_indexyear"
) %>%
  mutate(
    scb_dispincome_cat2 = case_when(
      scb_dispincome < incmed ~ 1,
      scb_dispincome >= incmed ~ 2
    ),
    scb_dispincome_cat2 = factor(scb_dispincome_cat2,
      levels = 1:2,
      labels = c("Below medium", "Above medium")
    )
  ) %>%
  select(-incmed)

pdata <- pdata %>%
  mutate_if(is_character, factor)
