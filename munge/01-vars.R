

# Variables for tabs/mods -------------------------------------------------


tabvars <- c(
  # demo
  "shf_sex",
  "shf_age",

  # organizational
  "shf_indexyear",
  "shf_indexyear_cat",
  "shf_followuphfunit",
  "shf_followuplocation",

  # clinical factors and lab measurments
  "shf_ef",
  "shf_durationhf",
  "shf_sos_com_hypertension",
  "sos_com_durationhypertension_cat",
  "shf_nyha",
  "shf_bmi",
  "shf_bmi_cat",
  "shf_bpsys",
  "shf_bpdia",
  "shf_map",
  "shf_heartrate",
  "shf_gfrckdepi",
  "shf_gfrckdepi_cat",
  "shf_potassium",
  "shf_hb",
  "shf_ntprobnp",
  "shf_ntprobnp_af",
  "shf_ntprobnp_noaf",

  # treatments
  "shf_rasarni",
  "ddr_rasarni",
  "ddr_acei",
  "ddr_arb",
  "ddr_arni",
  "shf_mra",
  "ddr_mra",
  "shf_diuretic",
  "ddr_diuretic",
  "ddr_thiazide",
  "ddr_loop",
  "ddr_ccbl",
  "shf_nitrate",
  "ddr_hydralazine",
  "shf_asaantiplatelet",
  "shf_anticoagulantia",
  "shf_statin",
  "shf_bbl",
  "ddr_bbl",
  "ddr_abl",
  "shf_device_cat",

  # comorbs
  "shf_smoking_cat",
  "shf_sos_com_diabetes",
  "shf_sos_com_ihd",
  "sos_com_peripheralartery",
  "sos_com_stroke",
  "shf_sos_com_af",
  "sos_com_valvular",
  "sos_com_liver",
  "sos_com_cancer3y",
  "sos_com_copd",

  # socec
  "scb_famtype",
  "scb_child",
  "scb_education",
  "scb_dispincome_cat2"
)

# vars fox log reg and cox reg
tabvars_not_in_mod <- c(
  "shf_followuphfunit",
  "shf_followuplocation",
  "shf_ef",
  "shf_indexyear",
  "shf_sos_com_hypertension",
  "sos_com_durationhypertension_cat",
  "sos_com_durationhypertension",
  "shf_bpsys",
  "shf_bpdia",
  "shf_map",
  "shf_gfrckdepi",
  "shf_bmi",
  "shf_ntprobnp_af",
  "shf_ntprobnp_noaf",
  
  # treatments
  "shf_rasarni",
  "ddr_rasarni",
  "ddr_acei",
  "ddr_arb",
  "ddr_arni",
  "shf_mra",
  "ddr_mra",
  "shf_diuretic",
  "ddr_diuretic",
  "ddr_thiazide",
  "ddr_loop",
  "ddr_ccbl",
  "shf_nitrate",
  "ddr_hydralazine",
  "shf_asaantiplatelet",
  "shf_anticoagulantia",
  "shf_statin",
  "shf_bbl",
  "ddr_bbl",
  "ddr_abl",
  "shf_device_cat"
)

modvars <- tibble(
  var = tabvars[!(tabvars %in% tabvars_not_in_mod)],
  unit = 1
)
modvars <- modvars %>%
  mutate(unit = case_when(
    var %in% c("shf_age", "shf_heartrate", "shf_hb") ~ 5,
    var %in% c("shf_ntprobnp") ~ 500,
    var %in% c("sos_com_durationhypertension") ~ 365,
    TRUE ~ unit
  ))
