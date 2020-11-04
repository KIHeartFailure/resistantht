
ProjectTemplate::reload.project()
memory.limit(size = 20000000000)

# Import LM from SoS -----------------------------------------------------

sospath <- "C:/Users/Lina/STATISTIK/Projects/20200225_shfdb3/dm/raw-data/SOS/lev3_15875_2019 Lina Benson/"

lm <- read_sas(paste0(sospath, "t_r_lmed__15875_2019.sas7bdat"))
lm <- zap_formats(lm)
lm <- zap_label(lm)

# Select ATC codes --------------------------------------------------------

lm <- lm %>%
  mutate(atcneed = stringr::str_detect(ATC, "^(C0|G04)")) %>%
  filter(
    Fall == 1,
    ANTAL >= 0,
    AR <= 2018, 
    atcneed
  ) %>%
  select(-atcneed)

# Store as RData in /data folder ------------------------------------------

save(file = "./data/lm.RData", list = c("lm"))


# Patient registry from SHFDB3, prepared in 08-prep_sosdata.R -----

#load(file = "C:/Users/Lina/STATISTIK/Projects/20200225_shfdb3/dm/data/patreg.RData")

# Store as RData in /data folder ------------------------------------------

#save(file = "./data/patreg.RData", list = c("patreg"))