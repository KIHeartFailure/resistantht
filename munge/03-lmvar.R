
lmtmp <- left_join(pdata, lm, by = "LopNr")

lmtreats <- function(atc, treatname) {
  lmtmp2 <- lmtmp %>%
    mutate(
      atcneed = stringr::str_detect(ATC, atc),
      diff = as.numeric(EDATUM - shf_indexdtm)
    ) %>%
    filter(
      atcneed,
      diff > -30.5 * 6
    )

  treatname <- paste0("ddr_", treatname)

  lmtmp2 <- lmtmp2 %>%
    group_by(LopNr) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(!!treatname := "Yes") %>%
    select(LopNr, !!sym(treatname))

  pdata <<- left_join(pdata,
    lmtmp2,
    by = "LopNr"
  ) %>%
    mutate(!!treatname := replace_na(!!sym(treatname), "No"))

  metatmp <- c(treatname, stringr::str_replace_all(atc, "\\|", ","))
  if (exists("metalm")) {
    metalm <<- rbind(metalm, metatmp) # global variable, writes to global env
  } else {
    metalm <<- metatmp # global variable, writes to global env
  }
}

lmtreats("^(C09A|C09B|C10BX04|C10BX06|C10BX07|C10BX1[1-5]|C10BX17)", "acei")
lmtreats("^(C09C|C09D(?!X04)|C10BX10|C10BX16)", "arb")
lmtreats("^C09DX04", "arni")
lmtreats("^(C03(?!DA|XA)|C08GA|C02L|C07B|C07C|C07D|C09BA|C09DA|C09DX(?!02|04|05)|C10BX13|C09XA52|C09XA54|C09BX01|C09BX03)", "diuretic")
lmtreats("^(C03A|C03B(?!C|D)|C10BX13|C07B|C07D|C03EA|C09DX(?!02|04|05)|C09XA52|C09XA54|C09BX01|C09BX03)", "thiazide")
lmtreats("^(C03C|C03EB)", "loop")
lmtreats("^(C08|C07FB|C09BB|C09DB|C09DX(?!02|04|05)|C09XA53|C09XA54|C09BX01|C09BX03|C09BX04|C10BX03|C10BX07|C10BX09|C10BX11|C10BX14)", "ccbl")
lmtreats("^C03DA", "mra")
lmtreats("^C07", "bbl")
lmtreats("^(C02LE01|C02CA01|C02CA04|C02CA06|G04CA03)", "abl")
lmtreats("^C02DB02", "hydralazine")

pdata <- pdata %>%
  mutate_if(is_character, factor)

colnames(metalm) <- c("Variable", "ATC")
metalm <- metalm %>%
  as_tibble() %>%
  mutate(
    ATC = gsub("^", "", ATC, fixed = TRUE),
    ATC = gsub("(", "", ATC, fixed = TRUE),
    ATC = gsub(")", "", ATC, fixed = TRUE),
    ATC = gsub("?!", " excl.", ATC, fixed = TRUE),
    Registry = "Dispensed Drug Registry",
    Period = "-6mo",
  )
