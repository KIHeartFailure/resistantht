

# Inclusion/exclusion criteria --------------------------------------------------------

pdata <- rsdata315 %>%
  filter(casecontrol == "Case")

flow <- c("Number of posts (cases) in SHFDB3", nrow(pdata))

pdata <- pdata %>%
  filter(shf_location == "Out-patient")
flow <- rbind(flow, c("Only out-patient visits", nrow(pdata)))

pdata <- pdata %>%
  filter(shf_indexdtm >= ymd("2006-01-01"))
flow <- rbind(flow, c("Indexdate >= 1 Jan 2006 (start DDR 1 July 2005 + 6 months)", nrow(pdata)))

pdata <- pdata %>%
  filter(!is.na(shf_ef))
flow <- rbind(flow, c("No missing EF", nrow(pdata)))

pdata <- pdata %>%
  filter(!is.na(shf_bpsys))
flow <- rbind(flow, c("No missing systolic blood pressure", nrow(pdata)))

#pdata <- pdata %>%
#  filter(sos_outtime_death >= 14)
#flow <- rbind(flow, c(">=14 days follow-up (to avoid immortal time bias*)", nrow(pdata)))

pdata <- pdata %>%
  group_by(LopNr) %>%
  arrange(shf_indexdtm) %>%
  slice(1) %>%
  ungroup()

flow <- rbind(flow, c("First post / patient", nrow(pdata)))

colnames(flow) <- c("Criteria", "N")
