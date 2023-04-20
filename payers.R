library(tidyverse)

# creating info on who the insurance is
payers = data.frame(
  num = as.character(c(1:11, 16, 18:21, 25:27, 31, 32, 34, 38, 40, 41, 42, 43, 99)),
  payers = c("Medicare", "Medicaid", "Anthem BC/BS",
             "Tricare", "Self Pay", "Aetna", "United Healthcare",
             "Cigna", "Other Commericial", "Indigent/Charity",
             "Worker's Comp", "Local Government",
             "Other Government", "Govt Assistance", "Jail/Detention",
             "Black Lung", "Research/Donor", "Foreign", 
             "Hospice", "Medicaid Out of State","BC/BS Out of State", "Kaiser Permanente",
             "Optima", "Dual Medicare/Medicaid",
             "CareFirst", "Piedmont Community Health Plan", "Humana",
             "Unknown")
                    )

# creating readable data on the outcome
pstat = data.frame(
  code =as.character(c(1:7, 20, 43, 50, 51)),
  outcome = c("Discharged Home", 
              "Discharged to Short Term Hospital",
              "Discharged to Nursing",
              "Discharged to Supportive Care",
              "Discharged to Cancer or Childrens",
              "Discharged to Home Health Service",
              "Left Against of Medical Advice",
              "Expired",
              "Discharged to Federal Facility",
              "Hospice",
              "Hospice-Medical Facility")
)

# joining it all together
df2 = left_join(df, payers, by = c("payer" = "num"))
df2 = df2%>%
  left_join( pstat, by = c("pstat" = "code"))

df2 = df2%>%
  mutate(outcome_group = ifelse(pstat == 1, "Home", "Other"),
         outcome_group = ifelse(pstat %in% c(2:6, 43), "Other_Medical", outcome_group),
         outcome_group = ifelse(pstat %in% c(7), "LAMA", outcome_group),
         outcome_group = ifelse(pstat %in% c(50:51), "Hospice", outcome_group),
         outcome_group = ifelse(pstat %in% c(20), "Expired", outcome_group),
         
         )

by_insurance = df2%>%
  group_by(payers, outcome_group)%>%
  summarise(count = n())%>%
  spread(outcome_group, count, fill = 0)

insurance_counts = df2%>%
  group_by(payers)%>%
  summarise(count = n())

total = by_insurance%>%
  left_join(insurance_counts)%>%
  filter(count >2000)%>%
  mutate(per_dead = Expired/count,
         per_home = Home/count,
         per_hospice = Hospice/count,
         per_lama = LAMA/count,
         per_other= Other/count,
         per_other_hospital = Other_Medical/count)


