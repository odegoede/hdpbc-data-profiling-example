## Rules to test data with (using the validate package)
## By: Olivia de Goede
## Created: August 8, 2022

library(validate)

rules_nacrs <- validator(
  # 1. no duplicate NACRS_CASE_ID values
  case_cardinality = is_unique(HDP_NACRS_CASE_ID),
  # 2. every record has at least a primary symptom/complaint/problem/reason for seeking care
  complaint_recorded = !is.na(COMPLAINT1),
  # 3. every record has some measure of wait time (WAITPIA or ERTIME)
  wait_time_recorded = (!is.na(WAITPIA) | !is.na(ERTIME))
)

