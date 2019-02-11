# Geocode new construction addresses and write to CSV

read_csv(here("data", "Building_Permits.csv")) %>%
  mutate(ISSUE_DATE = as.Date(ISSUE_DATE, '%m/%d/%Y')) %>%
  #filter(ISSUE_DATE > as.Date('2014-12-31')) %>%
  select(ID:WORK_DESCRIPTION) %>%
  filter(str_detect(PERMIT_TYPE, "NEW CONSTRUCTION")) %>%
  mutate(FULL_ADDRESS = paste(STREET_NUMBER, `STREET DIRECTION`, STREET_NAME, SUFFIX, 'Chicago, IL', sep = " ")) %>%
  mutate_geocode(FULL_ADDRESS) %>%
  write.csv(here("data", "geo_building_permit.csv"))

  