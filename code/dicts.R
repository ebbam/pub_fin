# Dictionaries and functions
# called in other scripts

# Function to get states from state fips codes
data(fips_codes)
getstate <- fips_codes$state
names(getstate) <- fips_codes$state_code


# Function to get region from state code
tem <- data.frame(state.x77)               # Transform matrix into data frame
sta <- cbind(state.abb, tem, state.region) # Combine the three data sets
colnames(sta)[1] <- "State"                # Rename first column
colnames(sta)[10] <- "Region"              # Rename the 10th column
getregion <- sta$Region
names(getregion) <- sta$State

# Function to convert standard fips codes to BEA fips codes
# FIPS modifications - inconsistent FIPS codes between BEA sources and others
# the following creates a lookup process for replacing standard FIPS codes with their BEA equivalents
bea_fips <- read_excel(here("data/out/FIPSModificationsVA.xlsx"), skip = 1, col_types = c("text", "text","text","text"))
getfips <- bea_fips$`BEA FIPS`
names(getfips) <- bea_fips$FIPS