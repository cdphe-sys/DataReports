library(Rnssp)
library(readr)

## Creating a user profile (token)
myProfile <- create_token_profile()

## Creating a user profile (username and password)
myProfile <- create_profile()

## Inspect your `myProfile` confirming that username and password are completely hidden
myProfile

write_rds(myProfile, "~/WeeklyData/myProfile.rds")

