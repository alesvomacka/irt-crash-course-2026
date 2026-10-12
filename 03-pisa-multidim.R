library(tidyverse)
library(mirt)
source("99-custom-functions.R")

pisa <- read_rds("data/pisa2025-env-efficacy.rds")

# ST499Q02DA - I believe that my school and local community can contribute to positively impacting global environmental issues (e.g. climate change, habitat restoration)
# ST499Q04DA - I believe that by working with my local community we can play an important role in positively impacting the environment
# ST499Q05DA - I believe that by working with my local community we can reduce the use of fossil fuels
# ST499Q06DA - I believe that by working with my school we can reduce their energy consumption

# ST500Q03DA - I know how to work with others to positively impact the environment
# ST500Q05DA - I know that there are things that I can do to positively impact the environment
# ST500Q06DA - If people work together, we can resolve environmental issues
# ST500Q09DA - Looking after the global environment is important to me

fit_2d <- mirt::mirt(
  item_data,
  model = 2,
  itemtype = "graded"
)

model_2d <- "
collective = 1-4
capacity   = 5-8
COV = collective*capacity
"


fit_2d <- mirt::mirt(
  item_data,
  model = model_2d,
  itemtype = "graded"
)
