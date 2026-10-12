library(tidyverse)
library(mirt)
source("99-custom-functions.R")

pisa <- read_rds("data/pisa2025-science-interest.rds")

# ST094Q01NA - I generally have fun when I am learning broad science topics
# ST094Q03NA - I am happy working on broad science topics
# ST094Q04NA - I enjoy acquiring new knowledge in broad science
# ST094Q05NA - I am interested in learning about broad science
