

#### Load and prepare myopia data --------------------------------

# Myopia (near-sightedness or short-sightedness) is the most common eye problem
# and is estimated to affect 1.5 billion people (22% of the population). It is a
# condition of the eye where light focuses in front, instead of on the retina.
# This causes distant objects to be blurry while close objects appear normal.
# Other symptoms may include headaches and eye strain. Severe near-sightedness
# increases the risk of retinal detachment, cataracts, and glaucoma.

# Here we study myopia's various likely influencing variables using logistic
# regression. We examine physiological variables (age, gender, eyeball
# parameters), environmental variables (time spent on near-work and outdoor
# activities) as well as hereditary variables (myopic mother and father). By
# doing the analysis, we examine the validity of the aforementioned hypotheses.


# https://rdrr.io/cran/aplore3/man/myopia.html
# http://astro1.panet.utoledo.edu/~terencezl/projects/myopia.html


#### Packages --------------------------------

library("tidyverse")  # Contains packages: dplyr, readr, and the %>% operator
library("janitor")  # useful functions for data cleaning and exploration


#### Load the data --------------------------------

myopia <- readr::read_csv(
  file = "https://learn.canvas.net/courses/1179/files/461762/download?download_frd=1",
  col_types = cols(
    ID = col_integer(),
    STUDYYEAR = col_integer(),
    MYOPIC = col_integer(),
    AGE = col_integer(),
    GENDER = col_integer(),
    SPHEQ = col_double(),
    AL = col_double(),
    ACD = col_double(),
    LT = col_double(),
    VCD = col_double(),
    SPORTHR = col_integer(),
    READHR = col_integer(),
    COMPHR = col_integer(),
    STUDYHR = col_integer(),
    TVHR = col_integer(),
    DIOPTERHR = col_integer(),
    MOMMY = col_integer(),
    DADMY = col_integer())
  )

#### Prepare the data --------------------------------

# Better naming
myopia <- myopia %>%
  janitor::clean_names()

# Convert factor variables
myopia <- myopia %>%
  mutate(
    myopic = factor(myopic,
                    levels = c(0, 1),
                    labels = c("No", "Yes")),
    gender = factor(gender,
                    levels = c(0, 1),
                    labels = c("Male", "Female")),
    mommy = factor(mommy,
                   levels = c(0, 1),
                   labels = c("No", "Yes")),
    dadmy = factor(dadmy,
                   levels = c(0, 1),
                   labels = c("No", "Yes")),
  )




# dplyr::glimpse(myopia)


# save(myopia,
#      file = "C:/Users/latour/Dropbox/repos/purposeful/data/myopia.rda")
