library("tidyverse")
library("devtools")

distances_raw <- read_csv("inst/extdata/distances.csv",
                          col_types = cols(
                              .default = col_integer(),
                              X1 = col_character()
                          ))

distances <- as.matrix(distances_raw[, 2:ncol(distances_raw)])
rownames(distances) <- distances_raw$X1

use_data(distances, overwrite = TRUE)