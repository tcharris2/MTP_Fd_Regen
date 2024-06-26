##### CORRELATION MATRIX ###########

# 1. Importing Data ---------------------------------------------------------------

regen <- read.csv(here("Data/03_Processed", "20240602_survival_fd_b_processed.csv"), 
                  header = TRUE)
# check for most recent CSV file

# 2. Load packed 
library(corrplot)

ClimaticVarList <- names(regen %>% select(starts_with("d_")))

var_order <- c("d_MAP", "d_MSP", "d_PAS", "d_MAT", "d_EXT", "d_EMT", "d_NFFD", "d_AHM", "d_RH")

matrix_df <- subset(regen, select = ClimaticVarList)
matrix_df <- matrix_df[, var_order]

colnames(matrix_df) <- c("$ MAP[td]", "$ MSP[td]", "$ PAS[td]",
                         "$ MAT[td]", "$ EXT[td]", "$ EMT[td]",
                         "$ NFFD[td]", "$ AHM[td]", "$ RH[td]")


res <- cor(matrix_df)
round(res, 2)

corrplot.mixed(res, upper = "color", lower = "number", 
               tl.col = "black", lower.col = "black", number.cex = .7)
