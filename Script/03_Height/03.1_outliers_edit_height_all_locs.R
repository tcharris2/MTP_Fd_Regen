
ln_height_group_harvest_fits_data


test_outliers <- ln_height_group_harvest_fits_data$data[[1]]

names(test_outliers)

test_outliers <- subset(test_outliers, select = c("tree_number", "resid_ln_h_group_model_harvest_1"))

test_outliers

large_outliers <- test_outliers[test_outliers$resid_ln_h_group_model_harvest_1 < -2, ]

large_outliers
