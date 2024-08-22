library(MASS)

unique(regen_harvest_height$tree_cover)

regen_canopy_height <- subset(regen, !(is.na(tree_cover)))
regen_canopy_height <- subset(regen_canopy_height, !(is.na(height)))


filled.contour(kde2d(regen_canopy_height$s_MAT, regen_canopy_height$height), scale = F)
filled.contour(kde2d(regen_canopy_height$p_MAT, regen_canopy_height$height), scale = F)
filled.contour(kde2d(regen_canopy_height$d_MAT, regen_canopy_height$height), scale = F)

