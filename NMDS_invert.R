# Library ----
library(vegan)

# load data
invert <- read.csv("NMDS_invertebratedata.csv")
invert_landuse <- read.csv("NMDS_invertebratedata_landuse.csv")
invert_matrix <- as.matrix(invert)

# NMDS ----
test_NMDS <- metaMDS(invert_matrix,
                     k = 2, trymax = 100)   # stress = 0.159...

stressplot(test_NMDS)

plot(test_NMDS)
ordiplot(test_NMDS,type="n")
points(test_NMDS, display = "site", 
       pch = c(16, 8, 17, 11, 0, 2) [as.numeric(invert_landuse$landuse)])
legend("topright", legend = levels(invert_landuse$landuse), pch = c(16, 8, 17, 11, 0, 2), 
                                  bty = "n", cex = 1) 
legend("topleft", "stress = 0.159", bty = "n", cex = 1) # displays legend text of stress value 


ordihull(test_NMDS, groups = invert_landuse$landuse,    # comparison of land use types, but
         draw = "polygon", col = "grey90", label = F)   # this isn't our RQ really

?ordihull
