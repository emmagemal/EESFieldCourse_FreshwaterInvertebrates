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
       pch = c(16, 8, 17, 11, 0, 5) [as.numeric(invert_landuse$landuse)])
legend("topright", legend = levels(invert_landuse$landuse), pch = c(16, 8, 17, 11, 0, 5), 
                                  bty = "n", cex = 1) 
legend("topleft", "stress = 0.159", bty = "n", cex = 1) # displays legend text of stress value 

ordihull(test_NMDS, groups = invert_landuse$landuse,    # comparison of land use types, but
         draw = "polygon", col = "grey90", label = F)   # this isn't our RQ really

# ANOSIM ---- 
## NOTE: we only need to do anosim() if we are wanting to show if there's a significant 
## difference between landuse types (can't be used for comparing sites by themselves).
## Would be best for a project that's got replicates for land use types on purpose = not ours

anosim(invert_matrix, invert_landuse$landuse, distance = "bray", permutations = 9999)
  # ANOSIM statistic R: 0.2044, the closer to 1 it is the more dissimilar groups are
  # Significance: 0.1087, should be <0.05 to be significant, so it's not significant 

