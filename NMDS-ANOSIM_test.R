## TEST OF NMDS & ANOSIM

# install.packages("vegan")
library(vegan)
set.seed(2)  # used to generate random numbers 
# making a ranomdized abundance data frame of communities vs species (called a community matrix)
community_matrix=matrix(
                 sample(1:100,300,replace=T),nrow=10,
                 dimnames=list(paste("community",1:10,sep=""),paste("sp",1:30,sep="")))

# to run NMDS we use metaMDS
# the goal of NMDS is to represent the original position of communities in multidimensional space
# as accurately as possible using a reduced number of dimensions
# each species would have a separate axis, so it tries to show its position well in 2-3 axes instead

example_NMDS=metaMDS(community_matrix, # Our community-by-species matrix
                     k=2) # The number of reduced dimensions (2 axes)
# we want the stess to be ideally ~0.05, or <0.1 (<0.2 is ok, but not great)
# if the stress is too high, you can increase the number of default iterations 
# trymax = the number of iterations you want 
example_NMDS=metaMDS(community_matrix,k=2,trymax=100)

# the Shepard plot below shows the scatter around the regression
stressplot(example_NMDS)
# large scatter around the line means the original dissimilarities between communities are not
# well-preserved in the reduced number of dimensions 
# this plot seems fine

plot(example_NMDS) # initial plot of data
ordiplot(example_NMDS,type="n")   # plotting without the points being shown
orditorp(example_NMDS,display="sites",cex=0.76,air=0.01)  # adding text instead of the points

# lets pretend we had 2 main treatments:
treat=c(rep("Treatment1",5),rep("Treatment2",5))
ordiplot(example_NMDS,type="n")
ordihull(example_NMDS,groups=treat,draw="polygon",col="grey90",label=F)
# can use ordiellipse to create ellipses instead of polygons 
orditorp(example_NMDS,display="species",col="red",air=0.01)  # adding back the labels
orditorp(example_NMDS,display="sites",col=c(rep("green",5),rep("blue",5)),  # adding back the labels
         air=0.01,cex=1.25)

# all of the above could just be done with a simple pipe


## ANOSIM test
anosim(community_matrix, grouping = , distance = "bray", permutations = 9999)
