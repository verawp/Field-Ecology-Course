


##################################################################################################################
################################# Community Ecology code introduction ############################################
##################################################################################################################
##################################################################################################################

setwd("~/Desktop/FieldEcology")
Herb<-read.csv("Herb.csv")
Trees<-read.csv("Trees.csv")
Int<-read.csv("Interactions.csv")

####### Subsetting #######

unique(Herb$Species)
head(Herb)


Herb[which(Herb$Species=="grass"),]
Herb[which(Herb$Species=="Grass"),]

str(Herb[which(Herb$Species=="Grass"),3])

HerbVector<-Herb$Species

##### Making a matrix #####

Plts<-xtabs(Cover~ Site + Plants, data = Herb)

############################################# Madison Map #######################################################


library(ggmap)

madison <- c(lon = -89.384357, lat = 43.074393)

# Add a maptype argument to get a satellite mapwas least between sites X and X and greatest between sites X and X
mad_map_sat <- get_map(madison, zoom = 13, maptype = "satellite")

# Edit to display satellite map
ggmap(mad_map_sat) 

# Add source and maptype to get toner map from Stamen Maps 
mad_map_bw <- get_map(madison, zoom = 12, source = "stamen", maptype = "toner")

ggmap(mad_map_bw)



######################################### Practice vegetation ordination ###########################################

library(vegan)
data(varespec)
data(varechem)


ord <- metaMDS(varespec)
plot(ord, type = "t")



## Fit environmental variables
ef <- envfit(ord, varechem)
ef
plot(ef, p.max = 0.05)


ord2<-metaMDS(Plts)
plot(ord2, type="t")


############################ Our ordination #########################










##################################### Plant pollinator interactions #######################################################

library(bipartite)

data(Safariland)
visweb(Safariland)
visweb(Safariland, type="diagonal", square="compartment", text="none",
       frame=TRUE)
visweb(Safariland, type="nested", text="compartment")
visweb(Safariland, circles=TRUE,  boxes=FALSE,  labsize=1, circle.max=3,
       text="no")
visweb(Safariland,square="b",box.col="green",box.border="red")


plotweb(Safariland)

# shorter labels
plotweb(Safariland, high.lablength=3, low.lablength=0, arrow="down")


# centered triangles for displaying interacions
plotweb(Safariland, text.rot=90, arrow="down.center", col.interaction="wheat2",
        y.lim=c(-1,2.5))

#orginal sequence, up arrows and different box width
plotweb(Safariland, method="normal", arrow="up", y.width.low=0.3, low.lablength=4)

# interactions as lines
plotweb(Safariland, arrow="both", y.width.low=0.05, text.rot=90, col.high="blue",
        col.low="green")


# add an abundance vector for lower trophic species
low.abun = round(runif(dim(Safariland)[1],1,40)) #create
names(low.abun) <- rownames(Safariland)
plotweb(Safariland, text.rot=90, low.abun=low.abun, col.interaction="purple",
        y.width.low=0.05, y.width.high=0.05)
plotweb(Safariland, text.rot=90, low.abun=low.abun, col.interaction ="red",
        bor.col.interaction="red", arrow="down")



######################## Our interaction networks ##############################


Int<-xtabs(~ Plants + Pollinators, data = Interactions)








##################################################################################################################