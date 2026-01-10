### CADA SOLUCION CUENTA: ATTRIBUTES OF COMMUNITY-LEVEL RESPONSE TO CHANGE IN MEXICAN SMALL-SCALE FISHERIES ###

## Authors: Niza Contreras Liedtke, Magdalena Précoma, Julia G. Mason, Jacob G. Eurich, Arturo Hernández Velasco, Jorge Torre, Mark H. Carr, and Elena M. Finkbeiner ##

## This code was written by Niza Contreras Liedtke as part of her capstone project for the Coastal Science and Policy Master of Science degree. Peter Raimondi provided valuable advice and statistical insight for these anlyses. ##


####### SETUP #######
## This code requires the following datasets
  # "Base de soluciones_Niza.csv" == solutions dataset
  # "Adaptive domains_resilience attributes.csv" == dataset with all of the attributes of resilience and the fisheries dimensions and domains of adaptive capacity they correspond to

## This code creates the following data frames:
  # *soluciones* == Niza's full solutions database with attribute and adaptive capacity data, only including soluciones that have been 1) documented on PescaData, 2) are considered "soluciones," and 3) haven't been selectively excluded.
  # *atributos* == dataset of all attributes, with English and Spanish names, and info on their fishery dimensions and domains of AC
  # *datos* == dataframe showing the presence/absence of each attribute for all solutions in the *soluciones* dataset
  # *data* == a dataset with selected columns from *soluciones* and the attribute data from *datos*
  # *totalAt* == a summary dataframe with shows the total number of occurences of each attributes across all solutions in *soluciones*
  # *problemas* == dataframe showing the presence/absence of each problem subcategory for all solutions in the *soluciones* dataset
  # *totalProb* == a summary dataframe of *problemas* showing the total number of occurences for each problem category across all solutions in *soluciones*
  # *retosCat* == a dataframe showing the presence/absence of each of the five problem categories across all solutions in the *soluciones* dataset
  # *totalAC* == a summary dataframe of the total number of solutions from *soluciones* that fall within each domain of adaptive capacity
  # *totalDim* == a dataframe showing the occurences of attributes in each of the three fishery system dimensions

## the most updated version of these dataframes can be accessed by loading the following R object:
  # setwd("/Users/nizacontreras/Desktop/Work Stuff/Capstone/Data")
  # load("solucionesDatos.RData")

## every time the solutions database is updated, it should be reloaded, this code should be rerun (minus the *atributos* step, unless applicable), and the files should be saved again
  # save(soluciones, atributos, datos, data, totalAt, problemas, totalProb, totalAC, totalDim, retosCat, groupings, datos_clust, finalmodel, file = "solucionesDatos.RData")

#### Creating the *soluciones* dataframe ####
## Load solutions dataset
soluciones <- read.csv(file = "data_soluciones_redownload.csv", stringsAsFactors = FALSE)

## Add values for total number of attributes assigned to each solution
for(i in 1:nrow(soluciones)){
  list <- as.numeric(unlist(strsplit(soluciones$ATRIBUTOS[i], ",")))
  soluciones$ATRIBUTOS_total[i] <- length(list)
}

#### Creating the *atributos* dataframe ####
## Load resilience attribute data
atributos <- read.csv(file="Resilience attributes.csv")

## create a column with attribute name_number, to be used in next step
atributos$name_number <- paste0(atributos$Attributes, "_", atributos$Number)

#### Creating the *data* data frame, which will be used for the rest of the analyses ####
## First, subset *soluciones* to just include the columns we're interested in: ID, year of the solution, gender of person who shared, title, problem description, solution description, challenges, attribute columns, and challenge category columns
subset <- soluciones[,c(1,2,4:42)]

## Now create a second data frame with attribute presence/absence info for all solutions
# make empty data frame
datos <- as.data.frame(matrix(0, nrow = nrow(soluciones), ncol = (nrow(atributos)+1)))
# first column for ID numbers
colnames(datos)[1] <- paste0("ID")
# the rest of the columns are named after the attributes
colnames(datos)[2:ncol(datos)] <- paste0(atributos$Attributes)
# fill in ID column
datos$ID <- soluciones$ID
# now, fill in attribute presence for each solutions
for(i in 1:nrow(datos)){
  if(datos$ID[i] %in% soluciones$ID){
    list <- as.numeric(unlist(strsplit(soluciones$ATRIBUTOS[match(datos$ID[i], soluciones$ID)], ",")))
    sub <- subset(atributos, atributos$Number %in% list)
    for(j in 1:nrow(sub)){
      if(sub$Attributes.of.resilience[j] %in% colnames(datos)){
        datos[i, match(sub$Attributes.of.resilience[j], names(datos))] <- 1
      }
      
    }
      }
}

## Lastly, merge the *subset* and *datos* data frames into the *data* df
data <- merge(subset, datos)

#### Creating the *totalAt* data frame ####
## Make a data frame with the total number of each attributes across all solutions
totalAt <- as.data.frame(colSums(data[,62:102]))
# change column name
colnames(totalAt)[1] <- paste0("Total_Attribute")

#### Creating the *problemas*, *totalProb*, and *retosCat* dataframes ####
## Make a data frame with just the problem category columns
problemas <- data[, c(1, 21:51)]
# replace all NAs with 0
problemas[is.na(problemas)] <- 0

## create a summary dataframe by summing each problem across all solutions
totalProb <- as.data.frame(colSums(problemas[,-1]))
# rename column
colnames(totalProb)[1] <- paste0("Total_Problems")

## load in the problem category dataset
probCats <- read.csv("Categorias de retos.csv")

## create a dataframe 
retosCat <- as.data.frame(soluciones$ID)
# R
# rename column
colnames(retosCat)[1] <- "ID"
## create columns
retosCat$Pesquero <- NA
retosCat$Ambiental <- NA
retosCat$Estructural <- NA
retosCat$Economico <- NA
retosCat$Social <- NA
  

## fill in the challenge category that each solutions applies to
# pesquero
pes <- problemas[,c(2:7)]
for(i in 1:nrow(pes)){
  if(rowSums(pes)[i] > 0){
    retosCat$Pesquero[i] <- 1
  }
  else{
   retosCat$Pesquero[i] <- 0
  }
}

# ambiental
amb <- problemas[,c(8:16)]
for(i in 1:nrow(amb)){
  if(rowSums(amb)[i] > 0){
    retosCat$Ambiental[i] <- 1
  }
  else{
    retosCat$Ambiental[i] <- 0
  }
}

# estructural
est <- problemas[,c(17:20)]
for(i in 1:nrow(est)){
  if(rowSums(est)[i] > 0){
    retosCat$Estructural[i] <- 1
  }
  else{
    retosCat$Estructural[i] <- 0
  }
}

# economico
eco <- problemas[,c(21:24)]
for(i in 1:nrow(eco)){
  if(rowSums(eco)[i] > 0){
    retosCat$Economico[i] <- 1
  }
  else{
    retosCat$Economico[i] <- 0
  }
}

# social
soc <- problemas[,c(25:32)]
for(i in 1:nrow(soc)){
  if(rowSums(soc)[i] > 0){
    retosCat$Social[i] <- 1
  }
  else{
    retosCat$Social[i] <- 0
  }
}

## create a variant of retosCat that has columns for combinations of challenges
## start with pesquero+

# create a throwaway df
retosCombo <- retosCat

# pesqueroSocial
for(i in 1:nrow(retosCombo)){
  if((retosCombo$Pesquero[i] + retosCombo$Social[i]) > 1) {
    retosCombo$pesqueroSocial[i] <- 1
  }
  else{
    retosCombo$pesqueroSocial[i] <- 0
  }
}

# pesqueroAmbiental
for(i in 1:nrow(retosCombo)){
  if((retosCombo$Pesquero[i] + retosCombo$Ambiental[i]) > 1) {
    retosCombo$pesqueroAmbiental[i] <- 1
  }
  else{
    retosCombo$pesqueroAmbiental[i] <- 0
  }
}

# pesqueroEstructural
for(i in 1:nrow(retosCombo)){
  if((retosCombo$Pesquero[i] + retosCombo$Estructural[i]) > 1) {
    retosCombo$pesqueroEstructural[i] <- 1
  }
  else{
    retosCombo$pesqueroEstructural[i] <- 0
  }
}

# pesqueroEconomico
for(i in 1:nrow(retosCombo)){
  if((retosCombo$Pesquero[i] + retosCombo$Economico[i]) > 1) {
    retosCombo$pesqueroEconomico[i] <- 1
  }
  else{
    retosCombo$pesqueroEconomico[i] <- 0
  }
}
# save the throwaway df as a new df
# retosPesquero <- retosCombo

## now doing Ambiental+

# create a throwaway df
retosCombo <- retosCat

# ambientalSocial
for(i in 1:nrow(retosCombo)){
  if((retosCombo$Ambiental[i] + retosCombo$Social[i]) > 1) {
    retosCombo$ambientalSocial[i] <- 1
  }
  else{
    retosCombo$ambientalSocial[i] <- 0
  }
}

# ambientalPesquero
for(i in 1:nrow(retosCombo)){
  if((retosCombo$Pesquero[i] + retosCombo$Ambiental[i]) > 1) {
    retosCombo$ambientalPesquero[i] <- 1
  }
  else{
    retosCombo$ambientalPesquero[i] <- 0
  }
}

# ambientalEstructural
for(i in 1:nrow(retosCombo)){
  if((retosCombo$Ambiental[i] + retosCombo$Estructural[i]) > 1) {
    retosCombo$ambientalEstructural[i] <- 1
  }
  else{
    retosCombo$ambientalEstructural[i] <- 0
  }
}

# ambientalEconomico
for(i in 1:nrow(retosCombo)){
  if((retosCombo$Ambiental[i] + retosCombo$Economico[i]) > 1) {
    retosCombo$ambientalEconomico[i] <- 1
  }
  else{
    retosCombo$ambientalEconomico[i] <- 0
  }
}
# save the throwaway df as a new df
# retosAmbiental <- retosCombo

## now doing Estructural+
# create a throwaway df
retosCombo <- retosCat

# estructuralSocial
for(i in 1:nrow(retosCombo)){
  if((retosCombo$Estructural[i] + retosCombo$Social[i]) > 1) {
    retosCombo$estructuralSocial[i] <- 1
  }
  else{
    retosCombo$estructuralSocial[i] <- 0
  }
}

# estructuralPesquero
for(i in 1:nrow(retosCombo)){
  if((retosCombo$Pesquero[i] + retosCombo$Estructural[i]) > 1) {
    retosCombo$estructuralPesquero[i] <- 1
  }
  else{
    retosCombo$estructuralPesquero[i] <- 0
  }
}

# estructuralAmbiental
for(i in 1:nrow(retosCombo)){
  if((retosCombo$Ambiental[i] + retosCombo$Estructural[i]) > 1) {
    retosCombo$estructuralAmbiental[i] <- 1
  }
  else{
    retosCombo$estructuralAmbiental[i] <- 0
  }
}

# estructuralEconomico
for(i in 1:nrow(retosCombo)){
  if((retosCombo$Estructural[i] + retosCombo$Economico[i]) > 1) {
    retosCombo$estructuralEconomico[i] <- 1
  }
  else{
    retosCombo$estructuralEconomico[i] <- 0
  }
}
# save the throwaway df as a new df
# retosEstructural <- retosCombo

## now doing Economico+
# create a throwaway df
retosCombo <- retosCat

# economicoSocial
for(i in 1:nrow(retosCombo)){
  if((retosCombo$Economico[i] + retosCombo$Social[i]) > 1) {
    retosCombo$economicoSocial[i] <- 1
  }
  else{
    retosCombo$economicoSocial[i] <- 0
  }
}

# economicoPesquero
for(i in 1:nrow(retosCombo)){
  if((retosCombo$Pesquero[i] + retosCombo$Economico[i]) > 1) {
    retosCombo$economicoPesquero[i] <- 1
  }
  else{
    retosCombo$economicoPesquero[i] <- 0
  }
}

# economicoAmbiental
for(i in 1:nrow(retosCombo)){
  if((retosCombo$Ambiental[i] + retosCombo$Economico[i]) > 1) {
    retosCombo$economicoAmbiental[i] <- 1
  }
  else{
    retosCombo$economicoAmbiental[i] <- 0
  }
}

# economicoEstructural
for(i in 1:nrow(retosCombo)){
  if((retosCombo$Estructural[i] + retosCombo$Economico[i]) > 1) {
    retosCombo$economicoEstructural[i] <- 1
  }
  else{
    retosCombo$economicoEstructural[i] <- 0
  }
}
# save the throwaway df as a new df
# retosEconomico <- retosCombo

## now Social+
# create a throwaway df
retosCombo <- retosCat

# socialPesquero
for(i in 1:nrow(retosCombo)){
  if((retosCombo$Pesquero[i] + retosCombo$Social[i]) > 1) {
    retosCombo$socialPesquero[i] <- 1
  }
  else{
    retosCombo$socialPesquero[i] <- 0
  }
}

# socialAmbiental
for(i in 1:nrow(retosCombo)){
  if((retosCombo$Social[i] + retosCombo$Ambiental[i]) > 1) {
    retosCombo$socialAmbiental[i] <- 1
  }
  else{
    retosCombo$socialAmbiental[i] <- 0
  }
}

# socialEstructural
for(i in 1:nrow(retosCombo)){
  if((retosCombo$Social[i] + retosCombo$Estructural[i]) > 1) {
    retosCombo$socialEstructural[i] <- 1
  }
  else{
    retosCombo$socialEstructural[i] <- 0
  }
}

# socialEconomico
for(i in 1:nrow(retosCombo)){
  if((retosCombo$Social[i] + retosCombo$Economico[i]) > 1) {
    retosCombo$socialEconomico[i] <- 1
  }
  else{
    retosCombo$socialEconomico[i] <- 0
  }
}
# save the throwaway df as a new df
# retosSocial <- retosCombo


#### Creating the *totalAC* dataframe ####
## Make a dataframe with total number of solutions in each domain of adaptive capacity
totalAC <- as.data.frame(colSums(data[15:19], na.rm = TRUE))
# rename the column
colnames(totalAC)[1] <- paste0("Total_Soluciones")


#### Creating dimensions data and *totalDim* dataframe #### 
## create a new column in *atributos* to specify which fishery dimension each attribute applies to
for(i in 1:nrow(atributos)){
  if(!is.na(atributos$Governance[i])){
    atributos$dimension[i] <- "governance"
  }
  else if(!is.na(atributos$Ecological[i])){
    atributos$dimension[i] <- "ecological"
  }
  else if(!is.na(atributos$Socio.economic[i])){
    atributos$dimension[i] <- "socioEconomic"
  }
}

## add the same column to *totalAt*
totalAt$dimension[match(row.names(totalAt), atributos$Attributes.of.resilience)] <- atributos$dimension[match(row.names(totalAt), atributos$Attributes.of.resilience)]

## create a new summary dataframe for total of occurrences across each dimension
totalDim <- data.frame(row.names = c("Governance", "Ecological", "SocioEconomic"))
# create an empty column
totalDim$Total_Dimension <- NA
# fill in the data for governance
totalDim$Total_Dimension[1] <- sum(subset(totalAt, dimension == "governance")$Total_Attribute)
# for ecological
totalDim$Total_Dimension[2] <- sum(subset(totalAt, dimension == "ecological")$Total_Attribute)
# and for socioEconomic
totalDim$Total_Dimension[3] <- sum(subset(totalAt, dimension == "socioEconomic")$Total_Attribute)

#### Creating cooperative data ####
## Make a dataframe with each cooperative and person that has shared a solution in the *soluciones* dataset
coops <- as.data.frame(unique(soluciones$COOPERATIVA_PERSONA))
# rename column
colnames(coops)[1] <- paste0("Nombre")
# add in state data
for(i in 1:nrow(coops)){
  if(coops$Nombre[i] %in% soluciones$COOPERATIVA_PERSONA){
    coops$Estado[i] <- soluciones$ESTADO[match(coops$Nombre[i], soluciones$COOPERATIVA_PERSONA)]
  }
}

# write.csv(coops, file = "nombres_coops.csv")

## load the filled in data
permisoData <- read.csv(file = "nombres_cooperativas_personas.csv")

## merge this data with the *data* dataframe
#data <- merge(data, permisoData)

####### ANALYSES ########
##### Co-occurrences between attributes ####
## library(remotes)
## install_github("cran/cooccur")
## library(cooccur)

## copy data frame
attOccurrence <- attObs
## change column names
colnames(attOccurrence) <- atributos$Attributes.of.resilience[match(colnames(attOccurrence), atributos$Number)] 
# rename Technology advancement
colnames(attOccurrence)[26] <- paste0("Technology advancement")

## run the cooccur function, transposing the attribute subset data
cooccur <- cooccur(t(attOccurrence), spp_name = TRUE)
# 
summary(cooccur)
library(reshape2)
plot.cooccur(cooccur)

##### Canonical correspondence analysis #####
# library(vegan)

## set up attribute occurrence data
attObs <- datos
# change column names to attribute numbers
colnames(attObs) <- c("ID", 1:41)
# remove attributes that don't have occurrences
# subset *datos*, creating a new dataframe 
attObs <- attObs[, -c(5,6,10,16,17,19,21,22,24,37)]
# rename rows
rownames(attObs) <- attObs$ID
# remove ID column
attObs <- attObs[,-1]

## run the analysis with just the explanatory variables (challenge categories)
ccaChallenge <- cca(attObs ~.,retosCat)

# We also have to automatically select variables of "retosCat" matrix that best explain "attObs" matrix. We can do that by using a stepwise model from "ordistep" function
finalmodel<- ordistep(ccaChallenge, scope=formula(ccaChallenge))

finalmodel

# Then, we can calculate Variance Inflation Factors (VIF) for each of the constraints (variables) from the "env" matrix (environmental matrix). If we find an environmental variable with VIF>10, we'll know that this variable presents colinearity with another or other variables. In that case, we would have to delete the variable from our initial dataset and redo all the analysis.
vif.cca(finalmodel) # no VIF > 10

# test for significance of the model
anova.cca(finalmodel) # p-value = 0.001

# test for the significance of each variable
anova.cca(finalmodel, by="terms") # all variables with P =< 0.002

# test for the significance of the axes
anova.cca(finalmodel, by="axis") # all axes are significant with P =< 0.032

## some plots, looking at different axis combos
plot(finalmodel, choices = c(1,2), xlim=c(-3,3), ylim=c(-3,3), display=c("sp","cn"))

plot(finalmodel, choices = c(1,3), xlim=c(-3,3), ylim=c(-3,3), display=c("sp","cn"))

plot(finalmodel, choices = c(2,3), xlim=c(-3,3), ylim=c(-3,3), display=c("sp","cn"))

plot(finalmodel, choices = c(1,4), xlim=c(-3,3), ylim=c(-3,3), display=c("sp","cn"))

##### Cluster analysis #####
library(tidyverse)
library(dplyr)
## convert data to matrix
# create new df
datos_mat <- datos
# label rows with solution IDs
rownames(datos_mat) <- datos_mat$ID
# then remove the ID column and agency
datos_mat <- datos_mat[,-c(1,42)]
# remove attributes that are not observed in any solutions
datos_mat  <- datos_mat %>% 
  select_if(is.numeric) %>% 
  select_if(~ sum(.x) > 0)
# make matrix
datos_mat <- as.matrix(datos_mat)

## calculate distance matrix
library(vegan)
datos_dist <- vegdist(datos_mat, method = "jaccard")

## perform clustering
datos_clust <- hclust(datos_dist, method = "ward.D2")

# plot
par(mfrow=c(1,1))
plot(datos_clust, labels = row.names(datos_clust), cex=0.4) # cex refers to font size in the plot
# add rectangle divisions
rect.hclust(datos_clust, k=9) # k refers to the number of clusters


# clusterplot
groups9 <- cutree(datos_clust, k=9) # Assign observations to groups

# turn cluster groupings into a dataframe
groupings9 <- as.data.frame(groups)

# reorder
groupings9 <- groupings %>% arrange(groups)


loadfonts(quiet=TRUE)

fonts <- fonttable()

## clsutering
library(NbClust)

Nb <- NbClust(diss = datos_dist, distance = NULL, min.nc = 2, max.nc = 15, method = "frey", index = "all")
Nb

library(factoextra)
fviz_nbclust(datos_mat, hcut, method = "gap_stat") +
  geom_vline(xintercept = 3, linetype = 2)

###### Prepping data for CCA figures ######

## function for making the results cca() compatible with ggplot
library(vegan)
`fortify.cca` <- function(model, data, axes = 1:6,
                         display = c("sp", "wa", "lc", "bp", "cn"), ...) {
  ## extract scores
  scrs <- scores(model, choices = axes, display = display, ...)
  ## handle case of only 1 set of scores
  if (length(display) == 1L) {
    scrs <- list(scrs)
    nam <- switch(display,
                  sp = "species",
                  species = "species",
                  wa = "sites",
                  sites = "sites",
                  lc = "constraints",
                  bp = "biplot",
                  cn = "centroids",
                  stop("Unknown value for 'display'"))
    names(scrs) <- nam
  }
  miss <- vapply(scrs, function(x ) all(is.na(x)), logical(1L))
  scrs <- scrs[!miss]
  nams <- names(scrs)
  nr <- vapply(scrs, FUN = NROW, FUN.VALUE = integer(1))
  df <- do.call('rbind', scrs)
  rownames(df) <- NULL
  df <- as.data.frame(df)
  df <- cbind(score = factor(rep(nams, times = nr)),
              label = unlist(lapply(scrs, rownames), use.names = FALSE),
              df)
  df
}

## data frame of attribute points
ccaAtts <- subset(fortify.cca(finalmodel), score == "species")[,-1]
# change row names
rownames(ccaAtts) <- ccaAtts$label
# subset for CCA1 and CC2
ccaAtts1v2 <- ccaAtts[,-c(1,4,5,6,7)]
# subset for CCA1 and CCA3
ccaAtts1v3 <- ccaAtts[,-c(1,3,5,6,7)]
# subset for CCA2 and CCA3
ccaAtts2v3 <- ccaAtts[,-c(1,2,5,6,7)]

scores <- scores(finalmodel, choices = 1, display = "sp", scaling = 0)

scores2 <- scores(finalmodel, choices = 2, display = "sp", scaling = 0)
scores3 <- scores(finalmodel, choices = 3, display = "sp", scaling = 0)
scoreAtt <- data.frame(CCA1 = scores, CCA2 = scores2, CCA3 = scores3)

corr <- cor(attObs, scoreAtt$CCA1)

sum <- summary(finalmodel)

summary(finalmodel)$sp

## data frame of challenge category vectors
ccaCh <- subset(fortify.cca(finalmodel), score == "biplot")[,-1]
# rename variables
ccaCh$label <- c("Fishery","Environmental","Structural","Economic","Social")
# change row names
rownames(ccaCh) <- ccaCh$label
# subset for CCA1 and CCA2
ccaCh1v2 <- ccaCh[,-c(1,4,5,6,7)]
# subset for CCA1 and CCA3
ccaCh1v3 <- ccaCh[,-c(1,3,5,6,7)]
# subset for CCA1 and CCA2
ccaCh2v3 <- ccaCh[,-c(1,2,5,6,7)]

## function for scaling the cca() plot vectors
`arrow_mul` <- function(arrows, data, at = c(0, 0), fill = 1.25) {
  u <- c(range(data[, 1], range(data[, 2])))
  u <- u - rep(at, each = 2)
  r <- c(range(arrows[, 1], na.rm = TRUE), range(arrows[, 2], na.rm = TRUE))
  rev <- sign(diff(u))[-2]
  if (rev[1] < 0)
    u[1:2] <- u[2:1]
  if (rev[2] < 0)
    u[3:4] <- u[4:3]
  u <- u/r
  u <- u[is.finite(u) & u > 0]
  fill * min(u)
}

# generate scale value for CCA1 v CCA2
scale1v2 <- arrow_mul(ccaCh1v2,ccaAtts1v2)
# generate scale value for CCA1 v CCA3
scale1v3 <- arrow_mul(ccaCh1v3,ccaAtts1v3)
# generate scale value for CCA2 v CCA3
scale2v3 <- arrow_mul(ccaCh2v3,ccaAtts2v3)

## for labels
ccaLabel <- subset(fortify.cca(finalmodel), score == "species" | score == "biplot")[,-1]
# change challenge category names
ccaLabel$label[c(32:36)] <- c("Fishery","Environmental","Structural","Economic","Social")
colorLabel <- c(rep(cobiDB,31), rep(cobiMB,5))
sizeLab <- c(rep(labAtts,31), rep(labChallenges,5))
positionLab <- c(rep("identity",31), rep(position_nudge_center(x = 0.2, y = 0.02, center_x = 0, center_y = 0), 5))

scaleLab1v2 <- c(rep(1, 31), rep(scale1v2,5))
scaleLab1v3 <- c(rep(1, 31), rep(scale1v3,5))
scaleLab2v3 <- c(rep(1, 31), rep(scale2v3,5))


## plot cooccur

plot.cooccur <-
  function(x, ...){
    
    ##
    allargs <- match.call(expand.dots = TRUE)
    plotrand <- allargs$plotrand
    plotrand <- ifelse(test = is.null(plotrand),yes = FALSE,no = plotrand)
    randsummary<- allargs$randsummary
    randsummary <- ifelse(test = is.null(randsummary),yes = FALSE,no = randsummary)
    
    ##
    
    dim <- x$species
    comat_pos <- comat_neg <- matrix(nrow=dim,ncol=dim)
    
    co_tab <- x$result
    for (i in 1:nrow(co_tab)){
      comat_pos[co_tab[i,"sp1"],co_tab[i,"sp2"]] <- co_tab[i,"p_gt"]
      comat_pos[co_tab[i,"sp2"],co_tab[i,"sp1"]] <- co_tab[i,"p_gt"]
      
      row.names(comat_pos[co_tab[i,"sp2"],co_tab[i,"sp1"]])
      
    }
    for (i in 1:nrow(co_tab)){
      comat_neg[co_tab[i,"sp1"],co_tab[i,"sp2"]] <- co_tab[i,"p_lt"]
      comat_neg[co_tab[i,"sp2"],co_tab[i,"sp1"]] <- co_tab[i,"p_lt"]
    }
    comat <- ifelse(comat_pos>=0.05,0,1) + ifelse(comat_neg>=0.05,0,-1)
    colnames(comat) <- 1:dim
    row.names(comat) <- 1:dim
    
    if ("spp_key" %in% names(x)){
      
      sp1_name <- merge(x=data.frame(order=1:length(colnames(comat)),sp1=colnames(comat)),y=x$spp_key,by.x="sp1",by.y="num",all.x=T)
      sp2_name <- merge(x=data.frame(order=1:length(row.names(comat)),sp2=row.names(comat)),y=x$spp_key,by.x="sp2",by.y="num",all.x=T)
      
      colnames(comat) <- sp1_name[with(sp1_name,order(order)),"spp"]  
      row.names(comat) <- sp2_name[with(sp2_name,order(order)),"spp"]
      
    }  
    
    #ind <- apply(comat, 1, function(x) all(is.na(x)))
    #comat <- comat[!ind,]
    #ind <- apply(comat, 2, function(x) all(is.na(x)))
    #comat <- comat[,!ind]
    
    comat[is.na(comat)] <- 0
    
    origN <- nrow(comat)
    
    # SECTION TO REMOVE SPECIES INTERACTION WITH NO OTHERS
    
    #rmrandomspp <- function(orimat,plotrand = FALSE,randsummary = FALSE){
    if(plotrand == FALSE){
      ind <- apply(comat, 1, function(x) all(x==0))
      comat <- comat[!ind,]    
      ind <- apply(comat, 2, function(x) all(x==0))
      comat <- comat[,!ind]
      #ind <- apply(orimat, 1, function(x) all(x==0))
      #orimat <- orimat[!ind,]    
      #ind <- apply(orimat, 2, function(x) all(x==0))
      #orimat <- orimat[,!ind]
    }
    #return(orimat)
    #}
    
    #comat <- rmrandomspp(orimat = comat, dots)
    ####################################################### 
    
    postN <- nrow(comat)
    
    
    ##comat <- comat[order(rowSums(comat)),]
    ##comat <- comat[,order(colSums(comat))]
    
    #comat <- rmrandomspp(orimat = comat, ...)
    
    #ind <- apply(comat, 1, function(x) all(x==0))
    #comat <- comat[!ind,]
    #ind <- apply(comat, 2, function(x) all(x==0))
    #comat <- comat[,!ind]
    
    ind <- apply(comat, 1, function(x) all(x==0))
    comat <- comat[names(sort(ind)),]
    ind <- apply(comat, 2, function(x) all(x==0))
    comat <- comat[,names(sort(ind))]
    
    #comat
    data.m = melt(comat)
    colnames(data.m) <- c("X1","X2","value")
    data.m$X1 <- as.character(data.m$X1)
    data.m$X2 <- as.character(data.m$X2)
    
    meas <- as.character(unique(data.m$X2))
    
    dfids <- subset(data.m, X1 == X2)
    
    X1 <- data.m$X1
    X2 <- data.m$X2
    
    df.lower = subset(data.m[lower.tri(comat),],X1 != X2)
    
    ##### testing the rand summary
    if(randsummary == FALSE){  
    }else{
      dim <- nrow(comat)
      ext.dim <- round(dim*0.2,digits = 0)
      if(ext.dim<0){ext.dim<-1}
      placehold <- paste("ext_", rep(c(1:ext.dim),each = dim), sep="")
      
      randcol.df <- data.frame(
        X1 = placehold,
        X2 = rep(meas,times = ext.dim),
        value = rep(x = c(-2), times = dim*ext.dim))
      
      df.lower <- rbind(df.lower,randcol.df)
      meas <- c(meas,unique(placehold))
    }
    
    
    
    
    #####
    
    X1 <- df.lower$X1
    X2 <- df.lower$X2
    value <- df.lower$value
    
    
    
    ####
    if(randsummary == FALSE){  
      p <- ggplot(df.lower, aes(X1, X2)) + geom_tile(aes(fill = factor(value,levels=c(-1,0,1))), colour ="white") 
      p <- p + scale_fill_manual(values = c(cobiLB, lightGrey ,cobiDB), name = "", labels = c("Negative","Random","Positive"),drop=FALSE) + 
        theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(),panel.background = element_rect(fill='white', colour='white'),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = c(0.9, 0.5),legend.text=element_text(size=axis.label, family = font, color = darkGrey)) + 
        ggtitle("") + 
        xlab("") + ylab("") + 
        scale_x_discrete(limits=meas, expand = c(0.3, 0),drop=FALSE) + 
        scale_y_discrete(limits=meas, expand = c(0.3, 0),drop=FALSE) 
      p <- p + geom_text(data=dfids,aes(label=X1),hjust=1,vjust=0,angle = -22.5, family = font, color = darkGrey)#, color="dark gray")
      
      
    }else{
      
      p <- ggplot(df.lower, aes(X1, X2)) + geom_tile(aes(fill = factor(value,levels=c(-1,0,1,-2))), colour ="white") 
      p <- p + scale_fill_manual(values = c("#FFCC66","dark gray","light blue","light gray"), name = "", labels = c("negative","random","positive","random"),drop=FALSE) + 
        theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(),plot.title = element_text(vjust=-4,size=20, face="bold"),panel.background = element_rect(fill='white', colour='white'),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = c(0.9, 0.5),legend.text=element_text(size=18)) + 
        ggtitle("") + 
        xlab("") + ylab("") + 
        scale_x_discrete(limits=meas, expand = c(0.3, 0),drop=FALSE) + 
        scale_y_discrete(limits=meas, expand = c(0.3, 0),drop=FALSE) 
      p <- p + geom_text(data=dfids,aes(label=X1),hjust=1,vjust=0,angle = -22.5)#, color="dark gray")
      
      dim <- nrow(comat)
      ext_x <- dim + 0.5 #(ext.dim/2)
      ext_y <- dim + 1
      nrem <- origN - postN
      randtext <- paste(nrem, " completely\nrandom species")
      ext_dat <- data.frame(ext_x=ext_x,ext_y=ext_y,randtext=randtext)
      
      p <- p + geom_text(data=ext_dat,aes(x = ext_x,y = ext_y,label=randtext),hjust=0,vjust=0, color="dark gray")
    }
    ####
    
    p
    
  }

co <- plot.cooccur(cooccur)
co
