### The overall goal of this code was to conduct an analysis investigating the cost of carnivory in plants
### A simple t-test was done to evaluate a significant difference in cost of creating plant organs 
## in carnivorous plants compared to non-carnivorous ones

#After this, the phylogenetic tree was created and labelled (based on a tree created in a separate program)
### A phylogenetic generalized linear model was run to account for phylogenetic non-independence between data points
## a phylogenetic variance-covariance function was used to compute the variances and covariances of a continuous trait (carnivory costs)

###The phylogenetic dataset was combined with the created phylogenetic tree of plant species
### Lastly a phylogenetic generalized linear model was run using maximum likelihood to assess the relationship between production costs and carnivorous plants

setwd("C:/Users/alanc")

CRNdata <- read.table("carnivory.cc.txt",header=TRUE)

carnivory <- CRNdata$Carnivory
cost <- CRNdata$Cost

t.test(cost~carnivory)

if (!require("pacman")) install.packages("pacman")
pacman::p_load(ape, caper)


CRNtree <- read.tree('carnivory.species.nwk.txt')
CRNtree.labeled <- makeLabel(CRNtree)
CRNtree.labeled$node.label

CRNdata <- read.table("carnivory.cc.txt",header=TRUE)

CRN.caper <- comparative.data(CRNtree.labeled,CRNdata, Species, vcv = TRUE)
CRN.caper

CRN.phylo.analysis <- pgls(Cost~Carnivory,CRN.caper,lambda='ML')
summary(CRN.phylo.analysis)

rm(list=ls())
