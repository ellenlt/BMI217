# Author: Ellen Tsay

setwd("/Users/ellen/BMI217/HW1/")

# Function: FindGenotypesAtLocus
# Inputs:
#   snp: Number of the SNP of interest, corresponding to the row number of the SNP
#       as it appears in the data for that chromosome
#   data: mxn dataframe containing the genotypes of m individuals for a particular chromosome. 
#         First two columns are the individual's ID and number indicating whether the individual
#         has disease (1=no, 2=yes), and the other n-2 columns are the alleles at (n-2)/2 SNP loci
#   map: wxz dataframe containing information on SNPs for a particular chromosome.
#        There are w rows, one for each SNP, where w = (n-2)/2 above. Columns of interest are
#       "major.allele" and "minor.allele" which contain the major and minor alleles at an SNP locus
# Output: nx1 vector of strings representing the genotypes of n individuals.
#         "AA" means homozygous for major allele, "aa" means homozygous for minor
#         allele, and "Aa" means heterozygous.
FindGenotypesAtLocus <- function(snp, data, map){
  snp.idx <- 2*snp+1
  alleles <- data[, snp.idx:(snp.idx+1)]
  M <- map[snp, "major.allele"]
  m <- map[snp, "minor.allele"]
  result <- vector(, nrow(alleles))
  result[alleles[, 1] == "0" | alleles[,2] == "0"] <- NA
  result[alleles[, 1] == M & alleles[,2] == M] <- "AA"
  result[alleles[, 1] == M & alleles[,2] == m] <- "Aa"
  result[alleles[, 1] == m & alleles[,2] == M] <- "Aa"
  result[alleles[, 1] == m & alleles[,2] == m] <- "aa"
  result
}

# Function: ContingencyForSNP
# Inputs: 
# Output: Contingency table containing genotype counts (homozygous major, homozygous minor,
#         or heterozygous) for cases and controls at a particular SNP locus
ContingencyForSNP <- function(snp, case.dat, case.map, control.dat, control.map){
  genotype <- c(FindGenotypesAtLocus(snp, case.dat, case.map),
                FindGenotypesAtLocus(snp, control.dat, control.map))
  case <- c(rep("Y", nrow(case.dat)), rep("N", nrow(control.dat)))
  table(case, genotype)
}

# Function: TestSNPforSignificance
# Inputs: 
# Output: p-value of the Chi-Squared Test indicating whether or not the genotype distribution
#         at a particular SNP locus differs significantly between cases and controls
TestSNPforSignificance <- function(snp, case.dat, case.map, control.dat, control.map){
  contingency.table <- ContingencyForSNP(snp, case.dat, case.map, control.dat, control.map)
  contingency.table <- matrix(unlist(contingency.table), ncol = 3, byrow = F)
  chisq.test(contingency.table, correct = F)$p.value
}

# Function: LoadDataForChromosome
# This function assumes that the files are organized in a particular way, and may not
# work with all systems.
# Input: The number of the chromosome (1-22, X, Y, or XY)
# Output: A named list containing all data on the chromosome, containing the following elements:
#         "case.dat": mxn dataframe containing the genotypes of m case individuals for the chromosome. 
#                     First two columns are the individual's ID and number indicating whether the individual
#                     has disease (1=no, 2=yes), and the other n-2 columns are the alleles at (n-2)/2 SNP loci
#         "case.map": wxz dataframe containing information on SNPs for the chromosome.
#                   There are w rows, one for each SNP, where w = (n-2)/2 above. Columns of interest are
#                   "major.allele" and "minor.allele" which contain the major and minor alleles for cases
#                   at an SNP locus on the chromosome
#         "control.dat": same format as case.dat but for control individuals
#         "control.map": same format as case.map but for control individuals
LoadDataForChromosome <- function(kchr){
  case.map <- read.csv(paste("pd_map/chr", kchr, ".map", sep = ""), header = F, sep = "\t", stringsAsFactors = F)
  case.dat <- read.csv(paste("pd_pre/chr", kchr, ".pre", sep = ""), header = F, sep = " ", stringsAsFactors = F)
  control.map <- read.csv(paste("cc_map/chr", kchr, ".map", sep = ""), header = F, sep = "\t", stringsAsFactors = F)
  control.dat <- read.csv(paste("cc_pre/cc_chr", kchr, ".pre", sep = ""), header = F, sep = " ", stringsAsFactors = F)
  
  # Format individual ID's of controls so they match with ID's of cases
  control.dat[, 1] <- paste(control.dat[, 1], control.dat[, 2], sep = "-")
  control.dat[, 2] <- NULL
  names(control.map) <- c("chromosome", "location", "dbSNP.id", "major.allele", "minor.allele",
                          "major.allele.freq", "minor.allele.freq", "number.missing")
  names(case.map) <- c("chromosome", "location", "dbSNP.id", "major.allele", "minor.allele",
                       "major.allele.freq", "minor.allele.freq", "number.missing")
  
  # Correct "T" bases that were corrupted to "TRUE" when loading csv
  case.dat[case.dat == T] <- "T"
  control.dat[control.dat == T] <- "T"
  control.dat[, 2] <- 1
  
  list("case.map" = case.map, "case.dat" = case.dat, "control.map" = control.map, "control.dat" = control.dat)
}

# Function: LoadAllData
# Input: none
# Output: A nested list containing 25 named elements for each chromosome (1-22, X, Y, and XY).
#         Each element contains four dataframes:
#         "case.dat": mxn dataframe containing the genotypes of m case individuals for the chromosome. 
#                     First two columns are the individual's ID and number indicating whether the individual
#                     has disease (1=no, 2=yes), and the other n-2 columns are the alleles at (n-2)/2 SNP loci
#         "case.map": wxz dataframe containing information on SNPs for the chromosome.
#                   There are w rows, one for each SNP, where w = (n-2)/2 above. Columns of interest are
#                   "major.allele" and "minor.allele" which contain the major and minor alleles for cases
#                   at an SNP locus on the chromosome
#         "control.dat": same format as case.dat but for control individuals
#         "control.map": same format as case.map but for control individuals
LoadAllData <- function(){
  data <- list()
  for(i in 1:22){
    data[[paste("chr", i, sep = "")]] <- LoadDataForChromosome(i)
  }
  data[["chrX"]] <- LoadDataForChromosome("X")
  data[["chrXY"]] <- LoadDataForChromosome("XY")
  data[["chrY"]] <- LoadDataForChromosome("Y")
  data
}

# Function: FindSignificantSNPsOnChromosome
# Inputs:
# Output: Dataframe containing the dbSNP ID's of the top ten most significant SNPs on 
#         a particular chromosome and their associated p-values.
FindSignificantSNPsOnChromosome <- function(case.dat, case.map, control.dat, control.map){
  kSNPs <- nrow(control.map)
  pvals <- sapply(1:kSNPs, function(x) 
    TestSNPforSignificance(x, case.dat, case.map, control.dat, control.map))
  top.ten.idx <- order(pvals)[1:10]
  data.frame(dbSNP.id = control.map[top.ten.idx,"dbSNP.id"], pvalue = pvals[top.ten.idx])
}

data <- LoadAllData()

FindSignificantSNPsOnChromosome(data$chr1$case.dat, data$chr1$case.map, data$chr1$control.dat, data$chr1$control.map)

FindSignificantSNPsInGenome(data){
  snps <- list()
  for(i in 1:22){
    name <- paste("chr", i, sep = "")
    snps[[name]] <- FindSignificantSNPsOnChromosome(data[name])
  }
  snps
}







# OLD FUNCTION
determineGenotype <- function(a1,a2,M,m){
  if(a1==0 || a2==0) {
    "missing"
  } else if(a1==M && a2==m || a1==m && a2==M){
    "Aa"
  } else if(a1==a2 && a1==M) {
    "AA"
  } else if(a1==a2 && a1==m) {
    "aa"
  }
}
# END OLD FUNCTION

findGenotypesForIndividual <- function(individual){
  sequence <- matrix(individual[3:length(individual)],ncol=2,byrow=T)
  genotypes <- mapply(determineGenotype, sequence[,1], sequence[,2], 
                      cc.chr1.map[,"major.allele"], cc.chr1.map[,"minor.allele"])
}

#Current runtime: 86 s for 31,532 snps for 271 people
system.time(genotypeCounts <- apply(cc.chr1.pre, 1, findGenotypesForIndividual))

# Test code
sequence <- matrix(cc.chr1.pre[1,3:ncol(cc.chr1.pre)],ncol=2,byrow=T)
genotypes <- mapply(determineGenotype, sequence[,1], sequence[,2], 
                    cc.chr1.map[,"major.allele"], cc.chr1.map[,"minor.allele"])
sequence <- cbind(sequence,genotypes)

individual<- cc.chr1.pre[1,]
sequence <- matrix(individual[3:length(individual)],ncol=2,byrow=T)
genotypes <- mapply(SIMPLIFY = F, determineGenotype, sequence[,1], sequence[,2], 
                    cc.chr1.map[,"major.allele"], cc.chr1.map[,"minor.allele"])
# End Test code



