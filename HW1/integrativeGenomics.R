# Author: Ellen Tsay

setwd("/Users/ellen/BMI217/HW1/")

pd.chr1.map <- read.csv("pd_map/chr1.map",header = F,sep = "\t",stringsAsFactors=F)
pd.chr1.pre <- read.csv("pd_pre/chr1.pre",header = F,sep = " ",stringsAsFactors=F)
cc.chr1.map <- read.csv("cc_map/chr1.map",header = F,sep = "\t",stringsAsFactors=F)
cc.chr1.pre <- read.csv("cc_pre/cc_chr1.pre",header = F,sep = " ",stringsAsFactors=F)
# Format individual ID's of controls so they match with ID's of cases
cc.chr1.pre[,1] <- paste(cc.chr1.pre[,1],cc.chr1.pre[,2],sep="-")
cc.chr1.pre[,2] <- NULL
names(cc.chr1.map) <- c("chromosome","location","dbSNP.id","major.allele","minor.allele",
                        "major.allele.freq","minor.allele.freq","number.missing")
names(pd.chr1.map) <- c("chromosome","location","dbSNP.id","major.allele","minor.allele",
                        "major.allele.freq","minor.allele.freq","number.missing")

# Correct "T" bases that were corrupted to "TRUE" when loading csv
cc.chr1.pre[cc.chr1.pre==T] <- "T"
cc.chr1.pre[,2] <- 1
pd.chr1.pre[pd.chr1.pre==T] <- "T"


determineGenotype <- function(alleles,M,m){
  result <- vector(,nrow(alleles))
  result[alleles[,1]=="0" | alleles[,2]=="0"] <- "missing"
  result[alleles[,1]==M & alleles[,2]==M] <- "AA"
  result[alleles[,1]==M & alleles[,2]==m] <- "Aa"
  result[alleles[,1]==m & alleles[,2]==M] <- "Aa"
  result[alleles[,1]==m & alleles[,2]==m] <- "aa"
  result
}


findGenotypeDistributionForSNP <- function(snp,data,map){
  snpIdx <- 2*snp+1
  alleles <- data[,snpIdx:(snpIdx+1)]
  genotypes <- determineGenotype(alleles, map[snp,"major.allele"], map[snp,"minor.allele"])
  table(genotypes)
}

#Current runtime: 58 s for 31,532 snps
system.time(genotypeCounts <- lapply(1:2, 
                                     function(x) findGenotypeDistributionForSNP(x,cc.chr1.pre, cc.chr1.map)))

# for each of 63,064 loci...
# vector of 271 cases, and vectors of 271 controls
# count number of homozygous (major), homozygous (minor), and heterozygous controls
# count number of homozygous (major), homozygous (minor), and heterozygous cases

# Problem 1
# Using all the control and affected individuals, calculate for each SNP locus
# the number of individuals having each of the three possible genotypes (“AA”, “Aa”, and “aa”). 
# At each locus, determine the likelihood that the genotype at the locus is significantly 
# different in Parkinson’s Disease individuals versus control individuals, using 
# chi-squared testing. You will need to decide what files will provide the information
# you need to compute the test. List the top 10 SNP loci asso. with Parkinson’s Disease,
# ordered by chi-squared test p-val. (20 pts)


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


            
            