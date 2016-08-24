library(data.table)
library(dplyr)
library(ggplot2)
library(ggcorplot)

filename<-"results_regions_5percent_ss_10kb_NEW_Affy_snps_210116_nohap_transformer_1M_8pls.txt_500_model0_BestSimsParamStats_Obs0.txt"
data<-fread(filename,data.table = FALSE)
columns.select<-c(3:6,7:12)
colnames<-names(data)

data.subset<-data %>% select(columns.select)

panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.6, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.4, txt2)
}

pairs(iris, upper.panel = panel.cor)

pairs(data.subset, lower.panel=panel.smooth,upper.panel = panel.cor)

##
library(Deducer)
data(mtcars)
corr.mat1<-cor.matrix(variables=d(mpg,carb,carb+rnorm(length(carb))),
                      data=mtcars,
                      test=cor.test,
                      method='spearman',
                      alternative="two.sided",exact=FALSE)

p<-ggcorplot(corr.mat1,data = mtcars)
print(p)
## Not run: 

has.hex<-require("hexbin")
if(has.hex){
  data(diamonds)
  corr.mat<-cor.matrix(variables=d(price,carat,color),
                       data=diamonds,
                       test=cor.test,
                       method='spearman',
                       alternative="two.sided")
  
  p1 <- ggcorplot(cor.mat=corr.mat,data=diamonds,type="bins",
                  cor_text_limits=c(5,15),
                  lines=FALSE)
  print(p1)
  rm('corr.mat')
  
}

##



corr.mat2<-cor.matrix(variables=d(Asc_NAF,Asc_NEU,Asc_NCHB,daf,Log10_NAF,Log10_NEU,Log10_NCHB,NEU_AS,TEU_AS,TAF),
                      data=data.subset,
                      test=cor.test,
                      method='spearman',
                      alternative="two.sided",exact=FALSE)

p<-ggcorplot(corr.mat2,data = data.subset)
print(p)
## Not run: 

has.hex<-require("hexbin")
if(has.hex){
  data(diamonds)
  corr.mat<-cor.matrix(variables=d(price,carat,color),
                       data=diamonds,
                       test=cor.test,
                       method='spearman',
                       alternative="two.sided")
  
  p1 <- ggcorplot(cor.mat=corr.mat,data=diamonds,type="bins",
                  cor_text_limits=c(5,15),
                  lines=FALSE)
  print(p1)
  rm('corr.mat')
  
}