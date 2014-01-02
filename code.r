library(tsne)
colors = rainbow(length(unique(iris$Species)))
names(colors) = unique(iris$Species)
ecb <- function(x,y) {
  plot(x,t="n")
  text(x,labels=iris$Species, col=colors[iris$Species])
}
tsne_iris = tsne(
  iris[,1:4],
  epoch_callback = ecb,
  perplexity=50
)
# compare to PCA
dev.new()
pca_iris = princomp(iris[,1:4])$scores[,1:2]
plot(pca_iris, t="n")
text(pca_iris, labels=iris$Species,col=colors[iris$Species])

#http://homepage.tudelft.nl/19j49/t-SNE.html
#http://homepage.tudelft.nl/19j49/t-SNE_files/SP500_tsne.png
#http://www.linkedin.com/in/stevewickert
#http://www.cs.ubc.ca/cgi-bin/tr/2012/TR-2012-01.pdf
#youtube of google talk 


library(quantmod)
dowtickers <- c(
  'AXP',
  'BA',
  'CAT',
  'CSCO',
  'CVX',
  'DD',
  'DIS',
  'GE',
  'GS',
  'HD',
  'IBM',
  'INTC',
  'JNJ',
  'JPM',
  'KO',
  'MCD',
  'MMM',
  'MRK',
  'MSFT',
  'NKE',
  'PFE',
  'PG',
  'T',
  'TRV',
  'UNH',
  'UTX',
  'V',
  'VZ',
  'WMT',
  'XOM'
)

getSymbols(dowtickers,src="google",from="2012-12-31")

prices <- do.call(
  merge,
  lapply(
    dowtickers,
    FUN = function(x){
      tempdata <- get(x)[,4]
      colnames(tempdata) <- x
      return(tempdata)
    }),
)

returns <- ROC(prices, n = 1, type = "discrete")
returns[1,] <- 0


ecb.returns <- function(x,y){
  plot(x,t="n")
  text(x,labels=rownames(t(as.matrix(returns))),cex=0.7)
}

tsne.dow <- tsne(
  t(as.matrix(returns)),
  epoch_callback = ecb.returns,
  max_iter=5000,
  perplexity = 5,
  initial_dims = 3
)
#plot(tsne.dow)


pimcotickers <- c("PISIX","PSKIX","PSDIX","PSTKX","PCRIX","PFIIX","PHMIX","PFCIX","PCDIX","PTSHX","PFMIX","PLMIX","PSPTX","PCIMX","PSTIX","PNYIX","PLDTX","PLDIX","PTLDX","PAAIX","PXTIX","PHIYX","PSCSX","PAUIX","PTRIX","PGBIX","PFORX","PELBX","PDMIX","PMDRX","PEBIX","PDIIX","PRRSX","PMBIX","PTSAX","PTTRX","PIGLX","PRRIX","PFUIX","PIMIX","PIGIX","PRAIX","PLRIX","PGOVX","PEDIX","VFINX")

getSymbols(pimcotickers,from="2012-12-31")

prices <- do.call(
  merge,
  lapply(
    pimcotickers,
    FUN = function(x){
      tempdata <- get(x)[,4]
      colnames(tempdata) <- x
      return(tempdata)
    }),
)

returns <- ROC(prices, n = 1, type = "discrete")
returns[1,] <- 0

tsne.pimco <- tsne(
  t(as.matrix(coredata(returns))),
  epoch_callback = ecb.returns,
  max_iter=5000,
  perplexity = 5,
  initial_dims = 3
)

#run an interesting corrgram chart
library(corrgram)
df1 <- data.frame(coredata(returns))
corrgram(df1, order=TRUE, 
         main="PIMCO data PC2/PC1 order",
         lower.panel=panel.shade, upper.panel=panel.pie,
         text.panel=panel.txt)       #using techniques from SciViews package


assetsCorEigenPlot(as.timeSeries(returns)) 
