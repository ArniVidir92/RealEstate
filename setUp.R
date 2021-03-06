# Þessi skrá sér um að fjarlægja þá dálka sem við þurfum ekki í módelinu
library(MASS)
library(ggplot2)
library(plyr)
library(faraway)
library(car)
library(gridExtra) 
library(MASS)
library(data.table)
library(GGally)

verdmat <-read.table("gagnasafn_endurmat2017_litid.csv", sep=',', header=T)
RE_data <- data.frame(verdmat)
dt.re <- data.table(verdmat)
#Matsv:
#  Vesturb. 11
#  miðbær s þ 31
#  hlíðar 80
#  grafarvogur 120
#  seljahverfi 150

dt.verdmat <- data.table(verdmat)
dt.RE_data <- dt.verdmat[matssvaedi %in% c(11,31,80,120,150)]
# Bæta við dálki kaupár
dt.RE_data <- dt.RE_data[,k.ar:=as.numeric(substring(kdagur,1,4))]
dt.RE_data <- dt.RE_data[,matssvaedi:=as.factor(matssvaedi)]
dt.RE_data <- dt.RE_data[,undirmatssvaedi:=as.factor(undirmatssvaedi)]
dt.RE_data <- dt.RE_data[,teg_eign:=as.factor(teg_eign)]


# Byrjum á að taka út þær breytur sem við teljum ekki  
dt.RE_data <- subset(dt.RE_data, select=c("kdagur","nuvirdi","teg_eign","byggar","haednr","lyfta","ibm2","fjhaed",
                                          "fjbilast","fjbkar","fjsturt","fjklos","fjeld","fjherb",
                                          "fjstof","fjgeym","stig10","matssvaedi","undirmatssvaedi","ibteg","k.ar"))

dt.RE_data <- dt.RE_data[,kdagur:=as.Date(kdagur)]

#breyta lyftu í boolean og breyta ibteg í factor, 11=serbýli, 12=fjölbýli

for(i in 1:nrow(dt.RE_data)){
  if(dt.RE_data$lyfta[i]!=0){dt.RE_data$lyfta[i]=1}
  else{}
}

for(i in 1:nrow(dt.RE_data)){
  if(dt.RE_data$ibteg[i]==11){dt.RE_data$ibteg="Serbyli"}
  else{dt.RE_data$ibteg[i]="Fjolbyli"}
}

dt.RE_data <- dt.RE_data[,ibteg:=as.factor(ibteg)]

n<-dim(dt.RE_data)[1]

# Training and testing data sets
set.seed(1)
rows<-sample(1:n,n/3)
train <- data.table(dt.RE_data[-rows,])
test <- data.table(dt.RE_data[rows,])


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}