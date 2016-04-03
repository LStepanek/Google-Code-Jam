###############################################################################
###############################################################################
###############################################################################

## library loading

library(gmp)


###############################################################################

## data loading

input<-file.choose()
data<-readLines(input)


###############################################################################

## changing of working directory

setwd(
gsub(
gsub("(.*)/","",gsub("\\","/",input,fixed=TRUE)),"",
gsub("\\","/",input,fixed=TRUE)
)
)


###############################################################################

## managing of number printing as fixed one

options("scipen"=25)


###############################################################################

## flow control

if(as.integer(data[1])!=length(data)-1){
stop("Number of cases is fishy.",call.=FALSE)
}


###############################################################################

## core computations

output<-NULL

for(i in 1:as.integer(data[1])){

my_value<-NULL
my_string<-strsplit(data[i+1],split="")[[1]]

if(length(my_string)==1){my_value<-1}else{

digits<-as.data.frame(
matrix(rep(NA,length(unique(my_string))),ncol=1)
)
rownames(digits)<-unique(my_string)
colnames(digits)<-"value"
my_base<-max(length(unique(my_string)),2)
unused_numbers<-seq(0,length(unique(my_string))-1,1)

digits[my_string[1],]<-1
unused_numbers<-unused_numbers[-which(unused_numbers==digits[my_string[1],])]

for(j in 2:length(my_string)){
if(is.na(digits[my_string[j],])){
digits[my_string[j],]<-min(unused_numbers)
unused_numbers<-unused_numbers[-which(unused_numbers==digits[my_string[j],])]
}
}

for(j in 1:length(my_string)){
my_value<-cbind(
my_value,
digits[my_string[j],]*as.bigz(my_base)^(length(my_string)-j)
)
}
}

output<-c(
output,
paste("Case #",i,": ",sum(my_value,initLine=FALSE),sep="")
)
}


###############################################################################

## outputting

writeLines(
output,
con=gsub(".in","-out.in",gsub("(.*)/","",gsub("\\","/",input,fixed=TRUE)))
)


###############################################################################
###############################################################################
###############################################################################






