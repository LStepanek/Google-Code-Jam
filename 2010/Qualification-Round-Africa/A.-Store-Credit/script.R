###############################################################################
###############################################################################
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

## flow control

if(as.integer(data[1])!=(length(data)-1)/3){
stop("Number of cases is fishy.",call.=FALSE)
}


###############################################################################

## core computations

output<-NULL

for(i in 1:as.integer(data[1])){

credit<-as.integer(data[3*i-1])
prices<-as.integer(strsplit(data[3*i+1],split=" ")[[1]])

for(j in 1:(length(prices)-1)){
for(k in (j+1):length(prices)){
if(prices[j]+prices[k]==credit){break}
}
if(prices[j]+prices[k]==credit){break}
}

output<-c(
output,
paste("Case #",i,": ",j," ",k,sep="")
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






