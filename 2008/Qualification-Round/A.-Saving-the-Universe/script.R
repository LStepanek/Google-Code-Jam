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

if(
as.integer(data[1])!=(length(which(!is.na(as.integer(data))))-1)/2|
sum(as.integer(data[which(!is.na(as.integer(data)))][-1]))+
length(as.integer(data[which(!is.na(as.integer(data)))][-1]))+
1!=
length(data)
){
stop("Number of cases is fishy.",call.=FALSE)
}


###############################################################################

## core computations

output<-NULL

i<-1
j<-2


while(!is.na(as.integer(data[j]))){

if(!is.na(as.integer(data[j]))){

engines<-data[(j+1):(j+as.integer(data[j]))]

if(as.integer(data[(j+as.integer(data[j])+1)])>0){
queries<-data[(j+as.integer(data[j])+2):
(j+as.integer(data[j])+1+as.integer(data[j+as.integer(data[j])+1]))]
}else{
queries<-NULL
}

switch<-0

if(length(queries)>0){

temporary_queries<-NULL
k<-1

while(k<=length(queries)){

if(length(unique(c(temporary_queries,queries[k])))==length(engines)){
temporary_queries<-queries[k]
switch<-switch+1
}

temporary_queries<-c(temporary_queries,queries[k])
k<-k+1
}

}


output<-c(output,
paste("Case #",i,": ",switch,sep="")
)

i<-i+1
}

j<-j+as.integer(data[j])+1+as.integer(data[j+as.integer(data[j])+1])+1
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






