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
as.integer(data[1])!=length(data[-1][!grepl(" ",data[-1])])|
sum(as.integer(data[-1][!grepl(" ",data[-1])]))+
length(data[-1][!grepl(" ",data[-1])])+
1!=length(data)
){
stop("Number of cases is fishy.",call.=FALSE)
}


###############################################################################

## core computations

output<-NULL

j<-2
i<-1

while(i<=as.integer(data[1])&j<=length(data)){

if(!grepl(" ",data[j])){

a<-NULL
b<-NULL
count<-0

for(k in (j+1):(j+as.integer(data[j]))){
a<-c(a,as.integer(strsplit(data[k],split=" ")[[1]][1]))
b<-c(b,as.integer(strsplit(data[k],split=" ")[[1]][2]))
}

b<-b[order(a)]
a<-a[order(a)]

if(length(a)>1){
for(k in 1:(length(a)-1)){
count<-count+sum(b[(k+1):(length(a))]<b[k])
}
}

output<-c(output,
paste(
"Case #",i,": ",count,sep=""
)
)

i<-i+1
}

j<-j+1
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






