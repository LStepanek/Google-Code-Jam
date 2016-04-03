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

## helper functions

getIdenticalBeginningPart<-function(a,b){

if(sum(is.na(c(a,b)))>0){
return("")
}

a_split<-strsplit(a,split="/")[[1]]
b_split<-strsplit(b,split="/")[[1]]

i<-1
while(a_split[i]==b_split[i]){
i<-i+1
}

return(paste(a_split[1:(i-1)],collapse="/"))
}


getParentFolders<-function(my_path){
output<-NULL

for(item in strsplit(my_path,split="/")[[1]][-1]){
output<-c(output,paste(output[length(output)],item,sep="/"))
}

return(output)
}


###############################################################################

## flow control

if(
as.integer(data[1])!=sum(grepl(" ",data))|
sum(as.integer(unlist(strsplit(data[grepl(" ",data)],split=" "))))+
length(strsplit(data[grepl(" ",data)],split=" "))+
1!=length(data)
){
stop("Number of cases is fishy.",call.=FALSE)
}


###############################################################################

## core computations

output<-NULL
i<-1
j<-1

while(i<=as.integer(data[1])&j<=length(data)){

if(grepl(" ",data[j])){

count<-0
my_files<-NULL

for(k in (j+1):(j+sum(as.integer(strsplit(data[j]," ")[[1]])))){
my_files<-c(my_files,data[k])
}

existing<-my_files[
(0:as.integer(strsplit(data[j]," ")[[1]])[1])
]
creating<-my_files[
(as.integer(strsplit(data[j]," ")[[1]])[1]+1):
(sum(as.integer(strsplit(data[j]," ")[[1]])))
]


if(length(existing)==0){

for(k in 1:length(creating)){
my_paths<-getParentFolders(creating[k])
for(path in my_paths){
if(!(path%in%existing)){
existing<-c(existing,path)
count<-count+1
}
}
}

}else{
for(l in 1:length(existing)){
existing<-c(existing,getParentFolders(existing))
}
existing<-unique(existing)

for(k in 1:length(creating)){
my_paths<-getParentFolders(creating[k])
for(path in my_paths){
if(!(path%in%existing)){
existing<-c(existing,path)
count<-count+1
}
}
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






