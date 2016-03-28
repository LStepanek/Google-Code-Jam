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

matrixRotation<-function(my_matrix){
return(t(apply(my_matrix,2,rev)))
}


gravityEffect<-function(my_matrix){
moved<-TRUE

while(moved){
moved<-FALSE
for(i in 1:dim(my_matrix)[2]){
for(j in (dim(my_matrix)[1]-1):1){
if(my_matrix[j+1,i]=="."&(my_matrix[j,i]%in%c("R","B"))){
my_matrix[j+1,i]=my_matrix[j,i]
my_matrix[j,i]="."
moved<-TRUE
}
}
}
}

return(my_matrix)
}


checkTheLine<-function(my_line,symbol){
output<-NULL
i<-1

while(i<=length(my_line)){
if(my_line[i]==symbol){
my_row<-0
j<-0

while(my_line[i+j]==symbol&i+j<=length(my_line)){
j<-j+1
}

output<-c(output,j)
i<-i+j
}
i<-i+1
}

if(length(output)>0){return(max(output))}else{return(0)}
}


isJoinedK<-function(my_matrix,symbol,K){
is_joined_K<-FALSE

i<-1
while(!is_joined_K&i<=dim(my_matrix)[1]){
if(checkTheLine(my_matrix[i,],symbol)>=K){is_joined_K<-TRUE}
i<-i+1
}

i<-1
while(!is_joined_K&i<=dim(my_matrix)[2]){
if(checkTheLine(my_matrix[,i],symbol)>=K){is_joined_K<-TRUE}
i<-i+1
}

my_diagonal<-split(my_matrix,row(my_matrix)-col(my_matrix))

i<-1
while(!is_joined_K&i<=length(my_diagonal)){
if(checkTheLine(my_diagonal[[i]],symbol)>=K){is_joined_K<-TRUE}
i<-i+1
}

my_diagonal<-split(apply(my_matrix,2,rev),row(my_matrix)-col(my_matrix))

i<-1
while(!is_joined_K&i<=length(my_diagonal)){
if(checkTheLine(my_diagonal[[i]],symbol)>=K){is_joined_K<-TRUE}
i<-i+1
}

return(is_joined_K)
}


###############################################################################

## flow control

if(
as.integer(data[1])!=sum(grepl(" ",data))|
sum(as.integer(unlist(strsplit(data[grepl(" ",data)],split=" "))[
seq(1,2*as.integer(data[1]),2)
]))+
length(strsplit(data[grepl(" ",data)],split=" "))+
1!=
length(data)
){
stop("Number of cases is fishy.",call.=FALSE)
}


###############################################################################

## core computations

output<-NULL

i<-1
j<-1
result<-""

while(i<=as.integer(data[1])&j<=length(data)){

if(grepl(" ",data[j])){
K<-as.integer(strsplit(data[j],split=" ")[[1]][2])

my_matrix<-NULL
for(k in 1:as.integer(strsplit(data[j],split=" ")[[1]][1])){
my_matrix<-rbind(
my_matrix,
strsplit(data[j+k],split="")[[1]]
)
}

my_matrix<-gravityEffect(matrixRotation(my_matrix))

B_joined_K<-isJoinedK(my_matrix,"B",K)
R_joined_K<-isJoinedK(my_matrix,"R",K)

if(B_joined_K&R_joined_K){result<-"Both"}
if(B_joined_K&!R_joined_K){result<-"Blue"}
if(!B_joined_K&R_joined_K){result<-"Red"}
if(!(B_joined_K|R_joined_K)){result<-"Neither"}

output<-c(
output,
paste("Case #",i,": ",result,sep="")
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






