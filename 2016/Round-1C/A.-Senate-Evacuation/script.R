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

solveTheCase<-function(my_counts){

my_counts<-as.integer(strsplit(my_counts,split=" ")[[1]])
my_senators<-NULL
my_plan<-NULL

for(i in 1:length(my_counts)){
my_senators<-c(my_senators,rep(LETTERS[i],my_counts[i]))
}

while(length(my_senators)>0){

my_step<-labels(which.max(table(my_senators)))
my_table<-table(my_senators[-which(my_senators==my_step)[1]])

if(prod(unname(my_table/sum(my_table))<=0.5)){

my_senators<-my_senators[-which(my_senators==my_step)[1]]
my_plan<-c(my_plan,my_step)

}else{

my_substep<-""

for(i in 1:2){
my_step<-labels(which.max(table(my_senators)))
my_senators<-my_senators[-which(my_senators==my_step)[1]]
my_substep<-paste(my_substep,my_step,sep="")
}

my_plan<-c(my_plan,my_substep)

}

}

return(paste(my_plan,collapse=" "))
}


###############################################################################

## flow control

if(
as.integer(data[1])!=(length(data)-1)/2
){
stop("Number of cases is fishy.",call.=FALSE)
}


###############################################################################

## core computations

output<-NULL

for(i in 1:as.integer(data[1])){

my_counts<-data[2*i+1]

output<-c(
output,
paste(
"Case #",i,": ",solveTheCase(my_counts),sep=""
)
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






