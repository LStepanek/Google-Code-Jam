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

getLastWord<-function(my_string){

my_letters<-strsplit(my_string,split="")[[1]]
output<-my_letters[1]

if(length(my_letters)==1){return(my_string)}

for(i in 2:length(my_letters)){

if(my_letters[i]>=output[1]){
output<-c(my_letters[i],output)
}else{
output<-c(output,my_letters[i])
}
}

return(paste(output,collapse=""))
}


###############################################################################

## flow control

if(
as.integer(data[1])!=length(data)-1
){
stop("Number of cases is fishy.",call.=FALSE)
}


###############################################################################

## core computations

output<-NULL

for(i in 1:as.integer(data[1])){

my_string<-data[i+1]

output<-c(
output,
paste(
"Case #",i,": ",getLastWord(my_string),sep=""
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






