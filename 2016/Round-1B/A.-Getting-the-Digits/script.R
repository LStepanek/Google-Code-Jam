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

solveTheCase<-function(my_string){

my_digits<-c(
"ZERO","ONE","TWO","THREE","FOUR","FIVE","SIX","SEVEN","EIGHT","NINE"
)

my_letters<-strsplit(my_string,split="")[[1]]

my_number<-NULL

while("Z"%in%my_letters){
my_number<-c(my_number,0)
for(item in strsplit(my_digits[1],split="")[[1]]){
my_letters<-my_letters[-which(my_letters==item)[1]]
}
}

while("W"%in%my_letters){
my_number<-c(my_number,2)
for(item in strsplit(my_digits[3],split="")[[1]]){
my_letters<-my_letters[-which(my_letters==item)[1]]
}
}

while("U"%in%my_letters){
my_number<-c(my_number,4)
for(item in strsplit(my_digits[5],split="")[[1]]){
my_letters<-my_letters[-which(my_letters==item)[1]]
}
}

while("X"%in%my_letters){
my_number<-c(my_number,6)
for(item in strsplit(my_digits[7],split="")[[1]]){
my_letters<-my_letters[-which(my_letters==item)[1]]
}
}

while("G"%in%my_letters){
my_number<-c(my_number,8)
for(item in strsplit(my_digits[9],split="")[[1]]){
my_letters<-my_letters[-which(my_letters==item)[1]]
}
}

while("O"%in%my_letters){
my_number<-c(my_number,1)
for(item in strsplit(my_digits[2],split="")[[1]]){
my_letters<-my_letters[-which(my_letters==item)[1]]
}
}

while("R"%in%my_letters){
my_number<-c(my_number,3)
for(item in strsplit(my_digits[4],split="")[[1]]){
my_letters<-my_letters[-which(my_letters==item)[1]]
}
}

while("F"%in%my_letters){
my_number<-c(my_number,5)
for(item in strsplit(my_digits[6],split="")[[1]]){
my_letters<-my_letters[-which(my_letters==item)[1]]
}
}

while("V"%in%my_letters){
my_number<-c(my_number,7)
for(item in strsplit(my_digits[8],split="")[[1]]){
my_letters<-my_letters[-which(my_letters==item)[1]]
}
}

while("I"%in%my_letters){
my_number<-c(my_number,9)
for(item in strsplit(my_digits[10],split="")[[1]]){
my_letters<-my_letters[-which(my_letters==item)[1]]
}
}

return(paste(sort(my_number),collapse=""))
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
"Case #",i,": ",solveTheCase(my_string),sep=""
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






