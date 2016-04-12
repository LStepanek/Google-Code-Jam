###############################################################################
###############################################################################
###############################################################################

## data loading

input<-file.choose()
data<-readLines(input)


###############################################################################

## setting of integer displaying

options("scipen"=25)


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
as.integer(data[1])!=length(data)-1
){
stop("Number of cases is fishy.",call.=FALSE)
}


###############################################################################

## core computations

output<-NULL

for(i in 1:as.integer(data[1])){

my_number<-as.integer(data[i+1])
actual_number<-my_number
my_digits<-unique(strsplit(as.character(my_number),split="")[[1]])
k<-2
next_number<-k*my_number

while(length(my_digits)<10&next_number!=actual_number){

actual_number<-k*my_number
my_digits<-unique(c(
my_digits,
strsplit(as.character(actual_number),split="")[[1]]
))
k<-k+1
next_number<-k*actual_number

}

if(next_number==actual_number){
output<-c(
output,
paste("Case #",i,": ","INSOMNIA",sep="")
)
}else{
output<-c(
output,
paste("Case #",i,": ",actual_number,sep="")
)
}

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






