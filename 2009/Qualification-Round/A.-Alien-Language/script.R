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

getMyWord<-function(string){

my_word<-list()

i<-1
while(i<=length(strsplit(string,"")[[1]])){

if(substr(string,i,i)!="("&substr(string,i,i)!=")"){
my_word[[length(my_word)+1]]<-substr(string,i,i)
i<-i+1
}

if(substr(string,i,i)=="("){
j<-i+1
group<-NULL
while(substr(string,j,j)!=")"){
group<-c(group,substr(string,j,j))
j<-j+1
}
my_word[[length(my_word)+1]]<-group
i<-j+1
}

}

return(my_word)
}


numberOfMatches<-function(dictionary,word){

my_length<-length(strsplit(dictionary[1],split="")[[1]])
word<-getMyWord(word)
output<-0

for(i in 1:length(dictionary)){

are_the_same<-TRUE
j<-1

while(are_the_same&j<=my_length){
if(!substr(dictionary[i],j,j)%in%word[[j]]){are_the_same<-FALSE}
j<-j+1
}

if(are_the_same){output<-output+1}
}

return(output)
}


###############################################################################

## flow control

if(sum(as.integer(strsplit(data[1],split=" ")[[1]])[2:3])!=length(data)-1){
stop("Number of cases is fishy.",call.=FALSE)
}


###############################################################################

## core computations

output<-NULL
dictionary<-data[2:(as.integer(strsplit(data[1]," ")[[1]][2])+1)]

for(i in 1:as.integer(strsplit(data[1]," ")[[1]][3])){

output<-c(output,
paste("Case #",i,": ",
numberOfMatches(dictionary,
data[as.integer(strsplit(data[1]," ")[[1]][2])+1+i]
),
sep=""
))

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






