###############################################################################
###############################################################################
###############################################################################

## I am loading input data ----------------------------------------------------

my_input <- file.choose()
my_data <- readLines(my_input)


## ----------------------------------------------------------------------------

###############################################################################

## I am changing a working directory ------------------------------------------

setwd(
    gsub("(.*\\\\)(.*)", "\\1", my_input)
)


## ----------------------------------------------------------------------------

###############################################################################

## helper functions -----------------------------------------------------------

isTidy <- function(my_digits){
    
    # '''
    # Returns TRUE if the integer constisting of digits "my_digits"
    # is tidy, i. e. its digits "my_digits" are sorted
    # in non-decreasing order. Otherwise it returns FALSE.
    # '''
    
    i <- 1
    
    while(my_digits[i] <= my_digits[i + 1] & i < length(my_digits)){
        
        i <- i + 1
        
    }
    
    return(i == length(my_digits))
    
}


solveTheCase <- function(my_case){
    
    # '''
    # Returns a solution for one case of the input data.
    # '''
    
    my_digits <- as.integer(strsplit(my_case, split = "")[[1]])
    
    i <- length(my_digits)
    
    while(!isTidy(my_digits) & i > 0){
        
        my_digits[i] <- 9
        my_digits[i - 1] <- my_digits[i - 1] - 1
        i <- i - 1
        
    }
    
    if(my_digits[1] == "0"){
        
        my_digits <- my_digits[2:length(my_digits)]
        
    }
    
    return(paste(my_digits, collapse = ""))
    
}


## ----------------------------------------------------------------------------

###############################################################################

## flow control ---------------------------------------------------------------

if(
    as.integer(my_data[1]) != length(my_data) - 1
){
    stop("Number of cases is fishy.", call. = FALSE)
}


## ----------------------------------------------------------------------------

###############################################################################

## core computations ----------------------------------------------------------

my_output <- NULL

for(i in 1:as.integer(my_data[1])){

    my_case <- my_data[i + 1]

    my_output <- c(
        my_output,
        paste(
            "Case #",
            i,
            ": ",
            solveTheCase(my_case),
            sep = ""
        )
    )
    
    flush.console()
    print(
        paste(
            "Process is ",
            format(
                round(
                    i / as.integer(my_data[1]) * 100,
                    digits = 2
                ),
                nsmall = 2
            ),
            " % complete.",
            sep = ""            
        )
    )
    
}


## ----------------------------------------------------------------------------

###############################################################################

## I am giving an output ------------------------------------------------------

writeLines(
    my_output,
    con = gsub(
        ".in",
        "-out.in",
        gsub("(.*\\\\)(.*)", "\\2", my_input)
    )
)


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################





