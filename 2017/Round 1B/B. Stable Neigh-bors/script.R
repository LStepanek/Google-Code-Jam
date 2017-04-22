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

solveTheCase <- function(
    
    my_case
    
){
    
    # '''
    # Returns a solution for one case of the input data (small dataset).
    # '''
    
    my_unicorn_numbers <- as.integer(
        strsplit(my_case, split = " ")[[1]]
    )
    
    for(my_letter in c("N", "R", "O", "Y", "G", "B", "V")){
        
        assign(
            my_letter,
            my_unicorn_numbers[
                which(c("N", "R", "O", "Y", "G", "B", "V") == my_letter)
            ]
        )
        
    }
    
    if(
        c(R, Y, B)[which.max(c(R, Y, B))] >
        sum(c(R, Y, B)[-which.max(c(R, Y, B))])
    ){
        return("IMPOSSIBLE")
    }
    
    my_stalls_order <- list()
    
    for(i in 1:max(c(R, Y, B))){
        
        my_stalls_order[[i]] <- c("R", "Y", "B")[which.max(c(R, Y, B))]
        
    }
    
    leaving_colours <- c("R", "Y", "B")[-which.max(c(R, Y, B))]
    leaving_colours_numbers <- c(R, Y, B)[-which.max(c(R, Y, B))]
    
    if(max(leaving_colours_numbers) > 0){
        
        for(i in 1:max(leaving_colours_numbers)){
            
            my_stalls_order[[i]] <- c(
                my_stalls_order[[i]],
                leaving_colours[which.max(leaving_colours_numbers)]
            )
            
        }
        
    }
    
    last_colour <- leaving_colours[-which.max(leaving_colours_numbers)]
    last_colour_number <- leaving_colours_numbers[
        -which.max(leaving_colours_numbers)
    ]
    
    if(last_colour_number > 0){
        
        for(i in length(my_stalls_order):(
            length(my_stalls_order) - last_colour_number + 1
        )){
            
            my_stalls_order[[i]] <- c(
                my_stalls_order[[i]],
                last_colour
            )
            
        }
        
    }
    
    return(
        paste(unlist(my_stalls_order), collapse = "")
    )
    
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





