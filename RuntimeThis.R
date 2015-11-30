#Check the time cost of a function or command
#Input : The function or command to be checked
#Output: The Running time of ...

RuntimeThis <- function(...){
    StartTime <- Sys.time()
    #Evaluate the command onto the base environment
    eval(..., sys.frame(sys.parent(sys.parent())))
    EndTime <- Sys.time()
    print(EndTime - StartTime)
}
