setClass("TimeSeries",
    representation(
        data = "numeric",
        start = "POSIXct",
        end = "POSIXct"
    )
)

My_TimeSeries <- new("TimeSeries",
    data = c(1,2,3,4,5,6),
    start = as.POSIXct("01/12/2015 0:00:00", tz = "GMT",
                        format = "%m/%d/%Y %H:%M:%S"),
    end = as.POSIXct("12/04/2015 0:00:00", tz = "GMT",
                        format = "%m/%d/%Y %H:%M:%S")
)

setValidity("TimeSeries",
    function(object) {
        object@start < object@end &&
        length(object@start) == 1 &&
        length(object@end) == 1
    }
)

validObject(My_TimeSeries)

bad_TimeSeries <- new("TimeSeries",
    data=c(7, 8, 9, 10, 11, 12),
    start=as.POSIXct("07/01/2009 0:06:00", tz="GMT",
                    format="%m/%d/%Y %H:%M:%S"),
    end=as.POSIXct("07/01/1999 0:11:00", tz="GMT",
                    format="%m/%d/%Y %H:%M:%S")
)

#setClass(Class, representation, prototype, contains=character(),
#         validity, access, where, version, sealed, package,
#         S3methods = FALSE, slots)

setClass("anotherTimeSeries",
    representation(
        data = "numeric",
        start = "POSIXct",
        end = "POSIXct"
    ),
    validity = function(object){  #定义时加上合法性判断
        object@start < object@end &&
        length(object@start) == 1 &&
        length(object@end) == 1
    }
)
another <- new("anotherTimeSeries",
    data = c(2,3,4,7,23),
    start = as.POSIXct("07/01/2009 0:06:00", tz="GMT",
                        format="%m/%d/%Y %H:%M:%S"),
    end=as.POSIXct("07/01/1999 0:11:00", tz="GMT",
                        format="%m/%d/%Y %H:%M:%S")
)

setMethod("summary",   #重载summary
    signature = "TimeSeries",
    definition = function(object){
        print( paste(object@start, " to ", object@end,
                    sep = "", collapse = ""))
        print( paste(object@data, sep = ";", collapse = ""))
    }
)

setMethod("[",
    signature("TimeSeries"),
    definition = function(x, i, j, ..., drop){
        return(x@data[i])
    }
)

setClass("WeightHistory",    #派生
    representation(
        height = "numeric",
        name = "character"
    ),
    contains = "TimeSeries"
)

AlexDannel <- new("WeightHistory",
    data = c(120,118,119,123,121,119),
    start = as.POSIXct("07/01/2015 0:00:00", tz = "GMT",
                       format = "%m/%d/%Y %H:%M:%S"),
    end = as.POSIXct("12/01/2015 0:00:00", tz = "GMT",
                     format = "%m/%d/%Y %H:%M:%S"),
    height = 166,
    name = "Alex Dannel"
)

setClass("Person",
    representation(
        height = "numeric",
        name = "character"
    )
)

setClass("anotherWeightHistory",
    contains = c("TimeSeries", "Person")
)


setClass("Cat",
    representation(
        breed = "character",
        name = "character"
    )
)

setClassUnion("NamedThing",
    c("Person", "Cat")
)

setGeneric("increment",
    def = function(object, step, ...)
        standardGeneric("increment")
)

setMethod("increment",
    signature = "TimeSeries",
    def = function(object, step, ...){
        return(object[step+1] - object[1])
    }
)

setMethod("increment",
    signature = "numeric",
    def = function(object, step, ...){
        return(object[step-1] - object[1])
    }
)

# setMethod("anotherIncrement",
#     signature = "TimeSeries",
#     def = function(object, step, ...){
#         return(object[step+1] - object[1])
#     }
# )


#Now is time series (ts object) in S3 classes
my.ts <- ts(data = c(1:13), start = c(2015,2), frequency = 12)

plot.TimeSeries <- function(object, ...){
    plot(object@data, ...)
}

methods(plot)
#setOldClass
getS3method("histogram", class = "formula")
