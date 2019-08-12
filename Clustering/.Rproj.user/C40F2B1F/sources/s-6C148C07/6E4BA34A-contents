FirstQuadrant <- setClass(
  # Set the name for the class
  "FirstQuadrant",
  
  # Define the slots
  slots = c(
    x = "numeric",
    y = "numeric"
  ),
  
  # Set the default values for the slots. (optional)
  prototype=list(
    x = 0.0,
    y = 0.0
  ),
  
  # Make a function that can test to see if the data is consistent.
  # This is not called if you have an initialize function defined!
  validity=function(object)
  {
    if((object@x < 0) || (object@y < 0)) {
      return("A negative number for one of the coordinates was given.")
    }
    return(TRUE)
  }
)

# create a method to assign the value of a coordinate
setGeneric(name="setCoordinate",
           def=function(theObject,xVal,yVal)
           {
             standardGeneric("setCoordinate")
           }
)

setMethod(f="setCoordinate",
          signature="FirstQuadrant",
          definition=function(theObject, xVal, yVal)
          {
            theObject@x <- xVal
            theObject@y <- yVal
            return(theObject)
          }
)

