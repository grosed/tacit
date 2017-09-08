

#' @import methods
#' @export bind
#' @export %.%
#' @export %..%



# not exported
setClass("tacit",representation(f="function"))
setMethod("initialize", signature("tacit"), function(.Object,f)
{
   .Object@f<-f
   .Object
}
)

setGeneric("tacit",function(f) standardGeneric("tacit"))



##############################################################
#' Composition of functions and tacit functions
#'
#' For functions or tacit functions f(x) and g(x), h<-f%.%g forms 
#' the composition h(x) where h(x) = f(g(x)) and h is a tacit 
#' function (contrast this with %..%).
#'
#' @param f  Function or tacit function
#' @param g  Function or tacit function
#'
#' @return A tacit function
#' 
#' @export
#' @docType methods
#' @rdname compose2tacit
#'
#' @examples
#' f <- sin %.% cos  # f(x) = sin(cos(x)) ... a tacit function
#' f <- I %.% exp      
#' g <- I %.% sin
#' h <- f * g %.% cos # h(x) = exp(x) * sin(cos(x)) ... a tacit function
#' h <- (f * g) %.% cos # h(x) = exp(cos(x)) * sin(cos(x)) ... a tacit function
setGeneric("%.%",function(f,g) standardGeneric("%.%"))


##############################################################
#' Composition of functions and tacit functions
#'
#' For functions or tacit functions f(x) and g(x), h<-f%..%g forms 
#' the composition h(x) where h(x) = f(g(x)) and h is a 
#' function (contrast this with %.%).
#'
#' @param f  Function or tacit function 
#' @param g  Function or tacit function
#'
#' @return A tacit function.
#' 
#' @export
#' @docType methods
#' @rdname compose2function
#'
#' @examples
#' f <- sin %.% cos  # f(x) = sin(cos(x)) ... a tacit function
#' g<-sin%.%cos%..%I # g(x) = sin(cos(x)) ... an ordinary function (I is the identity function) 
#' h<-I%..%sin%.%cos # g(x) = sin(cos(x)) ... a tacit function (operators read left to right !!)  
setGeneric("%..%",function(f,g) standardGeneric("%..%"))



#' Bind a function or tacit function to one or more of its arguments
#'
#' @param f A function or tacit function
#' @param ... values for arguments to be bound (can be more than one) 
#' @return A function or tacit function depending on the type of the argument f  
#' @examples
#' f<-function(a,b) a^b
#' g<-bind(f,2) # g(b) = f(2,b) = 2^b ... g is a function
#' g(3) # g(3) = 8
#' g<-bind(f,b=2) # g(a) = f(a,2) = a^2
#' g(3) # g(3) = 9
#' F<-I%.%f  # F(a,b) = f(a,b) ... F is a tacit function
#' G<-bind(F,b=2) # as before, but G is a tacit function
setGeneric("bind",function(f,...) standardGeneric("bind"))



