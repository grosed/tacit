

.onLoad<-function(libname, pkgname)
{


setMethod("tacit",signature(f="function"),function(f) new("tacit",f) )
setMethod("tacit",signature(f="tacit"),function(f) f )
setMethod("tacit",signature(f="list"),function(f) lapply(f,tacit) )

setGeneric("bin.op",function(op,f,g,x) standardGeneric("bin.op"))
setMethod("bin.op",signature(op="character",f="function",g="function",x="ANY"), function(op,f,g,x) match.fun(FUN = op)(f(x),g(x)) )
setMethod("bin.op",signature(op="character",f="function",g="ANY",x="ANY"), function(op,f,g,x) match.fun(FUN = op)(f(x),g) )
setMethod("bin.op",signature(op="character",f="ANY",g="function",x="ANY"), function(op,f,g,x) match.fun(FUN = op)(f,g(x)) )

set.bin.op<-function(op)
{

   setMethod(op, signature(e1="tacit",e2="tacit"), function(e1,e2) tacit(bind(bin.op,op,e1@f,e2@f)))
   setMethod(op, signature(e1="ANY",e2="tacit"), function(e1,e2) tacit(bind(bin.op,op,e1,e2@f)))
   setMethod(op, signature(e1="tacit",e2="ANY"), function(e1,e2) tacit(bind(bin.op,op,e1@f,e2)))
}
Map(set.bin.op,list("+","-","*","/","%%","%/%","^","<",">","==","<=",">=","!=","&","|"))
setMethod("%*%", signature(x="tacit",y="tacit"), function(x,y) tacit(bind(bin.op,"%*%",x@f,y@f)))
setMethod("%*%", signature(x="ANY",y="tacit"), function(x,y) tacit(bind(bin.op,"%*%",x,y@f)))
setMethod("%*%", signature(x="tacit",y="ANY"), function(x,y) tacit(bind(bin.op,"%*%",x@f,y)))

setMethod("-", signature(e1="tacit",e2="missing"),function(e1){-1*e1})

setMethod("%.%",signature(f="tacit",g="tacit"),function(f,g) tacit(function(x) f@f(g@f(x))) )
setMethod("%.%",signature(f="function",g="function"),function(f,g) tacit(function(x) f(g(x))) ) 
setMethod("%.%",signature(f="tacit",g="function"),function(f,g) tacit(function(x) f@f(g(x))) )
setMethod("%.%",signature(f="function",g="tacit"),function(f,g) tacit(function(x) f(g@f(x))) )
setMethod("%.%",signature(f="tacit",g="ANY"),function(f,g) tacit(function(x) f@f(g)) )
setMethod("%.%",signature(f="function",g="ANY"),function(f,g) tacit(function(x) f(g)) )


setMethod("%..%",signature(f="tacit",g="tacit"),function(f,g) function(x) f@f(g@f(x)) )
setMethod("%..%",signature(f="function",g="function"),function(f,g) function(x) f(g(x)) ) 
setMethod("%..%",signature(f="tacit",g="function"),function(f,g) function(x) f@f(g(x)) )
setMethod("%..%",signature(f="function",g="tacit"),function(f,g) function(x) f(g@f(x)) )
setMethod("%..%",signature(f="tacit",g="ANY"),function(f,g) function(x) f@f(g) )
setMethod("%..%",signature(f="function",g="ANY"),function(f,g) function(x) f(g) )


setMethod("bind","function",
function(f,...)
{
  .orig = list(...);
  function(...) do.call(f,c(.orig,list(...)))
}
) 
setMethod("bind","tacit",
function(f,...)
{
  .orig = list(...);
  tacit(function(...) do.call(f@f,c(.orig,list(...))))
}
) 

# subsetting
setMethod('[', signature(x="tacit"),
function(x,i,j,drop)
{
  if(missing(i)) i=NULL
  if(missing(j)) j=NULL  
  return(
    tacit(
    bind(
    function(arg,i,j)
    {
      if(is.null(i) && is.null(j))  return(x@f(arg)[,])
      if(is.null(j))  return(x@f(arg)[i,])
      if(is.null(i))  return(x@f(arg)[,j])
      return(x@f(arg)[i,j])
    }
    ,i=i,j=j)
    )
    )
}  
)


}