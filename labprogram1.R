is_valid_triangle <- function(a,b,c){
  return((a+b>c)&(a+c>b)&(b+c)>a)
}

triangle_type <- function(a,b,c){
  if(a==b & b==c){
    return("Equilateral")
  }else if(a==b || b==c || c==a){
    return("Isosceles")
  }else{
    return("Scalene")
  }
}

triangle_area <- function(a,b,c){
  s <- (a+b+c)/2
  area <- sqrt(s*(s-a)*(s-b)*(s-c))
  return(area)
}

validate_input <- function(x){
  if(!is.numeric(x) || x<=0){
    stop("Error: input must be positive")
  }
  return(TRUE)
}

cat("Enter the length of sides of a triangle:\n")
a <- as.numeric(readline(prompt="Side a:"))
b <- as.numeric(readline(prompt="Side b:"))
c <- as.numeric(readline(prompt="Side c:"))

tryCatch({
  validate_input(a)
  validate_input(b)
  validate_input(c)
  
  if(!is_valid_triangle(a,b,c)){
    stop("Given sides do not form a triangle")
  }
  type_of_triangle <- triangle_type(a,b,c)
  cat("The type of triangle:",type_of_triangle)
  area <- triangle_area(a,b,c)
  cat("\nArea of the triangle:",area)
},error=function(e){
  cat(e$message,"\n")
})