brk=c(40,950)
a=951
b=which(a>brk)
length(b)+1




calculateActivityLevel = function(counts, isWearing) {
  brk = c(40,950)
  a=cbind(counts, isWearing)
  apply(head(a), 1,function( line ) { 
    ifelse(
      !line[2], 
      0, 
      1+length(which(line[1]>brk)))
  } )
}