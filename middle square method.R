#generate random numbers with middle square method with seed number 3009

randgen = function(value,n){
  list_of_numbers = c()
  for (rnums in 1:n) {
  l = length(unlist(strsplit(as.character(value),"")))
  value = value*value
  value = as.character(value)
  value = strsplit(value , split =  "" )
  value = unlist(value)
  if(l%%2 == 1 ){
    if(length(value)%%2==0){
      value = value[-1]
    }
  }
  if(l%%2==0){
    if( length(value)%%2==1){
      value = c(0,value)
    }
  }
  i=(length(value) - l)/2 +1
  j = i+l-1
  value = as.numeric(paste(value[i:j], collapse = ""))
   list_of_numbers[rnums] = value/10^length(unlist(strsplit(as.character(value),"")))
  }
  return(list_of_numbers)
}
n = 100
rand = randgen(3009,n)
rand
par(mfrow=c(1,1))
plot(1:n,rand,type = 'b',cex = 0.3)
acf(rand)
