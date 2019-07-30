#### Deal with Missing Data ####

missing.data = function(ratings1, ratings2){
  combined = data.frame(ratings1,ratings2)  #put both sets of ratings together
  cleaned = na.omit(combined)  #get rid of rows with missing data
  
  #check to see if any rows were removed, if so, print message
  n = nrow(combined)
  n.new = nrow(cleaned)
  
  if(n != n.new)
    print("Missing Rows/Data Not Included", quote = FALSE)
  
  return(cleaned)
}


#### Check for Perfect Agreement ####

unused.category = function(ratings1, ratings2){
  
  r = NROW(ratings1)
   if (sum(ratings1 == ratings2) == r && (sum(ratings1)==0 || sum(ratings1)==r)){
     print("One category was never used by either coder - which results in undefined Kappa", quote = F)
   }
     combined = data.frame(ratings1,ratings2)
     return(combined)
}


    # make it a list to call kappa$ki

#### Kappa for Two Values Original Function ####

kappa.twovalues = function(ratings1, ratings2){

cleaned = missing.data(ratings1, ratings2)  # Get rid of missing data, piece by piece. Returns a data.frame with both.
ratings1 = cleaned$ratings1   # get two variables to feed into unused.category
ratings2 = cleaned$ratings2
ratings = unused.category(ratings1, ratings2)  #check for bad perfect agreement

n = nrow(ratings)

abcd = data.frame(matrix(ncol = 4, nrow = n))
x <- c("a", "b", "c", "d")
colnames(abcd) <- x

ratingScores = data.frame(ratings, abcd)

for (i in 1:n){
  
if (ratingScores$ratings1[i] == 1 & ratingScores$ratings2[i] == 1) {
  ratingScores$a[i] = 1
} else if (ratingScores$ratings1[i] == 0 & ratingScores$ratings2[i] == 1) {
  ratingScores$b[i] = 1
} else if (ratingScores$ratings1[i] == 1 & ratingScores$ratings2[i] == 0) {
  ratingScores$c[i] = 1
} else if (ratingScores$ratings1[i] == 0 & ratingScores$ratings2[i] == 0)
  ratingScores$d[i] = 1
}

ratingScores[is.na(ratingScores)] <- 0

a = sum(ratingScores$a)
b = sum(ratingScores$b)
c = sum(ratingScores$c)
d = sum(ratingScores$d)

ki = (2 * (a*d - b*c) ) / ((a + b) * (b + d) + (c + d) * (a + c))
#add in numerator
pdi = ((a + b) * (b + d) + (c + d) * (a + c))

k = data.frame(ki, pdi)

return(k)
}



#### Kappa for Multiple Categories (already in binary table) ####

multiple.categories = function(coder1scores, coder2scores){
    
categories = ncol(coder1scores)

  ki_pdi = data.frame(matrix(ncol = 3, nrow = categories))
  x <- c("ki", "pdi", "numerator")
  colnames(ki_pdi) <- x
  
  for (i in 1:categories){
  kap = kappa.twovalues(coder1[,i], coder2[,i]) 
  ki_pdi$ki[i] = kap$ki
  ki_pdi$pdi[i] = kap$pdi
  
  ki_pdi$numerator[i] = ki_pdi$ki[i] * ki_pdi$pdi[i] 
  }
  
  kpdI = sum(ki_pdi$numerator)
  pdI = sum(ki_pdi$pdi)
  
  multiple.k = kpdI / pdI

  
  return(multiple.k)
}


#### Kappa Function Decider ####

kappa = function(ratings1, ratings2){
  
  if (is.null(ncol(ratings1))){
    kipdi = kappa.twovalues(ratings1, ratings2)
    ki = kipdi$ki
  }
  else{
    ki = multiple.categories(ratings1, ratings2)
  }
  return(ki)
}

