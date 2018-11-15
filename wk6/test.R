verifySS2Debug<-function(){
  spl = split(healthyVector, KMC$cluster)
  cmp_withinClustSS = numeric(5)
  for(p in seq(spl)){
    # browser()
    tot = 0
    listItem = spl[p]
    vect=unlist(listItem)
    for(val in vect){
      #print(identical(val,listItem))
      # print(str(vect))
      # print(str(val))
      # break
      tot = tot + ((val - KMC$centers[p])^2)
    }
    # break
    cmp_withinClustSS[p] = tot
  }
  return (cmp_withinClustSS)
}

#Function calculates for each cluster the within-group sum of squared differences
#between elements of the cluster and the cluster's center. Returns a vector
#of sum of squared differences- one element for each cluster.
verifySS<-function() {
  spl = split(healthyVector, KMC$cluster)
  cmp_withinClustSS = numeric(5)
  for(p in seq(spl)){
    tot = 0
    listItem = unlist(spl[p])
    for(val in listItem){
      tot = tot + ((val - KMC$centers[p])^2)
    }
    cmp_withinClustSS[p] = tot
  }
  return (cmp_withinClustSS)
}

testVec<-function(){
  w=seq(3,20)
  browser()
  for (p in w){
    print(2*p)
    print(class(w))
    print(str(w))
    print(class(p))
    print(str(p))
  }
}
