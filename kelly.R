EV = function(odds,juice,unit){
  if(juice<0){
    odds*unit*(100/abs(juice))-(1-odds)*unit
  } else{
    odds*unit*(juice/100)-(1-odds)*unit
  }
}

kelly = function(odds,juice){
  if(juice<0){
    (odds*(100/abs(juice)) - (1-odds))/(100/abs(juice))
  } else{
    (odds*(juice/100) - (1-odds))/(juice/100)
  }
}

mult_kelly = function(money,...){
  for(i in list(...)){
    x = prod(1-c(...))
    print(money*i*x)
    }
}

kelly_units = function(money,...){
  for(i in list(...)){
    if(i <= 0.05){
      print(0.01*money)
    } else if(i <= 0.1){
      print(0.02*money)
    } else if(i <= 0.15){
      print(0.03*money)
    } else if(i <= 0.2){
      print(0.04*money)
    } else if(i > 0.2){
      print(0.05*money)
    } else return(FALSE)
  }
}

