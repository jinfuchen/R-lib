#-----------------------------------------------------------------------------
#function: count the uniqe number of letters in the string
#param: str
#-----------------------------------------------------------------------------
uniqeLetter <- function(str){
  return(rawToChar(unique(charToRaw(str))))
}

#-----------------------------------------------------------------------------
#function: construct event vector to SPS vector
#param: event_vec, characterVector
#aaaabbbccd
#a-4,b-3,c-2,d-1
#-----------------------------------------------------------------------------
convertSPS <- function(event_vec){
  event_sps <- with(rle(event_vec),paste(values,lengths,sep = "-"))
  return(event_sps)
}

#-----------------------------------------------------------------------------
#function: construct event vector to simple SPS vector
#param: event_vec, characterVector
#aaaabbbccd
#a,b,c,d
#-----------------------------------------------------------------------------
convertSimpleSPS <- function(event_vec){
  event_sps <- rle(event_vec)$values
  return(event_sps)
}

#construct event replace vector
#a1,a2,a3....z1,z2,z3
#-----------------------------------------------------------------------------
#function: generating combination both letters and digits
#param: no
#combine a1,a2..a9,b1,b2...
#-----------------------------------------------------------------------------
constructEventReplace <- function(){
  event_letters <- letters[1:26]
  event_numbers <- 1:9
  event_combine <- expand.grid(event_numbers,event_letters)
  event_replaces <- sprintf('%s%s',event_combine[,2],event_combine[,1])
  return(event_replaces)
}