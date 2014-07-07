# Calculate growth of $1
vami<-function (ror) { cumprod(na.omit(ror) + 1) }