# http://timelyportfolio.blogspot.com/2012/08/horizon-on-ggplot2.html
horizon.panel.ggplot <- function(df, title) {
  #df parameter should be in form of date (x), grouping, and a value (y)
  colnames(df) = c("date","grouping","y")
  # numbers above the origin will be colored differently than numbers below the origin
  origin = 0
  #get number of bands for the loop
  #limit to 3 so it will be much more manageable
  nbands = 3
  # make this value equal to 1/nbands * range of the dataset
  upperLimit = tail(pretty(df[,3],nrow(df)),1)
  lowerLimit = head(pretty(df[,3],nrow(df)),1)
  limit =  max(upperLimit,abs(lowerLimit))
  horizonscale = (1/nbands)*limit
  #get some decent colors from RColorBrewer
  #we will use colors on the edges so 2:4 for red and 7:9 for blue
  require(RColorBrewer)
  col.brew = brewer.pal(name="RdBu",n=10)
  #loop through nbands to add a column for each of the positive and negative bands
  for (i in 1:nbands) {
    #do positive
    df[,paste("ypos",i,sep="")] <- ifelse(df$y > origin,
                                          # true
                                          ifelse(abs(df$y) > horizonscale * i,
                                                 # true
                                                 horizonscale,
                                                 # false
                                                 ifelse(abs(df$y) - (horizonscale * (i - 1) - origin) > origin, abs(df$y) - (horizonscale * (i - 1) - origin), origin)),
                                          # false
                                          origin)
    #do negative
    df[,paste("yneg",i,sep="")] <- ifelse(df$y < origin,
                                          # true
                                          ifelse(abs(df$y) > horizonscale * i,
                                                 # true
                                                 horizonscale,
                                                 # false
                                                 ifelse(abs(df$y) - (horizonscale * (i - 1) - origin) > origin, abs(df$y) - (horizonscale * (i - 1) - origin), origin)),
                                          # false
                                          origin)
  }
  #melt data frame now that we have added a column for each band
  #this will fit ggplot2 expectations and make it much easier
  df.melt = melt(df[,c(1:2,4:9)],id.vars=1:2)    
  #name the columns for reference
  #try to be generic
  colnames(df.melt) <- c("date","grouping","band","value")
  
  #use ggplot to produce an area plot
  p = ggplot(data=df.melt) +
    geom_area(aes(x = date, y = value, fill=band),
              #alpha=0.25,
              position="identity") +  #this means not stacked
    scale_fill_manual(values=c("ypos1"=col.brew[7],  #assign the colors to each of the bands; colors get darker as values increase
                               "ypos2"=col.brew[8],
                               "ypos3"=col.brew[9],
                               "yneg1"=col.brew[4],
                               "yneg2"=col.brew[3],
                               "yneg3"=col.brew[2])) +
    ylim(origin,horizonscale) +   #limit plot to origin and horizonscale
    facet_grid(grouping ~ .) +    #do new subplot for each group
    theme_bw() +                  #this is optional, but I prefer to default
    theme(legend.position = "none",    #remove legend
          strip.text.y = element_text(angle=0, hjust=1),#rotate strip text to horizontal 
          axis.text.y = element_blank(),#remove y axis labels
          axis.ticks = element_blank(), #remove tick marks
          axis.title.y = element_blank(),#remove title for the y axis
          axis.title.x = element_blank(),#remove title for the x axis
          plot.title = element_text(size=16, face="bold", hjust=0))+ #format title
    labs(title = title)
  
  return(p)
}
