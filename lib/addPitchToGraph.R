addPitchToGraph <- function(p){

# Penalty boxes
p <- p + geom_segment(aes(y=83, yend=83, x=21.1, xend=78.9), size=0.2, colour="white")
p <- p + geom_segment(aes(y=83, yend=100, x=21.1, xend=21.1), size=0.2, colour="white")
p <- p + geom_segment(aes(y=83, yend=100, x=78.9, xend=78.9), size=0.2, colour="white")
p <- p + geom_segment(aes(y=17, yend=17, x=21.1, xend=78.9), size=0.2, colour="white")
p <- p + geom_segment(aes(y=0, yend=17, x=21.1, xend=21.1), size=0.2, colour="white")
p <- p + geom_segment(aes(y=0, yend=17, x=78.9, xend=78.9), size=0.2, colour="white")

# Siz-yard boz
p <- p + geom_segment(aes(y=94.2, yend=94.2, x=36.8, xend=63.2), size=0.2, colour="white")
p <- p + geom_segment(aes(y=94.2, yend=100, x=63.2, xend=63.2), size=0.2, colour="white")
p <- p + geom_segment(aes(y=94.2, yend=100, x=36.8, xend=36.8), size=0.2, colour="white")
p <- p + geom_segment(aes(y=5.8, yend=5.8, x=36.8, xend=63.2), size=0.2, colour="white")
p <- p + geom_segment(aes(y=0, yend=5.8, x=63.2, xend=63.2), size=0.2, colour="white")
p <- p + geom_segment(aes(y=0, yend=5.8, x=36.8, xend=36.8), size=0.2, colour="white")

#Sidelines
p <- p + geom_segment(aes(y=0, yend=0, x=0, xend=100), size=0.2, colour="white")
p <- p + geom_segment(aes(y=0, yend=100, x=0, xend=0), size=0.2, colour="white")
p <- p + geom_segment(aes(y=100, yend=100, x=0, xend=100), size=0.2, colour="white")
p <- p + geom_segment(aes(y=0, yend=100, x=100, xend=100), size=0.2, colour="white")

# Goals
p <- p + geom_segment(aes(y=100, yend=100, x=45.2, xend=54.8), size=0.2, colour="white")
p <- p + geom_segment(aes(y=0, yend=0, x=45.2, xend=54.8), size=0.2, colour="white")

# Half way line
p <- p + geom_segment(aes(y=50, yend=50, x=0, xend=100), size=0.2, colour="white")

# Data and size
p <- p + coord_fixed(ratio=1/0.95)

return(p)

}

addHalfToGraph <- function(p){
  
  # Penalty box
  p <- p + geom_segment(aes(x=83, xend=83, y=21.1, yend=78.9), size=0.2, colour="black")
  p <- p + geom_segment(aes(x=83, xend=100, y=21.1, yend=21.1), size=0.2, colour="black")
  p <- p + geom_segment(aes(x=83, xend=100, y=78.9, yend=78.9), size=0.2, colour="black")
  
  # Six-yard box
  p <- p + geom_segment(aes(x=94.2, xend=94.2, y=36.8, yend=63.2), size=0.2, colour="black")
  p <- p + geom_segment(aes(x=94.2, xend=100, y=63.2, yend=63.2), size=0.2, colour="black")
  p <- p + geom_segment(aes(x=94.2, xend=100, y=36.8, yend=36.8), size=0.2, colour="black")
  
  # Goal
  p <- p + geom_segment(aes(x=100, xend=100, y=45.2, yend=54.8), size=0.5, colour="white")
  
  #Sidelines
  p <- p + geom_segment(aes(x=0, xend=0, y=0, yend=100), size=0.2, colour="black")
  p <- p + geom_segment(aes(x=0, xend=100, y=0, yend=0), size=0.2, colour="black")
  p <- p + geom_segment(aes(x=100, xend=100, y=0, yend=100), size=0.2, colour="black")
  p <- p + geom_segment(aes(x=0, xend=100, y=100, yend=100), size=0.2, colour="black")
  
  # Half way line
  p <- p + geom_segment(aes(x=50, xend=50, y=0, yend=100), size=0.2, colour="black")
  
  # Data and size
  p <- p + coord_fixed(xlim=c(45, 100), ylim=c(0,100), ratio=0.68/1.05)
  
}

addBoxToGraph <- function(p){
  
  # Penalty box
  p <- p + geom_segment(aes(x=83, xend=83, y=21.1, yend=78.9), size=0.2, colour="black")
  p <- p + geom_segment(aes(x=83, xend=100, y=21.1, yend=21.1), size=0.2, colour="black")
  p <- p + geom_segment(aes(x=83, xend=100, y=78.9, yend=78.9), size=0.2, colour="black")
  
  # Six-yard box
  p <- p + geom_segment(aes(x=94.2, xend=94.2, y=36.8, yend=63.2), size=0.2, colour="black")
  p <- p + geom_segment(aes(x=94.2, xend=100, y=63.2, yend=63.2), size=0.2, colour="black")
  p <- p + geom_segment(aes(x=94.2, xend=100, y=36.8, yend=36.8), size=0.2, colour="black")
  
  #Sidelines
  p <- p + geom_segment(aes(x=0, xend=0, y=0, yend=100), size=0.2, colour="black")
  p <- p + geom_segment(aes(x=0, xend=100, y=0, yend=0), size=0.2, colour="black")
  p <- p + geom_segment(aes(x=100, xend=100, y=0, yend=100), size=0.2, colour="black")
  p <- p + geom_segment(aes(x=0, xend=100, y=100, yend=100), size=0.2, colour="black")
  
  # Goal
  p <- p + geom_segment(aes(x=100, xend=100, y=45.2, yend=54.8), size=0.5, colour="white")
  
  # Data and size
  p <- p + coord_fixed(xlim=c(80, 100), ylim=c(20,80), ratio=0.68/1.05)
  
}