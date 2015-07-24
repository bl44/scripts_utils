#!/usr/bin/env Rscript

## R CODE:

num = 0

while(num < 3 ){
num = num + 1
name <- readline("Hey dude, what's your name = ")
if( name == "quit" ) { break }
if( name == "Bieber" ) {
cat("Welcome" , name )
break } 

}
cat("End's Game!!\n")
