#!/usr/bin/env Rscript

##set up the environment for Teradata access on dev1
options(java.parameters="-Xmx2048m")
library(rJava,lib.loc = "/home/gzhang/R-2.15.2/lib")
library(DBI,lib.loc = "/home/gzhang/R-2.15.2/lib")
library(RJDBC,lib.loc = "/home/gzhang/R-2.15.2/lib")
library(xlsxjars,lib.loc="/home/gzhang/R-2.15.2/lib")
.jinit()
.jaddClassPath(dir("/home/gzhang/R-2.15.2/lib/xlsxjars/java", full.names=TRUE )) 
drv=JDBC("com.teradata.jdbc.TeraDriver","/home/gzhang/Tera_Util_Pak_Vol2/TeraJDBC/terajdbc4.jar:/home/gzhang/Tera_Util_Pak_Vol2/TeraJDBC/tdgssconfig.jar")

## drv, server* (tdwc, tdwb), username, password are used for setting up SQL queries
pass=file("~/pass.txt","rt")
username=readLines(pass,1); 
password=readLines(pass,1)
close(pass)
servertdwc="jdbc:teradata://tdwc/TMODE=ANSI,CHARSET=UTF8" ## international refund records are on tdwc dataserver
servertdwb="jdbc:teradata://tdwb/TMODE=ANSI,CHARSET=UTF8" ## most american (citydeals) refund records are on tdwb tataserver

## set the upper limit of date for querying teradata database
args <- commandArgs(TRUE)
if (is.na(args[1])){
    currentDate=Sys.Date()
}else{
    currentDate = as.Date(arg[1])
}

print(paste("The current date is set as: ", currentDate))
currentMonth = currentDate - as.numeric(format(currentDate,"%d"))+1 ##currentMonth is the first date of current month, month 1

sayHello<-function(){
    print(paste('Updateing the Redemption Analysis up to', currentMonth-1))    
}

sayHello()

getSqlQuery <- function(driver, server, username, password, queryString){
    conn=dbConnect(driver,server,user=username,password=password,dbname="sandbox")
    res=dbGetQuery(conn, queryString)
    dbDisconnect(conn)
    return (res)
}

sendSqlUpdate <- function(driver, server, username, password, queryString){
    conn=dbConnect(driver,server,user=username,password=password,dbname="sandbox")
    res=dbSendUpdate(conn, queryString)
    dbDisconnect(conn)
    return (res)
}

sentQuery <- function(driver, server, username, password, queryString){
    ## -- inputs:
    ##          driver, the driver used for interpretation of the command
    ##          server, where the data are stored (tdwc, tdwb, etc.)
    ##          username and password, the username and password used for data base access
    ##          queryString, string expression of SQL commands
    ## -- output:
    ##          res, the query result (usually, csv format), if fout exist 
    conn=dbConnect(driver,server,user=username,password=password,dbname="sandbox")
    res=dbSentQuery(conn, queryString)
    dbDisconnect(conn)
    return (res)
}

runSingleSqlFile <- function(driver, server, username, password, sqlFileName){
    ##read in file
    query = readLines(sqlFileName)
    getSqlQuery(driver,server,username, password, query)
}

executeSqlFile <- function(driver, server, username, password, sqlFileName){
    queryList = getQueriesFromSqlFile(sqlFileName)
    resultList = list()
    for(i in 1:length(queryList)){
        resultList[[i]] = executeQuery(driver, server, username, password, queryList[[i]])
    }
    return(resultList)
}

getQueriesFromSqlFile<-function(sqlFileName, variable, value){
    queryList = list()
    
}

## some test code

query1 = "CREATE TABLE sandbox.t_bli_test
(
PersonID int,
LastName varchar(255),
FirstName varchar(255),
Address varchar(255),
City varchar(255)
);"

query2 = "INSERT INTO sandbox.t_bli_test (PersonID, LastName, FirstName, Address, City) VALUES (5, 'LI', 'baolei', 'Rio Robles', 'San Jose');"
query3 = "DROP TABLE sandbox.t_bli_test;"
sendSqlUpdate(drv, servertdwc, username, password, query1) # for test only this is create a new table
sendSqlUpdate(drv, servertdwc, username, password, query2) # for test only this is create a new table
sendSqlUpdate(drv, servertdwc, username, password, paste(query2)) # for test only this is create a new table
sendSqlUpdate(drv, servertdwc, username, password, query3) # test only, drop a table


# print("test query string")
# query1 = paste("select count(*) from sandbox.v_gzhang_order_full;")
# executeQuery(drv,servertdwc,username,password,query1)

print("test query string to create a new table")
update1 = paste("create view sandbox.v_bli_test as (select * from sandbox.v_gzhang_order_full);")
Query(drv,servertdwc,username,password,query1)

# print("test single query file")
# executeSqlFile(drv,servertdwc,username,password,'test1.sql')

## difference in blocking
cat("123;\n abc;\t defgh; \n${end_date}", file = "test1")
fileContent = readLines("test1") # line with a warning
str1 = ""
for(line in fileContent){
    str1 = paste(str1, line)
}
print(str1)


#test for regular expression math and replace
grep("[a-z]", letters)

txt <- c("arm","foot","lefroo", "bafoobar")
if(length(i <- grep("foo", txt)))
    cat("'foo' appears at least once in\n\t", txt, "\n")
i # 2 and 4
txt[i]

## Double all 'a' or 'b's;  "\" must be escaped, i.e., 'doubled'
gsub("([ab])", "\\1\\1", "abc and ABC")


str1
str2 = gsub("([\t\r\n])", " ", str1)
str2
str3 = gsub("(/$/{end_date/})", '2014-05-01', str2)
txt <- c("The", "licenses", "for", "most", "software", "are",
         "designed", "to", "take", "away", "your", "freedom",
         "to", "share", "and", "change", "it.",
         "", "By", "contrast,", "the", "GNU", "General", "Public", "License",
         "is", "intended", "to", "guarantee", "your", "freedom", "to",
         "share", "and", "change", "free", "software", "--",
         "to", "make", "sure", "the", "software", "is",
         "free", "for", "all", "its", "users")
( i <- grep("[gu]", txt) ) # indices
stopifnot( txt[i] == grep("[gu]", txt, value = TRUE) )



con <- file("test1", "r", blocking = FALSE)
readLines(con) # empty
cat(" def\n", file = "test1", append = TRUE)
readLines(con) # gets both
close(con)
readLines('test1')
unlink("test1") # tidy up

