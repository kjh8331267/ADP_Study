######################
# 유닉스타입 시간 변환
######################
t = 1543590900

as.numeric(as.POSIXct("2013-09-16 2:13:46 EST"))
as.POSIXct(1543590900,origin="1970-01-01",tz="GMT")


# Unix epoch (or Unix time or POSIX time or Unix timestamp
#        the number of seconds that have elapsed since January 1, 1970 (midnight UTC/GMT)


# - How to get the current epoch time in
as.numeric(Sys.time())

# - Convert from human-readable date to epoch
as.numeric(as.POSIXct("YYYY-MM-dd HH:mm:ss", tz = "GMT", origin="1970-01-01"))


# - Convert from epoch to human-readable date
epoch <- 1616743724
as.POSIXct(epoch, origin="1970-01-01", tz="GMT")



