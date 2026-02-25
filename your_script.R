library(dplyr)
library(httr)

getCR6json2 <- function(username, pass, logger, port, suffix, timeout = NULL, maxretry = 10, retrynum = 0)
{
    
    url <- paste("http://",logger,":",port,suffix,sep="")
    
    print(url)
    
    if(is.null(timeout)){
        httpTimeout <- httr::timeout((retrynum+1)*2)
    }else{
        httpTimeout <- httr::timeout(timeout)
    }
    
    jsondata <- tryCatch(
        {
            httr::GET(url = url, httr::authenticate(username, pass), httpTimeout)
        },
        error = function(e){
            message("Error:")
            print(e)
            return(NULL)
        },
        finally = {
            
        }
    )
    
    
    if ( is.null(jsondata) ){
        if(retrynum >= maxretry) return( NULL )
        Sys.sleep(retrynum*2)
        message(paste("retry",retrynum,"of",maxretry))
        getCR6json2(username, pass, logger, port, suffix, maxretry = maxretry, retrynum = retrynum + 1)
    } 
    
    message("Succesfully connected")
    if(jsondata$status_code != 200) {
        message(paste("Bad status code:", jsondata$status_code ) )
        if(retrynum >= maxretry) return(jsondata$status_code)
        message(paste("retry",retrynum,"of",maxretry))
        getCR6json2(username, pass, logger, port, suffix, maxretry = maxretry, retrynum = maxretry)
    }
    
    message("Succesfully retrieved file list")
    return(jsonlite::fromJSON(jsondata$content %>% rawToChar))
}

suffix <- "/CRD/?command=ListFiles&format=json"

logger <- "1240062wqi.is-a-techie.com"
#logger <- "wetlandtsl5.ddns.net"
username <- "readonlyuser"
pass <- Sys.getenv("READ_ONLY_PASS")
port <- 997
suffix <- "/CRD/?command=ListFiles&format=json"

fileslist <- getCR6json2(username, pass, logger, port, suffix, timeout = NULL)

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
saveRDS(fileslist, paste0("fileslist_", timestamp, ".RDS"))


