## the function:
## subsets the data set to 1+2 Feb 2007
## adds date_time variable
## writes the data set into a file

download <- function(file = "null", zipFile = "download.zip", url) {
        ## ------------------------
        ##      setting up
        ## ------------------------
	## pre condition - verifying that data set exists
	## if not, it will be downloaded and extracted
	if (!file.exists(file)) {
		 message("Couldn't find data set. Trying out the zip file..")
		 if (!file.exists(zipFile)) {
                	message("Couldn't find zip. Downloading...")
			status <- download.file(url, destfile=zipFile, method = "curl")
                	if (status != 0) {
                        	## error downloading file
                        	stop(paste("error downloading zip file, status=", status))
               		 }		 
		}
		message("extracting dataset from zip file")
		unzip(zipFile)
		unzip(zipFile, list=T)
	}
	message("Done")
}
