REFERENCES {

http://datasciencespecialization.github.io/curated/
	* Verzani: simpleR – Using R for Introductory Statistics
	* Fig Data: 11 Tips on How to Handle Big Data in R (and 1 Bad Pun)
	* G. Sanchez - Strings in R

http://rayli.net/blog/data/top-10-data-mining-algorithms-in-plain-r/#.VYPQAaunRNU.linkedin	
	* Top 10 data mining algorithms in plain R
	
}

PATHS {
p_sp <- "K:/W2 Design  Realisation/10 - Signed Off Deliverables/20 - Build Phase/KL-IS/Draft Deliverables KL-IS"
p_lc <- "C:/Users/x071986/Documents/lp/AFKL Cargo/FA/FPLN-RARP/__SYNC Sharepoint__/W2-10-20-KL-IS_Draft Deliverables KL-IS"

}

FUNCTIONS {

	flfu <- function(dir_lfu) {							## Find (non-archived) LFU's
	af <- list.files(dir_lfu, recursive = TRUE)			## get all files in dir and subolders
	naf <- af[which(regexec("Archive/", af) == -1)] 	## remove the Archive folder
	lfu <- naf[which(regexec("LFU", naf) != -1)] 		## remove all non-LFU files
	}

	faf <- function(dir_lfu) {							## Find All Files
	af <- list.files(dir_lfu, recursive = TRUE)			## get all files in dir and subolders
	## naf <- af[which(regexec("Archive/", af) == -1)] 	## remove the Archive folder
	## lfu <- naf[which(regexec("LFU", naf) != -1)] 	## remove all non-LFU files
	}

}

READ XLSX {

library(readxl)
fn = "Overview Functional descriptions Wave 2.xlsx"		## Assuming that file is in getwd()
ofd <- read_excel(fn)									## file 

## REMOVING UNWANTED COLS
ofd_c <- ofd[,-(2:4)]									## After first inspection with head(ofd) and tail(ofd)

## REMOVING UNWANTED ROWS
ofd_fc <- ofd_c[-(43:44),]								## Finally cleaned

## INSPECTION OF RESULT
table(ofd_fc)											## Not very readable
unique(ofd_fc)											## Detailed listing unique names LFU 	=> 39
unique(sort(ofd_fc$`FDD in detail`))					## Sorted listing unique names LFU		=> 35


}