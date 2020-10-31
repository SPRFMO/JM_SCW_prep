dir.base <- here::here("assessment","config")
ctl.files <- list.files(getwd())
Nctls <- length(ctl.files)

for(i in 1:Nctls) {
	setwd(dir.base)
	old.ctl <- readLines(ctl.files[i])
	lineNos <- NULL

	lineNos[1] <- grep("#Nyrs_Random_walk_q", old.ctl) + 1
	Qstr <- unlist(strsplit(old.ctl[lineNos[1]], split = "  "))
	Qstr[5] <- "0"
	Qstr <- paste(Qstr, collapse = "  ")

	lineNos[2] <- grep("#Random_walk_q_yrs", old.ctl) + 3
	lineNos[3] <- grep("#Random_walk_q_sigmas", old.ctl) + 3

	chkpt <- lineNos[3] - lineNos[2]
	if(length(chkpt) == 0) {print(paste0(ctl.files[i], " didn't change."))
		next}
	if(chkpt == 4) {
	new.ctl <- old.ctl
	new.ctl[lineNos[1]] <- Qstr
	print(new.ctl[lineNos[1]])
	print(new.ctl[lineNos[2]])
	print(new.ctl[lineNos[3]])
	new.ctl <- new.ctl[-(lineNos[2:3])]

	gtools::ask("Continue?")
	writeLines(new.ctl, ctl.files[i])
	}
	else {print(paste0(ctl.files[i], " didn't change."))
	next}

}
