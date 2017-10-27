ExcludeVars <- c("R_SESSION_TMPDIR","R_HISTFILE")
IncludeVars <- Sys.getenv()
IncludeVars <- IncludeVars[grep("^R_",names(IncludeVars),perl=TRUE)]
cat('let name="OCaml-R"\n')
cat('let options = ["--vanilla"; "--slave"]\n')
cat('let signal_handlers = false\n')
cat('let packages = Some []\n')
cat('let env = [\n')
for (i in 1:length(IncludeVars)){
	if (names(IncludeVars)[i] %in% ExcludeVars)
		next
	cat('    "',names(IncludeVars)[i],'", "',IncludeVars[i],'";\n',sep='')
}
cat('\n]')
