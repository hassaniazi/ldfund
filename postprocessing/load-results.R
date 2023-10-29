# Load GCAM results: query model output from the database using rgcam
#
# Hassan Niazi, May 2023
#
# mostly run only when there is a change in the DB (ran a new scenario) or when
# a new query output is needed. Has method for quering both local and remote DBs

library(tidyverse)
library(rgcam)      # for support run: browseVignettes('rgcam')

# commons for both local and remote DB
dbFile <- "database_basexdb"
query.fname  <- 'ldfund_querylist.xml'

# from local DB ----
prj.name_local <- 'ldfund.dat'
# scenario.names_local <- c("Reference", "RCP6.5gSSP2")   # to query specific scenarios

gcam_home_local = 'C:/Users/niaz981/Downloads/Modelling/GCAM/gcam-umd-stash-LnDfund/'
file.path_local <- paste0(gcam_home_local, 'output')

conn_local <- localDBConn(dbPath = file.path_local, dbFile, migabble = FALSE, maxMemory="8g")
scenario.names_local <- listScenariosInDB(conn_local)$name

prj_ldfund_local <- addScenario(conn_local, prj.name_local, scenario = scenario.names_local, queryFile = query.fname)

# load already extracted project
ldfund_local <- loadProject(proj = prj.name_local)

# test: getQuery(ldfund, "GDP MER by region") -> gdp

# from remote DB ----
prj.name.remote <- 'ldfund_remote.dat'

# STEPS TO QUERY REMOTE DB
# go to Terminal tab in Rstudio
# run the following commands:
#   ssh niaz981@deception
#   module load java/1.8.0_31
#   ./basex-server-helper.sh ldfund/gcam-umd-stash-LnDfund/output
# a connection to the remote DB should haven been established by now
# <run the script below>
# run to stop the session: ./basex-server-helper.sh stop

# note which deception node is being used
deception_node = "deception01"

scenario.names_remote <- (listScenariosInDB(remoteDBConn(dbFile, "test", "test", deception_node)))$name

conn_remote <- remoteDBConn(dbFile, "test", "test", deception_node)
prj_ldfund_remote <- addScenario(conn_remote, prj.name.remote, scenario.names_remote, queryFile = query.fname)

# In case limited scenario need to be queried
# for(i in seq_along(scenario.names_remote)) {
#   conn_remote <- remoteDBConn(paste0(dbFile, "/", scenario.names_remote[i]), "test", "test", deception_node)
#   prj_ldfund_remote <- addScenario(conn_remote, prj.name.remote, scenario.names_remote[i], queryFile = query.fname)
# }

# load already extracted project
ldfund_remote <- loadProject(proj = prj.name.remote)

# run in terminal to stop the session: ./basex-server-helper.sh stop

