#=======================================================================================#
# This is the main script running the witness tree bot. 
# See README.Rmd for more information.
#
# Home repository: https://github.com/TTRademacher/witnessTree
#
# Project lead: Tim Tito Rademacher (rademacher.tim@gmail.com)
#
#---------------------------------------------------------------------------------------#
 
# load dependencies
#---------------------------------------------------------------------------------------#
library(cronR)<br>f <- system.file(package = "cronR", "extdata", "helloworld.R")<br>cmd <- cron_rscript(f, rscript_args = c("productx", "20160101"))<br>## Every minute<br>cron_add(cmd, frequency = 'minutely', id = 'job1', description = 'Customers')<br>## Every hour at 20 past the hour on Monday and Tuesday<br>cron_add(cmd, frequency = 'hourly', id = 'job2', at = '00:20', description = 'Weather', days_of_week = c(1, 2))<br>## Every day at 14h20 on Sunday, Wednesday and Friday<br>cron_add(cmd, frequency = 'daily', id = 'job3', at = '14:20', days_of_week = c(0, 3, 5))<br>## Every starting day of the month at 10h30<br>cron_add(cmd, frequency = 'monthly', id = 'job4', at = '10:30', days_of_month = 'first', days_of_week = '*')<br>## Get all the jobs<br>cron_ls()<br>## Remove all scheduled jobs<br>cron_clear(ask=FALSE)

# read in previously generated messages
#---------------------------------------------------------------------------------------#


# purge expired messages and reevaluate priority
#---------------------------------------------------------------------------------------#


# generate new messages concerning regularly recurrent events
#---------------------------------------------------------------------------------------#


# generate new messages concerning climatic events
#---------------------------------------------------------------------------------------#


# save messages in tmp/ folder for next iteration
#---------------------------------------------------------------------------------------#


#=======================================================================================#