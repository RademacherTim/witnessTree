#==============================================================================
# This script is the twitterbot. It reads messages for a particular hour and 
# associated information to transfer it to a linked twitter account with access 
# details for the account in the config file.
# Each message can only be posted once, so accidental repetition is impossible.
#------------------------------------------------------------------------------

# Import dependencies
#------------------------------------------------------------------------------
import sys        # library to use command line arguments
import tweepy     # twitter library
import twitter    # python-twitter library
import Tkinter    # graphical user interface library
import facebook   # library for facebook API 
import csv        # for csv handling
import pandas     # for csv file handling
import os         # library to interact with operating system
import random     # library to use random number generator
import array      # library for array handling
import pytz       # for timezone handling
from datetime import date
from datetime import time
from datetime import datetime

# Linking the Twitter accound with Tweepy through adding credentials. 
#------------------------------------------------------------------------------
consumer_key        = sys.argv [1] # twitter accountconsumer key
consumer_secret     = sys.argv [2] # twitter accountconsumer secrets
access_token        = sys.argv [3] # twitter account access token
access_token_secret = sys.argv [4] # twitter account access token secret
page_access_token   = sys.argv [5] # facebook page access token
facebook_page_id    = sys.argv [6] # facebook page ID
path                = sys.argv [7] # path to the current working directory
#print (consumer_key)
#print (consumer_secret)
#print (access_token)
#print (access_token_secret)
#print (page_access_token)
#print (facebook_page_id)

# Get working directory
#------------------------------------------------------------------------------
os.chdir (path)

# Authenticate the twitter account
#------------------------------------------------------------------------------
auth = tweepy.OAuthHandler(consumer_key, consumer_secret)
auth.set_access_token(access_token, access_token_secret)
api = tweepy.API(auth)

# Check points to create user object used later
#------------------------------------------------------------------------------
user = api.me()

# Check whether there is a file for this hour
#------------------------------------------------------------------------------
now = datetime.now ()
fileName = "./posts/%s.csv" % now.strftime ("%Y-%m-%d_%H")
print (fileName)

# Read post, if it exists
#------------------------------------------------------------------------------
if os.path.exists(fileName):
	with open(fileName) as csv_file:
	    csv_reader = csv.reader(csv_file, delimiter=',')
	    line_count = 0
	    for row in csv_reader:
		if line_count == 0:
		    #print(row)
		    line_count += 1
		else:
		    #print(row)
		    line_count += 1
	    #print(line_count)

        # Extract post information from the file
        #----------------------------------------------------------------------
	priority   = row [0]
	fFigure    = row [1]
        figureName = row [2]
	message    = row [3]
	hashtags   = row [4]
	expires    = row [5]
	print (message + hashtags + fFigure + figureName)

        # Get graph for facebook
        #----------------------------------------------------------------------
        #graph = facebook.GraphAPI(page_access_token)        

        # The post is not accompanied by an image
        #----------------------------------------------------------------------
        if fFigure == "FALSE":
             api.update_status (message + hashtags)

        # The post is accompanied by an image
	#----------------------------------------------------------------------
        elif fFigure == "TRUE":
             api.update_with_media (filename = figureName, 
                                    status = message + hashtags)

else:
	print ("Error: No file with a message!")

# Read in interactive responses to respond to questions
#------------------------------------------------------------------------------
if os.path.exists('./tmp/interactiveResponses.csv'):
	responses = pandas.read_csv ('./tmp/interactiveResponses.csv')
        print ('Responses for interactive tweets created.')
else:
	print ("Error: No responses for interactive messages available!")

# Create list of tweets that we respond to
#------------------------------------------------------------------------------
questions = ['how are you',
             'how are you doing',
             'how are you feeling',
	     'how r u',
             'how are u',
     	     'how r u doing',
             'how do you do',
             'how\'s it going',
             'how are you doing']
#print (t)

# Read in timestamp, when we last replied to tweets.
#------------------------------------------------------------------------------
if os.path.exists('./memory.csv'):
	tmp = pandas.read_csv ('./memory.csv')
	tmpTime = tmp ['lastResponse']
	tmpTime = tmpTime [0]
	tmpTime = datetime.strptime (tmpTime, '%Y-%m-%d %H:%M')
	local = pytz.timezone ("US/Eastern")
	local_dt = local.localize (tmpTime, is_dst = None)
        lastResponseTime = local_dt.astimezone (pytz.utc)
else:
	print ("Error: Could not find a last response time.")

# Look for tweets containing the questions the bot responds to
#------------------------------------------------------------------------------
tweetIDs = []
localTwitter = pytz.timezone ("UTC")
for i in questions:
	tmpTweets = api.search (q = "@%s " % (user.screen_name) + i, show_user = True)
	tweets = []
	for tweet in tmpTweets:
		local_dt = localTwitter.localize (tweet.created_at, is_dst = None)
                questionTime = local_dt.astimezone (pytz.utc)
    		if questionTime > lastResponseTime:
        		tweets.append   (tweet)	

	for tweet in tweets:
		if tweet.id in tweetIDs:
			print ('Question was already replied to.')
		else:
		        handle = tweet.user.screen_name
			response = random.sample (responses ['reply'] [1:len(responses)], 1) [0]
		        tweet = api.update_status ("@%s "% handle + response.decode ("utf-8"), tweet.id) # This does fail, if it has already replied.
			tweetIDs.append (tweet.id) # Add it to the replied to IDs after first reply.
print ('Responded to '+str (len (tweetIDs))+' questions.')


# Look for tweets containing "if a tree falls in the woods"
#------------------------------------------------------------------------------
question = 'if a tree falls a forest, does it make a sound'
tmpTweets = api.search (q = "@%s " % (user.screen_name) + question, show_user = True)
tweets = []
for tweet in tmpTweets:
	local_dt = localTwitter.localize (tweet.created_at, is_dst = None)
        questionTime = local_dt.astimezone (pytz.utc)
    	if questionTime > lastResponseTime:
        	tweets.append   (tweet)	
for tweet in tweets:
	handle = tweet.user.screen_name
	response = responses ['reply'] [0]
	tweet = api.update_status ("@%s "% handle + response.decode ("utf-8"), tweet.id)

# Update the memory.csv file to contain the timestamp, when we last replied to a question to avoid trying to re-post
#------------------------------------------------------------------------------
local_dt = local.localize (datetime.now (), is_dst = None)
tmp ['lastResponse'] = datetime.strftime (local_dt, '%Y-%m-%d %H:%M')
tmp ['lastResponse'] = '\"' + tmp ['lastResponse'] + '\"'
export_csv = tmp.to_csv (r'./memory.csv', index = None, header = True, quoting = csv.QUOTE_NONE)
print ('Updated lastResponse timestamp.')

# To delete a status use:'''
#------------------------------------------------------------------------------#
#api.destroy_status('tweet ID')# Example: api.destroy_status(1007329170437951488) tweet ID can be found on URL after clicking on the Tweet

# These next few lines will print any tweets that were previously tweeted on the account '''
#------------------------------------------------------------------------------#
#public_tweets = api.home_timeline()
#for tweet in public_tweets:
#                      print(tweet.text)

# Should the witness tree follow its followers?
# To follow-back or unfollow all of the followers of the specific twitter page'''
#------------------------------------------------------------------------------#
#for follower in tweepy.Cursor(api.followers).items():
#    follower.follow()
#    print ("Followed everyone that is following " + user.name)


#for follower in tweepy.Cursor(api.followers).items():
#    follower.unfollow()
#    print ("unfollowed everyone that is following " + user.name)
