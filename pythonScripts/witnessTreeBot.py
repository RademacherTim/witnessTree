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

# Check points to make sure tweepy is linked to the appropriate twitter page
#------------------------------------------------------------------------------
user = api.me()
#print (user.name)

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
        response = random.sample (responses ['reply'], 1)
else:
	print ("Error: No responses for interactive messages available!")

# Look for tweets containing the words "How are you"
#------------------------------------------------------------------------------
twts = api.search(q="@%s How " % (user.screen_name), show_user = True)
#print (user.screen_name)
#print (twts)

# Create list of tweets that we respond to
#------------------------------------------------------------------------------
t = ['How are you',
     'how are you',
     'How are you doing',
     'how are you doing',
     'How are you feeling',
     'how are you feeling',
     'How r u',
     'how r u',
     'How r u doing',
     'how r u doing',
     'How do you do',
     'how do you do',
     'How\'s it going?',
     'how\'s it going',
     'How are you doing',
     'how are you doing']
#print (t)

for s in twts:
    for i in t:
        #print (s.text)
        #print (i)
        if i == s.text:
            sn = s.user.screen_name
            s = api.update_status (sn + response, s.id) # This does fail, if it has already replied.
            print (sn + response)

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
