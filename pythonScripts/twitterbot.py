#==============================================================================
# This script is the twitterbot. It reads messages for a particular hour and 
# associated information to transfer it to a linked twitter account with access 
# details for the account in the config file.
# Each message can only be posted once, so accidental repetition is impossible.
#------------------------------------------------------------------------------

# Import dependencies
#------------------------------------------------------------------------------
import sys        # library to use command line arguments
import tweepy     # 
import Tkinter    #
import csv        # for csv handling
import os
import random     # library to use random number generator
from datetime import date
from datetime import time
from datetime import datetime

# Linking the Twitter accound with Tweepy through adding credentials. 
# Twitter handle @hf_tree (later @awitnesstree)
#------------------------------------------------------------------------------
consumer_key        = sys.argv [1] # consumer key
consumer_secret     = sys.argv [2] # consumer secrets
access_token        = sys.argv [3] # access token
access_token_secret = sys.argv [4] # access token secret
#print (consumer_key)
#print (consumer_secret)
#print (access_token)
#print (access_token_secret)

# Authenticate the twitter page with tweepy library
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
fileName = "./messages/%s.csv" % now.strftime ("%Y-%m-%d_%H")

# Read message, if it exists
#------------------------------------------------------------------------------
if os.path.exists(fileName):
	with open(fileName) as csv_file:
	    csv_reader = csv.reader(csv_file, delimiter=',')
	    line_count = 0
	    for row in csv_reader:
		if line_count == 0:
		    #print(row))
		    line_count += 1
		else:
		    #print(row)
		    line_count += 1
	    #print(line_count)
	priority   = row [0]
	fFigure    = row [1]
        figureName = row [2]
	message    = row [3]
	hashtags   = row [4]
	expires    = [5]
	print message + hashtags

	# Putting a message on Twitter
	#------------------------------------------------------------------------------
	api = tweepy.API (auth)
	api.update_status (message + hashtags)
	#api.update_with_media (filename='directory of image', status= 'status')

else:
	print ("Error: No file with a message!")




# Look for tweets containing the words "How are you"
#------------------------------------------------------------------------------
twts = api.search(q="@%s How are you" % (user.screen_name), show_user = True)
#print (user.screen_name)
#print (twts)

# Create list of tweets that we respond to
#------------------------------------------------------------------------------
t = ['How are you?',
     'how are you?',
     'How are you doing?',
     'how are you doing?',
     'How are you feeling?',
     'how are you feeling?']
#print (t)

for s in twts:
    for i in t:
        #print (s.text)
        #print (i)
        if i == s.text:
            sn = s.user.screen_name
            m = "@%s I am feeling..." % (sn)
            s = api.update_status (m, s.id) # This does fail, if it has already replied.
            #print (s.user.screen_name)
            #print (m)
            #print (s)

# To delete a status use:'''
#------------------------------------------------------------------------------#
#api.destroy_status('tweet ID')# Example: api.destroy_status(1007329170437951488) tweet ID can be found on URL after clicking on the Tweet

# These next few lines will print any tweets that were previously tweeted on the account '''
#------------------------------------------------------------------------------#
#public_tweets = api.home_timeline()
#for tweet in public_tweets:
#                      print(tweet.text)

# To follow-back or unfollow all of the followers of the specific twitter page'''
#------------------------------------------------------------------------------#
#for follower in tweepy.Cursor(api.followers).items():
#    follower.follow()
#    print ("Followed everyone that is following " + user.name)


#for follower in tweepy.Cursor(api.followers).items():
#    follower.unfollow()
#    print ("unfollowed everyone that is following " + user.name)
