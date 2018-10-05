#==============================================================================#
# This script is the twitterbot. It reads messages and associated information 
# to transfer it to a linked twitter account with access details for the account
# in the config file.
#------------------------------------------------------------------------------#

# Import dependencies
#------------------------------------------------------------------------------#
import sys        # library to use command line arguments
import tweepy     # 
import Tkinter    #
import csv        # for csv handling


# Read message TTR Need to include checking for a message in the first place
#------------------------------------------------------------------------------#
fileName = './messages/2018-10-05_08.csv'
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
priority = row [0]
fFigure = row [1]
message = row [2]
hastags = row [3]
expires = [4]
print (message)



# Linking the Twitter accound with Tweepy through adding credentials. 
# Twitter handle @awitnesstree
#------------------------------------------------------------------------------#
consumer_key        = sys.argv [1] # consumer key
consumer_secret     = sys.argv [2] # consumer secrets
access_token        = sys.argv [3] # access token
access_token_secret = sys.argv [4] # access token secret
#print (consumer_key)
#print (consumer_secret)
#print (access_token)
#print (access_token_secret)

# Authenticate the twitter page with tweepy library
#------------------------------------------------------------------------------#
auth = tweepy.OAuthHandler(consumer_key, consumer_secret)
auth.set_access_token(access_token, access_token_secret)
api = tweepy.API(auth)

# Check points to make sure tweepy is linked to the appropriate twitter page
#------------------------------------------------------------------------------#
user = api.me()
#print (user.name)

# Putting a message on Twitter
#------------------------------------------------------------------------------#
api = tweepy.API(auth)
api.update_status(message) # TTR Originally this read 'status' instead of message, but I was trying whether the actually message has to be added here before I ran out of time.
#api.update_with_media(filename='directory of image', status= 'status')

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
