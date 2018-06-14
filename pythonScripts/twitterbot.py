
import tweepy
import tkinter

'''Linking the Twitter accound with Tweepy through adding credentials.
Twitter page _______'''

consumer_key = 'consumer key'
consumer_secret = 'consumer secrets'
access_token = 'access token'
access_token_secret = 'access token secret'

'''Following lines authenticate the twitter page with tweepy'''
auth = tweepy.OAuthHandler(consumer_key, consumer_secret)
auth.set_access_token(access_token, access_token_secret)
api = tweepy.API(auth)

'''following lines are check points to make sure tweepy is linked to the appropriate twitter page'''
user = api.me()
print (user.name)


'''Initiate these codes in order to follow-back or unfollow all of the followers of our specific twitter page'''

for follower in tweepy.Cursor(api.followers).items():
    follower.unfollow()
    print ("Followed everyone that is following " + user.name)

    
for follower in tweepy.Cursor(api.followers).items():
    follower.unfollow()
    print ("Followed everyone that is following " + user.name)

'''writing in a file'''


