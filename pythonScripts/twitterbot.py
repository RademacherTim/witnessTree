messages = open('~/GitHub/witnessTree-1/messages/2018-06-15.csv')
message1 = messages.read() #today's message-tdmessage
messages.close()
message1

for line in message1:
    print (line)

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

'''putting a message on Twitter 'Hello World' '''

api = tweepy.API(auth)
api.update_status('Hello World')
api.update_with_media(filename[,'it is a bit chilly out'][file])  #How can I write "it's"? Also perhaps where it says chilly I can apply the exercise where based on a temperature the code fills a word

api.destroy_status('tweet ID')# Example: api.destroy_status(1007329170437951488) tweet ID can be found on URL after clicking on the Tweet
api.me() #gives information about the authenticated user

#These next few lines will print any tweets that were previously tweeted on the account 
public_tweets = api.home_timeline()
for tweet in public_tweets:
                      print(tweet.text)
