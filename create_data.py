# Python 3.8.3 was used

import pandas as pd
from sqlalchemy import create_engine
import csv
import spotipy
from spotipy.oauth2 import SpotifyClientCredentials
import spotipy.util as util
import os

def createDataFrame(playlist, cred):
    songs = playlist["tracks"]["items"]
    ids = []
    songName = []
    artist = []
    for i in range(len(songs)):
        ids.append(songs[i]["track"]["id"])
        songName.append(songs[i]["track"]["name"])
        artist.append(songs[i]["track"]["album"]["artists"][0]["name"])
    songName_df = pd.DataFrame({'song': songName})
    artist_df = pd.DataFrame({'artist': artist})
    features = cred.audio_features(ids)
    features_df = pd.DataFrame(features)
    df = pd.concat([songName_df, artist_df, features_df], axis=1, join='outer')
    return df

client_id = os.environ.get('SPOTIPY_CLIENT_ID')
client_secret = os.environ.get('SPOTIPY_CLIENT_SECRET')
client_credentials_manager = SpotifyClientCredentials(client_id=client_id, client_secret=client_secret)
sp = spotipy.Spotify(client_credentials_manager=client_credentials_manager)
sp.trace=False 

# Playlists used to create dataset
moodBooster_playlist = sp.playlist("spotify:playlist:37i9dQZF1DX3rxVfibe1L0")
moodBooster_df = createDataFrame(moodBooster_playlist, sp)

happyHits_playlist = sp.playlist("spotify:playlist:37i9dQZF1DXdPec7aLTmlC")
happyHits_df = createDataFrame(happyHits_playlist, sp)

chillRnB_playlist = sp.playlist("spotify:playlist:37i9dQZF1DX2UgsUIg75Vg")
chillRnB_df = createDataFrame(chillRnB_playlist,sp)

wfh_playlist = sp.playlist("spotify:playlist:37i9dQZF1DWTLSN7iG21yC")
wfh_df = createDataFrame(wfh_playlist,sp)

sadBeats_playlist = sp.playlist("spotify:playlist:37i9dQZF1DWVrtsSlLKzro")
sadBeats_df = createDataFrame(sadBeats_playlist,sp)

lifeSucks_playlist = sp.playlist("spotify:playlist:37i9dQZF1DX3YSRoSdA634")
lifeSucks_df = createDataFrame(lifeSucks_playlist,sp)

dataset = pd.concat([moodBooster_df, happyHits_df, chillRnB_df, wfh_df, sadBeats_df, lifeSucks_df], axis=0, join='outer').drop_duplicates().reset_index(drop=True)

# Create .csv file with data:
dataset.to_csv("/Users/sreysea/Documents/STAT510_linreg/dataset.csv")

# Or, if MySQL is preferred:
mysql_password = os.environ.get('MYSQL_PW')
engine = create_engine("mysql+pymysql://{user}:{pw}@localhost/{db}".format(user="root",pw=mysql_password,db="spotifyvalence"))
dataset.to_sql('songs_dataset', con = engine, if_exists = 'append') # Insert whole DataFrame into MySQL

print("DONE")