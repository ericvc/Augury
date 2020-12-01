import pymongo
import pandas as pd
import json
from datetime import datetime
from augury import Augury, eBirdQuery

## Load project variables - API keys, settings, etc.
with open("db_vars.json") as f:
    db_vars = json.load(f)

## EBIRD SETTINGS
API_KEY = db_vars["EBIRD"]["API_KEY"]

## MONGODB SETTINGS
USERNAME = db_vars["MONGODB"]["USERNAME"]
PASSWORD = db_vars["MONGODB"]["PASSWORD"]
ORGANIZATION = db_vars["MONGODB"]["ORGANIZATION"]
CLUSTER_NAME = db_vars["MONGODB"]["CLUSTER"]
DATABASE = db_vars["MONGODB"]["DATABASE"]

## INITIALIZE MONGODB CLIENT
augury = Augury(USERNAME, 
                PASSWORD,
                ORGANIZATION,
                CLUSTER_NAME,
                DATABASE)

# UPLOAD TO DATA TO ATLAS CLUSTER (Davis collection)
COLLECTION = db_vars["MONGODB"]["COLLECTION"]["DAVIS"]
LOCATION  = db_vars["LOCATION"]["DAVIS"]
augury.upload_to_mongodb(COLLECTION=COLLECTION, LOCATION=LOCATION, API_KEY=API_KEY, back=2, initialize=False)

# UPLOAD TO DATA TO ATLAS CLUSTER (PtReyes collection)
COLLECTION = db_vars["MONGODB"]["COLLECTION"]["POINT REYES"]
LOCATION  = db_vars["LOCATION"]["POINT REYES"]
augury.upload_to_mongodb(COLLECTION=COLLECTION, LOCATION=LOCATION, API_KEY=API_KEY, back=2, initialize=False)

# UPLOAD TO DATA TO ATLAS CLUSTER (Santa Rosa collection)
COLLECTION = db_vars["MONGODB"]["COLLECTION"]["SANTA ROSA"]
LOCATION  = db_vars["LOCATION"]["SANTA ROSA"]
augury.upload_to_mongodb(COLLECTION=COLLECTION, LOCATION=LOCATION, API_KEY=API_KEY, back=2, initialize=False)

# UPLOAD TO DATA TO ATLAS CLUSTER (PtReyes collection)
COLLECTION = db_vars["MONGODB"]["COLLECTION"]["OROVILLE"]
LOCATION  = db_vars["LOCATION"]["OROVILLE"]
augury.upload_to_mongodb(COLLECTION=COLLECTION, LOCATION=LOCATION, API_KEY=API_KEY, back=2, initialize=False)
