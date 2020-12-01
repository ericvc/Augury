import urllib
import requests
import json
import pandas as pd
from datetime import datetime
import pymongo


class eBirdQuery:

    def __init__(self, api_key, latitude: float=-999., longitude: float=-999., search_radius: int=50, back=30):

        self.lat = latitude
        self.lon = longitude
        self.__back = back  # How many days back to search for records
        self.__api_key = api_key
        self.__location()
        self.__search_radius = search_radius
        self.__base_url = "https://api.ebird.org/v2/data/obs/geo/recent?sort=date&cat=species&includeProvisional=false"

    ## Get Location Information from IP Address
    def __location(self):

        if self.lon != -999. and self.lon != -999.:

            assert -180. < self.lon < 180.
            assert -90. < self.lat < 90.
            location = json.load(urllib.request.urlopen('http://ipinfo.io/json'))
            self.__search_radius = 50  # If ## INITIALIZE MONGODB CLIENT
            return

        else:

            print("No coordinates provided. Determining approximate location from IP address.")

            try:
                
                location = json.load(urllib.request.urlopen('http://ipinfo.io/json'))
                lat, lon = location["loc"].split(",")
                self.lat = float(lat)
                self.lon = float(lon)
                self.__tz = location["timezone"]
                self.__city = location["city"]
                print(f"Approximate location from IP address: City: {self.__city} - Latitude: {self.lat}, Longitude: {self.lon}\n")
                
            except urllib.error.HTTPError:
                
                print("Error: could not determine your approximate location.")

    ## Get recent nearby observations of birds
    def get_recent_nearby_observations(self):
        
        url = self.__base_url + ("&lat=%.2f&lng=%.2f&dist=%s&back=%s" % (self.lat, self.lon, self.__search_radius, self.__back))
        print(url)
        response = requests.request("GET", url, headers={'X-eBirdApiToken': self.__api_key}, data={})
        
        # if response status code is 200
        if response:

            self.recent_nearby_obs = pd.DataFrame(response.json())
            print(f"eBird observation records found.")               
            self.num_nearby_records = self.recent_nearby_obs.shape[0]
            print(f"{self.num_nearby_records} records returned.")
            return True

        else:

            print(f"Something went wrong. No eBird records received. {response}")
            return False


class Augury:

    def __init__(self, USERNAME: str, PASSWORD: str, ORGANIZATION: str, CLUSTER: str, DATABASE: str):
    
        ## MONGODB SETTINGS
        self.ORGANIZATION = ORGANIZATION
        self.CLUSTER = CLUSTER
        self.DATABASE = DATABASE
        self.MONGO_URI = f"mongodb+srv://{USERNAME}:{PASSWORD}@{ORGANIZATION}.mongodb.net/{DATABASE}?retryWrites=true&w=majority"

    def pandas_to_json(self, df):
        """Converts eBird data from a pandas DataFrame format to JSON, with slight modifications, allowing uploads to MongoDB Atlas"""
        loc = []
        for i in range(df.shape[0]):
            loc.append([df["lng"][i], df["lat"][i]])
        df["loc"] = loc
        records = json.loads(df.T.to_json()).values()  # Convert data to json format
        for row in records:
            row['obsDt'] = datetime.strptime(row['obsDt'], "%Y-%m-%d %H:%M")  # Conver timestamp to datetime format
        return records

    def upload_to_mongodb(self, COLLECTION: str, LOCATION: dict, API_KEY: str, back: int=2, initialize: bool=False):
        """Downloads eBird data and uploads formatted version to MongoDB Atlas"""
        ## GET STARTING EBIRD DATA
        # INITIALIZE EBIRD CLIENT
        ebird = eBirdQuery(API_KEY, 
                           latitude=LOCATION["LATITUDE"], 
                           longitude=LOCATION["LONGITUDE"], 
                           search_radius=50,  
                           back=back)
        ebird.get_recent_nearby_observations()
        data = ebird.recent_nearby_obs.copy()
        self.open_connection()  # Create MongoDB client
        db = self.client[self.DATABASE]  # Connect to database
        collection = db[COLLECTION]  # Connect to client
        records = self.pandas_to_json(data)
        recordsInserted = 0
        if initialize:
            """If initializing a database/collection, also create index to prevent duplicate entries"""
            collection.insert_many(records)
            collection.create_index(
                [
                    ("speciesCode", 1),  # species observed
                    ("subId", 1),  # eBird user ID
                    ("obsDt", 1),  # date of record
                    ("locId", 1)  # location of record
                ],
                unique=True
            )
        else:
            """For each record, attempt insert. If duplicate entry, catch error and print notification."""
            for rec in records:
                try:
                    collection.insert_one(rec)
                    print("Record uploaded to MongoDB database.")
                    recordsInserted += 1
                except:
                    print("Duplicate entry detected. Skipping record.")
        print(f"\n{recordsInserted} new records inserted to '{COLLECTION}' collection")
        self.close_connection()  # Close MongoDB connection

    def spatial_query(self, COLLECTION: str, PROXIMITY: int, species: list, MIN_DATE, MAX_DATE, LOCATION: list):
        """Query MongoDB database and return the results in a DataFrame object"""
        self.open_connection()  # Create MongoDB client
        # convert your date string to datetime object
        MIN_DATE_DT = datetime.strptime(MIN_DATE, "%Y-%m-%d %H:%M")
        MAX_DATE_DT = datetime.strptime(MAX_DATE, "%Y-%m-%d %H:%M")
        db = self.client[self.DATABASE]  # Connect to database
        collection = db[COLLECTION]  # Connect to collection
        query = {
                "loc": {"$geoWithin": {"$center": [LOCATION, PROXIMITY]}}, 
                "comName":{"$in": species},
                "obsDt":{'$lte': MAX_DATE_DT, '$gte': MIN_DATE_DT}
                }
        docs = collection.find(query)
        df = pd.DataFrame(list(docs))
        if df.shape[0] > 0:
            df.sort_values(by=["comName"])
        self.close_connection()  # Close MongoDB connection
        return df

    def distinct_species(self, COLLECTION: str, PROXIMITY: int, LOCATION: list):
        """Get a list of distinct species contained in the entire collection"""
        self.open_connection()  # Create MongoDB client
        db = self.client[self.DATABASE]  # Connect to database
        collection = db[COLLECTION]  # Connect to collection
        pipeline = [
            {"$match":
                {
                "loc": {"$geoWithin": {"$center": [LOCATION, PROXIMITY]}}, 
                }
            },
            {"$group": 
                {
                "_id": {"comName":"$comName"},
                "count": {"$sum":1}
                }
            },
            {"$sort":{"_id.comName":1}}
        ]
        docs = collection.aggregate(pipeline)
        species = list()
        count = list()
        for doc in docs:
            species.append(doc["_id"]["comName"])
            count.append(doc["count"])
            #print(doc)
        df = pd.DataFrame({"Species":species, "Records":count})
        self.close_connection()  # Close MongoDB connection
        return df
    
    def date_range(self, COLLECTION: str, species: list, LOCATION: list, PROXIMITY: int):
        """Get a range of dates for the selected species"""
        self.open_connection()  # Create MongoDB client
        db = self.client[self.DATABASE]  # Connect to database
        collection = db[COLLECTION]  # Connect to collection
        query = {"comName":
                    {"$in": species},
                "loc": {"$geoWithin": {"$center": [LOCATION, PROXIMITY]}}
                }
        doc_earliest = collection.find(query).sort("obsDt", 1).limit(1)
        doc_recent = collection.find(query).sort("obsDt", -1).limit(1)
        self.close_connection()
        earliest_date = str(pd.DataFrame(list(doc_earliest))["obsDt"][0])
        latest_date = str(pd.DataFrame(list(doc_recent))["obsDt"][0])
        return earliest_date, latest_date

    def open_connection(self):
        """Initialize a MongoDB client"""
        self.client = pymongo.MongoClient(self.MONGO_URI)

    def close_connection(self):
        """Close MongoDB client connection"""
        try:
            self.client.close()
            self.client = None
        except AttributeError:
            print("No attibute 'client' found")
    
    def location_records(self, COLLECTION: str):
        """Returns unique records in a collection as a DataFrame object"""
        self.open_connection()  # Create MongoDB client
        db = self.client[self.DATABASE]  # Connect to database
        collection = db[COLLECTION]  # Connect to collection
        pipeline = [
            {"$group": 
                {
                "_id": {"lat":"$lat", "lng":"$lng"},
                "weight": {"$sum":1}
                }
            }
        ]
        docs = collection.aggregate(pipeline)
        lat = list()
        lng = list()
        weight = list()
        for doc in docs:
            lng.append(doc["_id"]["lng"])
            lat.append(doc["_id"]["lat"])
            weight.append(doc["weight"])
        df = pd.DataFrame({"lng":lng, "lat":lat, "weight":weight})
        self.close_connection()  # Close MongoDB connection
        return df
