import pymongo
import pandas as pd
import json
from datetime import datetime
from Python.augury import Augury, eBirdQuery

## Load project variables - API keys, settings, etc.
with open("db_vars.json") as f:
    db_vars = json.load(f)

## Initialize MongoDB Client
augury = Augury(db_vars)

## Update all collections
augury.update_collections(back=30)

## Save collections to local storage
augury.backup_collections("backups")
