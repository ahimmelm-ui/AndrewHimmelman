import json
import pandas as pd
import sqlalchemy

with open("credentials.json") as fp:
  server_details = json.load(fp)

server_details["database"] = "aspannba_bas479"

connection_string = sqlalchemy.URL.create(
  "mysql+pymysql",
  **server_details,
)

engine = sqlalchemy.create_engine(connection_string)

query = "SHOW TABLES;"
tables = pd.read_sql(query, engine)
engine.dispose()

print(tables)