
import csv
import pandas as pd
from math import log
prsa=pd.read_csv("C:\\Users\\X280/desktop/STAT 443/data_with_weekday.csv")
X=prsa[[ 'month', 'day', 'hour', 'SO2', 'NO2', 'CO', 'O3', 'TEMP', 'PRES', 'DEWP', 'RAIN', 'WSPM', 'season', 'weekday']]
Y=prsa['PM2.5']
from sklearn.model_selection import train_test_split
import numpy
x_train, x_test, y_train, y_test=train_test_split(X, Y, test_size=0.3)
# Random Forest Regression
from sklearn.ensemble import RandomForestRegressor
rf = RandomForestRegressor(n_estimators = 1000, random_state = 42)
rf=rf.fit(x_train,y_train)
pre_train=rf.predict(x_train)
pre_test=rf.predict(x_test)from math import sqrt
import numpy as np
y_train.to_numpy()
y_test.to_numpy()
#def rmse(predictions, targets):
#    return np.sqrt(((predictions - targets) ** 2).mean())
from sklearn.metrics import mean_squared_error
mean_squared_error(pre_train, y_train)
mean_squared_error(pre_test, y_test)
Rf.feature_importances_

