def cat_column(NewData):
    # concatenate index to a single string variable names for each variable
    df = NewData.copy()
    L1 = df.columns.get_level_values(0)
#     L2 = df.columns.get_level_values(3)
    L3 = NewData.columns.get_level_values(4)
    
    ## rename the columns - only depth plus the plot number 
    df.columns= (L1 + '_' + L3 ) 
    
    return df
def detect_faulty(df):
    for i in df.columns:
        df[i] = df[i].apply(lambda x: 1 if x > 1 else x )
        # fill NAs with 1 to warning
    df = df.fillna(1)
    faulty_sensors = df.loc[:, df.eq(1).any()].columns
        
    return faulty_sensors
# function to read data from .dat and sensor layout from Excel
# merge the index together to produce the DATAFRAME 
# which has the VWC only 


def merge_index(AllData, AllDataIndex):
    """merge data and index together"""
    
    # libraries
    import datetime
    import pandas as pd
    import numpy as np
    import time 
    
    # Processing index
    FilteredIndex=AllDataIndex[AllDataIndex.Measurement.isin(['VolumetricWaterContent'])] # structure to add in more cols
    FreshData=AllData.loc[:,FilteredIndex.index]
    
    # Processing data 
    FreshDataTransposed = FreshData.transpose()
    FreshDataIndexed = pd.concat([FilteredIndex,FreshDataTransposed], axis=1, sort=True)
    FreshDataIndexed.index.name='ColumnHeader'
    FreshDataIndexed.set_index(['Measurement',
                                'Depth','Cultivar',
                                'Irrigation','Plot','Sensor', 'MUX', 'Port','Units','Summary','Block','Treatment'],
                            append=False, inplace=True) # need to automate
    FreshDataIndexed.sort_index(inplace=True)
    NewData=FreshDataIndexed.transpose()
    NewData.index = pd.to_datetime(NewData.index) 
    
    # Handl NAs, 0 and non sense values 
    for i in NewData.columns:
        NewData[i] = NewData[i].apply(lambda x: 1 if x > 1 or x == 0 else x )
        
    # fill NAs with 1 to warning
    NewData = NewData.fillna(1)
    NewData = NewData.droplevel(['Measurement','MUX','Port','Summary', 'Treatment','Block','Units'], axis = 1)
    
    return NewData

