use cats;

CREATE TABLE IncidentReports (
    ID INT AUTO_INCREMENT PRIMARY KEY,
    Severity INT,
    Start_Time DATETIME,
    End_Time DATETIME,
    Start_Lat DECIMAL(9, 6),
    Start_Lng DECIMAL(9, 6),
    Distance_mi DECIMAL(5, 2),
    Street VARCHAR(255),
    City VARCHAR(100),
    County VARCHAR(100),
    State VARCHAR(50),
    Zipcode VARCHAR(20),
    Country VARCHAR(100),
    Timezone VARCHAR(50),
    Airport_Code VARCHAR(10),
    Weather_Timestamp DATETIME,
    Temperature_F DECIMAL(5, 2),
    Wind_Chill_F DECIMAL(5, 2),
    Humidity DECIMAL(5, 2),
    Pressure_in DECIMAL(5, 2),
    Visibility_mi DECIMAL(5, 2),
    Wind_Direction VARCHAR(10),
    Wind_Speed_mph DECIMAL(5, 2),
    Precipitation_in DECIMAL(5, 2),
    Weather_Condition VARCHAR(255),
    Amenity BOOLEAN,
    Bump BOOLEAN,
    Crossing BOOLEAN,
    Give_Way BOOLEAN,
    Junction BOOLEAN,
    No_Exit BOOLEAN,
    Railway BOOLEAN,
    Roundabout BOOLEAN,
    Station BOOLEAN,
    Stop BOOLEAN,
    Traffic_Calming BOOLEAN,
    Traffic_Signal BOOLEAN,
    Turning_Loop BOOLEAN,
    Sunrise_Sunset VARCHAR(5) CHECK (Sunrise_Sunset IN ('Day', 'Night')),
    Civil_Twilight VARCHAR(5) CHECK (Civil_Twilight IN ('Day', 'Night')),
    Nautical_Twilight VARCHAR(5) CHECK (Nautical_Twilight IN ('Day', 'Night')),
    Astronomical_Twilight VARCHAR(5) CHECK (Astronomical_Twilight IN ('Day', 'Night'))
);

-- drop table IncidentReports;

select count(*) from IncidentReports limit 100;
Describe IncidentReports;

-- Load data from csv file into the table. Requires restarting of mysql server
-- LOAD DATA INFILE 'data.csv' INTO TABLE IncidentReports
-- FIELDS TERMINATED BY ','
-- IGNORE 1 LINES;

-- SHOW GLOBAL VARIABLES LIKE 'local_infile';
-- SET GLOBAL local_infile=ON;
-- SHOW VARIABLES LIKE 'secure_file_priv';
truncate table IncidentReports;


