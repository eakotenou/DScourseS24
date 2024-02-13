-- Create a table to hold the CSV data
CREATE TABLE insurance_data (
  policyID INT,
  statecode TEXT,
  county TEXT,
  eq_site_limit REAL,
  hu_site_limit REAL,
  fl_site_limit REAL,
  fr_site_limit REAL,
  tiv_2011 REAL,
  tiv_2012 REAL,
  eq_site_deductible REAL,
  hu_site_deductible REAL,
  fl_site_deductible REAL,
  fr_site_deductible REAL,
  point_latitude REAL,
  point_longitude REAL,
  line TEXT,
  construction TEXT,
  point_granularity INT
);

-- Import the data from the CSV file
.mode csv
.import 'FL_insurance_sample.csv' insurance_data

-- (b) Print out the first 10 rows of the data set
SELECT *
  FROM insurance_data
LIMIT 10;
-- (c) List which counties are in the sample
SELECT DISTINCT county
FROM insurance_data;
-- Average property appreciation from 2011 to 2011.
SELECT AVG(tiv_2012 - tiv_2011) AS avg_appreciation
FROM insurance_data;
--Frequency table of the construction variable.
SELECT construction,
COUNT(*) AS frequency,
COUNT(*) * 1.0 / SUM(COUNT(*)) OVER () AS fraction
FROM insurance_data
GROUP BY construction;
