CREATE TABLE Time(
	TimeID INT NOT NULL,
	month_ INT NOT NULL CHECK (month_ >= 1 AND month_ <= 12),
	year_ INT NOT NULL CHECK (year_ >= 1),
	PRIMARY KEY (TimeID)
);


CREATE TABLE TimeDay(
	TimeDayID INT NOT NULL,
	day_ INT NOT NULL CHECK (day_ >= 1 AND day_ <= 31),
	month_ INT NOT NULL CHECK (month_ >= 1 AND month_ <= 12),
	year_ INT NOT NULL CHECK (year_ >= 1),
	PRIMARY KEY (TimeDayID)
);


CREATE TABLE Aircraft( 
	AircraftRegistration CHAR(6) NOT NULL,
	model CHAR(14) NOT NULL,
	manufacturer CHAR(8) NOT NULL,
	PRIMARY KEY (AircraftRegistration)
);


CREATE TABLE AircraftsInfo(
	TimeID INT NOT NULL,
	AircraftID	CHAR(6) NOT NULL,
	ADIS FLOAT NOT NULL CHECK (ADIS >= 0) ,
	ADOS FLOAT NOT NULL CHECK (ADOS >= 0),
	ADOSS FLOAT NOT NULL CHECK (ADOSS >= 0),
	ADOSU FLOAT NOT NULL CHECK (ADOSU >= 0),
	DYR FLOAT NOT NULL CHECK (DYR >= 0),
	CNR FLOAT NOT NULL CHECK (CNR >= 0),
	TDR FLOAT NOT NULL CHECK (TDR >= 0),
	ADD_ FLOAT NOT NULL CHECK (ADD_ >= 0),
	RRh FLOAT NOT NULL CHECK (RRh >= 0),
	RRc FLOAT NOT NULL CHECK (RRc >= 0),
	PRRh FLOAT NOT NULL CHECK (PRRh >= 0),
	PRRc FLOAT NOT NULL CHECK (PRRC >= 0),
	PRIMARY KEY (TimeID, AircraftID),
	FOREIGN KEY (TimeID) REFERENCES Time(TimeID),
	FOREIGN KEY (AircraftID) REFERENCES Aircraft(AircraftRegistration)	
);


CREATE TABLE MaintenanceReports(
	AircraftID CHAR(6) NOT NULL,
	MRRh FLOAT NOT NULL CHECK (MRRh >= 0),
	MRRc FLOAT NOT NULL CHECK (MRRc >= 0),
	Airport CHAR(3) NOT NULL,
	PRIMARY KEY (AircraftID),
	FOREIGN KEY (AircraftID) REFERENCES Aircraft(AircraftRegistration)
);


CREATE TABLE FlightsInfo (
	TimeDayID INT NOT NULL,
	AircraftID CHAR(6) NOT NULL,
	FH FLOAT NOT NULL CHECK (FH >= 0),
	TO_ INT NOT NULL CHECK (TO_ >= 0),
	PRIMARY KEY (TimeDayID, AircraftID),
	FOREIGN KEY (TimeDayID) REFERENCES TimeDay(TimeDayID),
	FOREIGN KEY (AircraftID) REFERENCES Aircraft(AircraftRegistration)
);


CREATE MATERIALIZED VIEW Flights REFRESH FORCE ON COMMIT WITH ROWID AS
SELECT FI.AircraftID, TD.day_, TD.month_, TD.year_, SUM(FI.FH), SUM(FI.TO_)
FROM FlightsInfo FI, TimeDay TD
WHERE FI.TimeDayID = TD.TimeDayID
GROUP BY FI.AircraftID, TD.day_, TD.month_, TD.year_;


CREATE MATERIALIZED VIEW AircraftsUsage 
BUILD IMMEDIATE
REFRESH FORCE ON DEMAND START WITH (SYSDATE) NEXT (ADD_MONTHS(SYSDATE, 1)) WITH ROWID
ENABLE QUERY REWRITE
AS
SELECT AI.AircraftID, T.month_, T.year_, SUM(AI.ADIS), SUM(AI.ADOS), SUM(AI.ADOSS), SUM(AI.ADOSU), SUM(AI.DYR), SUM(AI.CNR), SUM(AI.TDR), SUM(AI.ADD_)
FROM AircraftsInfo AI, Time T
WHERE AI.TimeID = T.TimeID
GROUP BY AI.AircraftID, T.month_, T.year_;