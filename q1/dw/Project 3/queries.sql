SELECT SUM(flightHours), SUM(flightCycles)
FROM AircraftUtilization au, AircraftDimesion a, TemporalDimension t
WHERE au.aircraftID = a.ID AND au.timeID = t.ID
GROUP BY a.model, t.monthID;

SELECT SUM(scheduledOutOfService) as ADOSS, SUM(unscheduledOutOfService) as ADOSU
FROM AircraftUtilization au, TemporalDimension t, Months m
WHERE au.timeID = t.ID and t.monthID = m.ID
GROUP BY au.aircraftID, m.y;
