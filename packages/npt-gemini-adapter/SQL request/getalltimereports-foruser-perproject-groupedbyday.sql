use gemini

DECLARE @Emails nvarchar(50), @StartDate nvarchar(10), @EndDate nvarchar(10);

SET @Emails = 'emp1@companyname.net emp2@companyname.net';
SET @StartDate = '2017/08/01';
SET @EndDate = '2017/08/31';

SELECT
SUM(t.hours) * 60 + SUM(t.minutes) AS [LoggedTime],
DATEADD(DAY,0, datediff(day,0, t.timeentrydate)) AS [TimeEntryDate],
p.projectid,
u.emailaddress
FROM gemini_timetracking t INNER JOIN gemini_users u ON u.userid = t.userid INNER JOIN gemini_projects p ON p.projectid = t.projectid 
WHERE CHARINDEX(u.emailaddress, @Emails, 1)<>0 
AND t.timeentrydate BETWEEN @StartDate AND @EndDate
GROUP BY u.emailaddress, p.projectid, dateadd(DAY,0, datediff(day,0, t.timeentrydate))