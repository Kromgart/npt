use [YOUR GEMINI DATABASE]

DECLARE @Emails nvarchar(50), @TimeStamp datetime;

SET @Emails = 'emp1@companyname.net emp2@companyname.net';
SET @TimeStamp = '2017/08/01';

SELECT
t.entryid,
t.hours * 60 + t.minutes AS [LoggedTime],
u.emailaddress,
t.timeentrydate,
t.projectid
FROM gemini_timetracking t INNER JOIN gemini_users u ON u.userid = t.userid
WHERE CHARINDEX(u.emailaddress, @Emails, 1)<>0 
AND updated > @TimeStamp 