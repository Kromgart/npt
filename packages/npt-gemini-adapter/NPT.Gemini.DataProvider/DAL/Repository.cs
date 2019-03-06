using Dapper;
using NPT.Gemini.DataProvider.DAL.DTO;
using System;
using System.Collections.Generic;
using System.Configuration;
using System.Data.Common;
using System.Data.SqlClient;

namespace NPT.Gemini.DataProvider.DAL
{
    public class Repository
    {
        private Func<DbConnection> ConnectionFactory = () => new SqlConnection(ConfigurationManager.ConnectionStrings["Gemini"].ConnectionString);
        
        public IEnumerable<Project> GetAllProjects()
        {
            using (var connection = ConnectionFactory())
            {
                connection.Open();
                var query = "SELECT p.projectid, l.labelname AS [projecttype], p.projectcode, p.projectname, p.color " +
                            "FROM gemini_projects p " +
                            "LEFT JOIN gemini_projectlabels l ON p.projectlabelid = l.labelid";

                return connection.Query<Project>(query);
            }
        }

        public IEnumerable<TimeReport> GetUpdatedTimeReports(string[] userEmails, DateTime timeStamp)
        {
            if (userEmails == null) throw new ArgumentException();
            var emails = string.Join(" ", userEmails);
            var query = $@"SELECT
                           t.entryid AS [reportid],
                           t.hours * 60 + t.minutes AS [LoggedTime],
                           u.emailaddress,
                           t.timeentrydate,
                           t.projectid
                           FROM gemini_timetracking t INNER JOIN gemini_users u ON u.userid = t.userid
                           WHERE CHARINDEX(u.emailaddress, '{emails}', 1)<>0 
                           AND updated > '{timeStamp.ToString("yyyy-MM-dd HH:mm:ss.fff")}'";

            using (var connection = ConnectionFactory())
            {
                connection.Open();
                var data = connection.Query<TimeReport>(query);
                return data;
            }
        }
    }
}