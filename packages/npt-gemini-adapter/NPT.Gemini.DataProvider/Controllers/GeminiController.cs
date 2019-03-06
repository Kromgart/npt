using NPT.Gemini.DataProvider.DAL;
using NPT.Gemini.DataProvider.Models;
using System.Web.Http;

namespace NPT.Gemini.DataProvider.Controllers
{
    public class GeminiController : ApiController
    {
        private Repository repository => new Repository();
        public IHttpActionResult GetAllProjects()
        {
            return Json(repository.GetAllProjects());
        }

        [HttpPost]
        public IHttpActionResult GetTimeReports([FromBody]GetUpdatedTimeReportsParameters parameters)
        {
            if (!ModelState.IsValid) return BadRequest("Bad or missing parameters");
            var result = repository.GetUpdatedTimeReports(parameters.emails, parameters.timeStamp);

            return Json(result);
        }
    }
}