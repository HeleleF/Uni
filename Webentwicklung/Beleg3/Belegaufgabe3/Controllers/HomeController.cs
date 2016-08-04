using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.Mvc;

namespace Belegaufgabe3.Controllers
{
    public class HomeController : Controller
    {
        public ActionResult Index()
        {
            return View();
        }

        public ActionResult About()
        {
            ViewBag.Message = "Belegaufgabe 3";

            return View();
        }

        public ActionResult Contact()
        {
            ViewBag.Message = "Informationen:";

            return View();
        }
    }
}