using Belegaufgabe3.Models;
using System.Collections.Generic;
using System.Linq;
using System.Net;
using System.Web.Mvc;

namespace Belegaufgabe3.Controllers
{
    public class TodoController : Controller
    {
        public static List<Todo> todoliste = new List<Todo>();
        public static int cnt = 0;

        // GET: Todo
        public ActionResult Index()
        {
            ViewBag.Message = "TodoController Index";

            return View(todoliste);
        }

        // GET: Todo/Add
        public ActionResult Add()
        {
            ViewBag.Message = "TodoController Add";
            return View();
        }

        // POST: Todo/Add
        [HttpPost]
        public ActionResult Add([Bind(Include = "titel,beschreibung")] Todo todos)
        {
            if (ModelState.IsValid)
            {
                todos.ID = cnt;
                todoliste.Add(todos);
                cnt++;

                return RedirectToAction("Index");
            }

            return View(todos);
        }

        // GET: Todo/Edit/1
        public ActionResult Edit(int? id)
        {
            ViewBag.Message = "TodoController Edit";

            if (id == null)
            {
                return new HttpStatusCodeResult(HttpStatusCode.BadRequest);
            }

            Todo todoeintrag = todoliste.Where(Todo => Todo.ID == id).First();
            if (todoeintrag == null)
            {
                return new HttpStatusCodeResult(HttpStatusCode.NotFound);
            }
            return View(todoeintrag);
        }

        // POST: Todo/Edit/1
        [HttpPost]
        public ActionResult Edit([Bind(Include = "ID,titel,beschreibung")] Todo todos)
        {
            if (ModelState.IsValid)
            {
                int index = todoliste.FindIndex(Todo => Todo.ID == todos.ID);
                todoliste[index] = todos;
                return RedirectToAction("Index");
            }
            return View(todos);
        }

        // GET: Todo/Delete/1
        public ActionResult Delete(int? id)
        {
            ViewBag.Message = "TodoController Delete";

            if (id == null)
            {
                return new HttpStatusCodeResult(HttpStatusCode.BadRequest);
            }

            Todo todoeintrag = todoliste.Where(Todo => Todo.ID == id).First();
            if (todoeintrag == null)
            {
                return new HttpStatusCodeResult(HttpStatusCode.NotFound);
            }
            return View(todoeintrag);
        }

        // POST: Todos/Delete/5
        [HttpPost, ActionName("Delete")]
        public ActionResult DeleteConfirmed(int? id)
        {
            if (id == null)
            {
                return new HttpStatusCodeResult(HttpStatusCode.BadRequest);
            }

            todoliste.RemoveAll(Todo => Todo.ID == id);
            return RedirectToAction("Index");
        }
    }
}