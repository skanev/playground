package simulation

abstract class Simulation {
  type Action = () => Unit

  case class WorkItem(time: Int, action: Action)

  private var curtime = 0
  def currentTime: Int = curtime

  private var agenda: List[WorkItem] = List()

  private def insert(agenda: List[WorkItem], item:WorkItem): List[WorkItem] = {
    if (agenda.isEmpty || item.time < agenda.head.time) item :: agenda
    else agenda.head :: insert(agenda.tail, item)
  }

  def afterDelay(delay: Int)(block: => Unit) {
    var item = WorkItem(currentTime + delay, () => block)
    agenda = insert(agenda, item)
  }

  private def next() {
    (agenda: @unchecked) match {
      case item :: rest =>
        agenda = rest
        curtime = item.time
        item.action()
    }
  }

  def run() {
    afterDelay(0) {
      println("*** simulation started, time = " + currentTime + "***")
    }
    while (!agenda.isEmpty) next()
  }
}
