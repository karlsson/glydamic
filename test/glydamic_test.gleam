import gleam/erlang/process.{type Subject}
import gleam/otp/actor
import gleam/otp/supervision
import gleeunit
import glydamic/child
import glydamic/supervisor as sup

pub fn main() -> Nil {
  gleeunit.main()
}

// gleeunit test functions end in `_test`
pub fn temporary_child_test() {
  let assert Ok(actor.Started(_, supervisor)) =
    sup.new(sup.OneForOne) |> sup.start

  let sub_sup_name = process.new_name("child_sup")
  let child_spec =
    supervision.supervisor(fn() { start_supervisor(sub_sup_name) })
  let assert Ok(child) = child.start(supervisor, child_spec)
  let sub_sup = child.started_data(child)
  let reply_subject = simple_subject()
  let child_spec =
    supervision.worker(fn() { start_link(reply_subject) })
    |> supervision.restart(supervision.Temporary)

  let assert Ok(child) = child.start(sub_sup, child_spec)
  let assert Ok(CReq(handler_subject, "Give me authentication string.")) =
    process.receive(reply_subject, 200)
  process.send(handler_subject, CResp("mysecretpw"))
  let assert Ok(CReq(_handler_subject, "You authenticated correct!")) =
    process.receive(reply_subject, 100)
  // Terminate shall fail since the child has terminated when correct auth is given
  let assert Error(child.NotExist) = child.terminate(supervisor, child)
  let assert Error(child.NotFound) = child.delete(supervisor, child)
}

pub fn transient_child_test() {
  let sup_name = process.new_name("one_for_all_sup")
  let assert Ok(actor.Started(_, supervisor)) = start_supervisor(sup_name)
  let reply_subject = simple_subject()
  let child_spec =
    supervision.worker(fn() { start_link(reply_subject) })
    |> supervision.restart(supervision.Transient)

  let assert Ok(child) = child.start(supervisor, child_spec)
  let assert Ok(CReq(handler_subject, "Give me authentication string.")) =
    process.receive(reply_subject, 200)
  process.send(handler_subject, CResp("die"))

  let assert Ok(CReq(handler_subject, "Give me authentication string.")) =
    process.receive(reply_subject, 200)
  assert Error(child.Running) == child.restart(supervisor, child)
  process.send(handler_subject, CResp("mysecretpw"))
  let assert Ok(CReq(_handler_subject, "You authenticated correct!")) =
    process.receive(reply_subject, 100)
  process.sleep(200)

  let assert Ok(_pid) = child.restart(supervisor, child)
  // echo system.get_state(pid)
  let assert Ok(CReq(handler_subject, "Give me authentication string.")) =
    process.receive(reply_subject, 200)
  let assert Error(child.Running) = child.delete(supervisor, child)
  process.send(handler_subject, CResp("mysecretpw"))
  let assert Ok(CReq(_handler_subject, "You authenticated correct!")) =
    process.receive(reply_subject, 100)

  // Shall Terminate fail for transient since the child has terminated when correct auth is given?
  // let assert Ok(Nil) = child.terminate(get_sup_pid(supervisor_name), id)
  process.sleep(200)
  let assert Ok(Nil) = child.delete(supervisor, child)
}

pub fn permanent_child_test() {
  let sup_name = process.new_name("one_for_all_sup")
  let assert Ok(actor.Started(_, supervisor)) = start_supervisor(sup_name)
  let reply_subject = simple_subject()
  let child_spec =
    supervision.worker(fn() { start_link(reply_subject) })
    |> supervision.restart(supervision.Permanent)

  let assert Ok(child) = child.start(supervisor, child_spec)

  let assert Ok(CReq(handler_subject, "Give me authentication string.")) =
    process.receive(reply_subject, 200)
  assert Error(child.Running) == child.restart(supervisor, child)
  process.send(handler_subject, CResp("mysecretpw"))
  let assert Ok(CReq(_handler_subject, "You authenticated correct!")) =
    process.receive(reply_subject, 100)

  let assert Ok(CReq(handler_subject, "Give me authentication string.")) =
    process.receive(reply_subject, 200)
  process.send(handler_subject, CResp("mysecretpw"))
  let assert Ok(CReq(_handler_subject, "You authenticated correct!")) =
    process.receive(reply_subject, 100)
  let assert Ok(Nil) = child.terminate(supervisor, child)
  process.sleep(200)
  let assert Ok(Nil) = child.delete(supervisor, child)
}

// ----------------- Internal functions -------------
fn start_supervisor(
  name: process.Name(Nil),
) -> actor.StartResult(sup.Supervisor) {
  sup.new(sup.OneForOne) |> sup.named(name) |> sup.start
}

// -----------------------------------
// request to client from handler
type ClientRequest(a) {
  CReq(handler_subject: Subject(ClientResponse(a)), request: a)
}

// response from client to handler
type ClientResponse(a) {
  CResp(resp: a)
}

type State {
  State(
    trials: Int,
    my_subject: Subject(ClientResponse(String)),
    client_subject: Subject(ClientRequest(String)),
  )
}

fn simple_subject() -> Subject(ClientRequest(String)) {
  process.new_subject()
}

fn start_link(
  client_subject: Subject(ClientRequest(String)),
) -> actor.StartResult(Subject(ClientResponse(String))) {
  actor.new_with_initialiser(100, fn(mysubject) {
    init(mysubject, client_subject)
  })
  |> actor.on_message(loop)
  |> actor.start()
}

fn init(
  mysubject: Subject(ClientResponse(String)),
  client_subject: Subject(ClientRequest(String)),
) -> Result(
  actor.Initialised(
    State,
    ClientResponse(String),
    Subject(ClientResponse(String)),
  ),
  String,
) {
  let selector =
    process.new_selector()
    |> process.select(mysubject)

  let state = State(0, mysubject, client_subject)

  let initialised =
    actor.initialised(state)
    |> actor.selecting(selector)
    |> actor.returning(mysubject)

  process.send_after(
    client_subject,
    100,
    CReq(mysubject, "Give me authentication string."),
  )
  Ok(initialised)
}

fn loop(
  state: State,
  msg: ClientResponse(String),
) -> actor.Next(State, ClientResponse(String)) {
  case msg {
    CResp("die") -> panic as "Client is bad and kills the handler"
    CResp("mysecretpw") -> {
      actor.send(
        state.client_subject,
        CReq(state.my_subject, "You authenticated correct!"),
      )
      actor.stop()
    }
    _ if state.trials < 3 -> {
      actor.send(
        state.client_subject,
        CReq(state.my_subject, "Wrong password try again."),
      )
      actor.continue(State(..state, trials: state.trials + 1))
    }
    CResp(_x) -> {
      actor.send(
        state.client_subject,
        CReq(state.my_subject, "Client tried too many times."),
      )
      actor.stop()
    }
  }
}
