import gleam
import gleam/erlang/process.{type Subject}
import gleam/otp/actor
import gleam/otp/static_supervisor as sup
import gleam/otp/supervision
import gleeunit
import glydamic/child

// import gleam/otp/system

pub fn main() -> Nil {
  gleeunit.main()
}

// gleeunit test functions end in `_test`
pub fn temporary_child_test() {
  let #(supervisor_pid, supervisor_name) = start_supervisor()
  let reply_subject = simple_subject()
  let child_spec =
    supervision.worker(fn() {
      gleam.Ok(actor.Started(start_link(reply_subject), Nil))
    })
    |> supervision.restart(supervision.Temporary)

  let assert Ok(child.SupervisedChild(_pid, id)) =
    child.start(supervisor_pid, child_spec)
  let assert Ok(CReq(handler_subject, "Give me authentication string.")) =
    process.receive(reply_subject, 200)
  process.send(handler_subject, CResp("mysecretpw"))
  let assert Ok(CReq(_handler_subject, "You authenticated correct!")) =
    process.receive(reply_subject, 100)
  // Terminate shall fail since the child has terminated when correct auth is given
  let assert Error(child.NotExist) =
    child.terminate(get_sup_pid(supervisor_name), id)
  let assert Error(child.NotFound) = child.delete(supervisor_pid, id)
}

pub fn transient_child_test() {
  let #(supervisor_pid, _supervisor_name) = start_supervisor()
  let reply_subject = simple_subject()
  let child_spec =
    supervision.worker(fn() {
      gleam.Ok(actor.Started(start_link(reply_subject), Nil))
    })
    |> supervision.restart(supervision.Transient)

  let assert Ok(child.SupervisedChild(_pid, id)) =
    child.start(supervisor_pid, child_spec)
  let assert Ok(CReq(handler_subject, "Give me authentication string.")) =
    process.receive(reply_subject, 200)
  process.send(handler_subject, CResp("die"))

  let assert Ok(CReq(handler_subject, "Give me authentication string.")) =
    process.receive(reply_subject, 200)
  assert Error(child.Running) == child.restart(supervisor_pid, id)
  process.send(handler_subject, CResp("mysecretpw"))
  let assert Ok(CReq(_handler_subject, "You authenticated correct!")) =
    process.receive(reply_subject, 100)
  process.sleep(200)

  let assert Ok(_pid) = child.restart(supervisor_pid, id)
  // echo system.get_state(pid)
  let assert Ok(CReq(handler_subject, "Give me authentication string.")) =
    process.receive(reply_subject, 200)
  let assert Error(child.Running) = child.delete(supervisor_pid, id)
  process.send(handler_subject, CResp("mysecretpw"))
  let assert Ok(CReq(_handler_subject, "You authenticated correct!")) =
    process.receive(reply_subject, 100)

  // Shall Terminate fail for transient since the child has terminated when correct auth is given?
  // let assert Ok(Nil) = child.terminate(get_sup_pid(supervisor_name), id)
  process.sleep(200)
  let assert Ok(Nil) = child.delete(supervisor_pid, id)
}

pub fn permanent_child_test() {
  let #(supervisor_pid, supervisor_name) = start_supervisor()
  let reply_subject = simple_subject()
  let child_spec =
    supervision.worker(fn() {
      gleam.Ok(actor.Started(start_link(reply_subject), Nil))
    })
    |> supervision.restart(supervision.Permanent)

  let assert Ok(child.SupervisedChild(_pid, id)) =
    child.start(supervisor_pid, child_spec)

  let assert Ok(CReq(handler_subject, "Give me authentication string.")) =
    process.receive(reply_subject, 200)
  assert Error(child.Running) == child.restart(supervisor_pid, id)
  process.send(handler_subject, CResp("mysecretpw"))
  let assert Ok(CReq(_handler_subject, "You authenticated correct!")) =
    process.receive(reply_subject, 100)

  let assert Ok(CReq(handler_subject, "Give me authentication string.")) =
    process.receive(reply_subject, 200)
  process.send(handler_subject, CResp("mysecretpw"))
  let assert Ok(CReq(_handler_subject, "You authenticated correct!")) =
    process.receive(reply_subject, 100)
  let assert Ok(Nil) = child.terminate(get_sup_pid(supervisor_name), id)
  process.sleep(200)
  let assert Ok(Nil) = child.delete(supervisor_pid, id)
}

// ----------------- Internal functions -------------
fn start_supervisor() -> #(process.Pid, process.Name(String)) {
  let sup_name = process.new_name("one_for_all_sup")
  let assert Ok(pid) = case { sup.new(sup.OneForAll) |> sup.start } {
    Ok(actor.Started(pid, _data)) -> {
      let assert Ok(Nil) = process.register(pid, sup_name)
      Ok(pid)
    }
    Error(reason) -> Error(reason)
  }
  #(pid, sup_name)
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

fn get_sup_pid(name: process.Name(a)) -> process.Pid {
  let assert Ok(pid) = process.named(name)
  pid
}

fn start_link(client_subject: Subject(ClientRequest(String))) -> process.Pid {
  let assert Ok(actor.Started(pid, _subject1)) =
    actor.new_with_initialiser(100, fn(mysubject) {
      init(mysubject, client_subject)
    })
    |> actor.on_message(loop)
    |> actor.start()
  pid
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
  let _ = child.set_label("glydamic_child")
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
