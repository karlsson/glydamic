# glydamic
<!--
[![Package Version](https://img.shields.io/hexpm/v/glydamic)](https://hex.pm/packages/glydamic)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/glydamic/)
-->
Use a general OTP supervisor that can be started with a child specification.
Children may also be dynamically added later using the `child` module.

Normally it is recommended to use the `static_supervisor` and `factory_supervisor` modules in the [`gleam_otp`](https://hexdocs.pm/gleam_otp/) package.

The `gleam_erlang` package uses the Erlang `proc_lib` module to spawn processes and the `factory_supervisor` implements the so called `simple_one_for_one` strategy.

Neither of these are supported on the [AtomVM](https://atomvm.org/) platform and `glydamic` implements functions to work around these limitations.

Add in gleam.toml under `dependencies`
```toml
[dependencies]
glydamic = { git = "https://github.com/karlsson/glydamic", ref="main"}
```
Example:
```gleam
import glydamic/child.{type Child, type StartError}
import glydamic/supervisor

pub fn start_child(
  supervisor: supervisor.Supervisor
  client_subject: Subject(ClientRequest(String)),
) -> Result(Child, StartError) {
  let child_spec =
    supervision.worker(fn() {
      start_link(client_subject)
    })
    |> supervision.restart(supervision.Temporary)
  child.start(supervisor, child_spec)
}

```

One should also be aware of that the `actor` in `gleam_otp` uses `proc_lib` to start the new process and hence cannot be used on the AtomVM platform. So you will need to roll your own process loop in this case:
```gleam
import gleam/erlang/process.{type Subject}
import gleam/otp/actor
import gleam/otp/supervision
import glydamic
import glydamic/child
import glydamic/supervisor

// ---- Messing around with Messages
pub type RpcMessage(request, reply) {
  RpcMessage(request, Subject(reply))
}

pub type RpcSubject(request, reply) =
  Subject(RpcMessage(request, reply))

/// Modified bang bang (!!) rpc
///
/// [Bang bang](https://erlang.org/download/armstrong_thesis_2003.pdf#page=215)
///
/// let answer = message |> bb(to)
pub fn bb(message: request, to: RpcSubject(request, reply)) -> reply {
  let return: Subject(reply) = process.new_subject()
  process.send(to, RpcMessage(message, return))
  process.receive_forever(return)
}

/// Bang bang server
pub fn bbs(from: RpcSubject(request, reply), f: fn(request) -> reply) -> Nil {
  bbs_loop(from, f)
}

pub fn bbs_loop(
  from: RpcSubject(request, reply),
  f: fn(request) -> reply,
) -> Nil {
  let RpcMessage(message, return) = process.receive_forever(from)
  let messageback: reply = f(message)
  process.send(return, messageback)
  bbs_loop(from, f)
}

pub fn main() {
  start()
}

/// Callback for AtomVM
pub fn start() {
  let assert Ok(actor.Started(_pid, supervisor)) =
    supervisor.new(supervisor.OneForOne) |> supervisor.start
  let name = process.new_name("bbs")
  let child_spec =
    supervision.worker(fn() { start_my_bbs(name) })
  let assert Ok(childret) = echo child.start(supervisor, child_spec)
  echo "hej" |> bb(child.started_data(childret))
}

fn start_my_bbs(name) {
  let server_subject: RpcSubject(String, String) = process.named_subject(name)
  let pid = glydamic.splink(fn() { bbs(server_subject, fn(a) { a <> " <-> " <> a }) })
  case process.register(pid, name) {
    Ok(Nil) -> Ok(actor.Started(pid: pid, data: server_subject))
    Error(Nil) -> Error(actor.InitFailed("Unable to register process"))
  }
}
```
