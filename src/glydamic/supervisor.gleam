//// The factory_supervisor in the Gleam erlang/otp library
//// uses the Erlang OTP `simple_one_for_one` strategy for
//// creating dynamic children. 
//// Some platforms, like the AtomVM, only implements the standard
//// OTP supervisor strategies. This supervisor does that. It can take
//// a child specification list at start but also add children
//// dynamically later using the `child` module.

import gleam
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid}
import gleam/list
import gleam/option
import gleam/otp/actor
import gleam/otp/supervision.{type ChildSpecification}

/// Supervisor restart strategies when children terminate.
/// **Note** that AtomVM currently only supports OneForOne.
pub type Strategy {
  OneForAll
  OneForOne
  RestForOne
}

pub opaque type Builder {
  Builder(
    strategy: Strategy,
    intensity: Int,
    period: Int,
    name: option.Option(process.Name(Nil)),
    children: List(ChildSpecification(Nil)),
  )
}

/// Create a new supervisor builder, ready for further configuration.
///
pub fn new(strategy strategy: Strategy) -> Builder {
  Builder(
    strategy: strategy,
    intensity: 2,
    period: 5,
    name: option.None,
    children: [],
  )
}

/// __Set the restart tolerance__
/// 
/// To prevent a supervisor from getting into an infinite loop of child
/// process terminations and restarts, a maximum restart tolerance is
/// defined. If more restarts than given by `intensity` within
/// within the number of seconds given by `period` occur,
/// the supervisor terminates all child processes and then itself. The
/// termination reason for the supervisor itself in that case will be
/// `shutdown`. 
///
/// Intensity defaults to 2 and period defaults to 5.
///
pub fn restart_tolerance(
  builder: Builder,
  intensity: Int,
  period: Int,
) -> Builder {
  Builder(..builder, intensity:, period:)
}

/// Add a child to the supervisor start list.
pub fn add(builder: Builder, child: ChildSpecification(data)) -> Builder {
  Builder(..builder, children: [
    supervision.map_data(child, fn(_) { Nil }),
    ..builder.children
  ])
}

pub fn named(builder: Builder, name: process.Name(Nil)) -> Builder {
  Builder(..builder, name: option.Some(name))
}

/// Opaque type of the supervisor handle.
pub opaque type Supervisor {
  Supervisor(process.Subject(Nil))
}

/// Get the Pid of the supervisor.
pub fn pid(s: Supervisor) -> Result(Pid, Nil) {
  let Supervisor(subject) = s
  process.subject_owner(subject)
}

/// Similar to named subject but returns a named
/// Supervisor to be used when creating children.
pub fn named_supervisor(name: process.Name(Nil)) {
  let named_subject = process.named_subject(name)
  Supervisor(named_subject)
}

/// Start a `one_for_one` or `one_for_all` supervisor.
pub fn start(builder: Builder) -> actor.StartResult(Supervisor) {
  let module = atom.create("glydamic@supervisor")
  case start_supervisor(module, builder) {
    Ok(pid) -> {
      let subject = case builder.name {
        option.Some(name) -> {
          process.named_subject(name)
        }
        option.None -> {
          process.unsafely_create_subject(pid, make_ref())
        }
      }

      gleam.Ok(actor.Started(pid, Supervisor(subject)))
    }
    Error(error) -> gleam.Error(convert_erlang_start_error(error))
  }
}

@external(erlang, "gleam_otp_external", "convert_erlang_start_error")
fn convert_erlang_start_error(dynamic: Dynamic) -> actor.StartError

@external(erlang, "supervisor", "start_link")
fn start_supervisor(cbm: Atom, args: Builder) -> Result(Pid, Dynamic)

/// Callback used by the Erlang OTP supervisor start_link.
@internal
pub fn init(builder: Builder) {
  let Builder(strategy:, intensity:, period:, name:, children:) = builder
  let children = list.map(children, erl_child_spec)
  let register_result = case name {
    option.Some(name) -> {
      process.register(process.self(), name)
    }
    option.None -> Ok(Nil)
  }
  case register_result {
    Ok(Nil) -> gleam.Ok(#(#(strategy, intensity, period), children))
    Error(Nil) -> Error(name)
  }
}

@internal
pub type ErlangChildSpec

@external(erlang, "glydamic_child_ffi", "erl_child_spec")
fn erl_child_spec(gleam_child_spec: ChildSpecification(a)) -> ErlangChildSpec

@external(erlang, "erlang", "make_ref")
fn make_ref() -> Dynamic
