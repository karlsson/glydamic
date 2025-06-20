//// Module for starting "dynamic" children.
////
//// The child module is supervisor agnostic, it does not care if
//// if the supervisor is "static" or anything else as long as it
//// is an Erlang OTP supervisor except simple_one_for_one strategy.

import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid}
import gleam/otp/actor
import gleam/otp/supervision.{type ChildSpecification}

/// `pid` is the Pid of the started child.
///
/// `id` is the supervisor internal id for the child.
pub type Child {
  SupervisedChild(pid: Pid, id: Int)
}

pub type StartError {
  NoSupervisor
  AlreadyPresent
  AlreadyStarted(Pid)
  Actor(actor.StartError)
}

pub type TerminateError {
  NotExist
}

pub type DeleteError {
  Running
  Restarting
  NotFound
  SimpleOneForOne
  Unknown
}

/// Starts the child with the corresponding child specification.
///
/// `sup_pid` is the Pid of the supervisor.
///
/// Returns the child Pid and its internal id used by the supervisor
/// for the other functions.
@external(erlang, "glydamic_child_ffi", "start_child")
pub fn start(
  sup_pid: Pid,
  child_spec: ChildSpecification(a),
) -> Result(Child, StartError)

/// Tells supervisor sup_pid to delete the child specification identified by Id.
/// The corresponding child process must not be running.
/// Use `terminate` to terminate it.
@external(erlang, "glydamic_child_ffi", "delete_child")
pub fn delete(sup_pid: Pid, id: Int) -> Result(Nil, DeleteError)

@external(erlang, "glydamic_child_ffi", "restart_child")
pub fn restart(sup_pid: Pid, id: Int) -> Result(Pid, DeleteError)

@external(erlang, "glydamic_child_ffi", "terminate_child")
pub fn terminate(sup_pid: Pid, id: Int) -> Result(Nil, TerminateError)

/// Sets the so called process label for unregistered processes
/// to aid in debugging.
/// See [proc_lib:set_label/1](https://www.erlang.org/docs/28/apps/stdlib/proc_lib.html#set_label/1)
pub fn set_label(label: String) -> Nil {
  erlang_set_label(atom.create(label))
  Nil
}

type SetLabelResult

@external(erlang, "proc_lib", "set_label")
fn erlang_set_label(l: Atom) -> SetLabelResult
