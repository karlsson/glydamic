//// Module for starting "dynamic" children.

import gleam
import gleam/erlang/process.{type Pid}
import gleam/otp/actor
import gleam/otp/supervision.{type ChildSpecification}
import glydamic/supervisor.{type Supervisor}

/// `id` is the supervisor internal id for the child.
///
/// `started` is the value returned to the parent when
/// their child successfully starts.
pub type Child(a) {
  SupervisedChild(id: ChildId, started: actor.Started(a))
}

/// Get the data part from the child process/actor start result.
/// Normally this will be a Subject to use when sending
/// messages to the child.
pub fn started_data(child: Child(a)) {
  child.started.data
}

pub type ChildId

pub type StartError {
  NoSupervisor
  AlreadyPresent
  AlreadyStarted(Pid)
  Actor(actor.StartError)
}

pub type DeleteError {
  LostSupervisor
  Running
  Restarting
  NotExist
  NotFound
  SimpleOneForOne
  Unknown
}

/// Starts the child with the corresponding child specification.
///
/// Returns the child Pid and its internal id used by the supervisor
/// for the other functions.
pub fn start(
  supervisor: Supervisor,
  child_spec: ChildSpecification(a),
) -> Result(Child(a), StartError) {
  case supervisor.pid(supervisor) {
    gleam.Ok(pid) -> erl_start(pid, child_spec)
    gleam.Error(Nil) -> gleam.Error(NoSupervisor)
  }
}

@external(erlang, "glydamic_child_ffi", "start_child")
fn erl_start(
  sup_pid: Pid,
  child_spec: ChildSpecification(a),
) -> Result(Child(a), StartError)

/// Tells supervisor to delete the child specification identified by Id.
/// The corresponding child process must not be running.
/// Use `terminate` to terminate it.
pub fn delete(
  supervisor: Supervisor,
  child: Child(a),
) -> Result(Nil, DeleteError) {
  case supervisor.pid(supervisor) {
    gleam.Ok(pid) -> erl_delete(pid, child.id)
    gleam.Error(Nil) -> gleam.Error(LostSupervisor)
  }
}

@external(erlang, "glydamic_child_ffi", "delete_child")
fn erl_delete(sup_pid: Pid, id: ChildId) -> Result(Nil, DeleteError)

/// Restart the child
pub fn restart(
  supervisor: Supervisor,
  child: Child(a),
) -> Result(Child(a), DeleteError) {
  case supervisor.pid(supervisor) {
    gleam.Ok(pid) -> erl_restart(pid, child.id)
    gleam.Error(Nil) -> gleam.Error(LostSupervisor)
  }
}

@external(erlang, "glydamic_child_ffi", "restart_child")
fn erl_restart(sup_pid: Pid, id: ChildId) -> Result(Child(a), DeleteError)

/// Stop the child
pub fn terminate(
  supervisor: Supervisor,
  child: Child(a),
) -> Result(Nil, DeleteError) {
  case supervisor.pid(supervisor) {
    gleam.Ok(pid) -> erl_terminate(pid, child.id)
    gleam.Error(Nil) -> gleam.Error(LostSupervisor)
  }
}

@external(erlang, "glydamic_child_ffi", "terminate_child")
fn erl_terminate(sup_pid: Pid, id: ChildId) -> Result(Nil, DeleteError)
