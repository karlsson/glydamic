//// The Gleam erlang/process lib makes use of the Erlang proc_lib module
//// to spawn processes.
//// This is not always available on some platforms like AtomVM. 
//// Glydamic adds functions for "bare" erlang `spawn/1` and `spawn_link/1`
//// functions.

import gleam/erlang/process

/// Spawns an un-linked erlang process.
@external(erlang, "erlang", "spawn")
pub fn splunk(start_fun: fn() -> Nil) -> process.Pid

/// Spawns a linked erlang process.
@external(erlang, "erlang", "spawn_link")
pub fn splink(start_fun: fn() -> Nil) -> process.Pid
