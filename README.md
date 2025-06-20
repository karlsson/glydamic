# glydamic
<!--
[![Package Version](https://img.shields.io/hexpm/v/glydamic)](https://hex.pm/packages/glydamic)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/glydamic/)
-->

Start a dynamic child under a supervisor.
This package may be deprecated once the `dynamic_supervisor`is implemented
in `gleam_otp`.

Add in gleam.toml under `dependencies`
```toml
[dependencies]
glydamic = { git = "https://github.com/karlsson/glydamic", ref="main"}
```
Example:
```gleam
import glydamic/child

pub fn start_child(
  sup_name: Name(String),
  client_subject: Subject(ClientRequest(String)),
) -> Result(Child, StartError) {
  case process.named(sup_name) {
    Ok(pid) -> {
      let child_spec =
        supervision.worker(fn() {
          gleam.Ok(actor.Started(start_link(client_subject), Nil))
        })
        |> supervision.restart(supervision.Temporary)
      child.start(pid, child_spec)
    }
    Error(Nil) -> Error(child.NoSupervisor)
  }
}
```
