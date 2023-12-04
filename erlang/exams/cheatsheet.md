# Erlang Cheat Sheet

This cheat sheet provides a quick reference to common Erlang commands and functions used for distributed computing and process management.

## Checking if a Process is Alive

- **Command**: `is_process_alive(Pid)`
- **Description**: Check if the given process is alive.

## Define a Macro

- **Command**: `-define(Macro, Value)`
- **Description**: Define a macro with the given value. Macros are replaced at compile time. Example of a macro which sums two numbers: `-define(SUM(X, Y), X + Y).`; to use the macro, call `?SUM(1, 2)`.

## Demonitoring a Process

- **Command**: `demonitor(Ref)`
- **Description**: Demonitor the given process. The `Ref` is the reference returned by `erlang:monitor`.

## Getting the Hostname of the Local Machine

- **Command**: `{ok, HostName} = inet:gethostname()`
- **Description**: Retrieve the hostname of the local machine.

## Getting a List of All Nodes in the Distributed System

- **Command**: `nodes()`
- **Description**: Retrieve a list of all nodes in the distributed system.

## Getting a List of All Registered Processes Globally

- **Command**: `global:registered_names()`
- **Description**: Retrieve a list of all registered processes in the global scope.

## Getting the Name of the Current Node

- **Command**: `Node = node()`
- **Description**: Retrieve the name of the current node.

## Handling an Exit Signal

- **Command**: `receive {'EXIT', Pid, Reason} -> ... end`
- **Description**: Handle an exit signal from a process. The `Pid` is the PID of the process that exited, and the `Reason` is the exit reason. Note that the process must be set as a system process to receive the exit signal of a linked process without terminating.

## Handling Exceptions

- **Command**: `try ... catch [error:Reason | exit:Reason | throw:Reason] -> ... end`
- **Description**: Handle an exception. The `Reason` is the exception reason. Note that the catch clause behaves like a pattern match, so it can be used to handle different types of exceptions.

## Linking to a Process

- **Command**: `link(Pid)`
- **Description**: Link the client process to the given process. This allows the client process to receive an exit signal if the linked process terminates. Note that the client process must be set as a system process to handle the exit signal without terminating.

## Loading a Module on All Nodes

- **Command**: `nl(modulename)`
- **Description**: Load the given module on all nodes in the distributed system. Note: the module must be compiled first using `c(modulename)`.

## Monitoring a Process

- **Command**: `monitor(process, Pid)`
- **Description**: Monitor the given process in an unidirectional manner. This allows the client process to receive an exit signal if the monitored process terminates. Since links are bidirectional, this is useful if you only want to receive an exit signal from the monitored process without having to set the client process as a system process every time. If B dies and A is monitoring B, A will receive an exit signal, but B will not; if A dies, B will not receive an exit signal. The exit signal can be handled using `receive {'DOWN', Ref, process, Pid, Reason} -> ... end` where `Ref` is the reference returned by `erlang:monitor`, `Pid` is the PID of the monitored process, and `Reason` is the exit reason.

## Pinging a Node

- **Command**: `net_adm:ping(nodename)`
- **Description**: Ping the given node to make it visible to the client.

## Raise an Exit Signal

- **Command**: `exit(Pid, Reason)`
- **Description**: Raise an exit signal to the given process with the given reason.

## Redirecting io:format Output to the Node's Shell

- **Command**: `group_leader(whereis(user), self())`
- **Description**: Redirect `io:format` output from the client's shell to the node's shell.

## Registering a Process Globally

- **Command**: `global:register_name(name, self())`
- **Description**: Register a process with a given name in the global scope. Useful for referencing the process across different nodes.

## Retrieving a Process PID by Name Globally

- **Command**: `Pid = global:whereis_name(name)`
- **Description**: Retrieve a process's PID using its name from the global scope. This is useful for communicating with processes across different nodes.

## Setting a Process as a System Process

- **Command**: `process_flag(trap_exit, true)`
- **Description**: Set a process as a system process. This allows it to receive exit signals from other processes without terminating as it receives the signal in its mailbox. The signal can be handled using `receive {'EXIT', Pid, Reason} -> ... end`.

    Examples:

  - I don't care if a process I create crashes: `Pid = spawn(fun()-> ... end)`.
  - I want to die if a process I create crashes: `Pid = spawn_link(fun()-> ... end)`.
  - I want to handle errors if a process I create crashes:
  `process_flag(trap_exits, true), Pid = spawn_link(fun()-> ... end)`

## Spawning a Process on a Given Node

- **Command**: `Pid = spawn(list_to_atom("nodename@" ++ HostName), module, function, [args])`
- **Description**: Spawn a given `module:function` on a specified node. The node is determined by combining the nodename with the HostName.

## Spawning a Process and Linking to It

- **Command**: `Pid = spawn_link(module, function, [args])`
- **Description**: Spawn a given `module:function` and link to it. This allows the client process to receive an exit signal if the spawned process terminates. Note that the spawned process must be set as a system process to handle the exit signal without terminating.

## Starting an Erlang Shell on a Node

- **Command**: `erl -sname nodename`
- **Description**: Start an Erlang shell on a given node. Note that the client shell must also be started with the `-sname` flag, or the nodes won't be visible to it.

## Unlinking from a Process

- **Command**: `unlink(Pid)`
- **Description**: Unlink the client process from the given process. This allows the client process to no longer receive an exit signal if the linked process terminates.
