# Erlang Cheat Sheet

This cheat sheet provides a quick reference to common Erlang commands and functions used for distributed computing and process management.

## Getting the Hostname of the Local Machine

- **Command**: `{ok, HostName} = inet:gethostname()`
- **Description**: Retrieve the hostname of the local machine.

## Getting a List of All Nodes in the Distributed System

- **Command**: `nodes()`
- **Description**: Retrieve a list of all nodes in the distributed system.

## Getting the Name of the Current Node

- **Command**: `Node = node()`
- **Description**: Retrieve the name of the current node.

## Handling an Exit Signal

- **Command**: `receive {'EXIT', Pid, Reason} -> ... end`
- **Description**: Handle an exit signal from a process. The `Pid` is the PID of the process that exited, and the `Reason` is the exit reason. Note that the process must be set as a system process to receive the exit signal of a linked process without terminating.

## Handling Exceptions

- **Command**: `try ... catch [error:Reason | exit:Reason | throw:Reason] -> ... end`
- **Description**: Handle an exception. The `Reason` is the exception reason. Note that the catch clause behaves like a pattern match, so it can be used to handle different types of exceptions.

## Loading a Module on All Nodes

- **Command**: `nl(modulename)`
- **Description**: Load the given module on all nodes in the distributed system.

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
- **Description**: Set a process as a system process. This allows it to receive exit signals from other processes without terminating.

## Spawning a Process on a Given Node

- **Command**: `Pid = spawn(list_to_atom("nodename@" ++ HostName), module, function, [args])`
- **Description**: Spawn a given `module:function` on a specified node. The node is determined by combining the nodename with the HostName.

## Spawning a Process and Linking to It

- **Command**: `Pid = spawn_link(module, function, [args])`
- **Description**: Spawn a given `module:function` and link to it. This allows the client process to receive an exit signal if the spawned process terminates. Note that the spawned process must be set as a system process to handle the exit signal without terminating.

## Starting an Erlang Shell on a Node

- **Command**: `erl -sname nodename`
- **Description**: Start an Erlang shell on a given node. Note that the client shell must also be started with the `-sname` flag, or the nodes won't be visible to it.
