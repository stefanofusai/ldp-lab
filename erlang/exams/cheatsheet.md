# Erlang Cheat Sheet

This cheat sheet provides a quick reference to common Erlang commands and functions used for distributed computing and process management.

## Starting an Erlang Shell on a Node

- **Command**: `erl -sname nodename`
- **Description**: Start an Erlang shell on a given node. Note that the client shell must also be started with the `-sname` flag, or the nodes won't be visible to it.

## Pinging a Node

- **Command**: `net_adm:ping(nodename)`
- **Description**: Ping the given node to make it visible to the client.

## Loading a Module on All Nodes

- **Command**: `nl(modulename)`
- **Description**: Load the given module on all nodes in the distributed system.

## Getting the Hostname of the Local Machine

- **Command**: `{ok, HostName} = inet:gethostname()`
- **Description**: Retrieve the hostname of the local machine.

## Getting the Name of the Current Node

- **Command**: `node()`
- **Description**: Retrieve the name of the current node.

## Spawning a Process on a Given Node

- **Command**: `Pid = spawn(list_to_atom("nodename@" ++ HostName), module, function, [args])`
- **Description**: Spawn a given `module:function` on a specified node. The node is determined by combining the nodename with the HostName.

## Redirecting io:format Output to the Node's Shell

- **Command**: `group_leader(whereis(user), self())`
- **Description**: Redirect `io:format` output from the client's shell to the node's shell.

## Registering a Process Globally

- **Command**: `global:register_name(name, self())`
- **Description**: Register a process with a given name in the global scope. Useful for referencing the process across different nodes.

## Retrieving a Process PID by Name Globally

- **Command**: `global:whereis_name(name)`
- **Description**: Retrieve a process's PID using its name from the global scope. This is useful for communicating with processes across different nodes.

## Setting a Process as a System Process

- **Command**: `process_flag(trap_exit, true)`
- **Description**: Set a process as a system process. This allows it to receive exit signals from other processes without terminating.
