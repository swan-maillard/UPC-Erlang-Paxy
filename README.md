# Paxy: Paxos-Based Consensus Simulation

Paxy is an Erlang-based simulation of a distributed consensus algorithm using Paxos. This project demonstrates the basic principles of Paxos through a visual representation of proposers and acceptors coordinating to achieve consensus.

## Paxos Algorithm

Paxos is a consensus algorithm that ensures a group of distributed processes can agree on a single value, even if some processes fail or messages are lost. The algorithm operates in two phases:

1. **Prepare Phase**: A proposer sends a prepare message with a proposal number to a quorum of acceptors. Acceptors respond with a promise not to accept proposals with lower numbers, and may include the value they have already voted on.
2. **Accept Phase**: If a proposer receives promises from a majority, it sends an accept request with its proposal. Acceptors will then vote for the proposal if it meets the criteria, and the consensus is reached.

## Features

- **Multiple Proposers and Acceptors**: Simulates a network of proposers that propose values, and acceptors that promise or vote on those proposals.
- **Consensus Algorithm Simulation**: Follows the Paxos algorithm’s phases to achieve consensus.
- **GUI**: Displays the state of proposers and acceptors, including rounds, votes, promises, and decisions.
- **Persistence**: Acceptor state is stored and retrieved using a persistence layer, ensuring the system can recover from failures.

## Project Structure
- **paxy.erl**: The main module that initializes the system, starts proposers and acceptors, and manages the simulation lifecycle.
- **proposer.erl**: Implements the proposer’s logic, which includes proposing values, handling promises, and driving the Paxos protocol forward.
- **acceptor.erl**: Implements the acceptor’s logic, including handling proposals, sending promises, and voting on proposals.
- **order.erl**: Manages the order of rounds and votes, ensuring the Paxos protocol's ordering requirements.
- **pers.erl**: Handles persistence for acceptor states, allowing recovery after failures.

## How It Works

1. **Start Simulation**: 
   The `start/1` function in `paxy.erl` initializes the simulation. It sets up a list of proposers and acceptors, registers them, and starts the proposer and acceptor processes.
   
2. **Proposers**: 
   Proposers run the Paxos protocol, sending out prepare messages and handling promises from acceptors. Once a majority of promises are received, the proposer moves to the accept phase and tries to convince the acceptors to vote on its proposal.

3. **Acceptors**: 
   Acceptors keep track of the highest proposal they have promised to consider and vote accordingly. They respond to prepare requests by either promising or rejecting them based on the round number, and they vote on accept requests if they match their promised round.

4. **GUI Interaction**: 
   The GUI displays the current state of the simulation, showing the rounds, proposals, votes, and other details of the Paxos protocol in real time.

## Usage

### Starting the Simulation

To start the Paxy simulation, run the `start/1` function with a list of sleep intervals for each proposer:

```erlang
paxy:start([1000, 1500, 2000]).
```

This will start the acceptors and proposers with the specified initial sleep times.

### Stopping the Simulation

You can stop the simulation with the `stop/0` or `stop/1` functions:

```erlang
paxy:stop().
```

This will cleanly shut down all acceptors, proposers, and the GUI.

## Authors

- Swan Maillard (maillard.swan@gmail.com)
- Felix Mühlenberend

## License

This project is licensed under the MIT License. Please consult the `LICENSE` file for more information.
