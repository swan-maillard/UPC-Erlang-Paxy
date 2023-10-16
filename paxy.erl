-module(paxy).
-export([start/1, stop/0, stop/1, crash/1]).

-define(RED, {255,0,0}).
-define(BLUE, {0,0,255}).
-define(GREEN, {0,255,0}).

-define(ACC, 'paxy-acc@LAPTOP-MC47CKNN').
-define(PRO, 'paxy-pro@LAPTOP-MC47CKNN').

% Sleep is a list with the initial sleep time for each proposer
start(Sleep) ->
  AcceptorNames = ["Marie-Josette", "Marie-Micheline", "Marie-Cunégonde", "Marie-Jeanne", "Marie-Elise"],
  AccRegister = [mariejosette, mariemicheline, mariecunegonde, mariejeanne, marieelise],
  ProposerNames = [{"Jean-Phillibert", ?RED}, {"Jean-Maxence", ?GREEN}, {"Jean-Eude", ?BLUE}],
  PropInfo = [{jeanphillibert, ?RED}, {jeanmaxence, ?GREEN}, {jeaneude, ?BLUE}],
  register(gui, spawn(fun() -> gui:start(AcceptorNames, ProposerNames) end)),
  gui ! {reqState, self()},
  receive
    {reqState, State} ->
      {AccIds, PropIds} = State,
      start_acceptors(AccIds, AccRegister),
      spawn(fun() ->
        Begin = erlang:monotonic_time(),
        start_proposers(PropIds, PropInfo, AccRegister, Sleep, self()),
        wait_proposers(length(PropIds)),
        End = erlang:monotonic_time(),
        Elapsed = erlang:convert_time_unit(End-Begin, native, millisecond),
        io:format("[Paxy] Total elapsed time: ~w ms~n", [Elapsed])
      end),
      spawn(fun() ->
        timer:sleep(rand:uniform(100)+100),
        crash(mariejosette)
      end)
  end.
    
start_acceptors(AccIds, AccReg) ->
  case AccIds of
    [] ->
      ok;
    [AccId|Rest] ->
      [RegName|RegNameRest] = AccReg,
      register(RegName, acceptor:start(RegName, AccId)),
      start_acceptors(Rest, RegNameRest)
  end.

start_proposers(PropIds, PropInfo, Acceptors, Sleep, Main) ->
  case PropIds of
    [] ->
      ok;
    [PropId|Rest] ->
      [{RegName, Colour}|RestInfo] = PropInfo,
      [FirstSleep|RestSleep] = Sleep,
      proposer:start(RegName, Colour, Acceptors, FirstSleep, PropId, Main),	
      start_proposers(Rest, RestInfo, Acceptors, RestSleep, Main)

  end.

wait_proposers(0) ->
  ok;
wait_proposers(N) ->
  receive
    done ->
      wait_proposers(N-1)
  end.

stop() ->
  stop(mariejosette),
  stop(mariemicheline),
  stop(mariecunegonde),
  stop(mariejeanne),
  stop(marieelise),
  stop(gui).

stop(Name) ->
  if is_tuple(Name) ->
    Name ! stop;
  true ->
    case whereis(Name) of
        undefined ->
          ok;
        Pid ->
          Pid ! stop
      end
  end.

crash(Name) ->
  case whereis(Name) of
    undefined ->
      ok;
    Pid ->
      pers:open(Name),
      {_, _, _, Pn} = pers:read(Name),
      Pn ! {updateAcc, "Voted: CRASHED", "Promised: CRASHED", {0,0,0}},
      io:format("CRASH\n"),
      pers:close(Name),
      unregister(Name),
      exit(Pid, "crash"),
      timer:sleep(100),
      register(Name, acceptor:start(Name, na))
end.