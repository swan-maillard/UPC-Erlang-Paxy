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
  AccRegister = [{mariejosette, ?ACC}, {mariemicheline, ?ACC}, {mariecunegonde, ?ACC}, {mariejeanne, ?ACC}, {marieelise, ?ACC}],
  ProposerNames = [{"Jean-Phillibert", ?RED}, {"Jean-Maxence", ?GREEN}, {"Jean-Eude", ?BLUE}],
  PropInfo = [{{jeanphillibert, ?PRO}, ?RED}, {{jeanmaxence, ?PRO}, ?GREEN}, {{jeaneude, ?PRO}, ?BLUE}],
  register(gui, spawn(fun() -> gui:start(AcceptorNames, ProposerNames) end)),
  gui ! {reqState, self()},
  receive
    {reqState, State} ->
      {AccIds, PropIds} = State,
      spawn(?ACC, fun() -> start_acceptors(AccIds, AccRegister) end),
      spawn(?PRO, fun() ->
        Begin = erlang:monotonic_time(),
        start_proposers(PropIds, PropInfo, AccRegister, Sleep, self()),
        wait_proposers(length(PropIds)),
        End = erlang:monotonic_time(),
        Elapsed = erlang:convert_time_unit(End-Begin, native, millisecond),
        io:format("[Paxy] Total elapsed time: ~w ms~n", [Elapsed])
      end)
  end.
    
start_acceptors(AccIds, AccReg) ->
  case AccIds of
    [] ->
      ok;
    [AccId|Rest] ->
      [{RegName, ?ACC}|RegNameRest] = AccReg,
      register(RegName, acceptor:start({RegName, ?ACC}, AccId)),
      start_acceptors(Rest, RegNameRest)
  end.

start_proposers(PropIds, PropInfo, Acceptors, Sleep, Main) ->
  case PropIds of
    [] ->
      ok;
    [PropId|Rest] ->
      [{{RegName, ?PRO}, Colour}|RestInfo] = PropInfo,
      [FirstSleep|RestSleep] = Sleep,
      proposer:start({RegName, ?PRO}, Colour, Acceptors, FirstSleep, PropId, Main),	
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
  stop({mariejosette, ?ACC}),
  stop({mariemicheline, ?ACC}),
  stop({mariecunegonde, ?ACC}),
  stop({mariejeanne, ?ACC}),
  stop({marieelise, ?ACC}),
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

crash(Name) when is_tuple(Name) ->
  {AccName, _} = Name,
  pers:open(AccName),
  {_, _, _, Pn} = pers:read(AccName),
  Pn ! {updateAcc, "Voted: CRASHED", "Promised: CRASHED", {0, 0, 0}},
  unregister(AccName),
  io:format("CRASH~n"),
  pers:close(AccName),
  exit(Name, "crash"),
  timer:sleep(3000),
  register(AccName, acceptor:start(Name, na));
crash(_) ->
    ok.