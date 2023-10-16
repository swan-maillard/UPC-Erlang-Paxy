-module(acceptor).
-export([start/2]).

start(Name, PanelId) ->
  spawn(fun() -> init(Name, PanelId) end).
        
init(Name, PanelId) ->
  {AccName, _} = Name,
  pers:open(AccName),
  {Promised, Voted, Value, StoredPanelId} = pers:read(AccName),
  case StoredPanelId of
    na ->
      pers:store(AccName, Promised, Voted, Value, PanelId),
      pers:close(AccName),
      acceptor(Name, Promised, Voted, Value, PanelId);
    _ ->
      pers:close(AccName),
      acceptor(Name, Promised, Voted, Value, StoredPanelId)
  end.

acceptor(Name, Promised, Voted, Value, PanelId) ->
  %% We receive a PREPARE request from PROPOSER
  receive
    {prepare, Proposer, Round} ->

      case order:gr(Round, Promised) of
       %% If Round greater than current promise, we promise it
        true ->
            Proposer ! {promise, Round, Voted, Value},
            {AccName, _} = Name,
            pers:open(AccName),
            pers:store(AccName, Round, Voted, Value, PanelId),
            pers:close(AccName),

            io:format("[Acceptor ~w] Phase 1: promised ~w voted ~w colour ~w~n", [Name, Round, Voted, Value]),

            % Update gui
            Colour = case Value of na -> {0,0,0}; _ -> Value end,
            PanelId ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Voted]), 
                      "Promised: " ++ io_lib:format("~p", [Round]), Colour},
            %% We update the ACCEPTOR values
            acceptor(Name, Round, Voted, Value, PanelId);
        %% Else
        false ->
          Proposer ! {sorry, {prepare, Round}},
          %% We update the ACCEPTOR values (they didn't change)
          acceptor(Name, Promised, Voted, Value, PanelId)
      end;
    %% We receive an ACCEPT request from PROPOSER
    {accept, Proposer, Round, Proposal} ->
      case order:goe(Round, Promised) of
        %% If Round is greater or equal than the promise, we vote for it
        true ->
          Proposer ! {vote, Round},
          case order:goe(Round, Voted) of
            %% If Round (new vote) is greater or equal than the current vote, we update the vote
            true ->
              io:format("[Acceptor ~w] Phase 2: promised ~w voted ~w colour ~w~n", [Name, Promised, Round, Proposal]),
              % Update gui
              PanelId ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Round]), 
                        "Promised: " ++ io_lib:format("~p", [Promised]), Proposal},
              {AccName, _} = Name,
              pers:open(AccName),
              pers:store(AccName, Promised, Round, Proposal, PanelId),
              pers:close(AccName),
              %% We update the ACCEPTOR values
              acceptor(Name, Promised, Round, Proposal, PanelId);
            %% Else we keep the previous vote
            false ->
              %% We update the ACCEPTOR values (they didn't change)
              acceptor(Name, Promised, Voted, Value, PanelId)
          end;
        %% Else                            
        false ->
          Proposer ! {sorry, {accept, Round}},
          %% We update the ACCEPTOR values (they didn't change)
          acceptor(Name, Promised, Voted, Value, PanelId)
      end;
    stop ->
      {AccName, _} = Name,
      pers:delete(AccName),
      PanelId ! stop,
      ok
  end.
