-module(paxy).
-export([start/1, stop/0, stop/1]). %PL: exposing interfaces

-define(RED, {255,0,0}). %PL: defining colors for GUI. colors represent values to be voted for
-define(BLUE, {0,0,255}).
-define(GREEN, {0,255,0}).

% Sleep is a list with the initial sleep time for each proposer
start(Sleep) -> %PL: entry point of the application
    AcceptorNames = ["Acceptor a", "Acceptor b", "Acceptor c", "Acceptor d", "Acceptor e"],
    AccRegister = [a, b, c, d, e],

    ProposerNames = [{"Proposer kurtz", ?RED}, {"Proposer kilgore", ?GREEN}, {"Proposer willard", ?BLUE}],
    PropInfo = [{kurtz, ?RED}, {kilgore, ?GREEN}, {willard, ?BLUE}],

    register(gui, spawn(fun() -> gui:start(AcceptorNames, ProposerNames) end)), %PL: spawn creates a new process - in this case the GUI - and register assigns the tag/atom (gui) to that new process/PID
    gui ! {reqState, self()}, %PL: send reference to itself (own PID) to GUI with request-state-command/atom
    receive
        {reqState, State} ->
            {AccIds, PropIds} = State, %PL: upon receiving message/response back from gui (with GUI-PID of acceptors and proposers... now GUI PIDs have to be linked with actual actors/PIDs - those are yet to be created), bind list-elements and start rest of execution
            start_acceptors(AccIds, AccRegister),
            start_proposers(PropIds, PropInfo, AccRegister, Sleep)
    end,
    true.

%PL: acceptors don't know each other or any of the proposers, they know only themselves
start_acceptors(AccIds, AccReg) ->
    case AccIds of
        [] ->%PL: break on list end
            ok;
        [AccId|Rest] ->%PL: process list head first
            [RegName|RegNameRest] = AccReg,
            register(RegName, acceptor:start(RegName, AccId)), %PL: start process in logic-module (not gui as before), assign passed atom-parameter as module-unique name
            start_acceptors(Rest, RegNameRest)%PL: process rest of list
    end.

%PL: proposers don't know each other, but all of the acceptors. each has a value=color to be voted for
start_proposers(PropIds, PropInfo, Acceptors, Sleep) ->
    case PropIds of
        [] ->
            ok;
        [PropId|Rest] ->
            [{RegName, Colour}|RestInfo] = PropInfo,
            [FirstSleep|RestSleep] = Sleep,
            proposer:start(RegName, Colour, Acceptors, FirstSleep, PropId),	
            start_proposers(Rest, RestInfo, Acceptors, RestSleep)
        end.

stop() -> %PL: stop logic first, view later
    stop(a),
    stop(b),
    stop(c),
    stop(d),
    stop(e),
    stop(gui).

stop(Name) -> %PL: stop all sorts of processes
    case whereis(Name) of
        undefined ->
            ok;
        Pid ->
            Pid ! stop
    end.

 
