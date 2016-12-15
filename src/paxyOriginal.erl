-module(paxyOriginal).
-export([start/1, stop/0, stop/1]). % exposing interfaces for console. start program with (for example) paxy:start([50,100,200]).

-define(RED, {255,0,0}). % defining colors for GUI. colors represent values to be voted for
-define(BLUE, {0,0,255}).
-define(GREEN, {0,255,0}).

% Entry point of the application. Sleep is a list with the initial sleep time for each proposer
start(Sleep) ->
    AcceptorNames = ["Acceptor a", "Acceptor b", "Acceptor c", "Acceptor d", "Acceptor e"],
    AccRegister = [a, b, c, d, e],

    ProposerNames = [{"Proposer kurtz", ?RED}, {"Proposer kilgore", ?GREEN}, {"Proposer willard", ?BLUE}],
    PropInfo = [{kurtz, ?RED}, {kilgore, ?GREEN}, {willard, ?BLUE}],

    register(gui, spawn(fun() -> gui:start(AcceptorNames, ProposerNames) end)), % create new gui process
    gui ! {reqState, self()}, % send reference of self (own PID) to GUI
    receive
        {reqState, State} ->
            {AccIds, PropIds} = State,
            start_acceptors(AccIds, AccRegister),
            start_proposers(PropIds, PropInfo, AccRegister, Sleep)
    end,
    true.

% acceptors don't know each other or any of the proposers (until receiving a message), they know only themselves
start_acceptors(AccIds, AccReg) ->
    case AccIds of
        [] ->% break on list end
            ok;
        [AccId|Rest] ->% process list head first
            [RegName|RegNameRest] = AccReg,
            register(RegName, acceptor:start(RegName, AccId)),
            start_acceptors(Rest, RegNameRest)
    end.

% proposers don't know each other, but they know all of the acceptors
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

stop() -> % stop logic first, view later
    stop(a),
    stop(b),
    stop(c),
    stop(d),
    stop(e),
    stop(gui).

stop(Name) -> % stop all sorts of processes
    case whereis(Name) of
        undefined ->
            ok;
        Pid ->
            Pid ! stop
    end.

 
