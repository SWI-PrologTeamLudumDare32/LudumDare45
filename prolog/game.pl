:- module(cooking, [go/0]).


:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(chr)).


go :- server(8888).

%!  server(+Port)
%
%   Start the server at http://localhost:Port

server(Port) :-
    create_chr_thread,
    http_server(http_dispatch,
                [ port(Port)
                ]).

:- chr_constraint
    chr_reset/1,
    touch/3,
    costume/3,
    get_costume/3,
    init_player/1.

:- http_handler('/touch', touched , []).

touched(Request) :-
    http_parameters(Request,
                [
                    me(Me, []),
                    dest(Dest, []),
                    sess(S, [integer])
                ]),
    init_session(S),
    do_in_chr_thread(touch(S, Me, Dest), get_costume(S, Me, Costume)),
    format('Access-Control-Allow-Origin: *~n'),
    format('Content-type: text/plain~n~n~w', [Costume]).


:- http_handler('/costume', respond_current_costume , []).

respond_current_costume(Request) :-
    http_parameters(Request,
                [
                    me(Me, []),
                    sess(S, [integer])
                ]),
    init_session(S),
    do_in_chr_thread(true, get_costume(S, Me, Costume)),
    format('Access-Control-Allow-Origin: *~n'),
    format('Content-type: text/plain~n~n~w', [Costume]).


:- http_handler('/reset', do_reset , []).

do_reset(Request) :-
    http_parameters(Request,
                [
                    sess(S, [integer])
                ]),
    init_session(S),
    reset_session(S),
    format('Access-Control-Allow-Origin: *~n'),
    format('Content-type: text/plain~n~nOK').

reset_session(S) :-
    do_in_chr_thread(chr_reset(S), get_dummy(_)),
    do_in_chr_thread(costume(S, egg, shell), get_dummy(_)).

init_session(S) :-
    do_in_chr_thread(init_player(S), get_dummy(_)).

		 /*******************************
		 *          Game Logic          *
		 *******************************/

get_dummy(ok).

chr_reset(S) \ costume(S, _, _) <=> true.
chr_reset(S) \ touch(S, _, _) <=> true.
chr_reset(S) \ get_costume(S, _, _) <=> true.
chr_reset(_) <=> true.

% set up player if we haven't seen them
% idempotic pattern
% if we've already got a costume we inited already
% only add costume if we didn't have one
costume(S, _, _) \ init_player(S) <=> true.
init_player(S) <=> costume(S, egg, shell).

% when I touch the pan I go from shell to fried egg
touch(S, egg, pan) \ costume(S, egg, shell) <=> costume(S, egg, friedegg).

% add get_costume with Y unbound to retrieve X's costume
costume(S, X, Current) \ get_costume(S, X, Is) <=> Is = Current.
get_costume(_, _, _) <=> fail.

		 /*******************************
		 * Debug help                   *
		 *******************************/


debug_constraints(Where) :-
    find_chr_constraint(X),
    debug(constraint(Where), '~w', [X]),
    fail.
debug_constraints(_).


		 /*******************************
		 *  Thread Component            *
		 *******************************/

create_chr_thread :-
   message_queue_create(_, [ alias(sub) ]),
   message_queue_create(_, [ alias(par) ]),
   thread_create(polling_sub, _, [ alias(chr) ]).

polling_sub :-
   % listen for new message on `sub` queue
   thread_get_message(sub, sync(ActionCHR, ResultCHR)),
   % do the actual constraint call
   (   call(ActionCHR)
   ;
       debug(constraint(polling_sub),
             'action constraint ~w failed unexpectedly~n',
             [ActionCHR])
   ),

   debug_constraints(polling_sub),
   % get the result using the get_foo pattern
   ResultCHR =.. List,
   append(StubList, [_], List),
   append(StubList, [Result], CallMeList),
   CallMe =.. CallMeList,
   (   call(CallMe)
   ;
       debug(constraint(polling_sub),
             'result constraint ~w failed unexpectedly~n',
             [ResultCHR])
   ),
   !, % nondet calls not allowed
   % send it back to the `par` message queue
   thread_send_message(par, Result),
   % repeat
   polling_sub.

%!  do_in_chr_thread(+ActionCHR:chr_constraint,
%!         +ResultCHR:chr_constraint) is det
%
%   queries ActionCHR in the chr thread, which must be
%   grounded chr_constraint or prolog predicate,
%   then calls ResultCHR, whose last argument must be unbound.
%   the last argument will be bound as if a direct chr call
%   was made.
%
% eg to touch the egg to the pan and then get the egg's costume do
% do_in_chr_thread(touch(S, egg, pan), get_costume(S, egg, Costume))
%
% Note that these are effectively called in once/1
%
do_in_chr_thread(ActionCHR, ResultCHR) :-
   ResultCHR =.. List,
   append(_, [Result], List),
   thread_send_message(sub, sync(ActionCHR, ResultCHR)),
   thread_get_message(par, Result).

:- debug(constraint(_)).
