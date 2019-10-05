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
    start_draw/1,
    do_end_stroke/1,
    do_end_draw/1,
    do_add_coord/2,
    x/2,
    point/3,
    start_point/3,
    last_point/3,
    line/5,
    get_a_line/5,
    inited/1,
    unmatched_end_stroke/1.

:- http_handler('/drawing', draw_handler , []).

draw_handler(Request) :-
    http_parameters(Request,
                [
                    sess(S, [integer]),
                    drawing(Drawing, [string])
                ]),
    init_session(S),
    split_string(Drawing, "|", " ", Strokes),
    do_in_chr_thread(start_draw(S), get_dummy(_)),
    maplist(do_stroke_elem(S), Strokes),
    do_in_chr_thread(do_end_draw(S), get_dummy(_)),
    format('Access-Control-Allow-Origin: *~n'),
    format('Content-type: text/plain~n~nok').

do_stroke_elem(S, NString) :-
    number_string(N, NString),
    do_in_chr_thread(do_add_coord(S, N), get_dummy(_)).

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
    do_in_chr_thread(chr_reset(S), get_dummy(_)).

init_session(S) :-
    do_in_chr_thread(init_player(S), get_dummy(_)).

		 /*******************************
		 *          Game Logic          *
		 *******************************/

% prolog to provide a dummy return from chr thread
get_dummy(ok).

% reset to the start of game state. Not same as init_player
% which establishes initial conditions when session first seen
chr_reset(_) <=> true. % temp, not storing any state long term now

% start a 'drawing' - single http set of lines
start_draw(S) \ x(S, _) <=> true.
start_draw(S) \ point(S, _, _) <=> true.
start_draw(S) \ start_point(S, _, _) <=> true.
start_draw(S) \ last_point(S, _, _) <=> true.
start_draw(S) \ start_draw(S) <=> true. % poorly formated last req
start_draw(_) <=> true.
% hang on to start_draw til we see a point

% add one more coordinate point to the store
do_add_coord(S, Y), x(S, X) <=> point(S, X, Y).
do_add_coord(S, X) <=> x(S, X).

point(S, 10000, 10000) <=> do_end_stroke(S).
point(S, X, Y), last_point(S, _, _) <=> last_point(S, X, Y).
point(S, X, Y) <=> start_point(S, X, Y), last_point(S, X, Y).

do_end_stroke(S), start_point(S, SX, SY), last_point(S, EX, EY) <=>
    line(S, SX, SY, EX, EY).
do_end_stroke(_) <=> true.

do_end_draw(S) ==> debug_lines(S).
do_end_draw(S) \ line(S, _, _, _, _) <=> true.
do_end_draw(S) \ start_draw(S) <=> true.
do_end_draw(_) <=> true.

% get_foo pattern to get the lines
line(S, SX, SY, EX, EY), get_a_line(S, GSX, GSY, GEX, GEY) ==>
    SX = GSX,
    SY = GSY,
    EX = GEX,
    EY = GEY.
get_a_line(_, _, _, _, _) <=> true.

% set up player if we haven't seen them
% idempotic pattern
% if we've already got a costume we inited already
% only add costume if we didn't have one
init_player(S) :-
    inited(S).

inited(S) \ inited(S) <=> true.

		 /*******************************
		 * Debug help                   *
		 *******************************/
debug_lines(S) :-
    debug(lines, '====', []),
    find_chr_constraint(line(S, SX, SY, EX, EY)),
    debug(lines, '~w,~w -- ~w,~w', [SX, SY, EX, EY]),
    fail.
debug_lines(_).


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
   debug_constraints(polling_sub),
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
:- debug(lines).
