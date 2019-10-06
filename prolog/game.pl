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
    unmatched_end_stroke/1,
    horiz/4,
    vert/4,
    corner/6,
    box/5,
    triangle/8,
    slant_line/6,
    house/5,
    rocket/5,
    persistant/2.

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
    do_in_chr_thread(true, get_all_persistent(S, Persistant)),
    list_html_string(Persistant, Response),
    format('Access-Control-Allow-Origin: *~n'),
    format('Content-type: text/plain~n~n~w', [Response]).

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
chr_reset(S) \ house(S, _, _, _, _) <=> true.
chr_reset(S) \ rocket(S, _, _, _, _) <=> true.
chr_reset(_) <=> true.

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
do_end_draw(S) \ horiz(S, _, _, _) <=> true.
do_end_draw(S) \ vert(S, _, _, _) <=> true.
do_end_draw(S) \ corner(S, _, _, _, _, _) <=> true.
do_end_draw(S) \ start_draw(S) <=> true.
do_end_draw(S) \ box(S, _, _, _, _) <=> true.
do_end_draw(S) \ triangle(S, _, _, _, _, _, _, _) <=> true.
do_end_draw(S) \ slant_line(S, _, _, _, _, _) <=> true.
% house stays
% rocket stays
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
		 *         recognizers          *
		 *******************************/

% discard short lines
line(_, X1, Y1, X2, Y2) <=>
                short(X1, Y1, X2, Y2) |
                true.

short(X1, Y1, X2, Y2) :-
    (X1 - X2)*(X1 - X2) + (Y1 - Y2)*(Y1 - Y2) < 256.

% ============= horizontal vertical lines ==============

line(S, X1, Y1, X2, Y2) ==>
                X1 < X2,
                horiz_line(X1, Y1, X2, Y2) |
                horiz(S, X1, Y1, X2).
line(S, X1, Y1, X2, Y2) ==>
                X2 < X1,
                horiz_line(X1, Y1, X2, Y2) |
                horiz(S, X2, Y1, X1).

horiz_line(X1, Y1, X2, Y2) :-
    DX is abs(X1 - X2),
    DX > 5.0 * abs(Y1 - Y2).

line(S, X1, Y1, X2, Y2) ==>
                Y1 < Y2,
                vert_line(X1, Y1, X2, Y2) |
                vert(S, X1, Y1, Y2).
line(S, X1, Y1, X2, Y2) ==>
                Y2 < Y1,
                vert_line(X1, Y1, X2, Y2) |
                vert(S, X1, Y2, Y1).

vert_line(X1, Y1, X2, Y2) :-
    horiz_line(Y1, X1, Y2, X2).

% =============  corner ================

% corners are usual RH coord system (So is Snap!)
% not comp graphics LH
horiz(S, XH1, YH, XH2), vert(S, XV, YV1, YV2) ==>
             short(XH1, YH, XV, YV1) |
             corner(S, ll, XV, YH, XH2, YV2).

horiz(S, XH1, YH, XH2), vert(S, XV, YV1, YV2) ==>
             short(XH1, YH, XV, YV2) |
             corner(S, ul, XV, YH, XH2, YV1).

horiz(S, XH1, YH, XH2), vert(S, XV, YV1, YV2) ==>
             short(XH2, YH, XV, YV2) |
             corner(S, ur, XV, YH, XH1, YV1).

horiz(S, XH1, YH, XH2), vert(S, XV, YV1, YV2) ==>
             short(XH2, YH, XV, YV1) |
             corner(S, lr, XV, YH, XH1, YV2).

% ======================== box (axis aligned rect) =============

corner(S, ll, LLX , LLY, LLRX, LLUY),
corner(S, ur, URX, URY, URLX, URLY) <=>
       short(LLX, LLY, URLX, URLY),
       short(URX, URY, LLRX, LLUY) |
       box(S, LLX, LLY, URX, URY).

corner(S, lr, LRX, LRY, LRLX, LRUY),
corner(S, ul, ULX, ULY, ULRX, ULLY) <=>
      short(LRX, LRY, ULRX, ULLY),
      short(ULX, ULY, LRLX, LRUY) |
      box(S, ULX, LRY, LRX, ULY).

% well drawn box is recognized twice
box(S, X1, Y1, X2, Y2) \ box(S, X1, Y1, X2, Y2) <=> true.

% ====================== triangles ==================

line(S, X1, Y1, X2, Y2) ==> X1 < X2,
                     Y1 < Y2,
                     \+ horiz_line(X1, Y1, X2, Y2),
                     \+ vert_line(X1, Y1, X2, Y2) |
                     slant_line(S, ur, X1, Y1, X2, Y2).
line(S, X1, Y1, X2, Y2) ==> X1 >= X2,
                     Y1 >= Y2,
                     \+ horiz_line(X1, Y1, X2, Y2),
                     \+ vert_line(X1, Y1, X2, Y2) |
                     slant_line(S, ur, X2, Y2, X1, Y1).
line(S, X1, Y1, X2, Y2) ==> X1 < X2,
                     Y1 >= Y2,
                     \+ horiz_line(X1, Y1, X2, Y2),
                     \+ vert_line(X1, Y1, X2, Y2) |
                     slant_line(S, ul, X1, Y1, X2, Y2).
line(S, X1, Y1, X2, Y2) ==> X1 >= X2,
                     Y1 < Y2,
                     \+ horiz_line(X1, Y1, X2, Y2),
                     \+ vert_line(X1, Y1, X2, Y2) |
                     slant_line(S, ul, X2, Y2, X1, Y1).

slant_line(S, ul, XL1, YL1, XL2, YL2),
slant_line(S, ur, XR1, YR1, XR2, YR2) <=>
        short(XL2, YL2, XR1, YR1),
        abs(YL1 - YR2) < 16 |
        Mid is (XL1 + YL2) / 2,
        triangle(S, gull, XL1, YL1, Mid, YL2, XR2, YR2).

slant_line(S, ur, XL1, YL1, XL2, YL2),
slant_line(S, ul, XR1, YR1, XR2, YR2) <=>
        short(XL2, YL2, XR1, YR1),
        abs(YL1 - YR2) < 16 |
        Mid is (XL1 + YL2) / 2,
        triangle(S, caret, XL1, YL1, Mid, YL2, XR2, YR2).

horiz(S, HX1, HY, HX2) \
triangle(S, gull, X1, Y1, XM, YM, X2, Y2) <=>
        short(X1, Y1, HX1, HY),
        short(X2, Y2, HX2, HY) |
        triangle(S, pride, X1, Y1, XM, YM, X2, Y2).

horiz(S, HX1, HY, HX2) \
triangle(S, caret, X1, Y1, XM, YM, X2, Y2) <=>
        short(X1, Y1, HX1, HY),
        short(X2, Y2, HX2, HY) |
        triangle(S, hat, X1, Y1, XM, YM, X2, Y2).

corner(S, ll, LLX , LLY, LLRX, LLUY),
slant_line(S, ul, X3, Y3, X4, Y4) <=>
      short(LLX, LLUY, X3, Y3),
      short(LLRX, LLRX, X4, Y4) |
      triangle(S, finll, LLX, LLY, LLX, LLUY, LLRX, LLY).

corner(S, lr, LRX, LRY, LRLX, LRUY),
slant_line(S, ur, X3, Y3, X4, Y4) <=>
      short(LRX, LRUY, X4, Y4),
      short(LRLX, LRY, X3, Y3) |
      triangle(S, finlr, LRLX, LRY, LRX, LRUY, LRX, LRY).

corner(S, ur, URX, URY, URLX, URLY),
slant_line(S, ul, X3, Y3, X4, Y4) <=>
     short(URX, URLY, X3, Y3),
     short(URLX, URY, X4, Y4) |
     triangle(S, finur, URLX, URY, URX, URY, URX, URLY).

corner(S, ul, ULX, ULY, ULRX, ULLY),
slant_line(S, ur, X3, Y3, X4, Y4) <=>
     short(ULX, ULLY, X3, Y3),
     short(ULRX, ULY, X4, Y4) |
     triangle(S, finul, ULX, ULLY, ULX, ULY, ULRX, ULY).

% ==================== persistant object collector =========
%


get_all_persistent(S, Persistant) :-
    nb_setval(all_p, []),
    gap(S, P),
%    findall(X, get_persist(S, X), P),
%    P = [[house, 1,1,100,100], [rocket, 10, 10, 50, 50]],
    flatten(P, Persistant).

gap(S, _) :-
    find_chr_constraint(persistant(S, Ret)),
    nb_getval(all_p, OldP),
    nb_setval(all_p, [Ret | OldP]),
    fail.
gap(_, P) :-
    nb_getval(all_p, P).
/*
:- chr_constraint temp_persist/2, collect_persist/2, get_persist/2.

get_all_persistant(S, Persistant) :-
    chr_trace, chr_leash(-all),
    get_persist(S, Persistant).

persistant(S, Data), get_persist(S, _) ==> temp_persist(S, Data).
get_persist(S, Persist) <=> collect_persist(S, Persist).
temp_persist(S, Data), collect_persist(S, Persist) <=>
       Persist = [Data | Rest],
       collect_persist(S, Rest).
collect_persist(_, L) <=> L=[].
*/

house(S, X1, Y1, X2, Y2) ==>
   W is X2 - X1,
   H is Y2 - Y1,
   persistant(S, [house, X1, Y1, W, H]).

rocket(S, X1, Y1, X2, Y2) ==>
   W is X2 - X1,
   H is Y2 - Y1,
   persistant(S, [rocket, X1, Y1, W, H]).

list_html_string(L, Str) :-
    list_html_string(L, [], C),
    string_codes(Str, C).

list_html_string([], C, C).
list_html_string([H|T], In, Out) :-
    atom(H),
    format(codes(HCodes), '~w', [H]),
    (   In == []
    ->  Down = HCodes
    ;   append([In, `,`, HCodes], Down)
    ),
    list_html_string(T, Down, Out).
list_html_string([H|T], In, Out) :-
    number(H),
    format(codes(HCodes), '~w', [H]),
    (   In == []
    ->  Down = HCodes
    ;   append([In, `\n`, HCodes], Down)
    ),
    list_html_string(T, Down, Out).
list_html_string([H|T], In, Out) :-
    var(H),
    list_html_string(T, In, Out).



% ======================= house ================================

box(S, X1, Y1, X2, Y2),
triangle(S, hat, XT1, YT1, _, YM, XT2, YT2) <=>
        short(X1, Y2, XT1, YT1),
        short(X2, Y2,  XT2, YT2) |
        house(S, XT1, Y1, XT2, YM).
box(S, X1, Y1, X2, Y2),
triangle(S, caret, XT1, YT1, _, YM, XT2, YT2) <=>
        short(X1, Y2, XT1, YT1),
        short(X2, Y2,  XT2, YT2) |
        house(S, XT1, Y1, XT2, YM).

% ============ rocket ==================

% facing right
box(S, X1, Y1, X2, Y2),
triangle(S, finll, LLFX, LLFY, _, LLUY, _, _),
triangle(S, finul, _, B1Y, ULFX, ULFY, _, _),
triangle(S, right_arrow, _, _, XM, _, XB, YB) <=>
       X2 - X1 > 2.5 * (Y2 - Y1),
       short(X1, Y1, ULFX, ULFY),
       short(X2, Y2, LLFX, LLFY),
       short(X2, Y2, XB, YB) |
       rocket(S, X1, B1Y, XM, LLUY).




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
