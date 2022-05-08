#!/usr/bin/env swipl
/*
 * LOGICMOO CogServer Socket Server
 *
 * Copyright (c) 2022 Logicmoo Co <support@logicmoo.org>
 *
 * LICENSE:
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

:- module(cogserver_shell, [start_cogserver/0,start_cogserver/1,run_cogshell/0,oc_shell/0,oc_shell/1,start_cogserver_main/0]).

:- reexport(library(opencog/atomese)).
:- reexport(library(opencog/atomspace)).

:- ensure_loaded(library(logicmoo_cogserver)).

start_cogserver(Call,Port,Description):- PortNum is Port,
  must(prolog_cogserver(PortNum, [allow(_),call(Call),description(Description)])),!.

prolog_cogserver(Port, Options):-  
 \+ member(alias(_),Options),
 option(call(Call),Options,cogshell_telnet),
 atomic_list_concat([Call,'_',Port],Alias),!, 
 prolog_cogserver(Port, [alias(Alias)|Options]).

prolog_cogserver(_Port, Options) :- 
  member(alias(Alias),Options),thread_property(Base, status(running)),Base==Alias,!.

prolog_cogserver(Port, Options) :-
    tcp_socket(ServerSocket),
    tcp_setopt(ServerSocket, reuseaddr),
    tcp_bind(ServerSocket, Port),
    tcp_listen(ServerSocket, 5),
    option(alias(Alias),Options,prolog_cogserver),
    option(description(Desc),Options,Alias),
    dmsg(Port=Desc),
    thread_create(cogserver_loop(ServerSocket, Options), _,
                  [ alias(Alias)
                  ]),!.

peer_to_host(Peer,Host):- catch(tcp_host_to_address(Host, Peer),_,fail),!.
peer_to_host(Peer,Host):- atom(Peer),Peer=Host,!.
peer_to_host(Peer,Host):- compound(Peer),catch((Peer=..PeerL,atomic_list_concat(PeerL,'.',Host)),_,fail),!.
peer_to_host(Peer,Host):- term_to_atom(Peer,Host),!.

set_stream_carelessly(X,Y):- carelessly1(set_stream(X,Y)).

cogserver_loop(ServerSocket, Options) :-
    tcp_accept(ServerSocket, PeerSock, Peer),
    tcp_open_socket(PeerSock, In, Out),
    set_stream_carelessly(In, close_on_abort(false)),
    set_stream_carelessly(Out, close_on_abort(false)),
    peer_to_host(Peer,Host),
    gensym(inst_,Num),
    option(alias(ServerAlias),Options,prolog_cogserver),
    atomic_list_concat(['peer_',Host,'_',Num, '@', ServerAlias], Alias),
    catch(thread_create(
              call_service_cogshell_peer(Host, Alias, PeerSock, In, Out, Peer, Options),
              _,
              [ alias(Alias),detached(true)
              ]),
          error(permission_error(create, thread, Alias), _),
          fail),
    !,
    cogserver_loop(ServerSocket, Options).


call_service_cogshell_peer(Host, Alias, PeerSock, In, Out, Peer, Options):-
  mmake,
  call(call,service_cogshell_peer(Host, Alias, PeerSock, In, Out, Peer, Options)).


service_cogshell_peer(Host,Alias,PeerSock,In,Out,Peer,Options) :-
    stream_property(Main_error, file_no(2)),
    option(allow(PeerAllow),Options,ip(127,0,0,1))-> PeerAllow=Peer,
    !,
    ((
    thread_self(Id),
    set_prolog_flag(tty_control, true),    
    set_stream(In, tty(true)),
    set_stream(Out, tty(true)),
    % TODO figure out how to get immedate results
    %set_stream_carelessly(In, buffer_size(1)),
    %set_stream_carelessly(In, buffered(true)),
    set_stream_carelessly(In, close_on_abort(false)),
    set_stream_carelessly(Out, close_on_abort(false)),
    set_stream_carelessly(In, close_on_exec(false)),
    set_stream_carelessly(Out, close_on_exec(false)),
    set_stream_carelessly(In, eof_action(eof_code)),
    set_stream_carelessly(In, write_errors(ignore)),
    set_stream_carelessly(Out, write_errors(ignore)), 
    set_stream_carelessly(In, alias(cog_input)),
    set_stream_carelessly(Out, alias(cog_output)),
    set_thread_error_stream(Id,Main_error),
    current_prolog_flag(encoding, Enc),
    set_stream_carelessly(In, encoding(Enc)),
    set_stream_carelessly(Out, encoding(Enc)),
    set_stream_carelessly(In, newline(detect)),
    set_stream_carelessly(Out, newline(dos)),
    call(retractall,thread_util:has_console(Id, _, _, _)),
    thread_at_exit(call(retractall,thread_util:has_console(Id, _, _, _))),
    call(asserta,thread_util:has_console(Id, In, Out, Out)),
    option(call(Call), Options, prolog),
    format(Main_error,'~N~n~q~n~n',[service_cogshell_peer_call(Call,Id,Alias,PeerSock,In,Out,Host,Peer,Options)]),
    format(Main_error,
           'LogicMOO CogServerShell (~q) on thread ~w~n~n',
           [Call,Id]),    
    call_cleanup(Call,
                 ( close(In),
                   close(Out),
                   thread_detach(Id))))).

service_cogshell_peer(Host,Alias,PeerSock,In,Out,Peer,Options):-
    thread_self(Id),option(call(Call), Options, prolog),
    format(main_error,'~N~n~q~n~n',[rejecting(Call,Id,Alias,PeerSock,In,Out,Host,Peer,Options)]),    
    format(Out, 'Bye!!~n', []),
    close(In),
    close(Out),
    thread_detach(Id).


make_peer_alias(Host,Alias):- thread_self(Prefix),make_peer_alias3(Prefix,Host,Alias).

make_peer_alias3(Prefix,Host,AliasH):- is_list(Host),must(atomic_list_concat([Prefix,'peer'| Host], '.', AliasH)),!.
make_peer_alias3(Prefix,Host,AliasH):- compound(Host),Host=..HostL,make_peer_alias3(Prefix,HostL,AliasH).
make_peer_alias3(Prefix,Host,AliasH):- term_to_atom(Host,AHost),must(atomic_list_concat([Prefix,'peer', AHost], '_', AliasH)).


call_close_and_detatch(In, Out, Id, Call):-
    call_cleanup(call(Call),( close_peer_connection(In, Out),ignore(thread_detach(Id)))).


close_peer_connection(In, Out) :-
        call(retractall,thread_util:has_console(_,In,Out,_)),
        ignore(catch(close(In, [force(true)]),_,true)),
        ignore(catch(close(Out, [force(true)]),_,true)).


port_busy_ocs(Port):-
    tcp_socket(ServerSocket),
    tcp_setopt(ServerSocket, reuseaddr),
    catch((tcp_bind(ServerSocket, Port),tcp_listen(ServerSocket, 5)),Error,true),    
    tcp_close_socket(ServerSocket),
    !,nonvar(Error).

:- dynamic(started_cogshell_telnet/1).
start_cogserver(Port):- started_cogshell_telnet(Port),!,threads.
start_cogserver(Port):- port_busy_ocs(Port),!, NewPort is Port+100, start_cogserver(NewPort).
start_cogserver(Port):-      
      asserta(started_cogshell_telnet(Port)),
      start_cogserver(run_cogshell , Port  , "CogServerShell").

run_cogshell:- current_predicate(oc_shell/0),!,call(oc_shell).
%run_cogshell:- current_predicate(guile/0),!,guile.
%run_cogshell:- current_predicate(lisp/0),!,lisp.
run_cogshell:- 
  my_ioe(In,Out,Main_error),
  set_prolog_IO(In, Out, Main_error), 
  set_stream_carelessly(Out,alias(cog_output)),
  writeln(';; LABS development version dropping you to prolog/0 shell'),
  prolog.

ocd(X):- stream_property(Main_error, file_no(2)), format(Main_error,"~N~q~n",[X]), flush_output(Main_error).

my_ioe(In,Out):- thread_self(Id), thread_util:has_console(Id,In,Out,_Err),!.
my_ioe(In,Out):- thread_util:has_console(_Id,In,Out,_Err),!.
my_ioe(In,Out):- stream_property(Out,alias(cog_output)),stream_property(In,cog_input),!.
my_ioe(In,Out):- current_input(In),current_output(Out).
my_ioe(In,Out,Main_error):- my_ioe(In,Out), stream_property(Main_error, file_no(2)),!.


carelessly1(X):- ignore(catch(X,E,ocd(X->E))).

fco:- catch(flush_output(user_error),_,true),catch(flush_output(cog_output),_,true).
debug_read_rest:-
  my_ioe(In,Out,Main_error),
  writeln(Out,''),
  repeat,
  get_code(In,Code),my_char_code(Char,Code),
  writeln(Main_error,my_char_code(Char,Code)),
  (Code == -1 -> true ; fail).

my_char_code(Char,Code):- catch(char_code(Char,Code),_,fail),!.
my_char_code(end_of_file,-1).

prompt_for(opencog,"opencog> ").
prompt_for(sexpr,"").
prompt_for(scm,"guile> ").
prompt_for(prolog,"?- ").
prompt_for(_,"").

oc_shell:- 
  mmake,
  set_prolog_flag(color_term,true),
  my_ioe(In,Out,Main_error),
  set_stream(In,tty(true)),
  set_stream(Out,alias(cog_output)),
  stream_property(Main_output,file_no(1)),
  set_prolog_IO(In, Main_output, Main_error),
  oc_shell(opencog),!.

w_no_p(G):- setup_call_cleanup(prompt(Old,''),G,prompt(_,Old)).

oc_shell(Type):- 
  repeat,
  my_ioe(In,Out),
  once(prompt_for(Type,Prompt)),
  ocd(prompt_for(Type,Prompt)),
  carelessly1(write(Out,Prompt)),fco,
  set_stream(In,tty(true)),
  cogshell_read_s(Type,In,X),ocd(read(X)),  
  (X==end_of_file -> (ocd("Exiting shell "=Type),nop(writeLN('Exiting the shell'))); (eval_and_print(Type,X), fail)).

white_code(32). white_code(10). white_code(13).
%cogshell_read_s(_Type,In,X):- at_end_of_stream(In),!,X=end_of_file.
cogshell_read_s(_Type,In,X):- peek_code(In,-1),get_code(In,_),!,X=end_of_file.
cogshell_read_s( Type,In,X):- peek_code(In,W),white_code(W),get_code(In,_),!,cogshell_read_s(Type,In,X).
cogshell_read_s(_Type,In,X):- peek_string(In,2, ".\r"),get_code(In,_),get_code(In,_),!,X=end_of_file.
cogshell_read_s(Type,In,Y):- % set_stream_carelessly(In,buffer_size(4096)),
   w_no_p(carelessly1(lisp_read(In,X))),translate_read(Type,X,Y),fco.

translate_read(_,'.',end_of_file).
translate_read(_,X,X).

sexpr:- oc_shell(sexpr).
scm:- oc_shell(scm).

eval_and_print(Type,X):- once(cog_eval(Type,X,Y)),ocd(eval(X->Y)), carelessly1(cog_print(Y)), fco.

cog_eval(Type,Const,Out):- atom(Const),atom_concat(New,'.',Const),!,cog_eval(Type,New,Out).
cog_eval(_Type,Const,''):- atom(Const),current_predicate(M:Const/0),!,call(M:Const).
cog_eval(_Type,'STRING'(S),S):-!.
cog_eval(_Type,Const,Const):- atom(Const),atom_concat('#',_,Const),!.
cog_eval(Type,X,''):- Type\==scm, 
  nop(writeLN('Entering scheme shell; use ^D or a single . on a line by itself to exit.')),
  eval_and_print(scm,X),scm.
cog_eval(_Type,X,Y):- is_special_eval(X,Y),!.
cog_eval(Type,X,Y):- 
   catch(f_eval(X,Y),E,(fail,ocd(eval(X,Y)=E),!,Y=[error,E,in,X])),
   ocd(eval_to(Type,Y)),!.
cog_eval(_,X,Y):- 
   atomese_to_s(X,Y),!,
   ocd(atomese_to_s(X,Y)),
   oc_assert(Y),!.
cog_eval(_Type,_,'#f').   

eval_arg(Var,Var):- \+ compound(Var).
eval_arg([quote,Q],Q):-!.
eval_arg([F|List],Out):- maplist(eval_arg,List,ListO), List\=@=ListO,!,eval_arg([F|ListO],Out).
eval_arg(['cog-new-node'|ARGS],Out):- eval_arg(ARGS,Out).
eval_arg(['cog-new-value'|ARGS],Out):- eval_arg(ARGS,Out).
eval_arg(['cog-new-stv'|Rest],[stv|Rest]).
eval_arg(['define',X,Y],''):- assert_value(X,Y).
eval_arg(X,Y):- oc_value(X,Y),!.
eval_arg(X,X).

oc_value(X,Y):- oc_as(X,sval,Y).
assert_value(X,Y):- retractall(oc_as(X,sval,_)),asserta(oc_as(X,sval,Y)).

is_special_eval('','').
is_special_eval(In,Out):- eval_arg(In,Mid),In\=@=Mid,eval_arg(Mid,Out).
is_special_eval(['use-modules'|_],'').
is_special_eval(['cog-delete',X],TF):- !, as_tf(oc_retract(X),TF).
is_special_eval(['cog-delete-recursive',X],TF):- !,as_tf(forall((oc_as_alist(A,B),contains_atom([A|B],X)),retract_alist(A,B)),TF).
is_special_eval(['cog-set-tv!',Atom,TVal],[]):- oc_assert(oc_as(Atom,TVal)).
is_special_eval(['cog-arity',X],A):- cog_arity(X,A).
is_special_eval(['cog-get-all-roots'|_],''):- 
   writeL('('),
   forall(oc_as_alist(A,B),cog_print([A|B])),
   writeLN(')').
is_special_eval(['cog-get-atoms',[quote,_],'#t'],''):- writeLN('( (List (Concept "a")(Concept "b")) )').
is_special_eval(['cog-keys->alist'|_],''):- writeL('(
  ((PredicateNode "flo") . (FloatValue 1 2 3) ) 
  ((PredicateNode "blo") . (FloatValue 4 5 6) ))').

as_tf(G,TF):- call(G)->TF='#t';TF='#f'.

cog_arity(X,A):- s_to_atomspace(X,L),length(L,A).


contains_atom([A|B],X):- sub_term(E,[A|B]),compound(E),E=X.
retract_alist(P,V):- retractall(oc_as(P,V)).
oc_as_alist(P,V):- oc_as(P,V).

:- dynamic(oc_as/2).
:- dynamic(oc_as/3).


oc_assert(oc_as(X,Y)):- !, assert_if_new(oc_as(X,Y)).
oc_assert(oc_as(X,Y,Z)):- !, assert_if_new(oc_as(X,Y,Z)).
oc_assert(X):- assert_if_new(oc_as(X,[])).

oc_retract(oc_as(X,Y)):- !, retract(oc_as(X,Y)).
oc_retract(oc_as(X,Y,Z)):- !, retract(oc_as(X,Y,Z)).
oc_retract(X):- retract(oc_as(X,_)).


to_cog_client(G):- with_output_to(cog_output,G).
writeL(X):- \+atomic(X),!,cog_print(X).
writeL(X):- carelessly1(write(cog_output,X)),!.
writeLN(X):- writeL(X),fco,carelessly1(format(cog_output,'~N',[])),fco.

cog_print(Var):- var(Var), !, cog_print(var(Var)).
cog_print('#t'):- writeLN('#t'),!.
cog_print('#f'):- writeLN('#f'),!.
cog_print(''):- nop(writeLN('')),!.
cog_print(Y):-
   catch(to_cog_client(f_print(Y,[],_)),E,(ocd(f_print(Y)=E),fail)),!.
cog_print(_):- writeLN('#f').

start_cogserver:-
   Port is 17001,
   start_cogserver(Port),!.

start_cogserver_main:- start_cogserver, prolog.

:- initialization(start_cogserver_main,main).
