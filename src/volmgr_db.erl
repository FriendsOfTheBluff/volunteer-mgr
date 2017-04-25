-module(volmgr_db).

-export([start/0,
         create_tag/1,
         create_tags/1,
         retrieve_tag/1,
         retrieve_tags/0,
         ensure_tags/1,
         do_ensure_tags/1
        ]).

-include_lib("stdlib/include/qlc.hrl").
-include("types.hrl").
-include("db.hrl").
-include("entities.hrl").

-spec start() ->
    ok | {timeout, list(term())} | {error, any()}.
start() ->
    mnesia:wait_for_tables(tables(), 5000).

-spec tables() -> list(atom()).
tables() ->
    [volmgr_people, volmgr_people_tags, volmgr_tags].

-spec create_tag(Tag :: tag()) -> ok | {error, any()} | no_return().
create_tag(Tag) ->
    create_tags([Tag]).

-spec create_tags(Tags :: list(tag())) -> ok | {aborted, any()}.
create_tags(Tags) ->
    handle_validate_tags(validate_tags(Tags), Tags).

handle_validate_tags({error, invalid_tag}, _Tags) ->
    {error, invalid_tag};
handle_validate_tags(ok, Tags) ->
    Records = [#volmgr_tags{id=Tag, active=true} || Tag <- Tags],
    Writer = fun W([]) ->
                 ok;
             W([Record|T]) ->
                 mnesia:write(Record),
                 W(T)
             end,
    mnesia:activity(transaction, Writer, [Records], mnesia).

-spec validate_tags(Tags :: list(tag())) -> ok | {error, invalid_tag}.
validate_tags(Tags) ->
    Bad = ['ok', 'error', 'notfound', 'undefined'],
    Pred = fun(T) ->
               lists:member(T, Bad) orelse erl_scan:reserved_word(T)
           end,
    case lists:any(Pred, Tags) of
        true -> {error, invalid_tag};
        false -> ok
    end.

-spec retrieve_tag(Tag :: tag()) -> tag() | {error, notfound}.
retrieve_tag(Tag) ->
    F = fun() ->
            case mnesia:read({volmgr_tags, Tag}) of
                [#volmgr_tags{id=Tag, active=A}] ->
                    {Tag, A};
                [] ->
                    {error, notfound}
            end
        end,
    mnesia:activity(transaction, F).

-spec retrieve_tags() -> list(tag()) | list().
retrieve_tags() ->
    I = fun(#volmgr_tags{id=T, active=A}, Acc)->
	        [{T, A}|Acc]
	    end,
	F = fun() ->
	        mnesia:foldl(I, [], volmgr_tags)
	    end,
	mnesia:activity(transaction, F).

-spec ensure_tags(list(tag())) -> ok | {error, notfound} | {error, {any(), any()}}.
ensure_tags(Tags) ->
    try
        do_ensure_tags(Tags)
    catch
        exit:{aborted, notfound} ->
            {error, notfound};
        ExType:ExReason ->
            {error, {ExType, ExReason}}
    end.

-spec do_ensure_tags(list(tag())) -> ok | no_return().
do_ensure_tags(Tags) ->
    % NOTE: this is a function that does not catch any
    % mnesia exit exceptions, so it can be used within a transaction
    % OR by a function that does catch the exit exception
    Reader = fun R([]) ->
                 ok;
             R([Tag|T]) ->
                 case mnesia:read({volmgr_tags, Tag}) of
                     [#volmgr_tags{id=Tag, active=true}] -> R(T);
                     [] -> mnesia:abort(notfound)
                 end
             end,
	mnesia:activity(transaction, Reader, [Tags], mnesia).
