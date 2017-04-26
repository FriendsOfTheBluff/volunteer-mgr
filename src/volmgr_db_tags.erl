-module(volmgr_db_tags).

-export([create/1,
         retrieve/0, retrieve/1,
         ensure/1, ensure_transactional/1
        ]).

-include_lib("stdlib/include/qlc.hrl").

-include("types.hrl").
-include("db.hrl").
-include("entities.hrl").

-spec create(T :: list(tag()) | tag()) -> ok | {error, any()} | no_return().
create(T) when is_list(T) ->
    handle_validate(validate(T), T);
create(T) ->
    create([T]).

handle_validate({error, invalid_tag}, _Tags) ->
    {error, invalid_tag};
handle_validate(ok, Tags) ->
    Records = [#volmgr_tags{id=Tag, active=true} || Tag <- Tags],
    Writer = fun W([]) ->
                 ok;
             W([Record|T]) ->
                 mnesia:write(Record),
                 W(T)
             end,
    mnesia:activity(transaction, Writer, [Records], mnesia).

-spec validate(Tags :: list(tag())) -> ok | {error, invalid_tag}.
validate(Tags) ->
    Bad = [<<"ok">>, <<"error">>, <<"notfound">>, <<"undefined">>],
    % http://erlang.org/doc/reference_manual/introduction.html
    Res = [<<"after">>, <<"and">>, <<"andalso">>,
           <<"band">>, <<"begin">>, <<"bnot">>, <<"bor">>, <<"bsl">>, <<"bsr">>, <<"bxor">>,
           <<"case">>, <<"catch">>, <<"cond">>,
           <<"div">>, <<"end">>, <<"fun">>, <<"if">>, <<"let">>, <<"not">>,
           <<"of">>, <<"or">>, <<"orelse">>,
           <<"receive">>, <<"rem">>, <<"try">>, <<"when">>, <<"xor">>],
    Pred = fun(T) ->
               lists:member(T, Bad) orelse lists:member(T, Res)
           end,
    case lists:any(Pred, Tags) of
        true -> {error, invalid_tag};
        false -> ok
    end.

-spec retrieve() -> list(tag()) | list().
retrieve() ->
    %% TODO FIXME only active tags?
    I = fun(#volmgr_tags{id=T, active=A}, Acc)->
	        [{T, A}|Acc]
	    end,
	F = fun() ->
	        mnesia:foldl(I, [], volmgr_tags)
	    end,
	mnesia:activity(transaction, F).

-spec retrieve(Tag :: tag()) -> tag() | {error, notfound}.
retrieve(Tag) ->
    F = fun() ->
            case mnesia:read({volmgr_tags, Tag}) of
                [#volmgr_tags{id=Tag, active=A}] ->
                    {Tag, A};
                [] ->
                    {error, notfound}
            end
        end,
    mnesia:activity(transaction, F).

-spec ensure(list(tag())) -> ok | {error, notfound} | {error, {any(), any()}}.
ensure(Tags) ->
    try
        ensure_transactional(Tags)
    catch
        exit:{aborted, notfound} ->
            {error, notfound};
        ExType:ExReason ->
            {error, {ExType, ExReason}}
    end.

-spec ensure_transactional(list(tag())) -> ok | no_return().
ensure_transactional(Tags) ->
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
