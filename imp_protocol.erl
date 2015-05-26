-module(imp_protocol).
-export([decode/1, encode/1, stream_decode/1]).

decode(<<"[", Msg/binary>>) ->
    case decode(Msg, [], [], false) of
        {impsig, Parts} ->
            {impsig, lists:reverse(Parts)};
        Other ->
            {error, Other}
    end;
decode(Msg) ->
    {error, {invalid_format, Msg}}.

decode(<<"]">>, PriorParts, Buffer, false) ->
    {impsig, [lists:reverse(Buffer) | PriorParts]};
decode(<<Char, Rest/binary>>, PriorParts, Buffer, false) ->
    case Char of
        $/ ->
            decode(Rest, [lists:reverse(Buffer) | PriorParts], [], false);
        $@ ->
            decode(Rest, PriorParts, Buffer, true);
        _ ->
            decode(Rest, PriorParts, [Char | Buffer], false)
    end;
decode(<<Char, Rest/binary>>, PriorParts, Buffer, true) ->
    case Char of
        $@ ->
            decode(Rest, PriorParts, [$@ | Buffer], false);
        $/ ->
            decode(Rest, PriorParts, [$/ | Buffer], false);
        $r ->
            decode(Rest, PriorParts, [$] | Buffer], false);
        $l ->
            decode(Rest, PriorParts, [$[ | Buffer], false);
        _ ->
            {error, {unexpected_escape, Char}}
    end;
decode(_, _, _, _) ->
    {error, nomatch}.

encode(Msg) ->
    list_to_binary([$[ | encode1(Msg)]).

encode1([]) ->
    "]";
encode1([""]) ->
    "]";
encode1(["" | Rest]) ->
    [$/ | encode1(Rest)];
encode1([[$@ | RestOfField] | Rest]) ->
    "@@" ++ encode1([RestOfField | Rest]);
encode1([[$/ | RestOfField] | Rest]) ->
    "@/" ++ encode1([RestOfField | Rest]);
encode1([[$[ | RestOfField] | Rest]) ->
    "@l" ++ encode1([RestOfField | Rest]);
encode1([[$] | RestOfField] | Rest]) ->
    "@r" ++ encode1([RestOfField | Rest]);
encode1([[Char | RestOfField] | Rest]) ->
    [Char | encode1([RestOfField | Rest])].

stream_decode(Receiver) ->
    stream_decode(Receiver, "", "", false).

stream_decode(Receiver, "", Acc, Escaped) ->
    receive
        {decode, Chunk} ->
            stream_decode(Receiver, binary_to_list(Chunk), Acc, Escaped);
        stop ->
            io:format("Stream decoder stops~n"),
            ok;
        Unexpected ->
            %% Maybe a log line here? @@SMK@@
            io:format("Stream decoder skips unexpected data ~p~n", [Unexpected]),
            stream_decode(Receiver, "", Acc, Escaped)
    end;
stream_decode(Receiver, [Char | Rest], Acc, false) ->
    case Char of
        $] ->
            Result = {decode_result, decode(list_to_binary(lists:reverse([$] | Acc])))},
            Receiver ! Result,
            stream_decode(Receiver, Rest, "", false);
        $@ ->
            stream_decode(Receiver, Rest, [$@ | Acc], true);
        _ ->
            stream_decode(Receiver, Rest, [Char | Acc], false)
    end;
stream_decode(Receiver, [Char | Rest], Acc, true) ->
    stream_decode(Receiver, Rest, [Char | Acc], false).
