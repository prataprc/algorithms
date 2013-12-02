-module(rle).
-export([ encode/2, decode/2, main/1 ]).

-include_lib("eunit/include/eunit.hrl").

encode( <<>>, X, C, Fd ) ->
    file:write( Fd, <<C:8,X:8>> );
encode( <<X:8,Tail/binary>>, X, C, Fd ) when C >= 255 ->
    file:write( Fd, <<C:8,X:8>> ),
    encode( Tail, X, 1, Fd );
encode( <<X:8,Tail/binary>>, X, C, Fd ) ->
    encode( Tail, X, C+1, Fd );
encode( <<Y:8,Tail/binary>>, X, C, Fd ) ->
    file:write( Fd, <<C:8,X:8>> ),
    encode( Tail, Y, 1, Fd ).

encode( <<X:8,Tail/binary>>, Fd ) ->
    encode( Tail, X, 1, Fd ).

decode( <<>>, Fd ) -> 
    ok;
decode( <<C:8,X:8,Tail/binary>>, Fd ) ->
    file:write( Fd, << <<X>> || _ <- lists:seq(1,C) >> ),
    decode( Tail, Fd ).


main( [Cmd, FileIn, FileOut] ) -> 
    {ok, TextBin} = file:read_file( FileIn ),
    {ok, Fd} = file:open( FileOut, write ),
    Out = case Cmd of
            "e" -> encode( TextBin, Fd );
            "d" -> decode( TextBin, Fd )
          end,
    file:write( Fd, Out ).

