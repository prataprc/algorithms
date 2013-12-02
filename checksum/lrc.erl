%% #! /usr/bin/env escript

-module(lrc).
-export([ main/1, lrc/2, lrc1/2 ]).
-define(SIZE, 131072).

lrc( <<>>, Acc ) -> Acc;
lrc( <<Byte:8,Tail/binary>>, Acc ) -> lrc( Tail, (Acc bxor Byte) ).

lrc2( <<>>, Acc ) -> lrc( <<Acc:1024>>, 0 );
lrc2( <<Byte:1024,Tail/binary>>, Acc ) -> lrc2( Tail, (Acc bxor Byte) );
lrc2( Tail, Acc ) -> lrc( <<Acc:1024,Tail/binary>>, 0 ).

lrc1( <<>>, Acc ) -> lrc2( <<Acc:?SIZE/integer>>, 0 );
lrc1( <<Byte:?SIZE,Tail/binary>>, Acc ) -> lrc1( Tail, (Acc bxor Byte) );
lrc1( Tail, Acc ) -> lrc2( <<Acc:?SIZE,Tail/binary>>, 0 ).

main( [File] ) -> 
    {ok, TextBin} = file:read_file( File ),
    Lrc = lrc1( TextBin, 0 ),
    io:format( "Longest redundancy check for ~s is ~.16B ~n", [ File, Lrc ] ).
