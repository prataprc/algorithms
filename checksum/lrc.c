#include "stdio.h"
#include "stdlib.h"

/* How to use this program ? */
void usage(void)
{
    printf("lrc filename \n");
    printf("\t Compute lrc checksum for filename\n");
    return;
}

int main( int argc, char **argv )
{
    unsigned long i, len;
    unsigned char *s, lrc=0;
    FILE *fp = NULL;

    if( argc < 2 ) { usage(); exit(0); }

    fp = fopen( argv[1], "r" );
    fseek( fp, 0L, SEEK_END );
    len = ftell(fp);
    fseek( fp, 0L, SEEK_SET );
    s = malloc( len );
    fread( s, 1, len, fp );
    for( i = 0 ; i < len; i++ ) {
        lrc ^= s[i];
    }
    free( s );
    printf( "Longest redundancy check for %s is %x \n", argv[1], lrc );
}
