#include <stdio.h>
#include <stdlib.h>

/* How to use this program ? */
void usage( void ) {

    printf("lrc filename \n");
    printf("\t Compute lrc checksum for filename\n");
    return;
}

int main( int argc, char **argv ) {

    unsigned long i, j, len;
    unsigned int  ch, c, code;
    unsigned char block[260], *s;
    FILE *fp = NULL;

    if( argc < 3 ) { usage(); exit(0); }

    fp = fopen( argv[2], "r" );
    fseek( fp, 0L, SEEK_END );
    len = ftell(fp);
    fseek( fp, 0L, SEEK_SET );
    s = malloc( len );
    fread( s, 1, len, fp );

    if( argv[1][0] == 'e' ) {
        c = 0; ch = s[0];
        for( i=1; i < len; i++ ) {
            c += 1;
            if( ch != s[i] || c >= 255 ) {
                code = (ch << 8) | (c & 0xFF);
                fwrite( &code, 1, 2, stdout );
                c = 0; ch = s[i];
            }
        }
        if( c > 0 ) {
            code = (ch << 8) & (c & 0xFF);
            fwrite( &code, 1, 2, stdout );
        }
    } else {
        for( i = 0; i < len; i+=2 ) {
            c = s[i]; ch=s[i+1];
            for( j=0 ; j < c ; j++ ) { block[j] = ch; }
            block[j] = NULL;
            fprintf( stdout, "%s", block );
        }
    }
    free(s);
}
