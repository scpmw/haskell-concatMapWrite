#include <err.h>
#include <stdio.h>

/*
 * Like c_escape_copy_text, but escape the datum so it will be suitable for
 * PostgreSQL's BYTEA input function.  Note that this does not use the hex
 * format introduced by PostgreSQL 9.0, as it is readable only by
 * PostgreSQL 9.0 and up.
 *
 * This performs two escape operations:
 *
 *  * Convert raw binary data to the format accepted by PostgreSQL's BYTEA
 *    input function.
 *
 *  * Escape the result for use in COPY FROM data.
 *
 * The buffer pointed to by @out should be at least 5*in_size bytes long.
 */
unsigned char *c_escape_copy_bytea(const unsigned char *in, int in_size, unsigned char *out)
{
    while (in_size-- > 0) {
        unsigned char c = *in++;

        if (c == '\\') {
            /* Escape backslash twice, once for BYTEA, and again for COPY FROM. */
            *out++ = '\\';
            *out++ = '\\';
            *out++ = '\\';
            *out++ = '\\';
        } else if (c >= 32 && c <= 126) {
            /*
             * Printable characters (except backslash) are subject to neither
             * BYTEA escaping nor COPY FROM escaping.
             */
            *out++ = c;
        } else {
            /*
             * Escape using octal format.  This consists of two backslashes
             * (single backslash, escaped for COPY FROM) followed by three
             * digits [0-7].
             *
             * We can't use letter escapes \t, \n, \r because:
             *
             *  * The BYTEA input function doesn't understand letter escapes.
             *
             *  * We could use only one backslash so BYTEA sees the literal
             *    octet values of 9, 10, and 13.  However, we're escaping other
             *    non-printable characters for BYTEA; why give 9, 10, and 13
             *    special treatment?
             */
            *out++ = '\\';
            *out++ = '\\';
            *out++ = '0' + ((c >> 6) & 0x7);
            *out++ = '0' + ((c >> 3) & 0x7);
            *out++ = '0' + (c & 0x7);
        }
    }

    return out;
}

int main(void)
{
    char in[512];
    char out[sizeof(in) * 5];
    size_t readlen;
    size_t writelen;
    size_t wrotelen;

    for (;;) {
        readlen = fread(in, 1, sizeof(in), stdin);
        if (readlen == 0) {
            if (ferror(stdin))
                err(1, "stdin");
            break;
        }

        writelen = (char*)c_escape_copy_bytea((const unsigned char *) in, readlen, (unsigned char *) out) - out;

        wrotelen = fwrite(out, 1, writelen, stdout);
        if (wrotelen != writelen)
            err(1, "stdout");
    }

    return 0;
}
