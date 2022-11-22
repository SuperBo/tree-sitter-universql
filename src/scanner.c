#include <stdint.h>
#include <wctype.h>
#include <tree_sitter/parser.h>


#define RANDOM_SEED 207
const uint32_t RANDOM_ARR[16] = {
    30387, 19786, 20273, 354, 23675, 1658, 17224, 18692, 8214, 5491, 3406, 8035, 12790, 19386, 9273, 31541
};

#define NUM_KEYWORDS 97
enum TokenType {
    KW_IF = 0,
    KW_AS,
    KW_AT,
    KW_IN,
    KW_BY,
    KW_OF,
    KW_IS,
    KW_NO,
    KW_ON,
    KW_OR,
    KW_TO,
    KW_AND,
    KW_ASC,
    KW_END,
    KW_ALL,
    KW_CASE,
    KW_CAST,
    KW_CUBE,
    KW_LEFT,
    KW_DESC,
    KW_HASH,
    KW_LIKE,
    KW_ELSE,
    KW_ANY,
    KW_JOIN,
    KW_FOR,
    KW_FULL,
    KW_FROM,
    KW_ENUM,
    KW_THEN,
    KW_NEW,
    KW_INTO,
    KW_WHEN,
    KW_OVER,
    KW_SET,
    KW_NULL,
    KW_NOT,
    KW_SOME,
    KW_WITH,
    KW_ROWS,
    KW_TRUE,
    KW_FALSE,
    KW_DEFINE,
    KW_FETCH,
    KW_RANGE,
    KW_MERGE,
    KW_WHERE,
    KW_ESCAPE,
    KW_SELECT,
    KW_CREATE,
    KW_CROSS,
    KW_HAVING,
    KW_GROUP,
    KW_EXCEPT,
    KW_INNER,
    KW_LIMIT,
    KW_IGNORE,
    KW_ORDER,
    KW_RIGHT,
    KW_ARRAY,
    KW_USING,
    KW_ASSERT,
    KW_UNION,
    KW_GROUPS,
    KW_EXISTS,
    KW_PROTO,
    KW_NULLS,
    KW_TREAT,
    KW_PIVOT,
    KW_STRUCT,
    KW_LOOKUP,
    KW_WITHIN,
    KW_OUTER,
    KW_WINDOW,
    KW_ROLLUP,
    KW_UNNEST,
    KW_COLLATE,
    KW_BETWEEN,
    KW_DEFAULT,
    KW_EXCLUDE,
    KW_CURRENT,
    KW_EXTRACT,
    KW_LATERAL,
    KW_NATURAL,
    KW_RESPECT,
    KW_UNPIVOT,
    KW_CONTAINS,
    KW_DISTINCT,
    KW_GROUPING,
    KW_INTERVAL,
    KW_PRECEDING,
    KW_FOLLOWING,
    KW_PARTITION,
    KW_UNBOUNDED,
    KW_INTERSECT,
    KW_RECURSIVE,
    KW_TABLESAMPLE,
    IDENT
};
const uint32_t KEYWORD_HASH [NUM_KEYWORDS] = {
    5209014, // IF
    5223136, // AS
    5242922, // AT
    5367302, // IN
    5372239, // BY
    5391336, // OF
    5466232, // IS
    5539023, // NO
    5549624, // ON
    5628768, // OR
    5721345, // TO
    7151506, // AND
    7230163, // ASC
    7273054, // END
    7274118, // ALL
    7294911, // CASE
    7300221, // CAST
    7345990, // CUBE
    7389299, // LEFT
    7403734, // DESC
    7447908, // HASH
    7564498, // LIKE
    7573331, // ELSE
    7577239, // ANY
    7585080, // JOIN
    7607049, // FOR
    7642359, // FULL
    7644174, // FROM
    7656281, // ENUM
    7669356, // THEN
    7753650, // NEW
    7758264, // INTO
    7760517, // WHEN
    7795841, // OVER
    7844766, // SET
    7885455, // NULL
    7890691, // NOT
    7936469, // SOME
    8082274, // WITH
    8113768, // ROWS
    8188398, // TRUE
    9640292, // FALSE
    9914017, // DEFINE
    9946981, // FETCH
    10041234, // RANGE
    10049535, // MERGE
    10153108, // WHERE
    10205107, // ESCAPE
    10254489, // SELECT
    10259793, // CREATE
    10277762, // CROSS
    10284105, // HAVING
    10328993, // GROUP
    10330323, // EXCEPT
    10332036, // INNER
    10352760, // LIMIT
    10364532, // IGNORE
    10390772, // ORDER
    10413090, // RIGHT
    10413485, // ARRAY
    10437006, // USING
    10481563, // ASSERT
    10504155, // UNION
    10519663, // GROUPS
    10549959, // EXISTS
    10578447, // PROTO
    10608080, // NULLS
    10608914, // TREAT
    10658889, // PIVOT
    10678581, // STRUCT
    10722101, // LOOKUP
    10750529, // WITHIN
    10774498, // OUTER
    10784447, // WINDOW
    10843958, // ROLLUP
    10912683, // UNNEST
    11660909, // COLLATE
    11823586, // BETWEEN
    12086500, // DEFAULT
    12164272, // EXCLUDE
    12246499, // CURRENT
    12294239, // EXTRACT
    12308635, // LATERAL
    12375073, // NATURAL
    12387055, // RESPECT
    13015364, // UNPIVOT
    13990645, // CONTAINS
    14030599, // DISTINCT
    14322999, // GROUPING
    14338782, // INTERVAL
    15009431, // PRECEDING
    15059552, // FOLLOWING
    15140054, // PARTITION
    15142041, // UNBOUNDED
    15187300, // INTERSECT
    15244566, // RECURSIVE
    15616330,// TABLESAMPLE
    0
};

int32_t binary_search(uint32_t code, size_t start, size_t end) {
    while (start <= end) {
        if (start == end ) {
            if (KEYWORD_HASH[start] == code) {
                return start;
            } else {
                return -1;
            } 
        }

        if (end - start < 4) {
            for (size_t i = start; i <= end; i++) {
                if (KEYWORD_HASH[i] == code) {
                    return i;
                }
            }
            return -1;
        }

        size_t middle = (start + end) / 2;
        if (code == KEYWORD_HASH[middle]) {
            return middle;
        } else if (code < KEYWORD_HASH[middle]) {
            end = middle - 1;
        } else {
            start = middle + 1;
        }
    }
    return -1;
}

void * tree_sitter_UniverSQL_external_scanner_create() {
    return NULL;
}

void tree_sitter_UniverSQL_external_scanner_destroy(void *payload) {
    return;
}

unsigned tree_sitter_UniverSQL_external_scanner_serialize(void *payload, char *buffer) {
    return 0;
}

void tree_sitter_UniverSQL_external_scanner_deserialize(
    void *payload,
    const char *buffer,
    unsigned length
) {
    return;
}

bool tree_sitter_UniverSQL_external_scanner_scan(
    void *payload,
    TSLexer *lexer,
    const bool *valid_symbols
) {
    while (iswspace(lexer->lookahead)) {
        lexer->advance(lexer, true);
    }

    size_t i = 0;
    uint32_t hashcode = RANDOM_SEED;

    for (;;) {
        bool is_upper_letter = iswupper(lexer->lookahead);
        bool is_lower_letter = iswlower(lexer->lookahead);
        if (!is_upper_letter && !is_lower_letter) {
            break;
        }

        uint32_t c = (uint32_t)(is_upper_letter ? lexer->lookahead + 32 : lexer->lookahead);
        hashcode += RANDOM_ARR[i] * c;

        lexer->advance(lexer, false);
        i++;
        if (i > 15) {
            if (valid_symbols[IDENT]) {
                lexer->result_symbol = IDENT;
                return true;
            }
            return false;
        }
    }

    if (i == 0) {
        return false;
    }

    int32_t found = binary_search(hashcode, 0, NUM_KEYWORDS - 1);
    if (found > -1 && valid_symbols[found]) {
        lexer->result_symbol = found;
        return true;
    } else if (valid_symbols[IDENT]) {
        lexer->result_symbol = IDENT;
        return true;
    }
    return false;
}
