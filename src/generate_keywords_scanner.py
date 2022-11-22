from collections import OrderedDict
import numpy

RANDOM_SEED = 207
RANDOM_ARRAY = numpy.array([
    30387, 19786, 20273, 354,  23675,  1658, 17224, 18692,
    8214,   5491,  3406, 8035, 12790, 19386,  9273, 31541
], dtype=numpy.uint32)


def hahscode(s, random_seed: int = RANDOM_SEED):
    value = numpy.uint32(random_seed)
    for c, r in zip(s.lower(), RANDOM_ARRAY):
        value += numpy.uint32(ord((c))) * r
    return value


KEYWORDS = [
    "ALL",
    "AND",
    "ANY",
    "ARRAY",
    "AS",
    "ASC",
    "ASSERT",
    "AT",
    "BETWEEN",
    "BY",
    "CASE",
    "CAST",
    "COLLATE",
    "CREATE",
    "CROSS",
    "CURRENT",
    "DEFAULT",
    "DEFINE",
    "DESC",
    "DISTINCT",
    "ELSE",
    "END",
    "ENUM",
    "EXCEPT",
    "EXISTS",
    "EXTRACT",
    "FALSE",
    "FOLLOWING",
    "FROM",
    "FULL",
    "GROUP",
    "GROUPING",
    "HASH",
    "HAVING",
    "IF",
    "IGNORE",
    "IN",
    "INNER",
    "INTERSECT",
    "INTERVAL",
    "INTO",
    "IS",
    "JOIN",
    "LEFT",
    "LIKE",
    "LIMIT",
    "LOOKUP",
    "MERGE",
    "NATURAL",
    "NEW",
    "NO",
    "NOT",
    "NULL",
    "NULLS",
    "ON",
    "OR",
    "ORDER",
    "OUTER",
    "OVER",
    "PARTITION",
    "PRECEDING",
    "PROTO",
    "RANGE",
    "RECURSIVE",
    "RESPECT",
    "RIGHT",
    "ROLLUP",
    "ROWS",
    "SELECT",
    "SET",
    "STRUCT",
    "TABLESAMPLE",
    "THEN",
    "TO",
    "TRUE",
    "UNBOUNDED",
    "UNION",
    "USING",
    "WHEN",
    "WHERE",
    "WINDOW",
    "WITH",
    "UNNEST",
    "CONTAINS",
    "CUBE",
    "ESCAPE",
    "EXCLUDE",
    "FETCH",
    "FOR",
    "GROUPS",
    "LATERAL",
    "OF",
    "SOME",
    "TREAT",
    "WITHIN",
    "PIVOT",
    "UNPIVOT"
]


def generate_randoms():
    print("#define RANDOM_SEED {}".format(RANDOM_SEED))
    print("const uint32_t RANDOM_ARR[16] = {")
    print("   ", ", ".join(str(i) for i in RANDOM_ARRAY))
    print("};")


def generate_token_type(ht: dict):
    keys = list(ht.keys())
    print("enum TokenType {")
    print("    KW_{} = 0,".format(keys[0]))
    print(",\n".join("    KW_{}".format(w) for w in keys[1:]))
    print("};")


def generate_keyword_hash(ht: dict):
    word_hash = list(ht.items())
    print("const uint32_t KEYWORD_HASH [NUM_KEYWORDS] = {{")
    print("\n".join("    {}, // {}".format(h, w) for w, h in word_hash[:-1]))
    print("    {} // {}".format(word_hash[-1][1], word_hash[-1][0]))
    print("};")


def generate_externals(ht: dict):
    print("externals: $ => [")
    print(",\n".join("  $.kw_{}".format(w.lower()) for w in ht.keys()))
    print("],")


def main():
    hashtable = [(w, hahscode(w)) for w in KEYWORDS]
    hashtable.sort(key=lambda e: e[1]) # Sort by hashcode
    hashtable = OrderedDict(hashtable)

    generate_randoms()
    print("#define NUM_KEYWORDS {}".format(len(KEYWORDS)))
    generate_token_type(hashtable)
    generate_keyword_hash(hashtable)
    generate_externals(hashtable)


if __name__ == "__main__":
    main()
