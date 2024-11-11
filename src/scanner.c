#include "tree_sitter/array.h"
#include "tree_sitter/parser.h"

#include <wctype.h>

enum TokenType {
    BIN_MIN,
    BIN_PLUS
};

static inline void advance(TSLexer *lexer) { 
    lexer->advance(lexer, false); 
}

static inline void skip(TSLexer *lexer) { lexer->advance(lexer, true); }

static inline void skip_whitespace(TSLexer *lexer) {
    while (iswspace(lexer->lookahead)) {
        skip(lexer);
    }
}

void *tree_sitter_kotlin_external_scanner_create() { return NULL; }

void tree_sitter_kotlin_external_scanner_destroy(void *payload) {}

unsigned tree_sitter_kotlin_external_scanner_serialize(void *payload, char *buffer) { return 0; }

void tree_sitter_kotlin_external_scanner_deserialize(void *payload, const char *buffer, unsigned length) {}

bool tree_sitter_kotlin_external_scanner_scan(void *payload, TSLexer *lexer, const bool *valid_symbols) {
    if (valid_symbols[BIN_MIN] || valid_symbols[BIN_PLUS]) {
        skip_whitespace(lexer);
        if (lexer->lookahead == '-') {
            advance(lexer);
            // do not recognize --, ->, -=
            if (lexer -> lookahead != '-' 
                && lexer -> lookahead != '>' 
                && lexer -> lookahead != '=') {
                return true;
            }
        }
        else if (lexer->lookahead == '+') {
            advance(lexer);
            // do not recognize ++, +=
            if (lexer -> lookahead != '+' && lexer -> lookahead != '=') {
                return true;
            }
        }
        return false;
    }
    return false;
}