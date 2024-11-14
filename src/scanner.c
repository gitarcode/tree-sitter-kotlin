#include "tree_sitter/array.h"
#include "tree_sitter/parser.h"

#include <wctype.h>

enum TokenType {
    BIN_MIN,
    BIN_PLUS,
};

static inline void advance(TSLexer *lexer) { 
    // printf("advance '%c'\n", lexer->lookahead);
    lexer->advance(lexer, false); 
}

static inline void skip(TSLexer *lexer) { lexer->advance(lexer, true); }

static inline void skip_whitespace(TSLexer *lexer) {
    while (iswspace(lexer->lookahead)) {
        advance(lexer);
    }
}

void *tree_sitter_kotlin_external_scanner_create() { return NULL; }

void tree_sitter_kotlin_external_scanner_destroy(void *payload) {}

unsigned tree_sitter_kotlin_external_scanner_serialize(void *payload, char *buffer) { return 0; }

void tree_sitter_kotlin_external_scanner_deserialize(void *payload, const char *buffer, unsigned length) {}

bool tree_sitter_kotlin_external_scanner_scan(void *payload, TSLexer *lexer, const bool *valid_symbols) {
    if (valid_symbols[BIN_MIN]) {
        lexer->result_symbol = BIN_MIN;
        if (lexer->lookahead == '-') {
            advance(lexer);
            lexer->mark_end(lexer);
            // do not recognize --, ->, -=
            if (lexer -> lookahead != '-' && lexer -> lookahead != '>' && lexer -> lookahead != '=') {
                return true;
            }
        }
    }
    if (valid_symbols[BIN_PLUS]) {
        lexer->result_symbol = BIN_PLUS;
        if (lexer->lookahead == '+') {
            advance(lexer);
            lexer->mark_end(lexer);
            // do not recognize ++, +=
            if (lexer -> lookahead != '+' && lexer -> lookahead != '=') {
                return true;
            }
        }
    }
    return false;
}