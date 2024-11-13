#include "tree_sitter/array.h"
#include "tree_sitter/parser.h"

#include <wctype.h>

enum TokenType {
    BIN_MIN,
    BIN_PLUS,
    SAFE_DOT,
    ELVIS,
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
        lexer->mark_end(lexer);
        skip_whitespace(lexer);
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
        lexer->mark_end(lexer);
        skip_whitespace(lexer);
        if (lexer->lookahead == '+') {
            advance(lexer);
            lexer->mark_end(lexer);
            // do not recognize ++, +=
            if (lexer -> lookahead != '+' && lexer -> lookahead != '=') {
                return true;
            }
        }
    }
    if (valid_symbols[SAFE_DOT] || valid_symbols[ELVIS]) {
        lexer->mark_end(lexer);
        skip_whitespace(lexer);
        if (lexer->lookahead == '?') {
            advance(lexer);
        } else {
            return false;
        }
        // Safe dot
        if (lexer->lookahead == '.') {
            advance(lexer);
            lexer->mark_end(lexer);
            lexer->result_symbol = SAFE_DOT;
            return true;
        }
        // Elvis
        if (lexer->lookahead == ':') {
            advance(lexer);
        } else {
            return false;
        }
        // ?:: should be tokenized as ? ::, so we should not emit an elvis operator here
        if (lexer -> lookahead != ':') {
            lexer->mark_end(lexer);
            lexer->result_symbol = ELVIS;
            return true;
        }
    }
    return false;
}