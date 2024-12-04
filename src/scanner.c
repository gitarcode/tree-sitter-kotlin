#include "tree_sitter/array.h"
#include "tree_sitter/parser.h"

#include <wctype.h>

enum TokenType {
    ELVIS,
    EXTERNAL_NEWLINE,
};

static inline void advance(TSLexer *lexer) { 
    // printf("advance '%c'\n", lexer->lookahead);
    lexer->advance(lexer, false); 
}

static inline void skip_whitespace(TSLexer *lexer) {
    while (iswspace(lexer->lookahead)) {
        advance(lexer);
    }
}

void *tree_sitter_kotlin_external_scanner_create() { return NULL; }

void tree_sitter_kotlin_external_scanner_destroy(void *payload) {}

unsigned tree_sitter_kotlin_external_scanner_serialize(void *payload, char *buffer) { return 0; }

void tree_sitter_kotlin_external_scanner_deserialize(void *payload, const char *buffer, unsigned length) {}

static bool line_comment(TSLexer *lexer) {
    if (lexer->lookahead == '/') {
        advance(lexer);
        if (lexer->lookahead == '/') {
            advance(lexer);
            while (lexer->lookahead != '\n') {
                advance(lexer);
            }
            return true;
        }
    }
    return false;
}

static bool lookahead_operator(TSLexer *lexer, const char *operator, int length) {
    for (int i = 0; i < length; i++) {
        if (lexer->lookahead != operator[i]) {
            return false;
        }
        advance(lexer);
        if (lexer->eof(lexer)) {
            return false;
        }
    }
    return true;
}

bool tree_sitter_kotlin_external_scanner_scan(void *payload, TSLexer *lexer, const bool *valid_symbols) {
    if (valid_symbols[EXTERNAL_NEWLINE] || valid_symbols[ELVIS]) {
        switch(lexer->lookahead) {
            case '\n':
                while (lexer->lookahead == '\n') {
                    advance(lexer);
                }
                lexer->result_symbol = EXTERNAL_NEWLINE;
                lexer->mark_end(lexer);
                while (iswspace(lexer->lookahead) || lexer->lookahead == '/') {
                    skip_whitespace(lexer);
                    if (lexer->lookahead == '/') {
                        if (!line_comment(lexer)) {
                            return false;
                        }
                    }
                }
                if (lookahead_operator(lexer, "&&", 2)) {
                    return true;
                }
                if (lookahead_operator(lexer, "+", 1)) {
                    if (lexer->lookahead != '+') {
                        return true;
                    }
                }
                if (lookahead_operator(lexer, "-", 1)) {
                    if (lexer->lookahead != '-') {
                        return true;
                    }
                }
                if (lookahead_operator(lexer, "?:", 2)) {
                    return true;
                }
                if (lookahead_operator(lexer, "?.", 2)) {
                    return true;
                }
                if (lookahead_operator(lexer, ".", 1)) {
                    return true;
                }
                return false;

            case '?':
                advance(lexer);
                if (lexer->lookahead != ':') {
                    return false;
                }
                advance(lexer);
                // ?:: should be tokenized as ? ::, so we should not emit an elvis operator here
                if (lexer->lookahead == ':') {
                    return false;
                }
                lexer->result_symbol = ELVIS;
                return true;

            default:
                return false;    
        }
    }
    return false;
}