#include "tree_sitter/array.h"
#include "tree_sitter/parser.h"

#include <wctype.h>

enum TokenType {
    ELVIS,
    EXTERNAL_NEWLINE,
    MULTILINE_COMMENT,
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

static bool line_comment_body(TSLexer *lexer) {
    while (lexer->lookahead != '\n') {
        advance(lexer);
    }
    return true;
}

static bool line_comment(TSLexer *lexer) {
    if (lexer->lookahead == '/') {
        advance(lexer);
        if (lexer->lookahead == '/') {
            advance(lexer);
            return line_comment_body(lexer);
        }
    }
    return false;
}

/*
 * After this function ends, the lexer is advanced at least 0 and at most length times.
 */
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

static bool multiline_comment_body(TSLexer *lexer) {
    while (!lexer->eof(lexer)) {
        switch (lexer->lookahead) {
            case '*':
                advance(lexer);
                if (lexer->lookahead == '/') {
                    advance(lexer);
                    return true;
                }
                break;
            case '/':
                advance(lexer);
                if (lexer->lookahead == '*') {
                    advance(lexer);
                    if (!multiline_comment_body(lexer)) {
                        return false;
                    } 
                }
                break;
            default:  
                advance(lexer);  
        }
    }
    return false;
}

static bool multiline_comment(TSLexer *lexer) {
    if (lexer->lookahead == '/') {
        advance(lexer);
        if (lexer->lookahead == '*') {
            advance(lexer);
            return multiline_comment_body(lexer);
        }
    }
    return false;
}

bool tree_sitter_kotlin_external_scanner_scan(void *payload, TSLexer *lexer, const bool *valid_symbols) {
    if (valid_symbols[MULTILINE_COMMENT]) {
        if (lexer->lookahead == '/') {
            if (multiline_comment(lexer)) {
                lexer->result_symbol = MULTILINE_COMMENT;
                return true;
            }
            return false;
        }
    }
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
                        advance(lexer);
                        switch (lexer->lookahead) {
                            case '/':
                                if (!line_comment_body(lexer)) {
                                    return false;
                                }
                                break;
                             case '*':
                                if (!multiline_comment_body(lexer)) {
                                    return false;
                                }
                                break;
                            default:
                                return false; 
                        }
                    }
                }
                if (lookahead_operator(lexer, "&&", 2)) {
                    return true;
                }
                if (lookahead_operator(lexer, "||", 2)) {
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