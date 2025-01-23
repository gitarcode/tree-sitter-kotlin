#include "tree_sitter/array.h"
#include "tree_sitter/parser.h"

#include <wctype.h>

enum TokenType {
    ELVIS,
    EXTERNAL_NEWLINE,
    MULTILINE_COMMENT,
    LPAR,
    RPAR,
    LCURL,
    RCURL,
    LSQR,
    RSQR,
    DOLLAR_CURL, // ${
    NOT_IS, // !is
};

typedef struct {
    Array(char) parenthesis;
} Scanner;

void *tree_sitter_kotlin_external_scanner_create() { 
    Scanner *scanner = ts_calloc(1, sizeof(Scanner));
    array_init(&scanner->parenthesis);
    return scanner;
 }

void tree_sitter_kotlin_external_scanner_destroy(void *payload) {
    Scanner *scanner = (Scanner *)payload;
    array_delete(&scanner->parenthesis);
    ts_free(scanner);
}

unsigned tree_sitter_kotlin_external_scanner_serialize(void *payload, char *buffer) { 
  Scanner *scanner = (Scanner *)payload;
  int size = scanner->parenthesis.size;
  if (size > 0) {
    memcpy(buffer, scanner->parenthesis.contents, size);
  }
  return size;
 }

void tree_sitter_kotlin_external_scanner_deserialize(void *payload, const char *buffer, unsigned length) {
  Scanner *scanner = (Scanner *)payload;
  if (length > 0) {
    array_reserve(&scanner->parenthesis, length);
    memcpy(scanner->parenthesis.contents, buffer, length);
    scanner->parenthesis.size = length;
  } else {
    array_clear(&scanner->parenthesis);
  }
}

static void push(Scanner *scanner, const char c) {
    array_push(&scanner->parenthesis, c);
}

static bool pop(TSLexer *lexer, Scanner *scanner, const char c) {
    if (scanner->parenthesis.size == 0) {
        lexer->log(lexer, "empty parentheses stack\n");
        return false;
    }
    char popped_char = array_pop(&scanner->parenthesis);
    if (popped_char != c) {
        lexer->log(lexer, "Unexpected top of stack\n");
        return false;
    }
    return true;
}

static bool is_inside_parentheses(Scanner *scanner) {
    return scanner->parenthesis.size > 0 && *array_back(&scanner->parenthesis) != '{';
}

static inline void advance(TSLexer *lexer) { 
    lexer->advance(lexer, false); 
}

static inline void advance_and_skip(TSLexer *lexer) { 
    lexer->advance(lexer, true); 
}

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

static inline bool is_lookahead_valid_identifier_char(TSLexer *lexer) {
    return lexer->lookahead == '_' || iswalnum(lexer->lookahead);
}

bool tree_sitter_kotlin_external_scanner_scan(void *payload, TSLexer *lexer, const bool *valid_symbols) {
    Scanner *scanner = (Scanner *)payload;
    while (iswspace(lexer->lookahead) && lexer->lookahead != '\n') {
        advance_and_skip(lexer);
    }
    if (valid_symbols[MULTILINE_COMMENT]) {
        if (lexer->lookahead == '/') {
            if (multiline_comment(lexer)) {
                lexer->result_symbol = MULTILINE_COMMENT;
                return true;
            }
            return false;
        }
    }
    if (valid_symbols[LPAR]) {
        if (lexer->lookahead == '(') {
            push(scanner, '(');
            lexer->result_symbol = LPAR;
            advance(lexer);
            return true;
        }
    }
    if (valid_symbols[RPAR]) {
        if (scanner->parenthesis.size > 0) {
            if (lexer->lookahead == ')') {
                if (pop(lexer, scanner, '(')) {
                    lexer->result_symbol = RPAR;
                    advance(lexer);
                    return true;
                }
            }
        }
    }
    if (valid_symbols[LCURL]) {
        if (lexer->lookahead == '{') {
            push(scanner, '{');
            lexer->result_symbol = LCURL;
            advance(lexer);
            return true;
        }
    }
    if (valid_symbols[DOLLAR_CURL]) {
        if (lexer->lookahead == '$') {
            advance(lexer);
            if (lexer->lookahead == '{') {
                push(scanner, '{');
                lexer->result_symbol = DOLLAR_CURL;
                advance(lexer);
                return true;
            }
        }
    }
    if (valid_symbols[RCURL]) {
        if (scanner->parenthesis.size > 0) {
            if (lexer->lookahead == '}') {
                if (pop(lexer, scanner, '{')) {
                    lexer->result_symbol = RCURL;
                    advance(lexer);
                    return true;
                }
            }
        }
    }
    if (valid_symbols[LSQR]) {
        if (lexer->lookahead == '[') {
            push(scanner, '[');
            lexer->result_symbol = LSQR;
            advance(lexer);
            return true;
        }
    }
    if (valid_symbols[RSQR]) {
        if (lexer->lookahead == ']') {
            if (pop(lexer, scanner, '[')) {
                lexer->result_symbol = RSQR;
                advance(lexer);
                return true;
            }
        }
    }
    // elvis operator: `?:`
    if (valid_symbols[ELVIS]) {
        if (lexer->lookahead == '?') {
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
        }
    }
    if (valid_symbols[NOT_IS]) {
        // We should only emit an "!is" token if it's not followed by an alpha character.
        // !isa should be parsed as a unary expression: `! (isa)`
        if (lexer->lookahead == '!') {
            advance(lexer);
            if (lexer->lookahead != 'i') {
                return false;
            }
            advance(lexer);
            if (lexer->lookahead != 's') {
                return false;
            }
            advance(lexer);
            if (is_lookahead_valid_identifier_char(lexer)) {
                return false;
            }
            lexer->result_symbol = NOT_IS;
            return true;
        }
    }
    if (valid_symbols[EXTERNAL_NEWLINE]) {
        if (lexer->lookahead != '\n') {
            return false;
        }
        while (lexer->lookahead == '\n') {
            advance(lexer);
        }
        lexer->result_symbol = EXTERNAL_NEWLINE;
        lexer->mark_end(lexer);
        while (iswspace(lexer->lookahead) || lexer->lookahead == '/') {
            while (iswspace(lexer->lookahead)) {
                advance(lexer);
            }
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
                        if (is_inside_parentheses(scanner)) {
                                return true;
                        }
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
            if (is_inside_parentheses(scanner)) {
                // Do not match '++' and '+='
                if (lexer->lookahead != '+' && lexer->lookahead != '=') {
                    return true;
                }
            }
        }
        if (lookahead_operator(lexer, "-", 1)) {
            if (is_inside_parentheses(scanner)) {
                // Do not match '--', '->', and '-='
                if (lexer->lookahead != '-' && lexer->lookahead != '>' && lexer->lookahead != '=') {
                    return true;
                }
            }
        }
        if (lookahead_operator(lexer, "*", 1)) {
            if (is_inside_parentheses(scanner)) {
                // Do not match '*='
                if (lexer->lookahead != '=') {
                    return true;
                }
            }
        }
        if (lookahead_operator(lexer, "?", 1)) {
            if (lexer->lookahead == ':' || lexer->lookahead == '.') {
                return true;
            }
        }
        if (lookahead_operator(lexer, ".", 1)) {
            // Do not match '..'
            if (lexer->lookahead != '.') {
                return true;
            } else {
                // Range operators: '..' '..<'
                if (is_inside_parentheses(scanner)) {
                    return true;
                }
            }
        }
        if (lookahead_operator(lexer, "!", 1)) {
            if (is_inside_parentheses(scanner)) {
                switch (lexer->lookahead) {
                    case 'i':
                        advance(lexer);
                        // in or is
                        if (lexer->lookahead == 'n' || lexer->lookahead == 's') {
                            return true;
                        }
                    // !=
                    case '=':
                        return true;
                }
            }
        }
        if (lookahead_operator(lexer, "in", 2)) {
            if (!is_lookahead_valid_identifier_char(lexer)) {
                if (is_inside_parentheses(scanner)) {
                    return true;
                }
            }
        }
        if (lookahead_operator(lexer, "as", 2)) {
            if (!is_lookahead_valid_identifier_char(lexer)) {
                return true;
            }
        }
        if (lookahead_operator(lexer, "<", 1)) {
            if (is_inside_parentheses(scanner)) {
                return true;
            }
        }
        if (lookahead_operator(lexer, ">", 1)) {
            if (is_inside_parentheses(scanner)) {
                return true;
            }
        }
        if (lookahead_operator(lexer, "=", 1)) {
            if (lexer->lookahead == '=') {
                if (is_inside_parentheses(scanner)) {
                    return true;
                }                        
            }
        }
        // infix operators, if the next character is either `_` or an alphanumeric, corresponding to
        // the beginning of an identifier
        if (is_lookahead_valid_identifier_char(lexer)) {
            if (is_inside_parentheses(scanner)) {
                return true;
            }
        }
    }
    return false;
}
