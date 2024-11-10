#include "tree_sitter/array.h"
#include "tree_sitter/parser.h"

#include <string.h>
#include <wctype.h>

enum TokenType {
  MULTILINE_COMMENT,
};


static inline void skip(TSLexer *lexer) { lexer->advance(lexer, true); }

static inline void advance(TSLexer *lexer) { lexer->advance(lexer, false); }

// Scanner functions

static bool scan_multiline_comment(TSLexer *lexer) {
  if (lexer->lookahead != '/') return false;
  advance(lexer);
  if (lexer->lookahead != '*') return false;
  advance(lexer);

  bool after_star = false;
  unsigned nesting_depth = 1;
  for (;;) {
    switch (lexer->lookahead) {
      case '*':
        advance(lexer);
        after_star = true;
        break;
      case '/':
        advance(lexer);
        if (after_star) {
          after_star = false;
          nesting_depth -= 1;
          if (nesting_depth == 0) {
            lexer->result_symbol = MULTILINE_COMMENT;
            lexer->mark_end(lexer);
            return true;
          }
        } else {
          after_star = false;
          if (lexer->lookahead == '*') {
            nesting_depth += 1;
            advance(lexer);
          }
        }
        break;
      case '\0':
        return false;
      default:
        advance(lexer);
        after_star = false;
        break;
    }
  }
}

bool tree_sitter_kotlin_external_scanner_scan(void *payload, TSLexer *lexer, const bool *valid_symbols) {
  if (valid_symbols[MULTILINE_COMMENT]) {
    return scan_multiline_comment(lexer);
  }

  return false;
}

void *tree_sitter_kotlin_external_scanner_create() {
  return NULL;
}

void tree_sitter_kotlin_external_scanner_destroy(void *payload) {
}

unsigned tree_sitter_kotlin_external_scanner_serialize(void *payload, char *buffer) {
  return 0;
}

void tree_sitter_kotlin_external_scanner_deserialize(void *payload, const char *buffer, unsigned length) {
}
