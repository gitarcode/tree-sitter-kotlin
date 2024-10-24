/*
 * MIT License
 *
 * Copyright (c) 2019 fwcd
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

// Using an adapted version of https://kotlinlang.org/docs/reference/grammar.html

const PREC = {
  INDEX: 18,
  CALL: 17,
  DOT: 16,
  POSTFIX: 16,
  PREFIX: 15,
  TYPE_RHS: 14,
  AS: 13,
  MULTIPLICATIVE: 12,
  ADDITIVE: 11,
  RANGE: 10,
  INFIX: 9,
  ELVIS: 8,
  CHECK: 7,
  COMPARISON: 6,
  EQUALITY: 5,
  CONJUNCTION: 4,
  DISJUNCTION: 3,
  VAR_DECL: 3,
  SPREAD: 2,
  SIMPLE_USER_TYPE: 2,
  ASSIGNMENT: 1,
  BLOCK: 1,
  ARGUMENTS: 1,
  STRING_CONTENT: 1,
  LAMBDA_LITERAL: 0,
  RETURN_OR_THROW: 0,
  COMMENT: 0
};
const DEC_DIGITS = token(sep1(/[0-9]+/, /_+/));
const HEX_DIGITS = token(sep1(/[0-9a-fA-F]+/, /_+/));
const BIN_DIGITS = token(sep1(/[01]/, /_+/));
const REAL_EXPONENT = token(seq(/[eE]/, optional(/[+-]/), DEC_DIGITS));

const uni_character_literal = token(seq(
  "\\u",
  /[0-9a-fA-F]{4}/
));

const escaped_identifier = token(/\\[tbrn'"\\$]/);

// Here, we should only match the '$' character if it's not followed by an alpha character
// If it is, it should be matched as part of the _interpolation rule.
const DOLLAR_IN_STRING_CONTENT = token(/\$[^\p{L}_{"]+/);

const QUOTE_IN_MULTI_LINE_STRING_CONTENT = token(/"[^"]|""[^"]/);

module.exports = grammar({
  name: "kotlin",

  conflicts: $ => [
    // Ambiguous when used in an explicit delegation expression,
    // since the '{' could either be interpreted as the class body
    // or as the anonymous function body. Consider the following sequence:

    // Member access operator '::' conflicts with callable reference
    [$._primary_expression, $.callable_reference],

    // @Type(... could either be an annotation constructor invocation or an annotated expression
    [$.constructor_invocation, $._unescaped_annotation],

    // "expect" as a plaform modifier conflicts with expect as an identifier
    [$.platform_modifier, $.simple_identifier],
    // "data", "inner" as class modifier or id
    [$.class_modifier, $.simple_identifier],

    // ambiguity between prefix expressions and annotations before functions
    [$._statement, $.prefix_expression],
    [$._statement, $.prefix_expression, $.modifiers],
    [$.prefix_expression, $.when_subject],
    [$.prefix_expression, $.value_argument],

    // ambiguity between multiple user types and class property/function declarations
    [$.user_type],
    [$.user_type, $.anonymous_function],

    // ambiguity between simple identifier 'set/get' with actual setter/getter functions.
    [$.setter, $.simple_identifier],
    [$.getter, $.simple_identifier],

    // ambiguity between parameter modifiers in anonymous functions
    [$.parameter_modifiers, $._type_modifier],

    // ambiguity between type modifiers before an @
    [$.type_modifiers],
    // ambiguity between associating type modifiers
    [$.not_nullable_type],

    // shift/reduce conflicts when matching simple identifiers.
    //   - 'import'  (identifier  simple_identifier  •  identifier_repeat1)
    //   - 'import'  (identifier  simple_identifier)  •  '.'  …
    // By defining a conflict here, we let the parser to continue. The second path
    // eventually dies if there is no '.'
    [$.identifier],

    [$.expression, $.call_expression],

    [$.object_declaration]
  ],

  externals: $ => [
    $._automatic_semicolon,
    $._import_list_delimiter,
    $.safe_nav,
    $.multiline_comment,
  ],

  extras: $ => [
    $.line_comment,
    $.multiline_comment,
    /\s+/ // Whitespace
  ],

  supertypes: $ => [
    $.expression
  ],

  word: $ => $._alpha_identifier,

  rules: {
    // ====================
    // Syntax grammar
    // ====================

    // ==========
    // General
    // ==========

    // start
    source_file: $ => seq(
      optional($.shebang_line),
      repeat($.file_annotation),
      optional($.package_header),
      repeat($.import_list),
      repeat(seq($._statement, $._semi))
    ),

    shebang_line: $ => seq("#!", /[^\r\n]*/),

    file_annotation: $ => seq(
      "@", "file", ":",
      choice(
        seq("[", repeat1($._unescaped_annotation), "]"),
        $._unescaped_annotation
      ),
      $._semi
    ),

    package_header: $ => seq("package", $.identifier, $._semi),

    import_list: $ => seq(
      repeat1($.import_header),
      $._import_list_delimiter
    ),

    import_header: $ => seq(
      "import",
      field('name', $.identifier),
      optional(choice(seq(".", $.wildcard_import), $._import_alias)),
      $._semi
    ),

    wildcard_import: _ =>"*",

    _import_alias: $ => seq("as", field('alias', $.simple_identifier)),

    top_level_object: $ => seq($._declaration, optional($._semi)),

    type_alias: $ => seq(
      optional(field('modifiers', $.modifiers)),
      "typealias",
      alias($.simple_identifier, $.type_identifier),
      optional($.type_parameters),
      "=",
      $._type
    ),

    _declaration: $ => choice(
      $.class_declaration,
      $.object_declaration,
      $.function_declaration,
      $.property_declaration,
      // TODO: it would be better to have getter/setter only in
      // property_declaration but it's difficult to get ASI
      // (Automatic Semicolon Insertion) working in the lexer for
      // getter/setter. Indeed, they can also have modifiers in
      // front, which means it's not enough to lookahead for 'get' or 'set' in
      // the lexer, you also need to handle modifier keywords. It is thus
      // simpler to accept them here.
      $.getter,
      $.setter,
      $.type_alias
    ),

    // ==========
    // Classes
    // ==========

    class_declaration: $ => prec.left(seq(
      optional(field('modifiers', $.modifiers)),
      choice("class", seq(optional("fun"), "interface")),
      field('name', $.simple_identifier),
      optional($.type_parameters),
      optional($.primary_constructor),
      optional(seq(":", $._delegation_specifiers)),
      optional($.type_constraints),
      optional(field('body', $.class_body))
    )),

    primary_constructor: $ => seq(
      optional(seq(optional(field('modifiers', $.modifiers)), "constructor")),
      $.class_parameters
    ),

    class_body: $ => seq(
      "{",
      seq(optional($._enum_entries), optional(";")),
      optional($._class_member_declarations),
      "}"
    ),

    class_parameters: $ => seq(
      "(",
      optional(sep1($.class_parameter, ",")),
      optional(","),
      ")"
    ),

    binding_pattern_kind: $ => choice("val", "var"),

    class_parameter: $ => seq(
      optional(field('modifiers', $.modifiers)),
      optional(field('binding_pattern', $.binding_pattern_kind)),
      field('name', $.simple_identifier),
      ":",
      field('type', $._type),
      optional(seq("=", field('initializer', $.expression)))
    ),

    _delegation_specifiers: $ => prec.left(sep1(
      $.delegation_specifier,
      // $._annotated_delegation_specifier, // TODO: Annotations cause ambiguities with type modifiers
      ","
    )),

    delegation_specifier: $ => prec.left(choice(
      $.constructor_invocation,
      $.explicit_delegation,
      $.user_type,
      $.function_type
    )),

    constructor_invocation: $ => seq($.user_type, $.value_arguments),

    _annotated_delegation_specifier: $ => seq(repeat($.annotation), $.delegation_specifier),

    explicit_delegation: $ => seq(
      choice(
        $.user_type,
        $.function_type
      ),
      "by",
      $.expression
    ),

    type_parameters: $ => seq("<", sep1($.type_parameter, ","), ">"),

    type_parameter: $ => seq(
      optional($.type_parameter_modifiers),
      alias($.simple_identifier, $.type_identifier),
      optional(seq(":", $._type))
    ),

    type_constraints: $ => prec.right(seq("where", sep1($.type_constraint, ","))),

    type_constraint: $ => seq(
      repeat($.annotation),
      alias($.simple_identifier, $.type_identifier),
      ":",
      $._type
    ),

    // ==========
    // Class members
    // ==========

    _class_member_declarations: $ => repeat1(seq($._class_member_declaration, $._semi)),

    _class_member_declaration: $ => choice(
      $._declaration,
      $.companion_object,
      $.anonymous_initializer,
      $.secondary_constructor
    ),

    anonymous_initializer: $ => seq("init", $.block),

    companion_object: $ => seq(
      optional(field('modifiers', $.modifiers)),
      "companion",
      "object",
      optional(field('name', $.simple_identifier)),
      optional(seq(":", $._delegation_specifiers)),
      optional(field('body', $.class_body))
    ),

    function_value_parameters: $ => seq(
      "(",
      optional(sep1($.function_value_parameter, ",")),
      optional(","),
      ")"
    ),

    function_value_parameter: $ => seq(
      optional(field('modifiers', $.parameter_modifiers)),
      field('parameter', $.parameter),
      optional(seq("=", field('initializer', $.expression)))
    ),

    _receiver_type: $ => seq(
      optional($.type_modifiers),
      choice(
        $._type_reference,
        $.parenthesized_type,
        $.nullable_type
      )
    ),

    function_declaration: $ => prec.right(seq( // TODO
      optional(field('modifiers', $.modifiers)),
      "fun",
      optional($.type_parameters),
      optional(seq($._receiver_type, optional('.'))),
      field('name', $.simple_identifier),
      field('parameters', $.function_value_parameters),
      optional(seq(":", field('type', $._type))),
      optional($.type_constraints),
      optional(field('body', $.function_body))
    )),

    function_body: $ => choice($.block, seq("=", field('expression', $.expression))),

    variable_declaration: $ => prec.left(PREC.VAR_DECL, seq(
      // repeat($.annotation), TODO
      field('id', $.simple_identifier),
      optional(field('type', seq(":", $._type)))
    )),

    property_declaration: $ => prec.right(seq(
      optional(field('modifiers', $.modifiers)),
      $.binding_pattern_kind,
      optional(field('type_parameters', $.type_parameters)),
      optional(seq($._receiver_type, optional('.'))),
      field('var_decl', choice($.variable_declaration, $.multi_variable_declaration)),
      optional(field('type_constraints', $.type_constraints)),
      optional(choice(
        seq("=", field('initializer', $.expression)),
        $.property_delegate
      )),
      optional(';'),
      choice(
        // TODO: Getter-setter combinations
        optional($.getter),
        optional($.setter)
      )
    )),

    property_delegate: $ => seq("by", $.expression),

    getter: $ => prec.right(seq(
      optional(field('modifiers', $.modifiers)),
      "get",
      optional(seq(
        "(", ")",
        optional(seq(":", $._type)),
        $.function_body
      ))
    )),

    setter: $ => prec.right(seq(
      optional(field('modifiers', $.modifiers)),
      "set",
      optional(seq(
        "(",
        $.parameter_with_optional_type,
        ")",
        optional(seq(":", $._type)),
        $.function_body
      ))
    )),

    parameters_with_optional_type: $ => seq("(", sep1($.parameter_with_optional_type, ","), ")"),

    parameter_with_optional_type: $ => seq(
      optional($.parameter_modifiers),
      $.simple_identifier,
      optional(seq(":", $._type))
    ),

    parameter: $ => seq(field('name', $.simple_identifier), ":", field('type', $._type)),

    object_declaration: $ => seq(
      optional(field('modifiers', $.modifiers)),
      "object",
      field('name', $.simple_identifier),
      optional(seq(":", $._delegation_specifiers)),
      optional(field('body', $.class_body))
    ),

    secondary_constructor: $ => seq(
      optional(field('modifiers', $.modifiers)),
      "constructor",
      field('parameters', $.function_value_parameters),
      optional(seq(":", $.constructor_delegation_call)),
      optional(field('block', $.block))
    ),

    constructor_delegation_call: $ => seq(choice("this", "super"), $.value_arguments),

    // ==========
    // Enum classes
    // ==========

    _enum_entries: $ => prec.left(seq(sep1($.enum_entry, ","), optional(","))),

    enum_entry: $ => seq(
      optional(field('modifiers', $.modifiers)),
      field('name', $.simple_identifier),
      optional(field('arguments', $.value_arguments)),
      optional(field('body', $.class_body))
    ),

    // ==========
    // Types
    // ==========

    _type: $ => seq(
      optional($.type_modifiers),
      choice(
        $.parenthesized_type,
        $.nullable_type,
        $._type_reference,
        $.function_type,
        $.not_nullable_type
      )
    ),

    _type_reference: $ => prec.left(1, choice(
      $.user_type,
      "dynamic"
    )),

    not_nullable_type: $ => seq(
      optional($.type_modifiers),
      choice($.user_type, $.parenthesized_user_type),
      '&',
      optional($.type_modifiers),
      choice($.user_type, $.parenthesized_user_type),
    ),

    nullable_type: $ => seq(
      choice($._type_reference, $.parenthesized_type),
      repeat1($._quest)
    ),

    _quest: $ => "?",

    // TODO: Figure out a better solution than right associativity
    //       to prevent nested types from being recognized as
    //       unary expresions with navigation suffixes.

    user_type: $ => sep1($._simple_user_type, "."),

    _simple_user_type: $ => prec.right(PREC.SIMPLE_USER_TYPE, seq(
      alias($.simple_identifier, $.type_identifier),
      optional($.type_arguments)
    )),

    type_projection: $ => choice(
      seq(optional($.type_projection_modifiers), $._type),
      "*"
    ),

    type_projection_modifiers: $ => repeat1($._type_projection_modifier),

    _type_projection_modifier: $ => $.variance_modifier,

    function_type: $ => seq(
      optional(seq($.user_type, ".")), // TODO: Support "real" types
      $.function_type_parameters,
      "->",
      $._type
    ),

    // A higher-than-default precedence resolves the ambiguity with 'parenthesized_type'
    function_type_parameters: $ => prec.left(1, seq(
      "(",
      optional(sep1(choice($.parameter, $._type), ",")),
      ")"
    )),

    parenthesized_type: $ => seq("(", $._type, ")"),

    parenthesized_user_type: $ => seq(
      "(",
      choice($.user_type, $.parenthesized_user_type),
      ")"
    ),

    // ==========
    // Statements
    // ==========

    statements: $ => seq(
      $._statement,
      repeat(seq($._semi, $._statement)),
      optional($._semi),
    ),

    _statement: $ => choice(
      $._declaration,
      seq(
        repeat(choice($.label, $.annotation)),
        choice(
          $.assignment,
          $._loop_statement,
          $.expression
        )
      )
    ),

    label: $ => token(seq(
      /[a-zA-Z_][a-zA-Z_0-9]*/,
      "@"
    )),

    control_structure_body: $ => choice($.block, $._statement),

    block: $ => prec(PREC.BLOCK, seq("{", optional($.statements), "}")),

    _loop_statement: $ => choice(
      $.for_statement,
      $.while_statement,
      $.do_while_statement
    ),

    for_statement: $ => prec.right(seq(
      "for",
      "(",
      repeat($.annotation),
      field('var_decl', choice($.variable_declaration, $.multi_variable_declaration)),
      "in",
      field('expression', $.expression),
      ")",
      optional(field('body', $.control_structure_body))
    )),

    while_statement: $ => seq(
      "while",
      "(",
      $.expression,
      ")",
      choice(";", $.control_structure_body)
    ),

    do_while_statement: $ => prec.right(seq(
      "do",
      optional($.control_structure_body),
      "while",
      "(",
      $.expression,
      ")",
    )),

    // See also https://github.com/tree-sitter/tree-sitter/issues/160
    // generic EOF/newline token
    _semi: $ => $._automatic_semicolon,

    assignment: $ =>
      prec.left(PREC.ASSIGNMENT, seq(
        field('left', $._directly_assignable_expression),
        field('op', choice('=', $._assignment_and_operator)),
        field('right', $.expression))),

    // ==========
    // Expressions
    // ==========

    expression: $ => choice(
      $._unary_expression,
      $._binary_expression,
      $._primary_expression
    ),

    // Unary expressions

    _unary_expression: $ => choice(
      $.prefix_expression,
      $.postfix_expression,
      $.as_expression,
      $.spread_expression,
      $.if_expression,
      $.jump_expression,
    ),

    postfix_expression: $ => prec(PREC.POSTFIX, seq(field('expression', $.expression), field('operator', $.postfix_unary_operator))),

    dot_qualified_expression: $ => prec(PREC.DOT, 
      seq(field('receiver', choice($._primary_expression, $.postfix_expression)), 
      $._member_access_operator,
      field('selector', choice(
        $.simple_identifier,
        $.parenthesized_expression,
        "class"
      ))
    )),

    call_expression: $ => seq(field('expression', $._primary_expression), 
      optional($.type_arguments),
      choice(
        prec(PREC.CALL, seq(optional(field('args', $.value_arguments)), field('lambda_arg', $.annotated_lambda))),
        field('args', $.value_arguments)
      )),
    
    index_access_expression: $ => prec(PREC.INDEX, seq(
      field('expression', $.expression), 
      '[',
      field('index', $.expression),
      repeat(seq(',', $.expression)),
      optional(','),
      ']')
    ),

    prefix_expression: $ => choice(
      seq(choice($.annotation, $.label), field('expression', $.expression)),
      prec(PREC.PREFIX, seq(field('op', $.prefix_unary_operator), field('expression', $.expression)))),

    as_expression: $ => prec(PREC.AS, seq($.expression, $._as_operator, $._type)),

    spread_expression: $ => prec(PREC.SPREAD, seq("*", $.expression)),

    // Binary expressions

    _binary_expression: $ => choice(
      $.multiplicative_expression,
      $.additive_expression,
      $.range_expression,
      $.infix_expression,
      $.elvis_expression,
      $.check_expression,
      $.comparison_expression,
      $.equality_expression,
      $.conjunction_expression,
      $.disjunction_expression
    ),

    multiplicative_expression: $ => prec.left(PREC.MULTIPLICATIVE, seq($.expression, $._multiplicative_operator, $.expression)),

    additive_expression: $ => prec.left(PREC.ADDITIVE, seq($.expression, $._additive_operator, $.expression)),

    range_expression: $ => prec.left(PREC.RANGE, seq($.expression, $._range_opeartor, $.expression)),

    _range_opeartor: $ => choice("..", "..<"),

    infix_expression: $ => prec.left(PREC.INFIX, seq($.expression, $.simple_identifier, $.expression)),

    elvis_expression: $ => prec.left(PREC.ELVIS, seq($.expression, "?:", $.expression)),

    check_expression: $ => prec.left(PREC.CHECK, seq($.expression, choice(
      seq($._in_operator, $.expression),
      seq($._is_operator, $._type)))),

    comparison_expression: $ => prec.left(PREC.COMPARISON, seq($.expression, $._comparison_operator, $.expression)),

    equality_expression: $ => prec.left(PREC.EQUALITY, seq($.expression, $._equality_operator, $.expression)),

    conjunction_expression: $ => prec.left(PREC.CONJUNCTION, seq(
      field('left', $.expression), 
      "&&", 
      field('right', $.expression))
    ),

    disjunction_expression: $ => prec.left(PREC.DISJUNCTION, seq(
      field('left', $.expression), 
      "||", 
      field('right', $.expression))
    ),

    // Suffixes

    annotated_lambda: $ => seq(
      repeat($.annotation),
      optional($.label),
      $.lambda_literal
    ),

    type_arguments: $ => seq("<", sep1($.type_projection, ","), ">"),

    value_arguments: $ => seq(
      "(",
      optional(
        seq(
          sep1($.value_argument, ","),
          optional(","),
        )
      ),
      ")"
    ),

    value_argument: $ => seq(
      optional($.annotation),
      optional(seq($.simple_identifier, "=")),
      optional("*"),
      $.expression
    ),

    _primary_expression: $ => choice(
      $.parenthesized_expression,
      $.simple_identifier,
      $._literal_constant,
      $.string_literal,
      $.callable_reference,
      $._function_literal,
      $.object_literal,
      $.collection_literal,
      $.this_expression,
      $.super_expression,
      $.when_expression,
      $.try_expression,
      $.dot_qualified_expression,
      $.call_expression,
      $.index_access_expression
    ),

    parenthesized_expression: $ => seq("(", $.expression, ")"),

    collection_literal: $ => seq("[", $.expression, repeat(seq(",", $.expression)), "]"),

    _literal_constant: $ => choice(
      $.boolean_literal,
      $.integer_literal,
      $.hex_literal,
      $.bin_literal,
      $.character_literal,
      $.real_literal,
      $.null_literal,
      $.long_literal,
      $.unsigned_literal
    ),

    string_literal: $ => choice(
      $._line_string_literal,
      $._multi_line_string_literal
    ),

    _line_string_literal: $ => seq(
      '"',
      repeat(choice(
        alias($.line_string_content, $.string_content), 
        alias(DOLLAR_IN_STRING_CONTENT, $.string_content),
        $._interpolation
      )),
      // Need to consume the last '$' character here, and create a node in the tree
      choice('"', seq(alias("$", $.string_content), '"'))
    ),

    line_string_content: $ => token(prec(PREC.STRING_CONTENT, choice(
      /[^\\"$]+/,
      repeat1(uni_character_literal),
      escaped_identifier
    ))),

    _multi_line_string_literal: $ => prec.right(seq(
      '"""',
      repeat(choice(
        alias($.multi_line_string_content, $.string_content), 
        alias(DOLLAR_IN_STRING_CONTENT, $.string_content), 
        QUOTE_IN_MULTI_LINE_STRING_CONTENT,
        $._interpolation,
      )),
      // Need to consume the last '$' character here, and create a node in the tree
      optional(alias("$", $.string_content)),
      '"""',
      repeat('"')
    )),

    multi_line_string_content: $ => token(prec(PREC.STRING_CONTENT, /[^"$]+/)),

    _interpolation: $ => choice(
      seq("${", alias($.expression, $.interpolated_expression), "}"),
      seq("$", alias($.simple_identifier, $.interpolated_identifier))
    ),

    lambda_literal: $ => prec(PREC.LAMBDA_LITERAL, seq(
      "{",
      optional(seq(optional(field('parameters', $.lambda_parameters)), "->")),
      optional(field('body', $.statements)),
      "}"
    )),

    multi_variable_declaration: $ => seq(
      '(',
      sep1($.variable_declaration, ','),
      ')'
    ),

    lambda_parameters: $ => seq(sep1($._lambda_parameter, ","), optional(",")),

    _lambda_parameter: $ => choice(
      $.variable_declaration,
      $.multi_variable_declaration
    ),

    anonymous_function: $ => prec.right(seq(
      "fun",
      optional(seq(sep1($._simple_user_type, "."), ".")), // TODO
      $.function_value_parameters,
      optional(seq(":", $._type)),
      optional($.function_body)
    )),

    _function_literal: $ => choice(
      $.lambda_literal,
      $.anonymous_function
    ),

    object_literal: $ => seq(
      "object",
      optional(seq(":", $._delegation_specifiers)),
      $.class_body
    ),

    this_expression: $ => choice(
      "this",
      $._this_at
    ),

    super_expression: $ => prec.right(choice(
      "super",
      seq("super", "<", $._type, ">"),
      $._super_at
    )),

    if_expression: $ => prec.right(seq(
      "if",
      "(", field('condition', $.expression), ")",
      choice(
        field('consequence', $.control_structure_body),
        ";"
      ),
      optional(seq(
        "else",
        choice(field('alternative', $.control_structure_body), ";")
      )),
    )),

    when_subject: $ => seq(
      "(",
      optional(seq(
        repeat($.annotation),
        "val",
        $.variable_declaration,
        "="
      )),
      $.expression,
      ")",
    ),

    when_expression: $ => seq(
      "when",
      optional($.when_subject),
      "{",
      repeat($.when_entry),
      "}"
    ),

    when_entry: $ => seq(
      choice(
        seq($.when_condition, repeat(seq(",", $.when_condition)), optional(",")),
        "else"
      ),
      "->",
      $.control_structure_body,
      optional($._semi)
    ),

    when_condition: $ => choice(
      $.expression,
      $.range_test,
      $.type_test
    ),

    range_test: $ => seq($._in_operator, $.expression),

    type_test: $ => seq($._is_operator, $._type),

    try_expression: $ => seq(
      "try",
      $.block,
      choice(
        seq(repeat1($.catch_block), optional($.finally_block)),
        $.finally_block
      )
    ),

    catch_block: $ => seq(
      "catch",
      "(",
      repeat($.annotation),
      field('name', $.simple_identifier),
      ":",
      field('type', $._type),
      ")",
      field('body', $.block),
    ),

    finally_block: $ => seq("finally", $.block),

    jump_expression: $ => choice(
      prec.right(PREC.RETURN_OR_THROW, seq("throw", $.expression)),
      prec.right(PREC.RETURN_OR_THROW, seq(choice("return", $._return_at), optional($.expression))),
      "continue",
      $._continue_at,
      "break",
      $._break_at
    ),

    callable_reference: $ => seq(
      optional(choice(alias($.simple_identifier, $.type_identifier), $.this_expression)),
      "::",
      choice($.simple_identifier, "class")
    ),

    _assignment_and_operator: $ => choice("+=", "-=", "*=", "/=", "%="),

    _equality_operator: $ => choice("!=", "!==", "==", "==="),

    _comparison_operator: $ => choice("<", ">", "<=", ">="),

    _in_operator: $ => choice("in", "!in"),

    _is_operator: $ => choice("is", "!is"),

    _additive_operator: $ => choice("+", "-"),

    _multiplicative_operator: $ => choice("*", "/", "%"),

    _as_operator: $ => choice("as", "as?"),

    prefix_unary_operator: $ => choice("++", "--", "-", "+", "!"),

    postfix_unary_operator: $ => choice("++", "--", "!!"),

    _member_access_operator: $ => choice(".", alias($.safe_nav, '?.')),

    _directly_assignable_expression: $ => prec(
      PREC.ASSIGNMENT,
      choice(
        $.dot_qualified_expression,
        $.index_access_expression,
        $.simple_identifier,
        // TODO
      )
    ),

    // ==========
    // Modifiers
    // ==========

    modifiers: $ => prec.left(repeat1(choice($.annotation, $._modifier))),

    parameter_modifiers: $ => repeat1(choice($.annotation, $.parameter_modifier)),

    _modifier: $ => choice(
      $.class_modifier,
      $.member_modifier,
      $.visibility_modifier,
      $.function_modifier,
      $.property_modifier,
      $.inheritance_modifier,
      $.parameter_modifier,
      $.platform_modifier
    ),

    type_modifiers: $ => repeat1($._type_modifier),

    _type_modifier: $ => choice($.annotation, "suspend"),

    class_modifier: $ => choice(
      "enum",
      "sealed",
      "annotation",
      "data",
      "inner",
      "value",
    ),

    member_modifier: $ => choice(
      "override",
      "lateinit"
    ),

    visibility_modifier: $ => choice(
      "public",
      "private",
      "internal",
      "protected"
    ),

    variance_modifier: $ => choice(
      "in",
      "out"
    ),

    type_parameter_modifiers: $ => repeat1($._type_parameter_modifier),

    _type_parameter_modifier: $ => choice(
      $.reification_modifier,
      $.variance_modifier,
      $.annotation
    ),

    function_modifier: $ => choice(
      "tailrec",
      "operator",
      "infix",
      "inline",
      "external",
      "suspend"
    ),

    property_modifier: $ => "const",

    inheritance_modifier: $ => choice(
      "abstract",
      "final",
      "open"
    ),

    parameter_modifier: $ => choice(
      "vararg",
      "noinline",
      "crossinline"
    ),

    reification_modifier: $ => "reified",

    platform_modifier: $ => choice(
      "expect",
      "actual"
    ),

    // ==========
    // Annotations
    // ==========

    annotation: $ => choice(
      $._single_annotation,
      $._multi_annotation
    ),

    _single_annotation: $ => seq(
      "@",
      optional($.use_site_target),
      $._unescaped_annotation
    ),

    _multi_annotation: $ => seq(
      "@",
      optional($.use_site_target),
      "[",
      repeat1($._unescaped_annotation),
      "]"
    ),

    use_site_target: $ => seq(
      choice("field", "property", "get", "set", "receiver", "param", "setparam", "delegate"),
      ":"
    ),

    _unescaped_annotation: $ => choice(
      $.constructor_invocation,
      $.user_type
    ),

    // ==========
    // Identifiers
    // ==========

    simple_identifier: $ => choice(
      $._lexical_identifier,
      "expect",
      "data",
      "inner",
      "value",
      "actual",
      "set",
      "get",
      "annotation"
      // TODO: More soft keywords
    ),

    identifier: $ => sep1($.simple_identifier, "."),

    // ====================
    // Lexical grammar
    // ====================


    // ==========
    // General
    // ==========

    line_comment: $ => token(prec(PREC.COMMENT, seq('//', /[^\r\n]*/))),

    // ==========
    // Separators and operations
    // ==========


    // ==========
    // Keywords
    // ==========

    _return_at: $ => seq(
      "return@",
      alias($._lexical_identifier, $.label)
    ),

    _continue_at: $ => seq(
      "continue@",
      alias($._lexical_identifier, $.label)
    ),

    _break_at: $ => seq(
      "break@",
      alias($._lexical_identifier, $.label)
    ),

    _this_at: $ => seq(
      "this@",
      alias($._lexical_identifier, $.type_identifier)
    ),

    _super_at: $ => choice(
      seq(
        "super@",
        alias($._lexical_identifier, $.type_identifier)
      ),
      seq(
        "super",
        "<", $._type, ">",
        token.immediate("@"),
        alias($._lexical_identifier, $.type_identifier)
      )
    ),

    // ==========
    // Literals
    // ==========

    real_literal: $ => token(choice(
      seq(
        choice(
          seq(DEC_DIGITS, REAL_EXPONENT),
          seq(optional(DEC_DIGITS), ".", DEC_DIGITS, optional(REAL_EXPONENT))
        ),
        optional(/[fF]/)
      ),
      seq(DEC_DIGITS, /[fF]/)
    )),

    integer_literal: $ => token(seq(optional(/[1-9]/), DEC_DIGITS)),

    hex_literal: $ => token(seq("0", /[xX]/, HEX_DIGITS)),

    bin_literal: $ => token(seq("0", /[bB]/, BIN_DIGITS)),

    unsigned_literal: $ => seq(
      choice($.integer_literal, $.hex_literal, $.bin_literal),
      /[uU]L?/
    ),

    long_literal: $ => seq(
      choice($.integer_literal, $.hex_literal, $.bin_literal),
      "L"
    ),

    boolean_literal: $ => choice("true", "false"),

    character_literal: $ => seq(
      "'",
      choice($.character_escape_seq, /[^\n\r'\\]/),
      "'"
    ),

    character_escape_seq: $ => token(choice(
      uni_character_literal,
      escaped_identifier
    )),    

    null_literal: $ => "null",

    // ==========
    // Identifiers
    // ==========

    _lexical_identifier: $ => choice(
      $._alpha_identifier,
      $._backtick_identifier,
    ),

    _alpha_identifier: $ => /[\p{L}_][\p{L}_\p{Nd}]*/,

    _backtick_identifier: $ => /`[^\r\n`]+`/,
  }
});

function sep1(rule, separator) {
  return seq(rule, repeat(seq(separator, rule)));
}
