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
  GENERIC: 3,
  SPREAD: 2,
  TYPE_REFERENCE: 1,
  ASSIGNMENT: 1,
  BLOCK: 1,
  ARGUMENTS: 1,
  STRING_CONTENT: 1,
  RETURN_OR_THROW: 0,
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

const NON_NL_WHITESPACE = /[\t\v\f \u00a0\u2000\u2001\u2002\u2003\u2004\u2005\u2006\u2007\u2008\u2009\u200a\u200b\u2028\u2029\u3000]+/;

module.exports = grammar({
  name: "kotlin",

  conflicts: $ => [
    [$.class_declaration],
    [$.class_declaration, $.primary_constructor],
    
    // @Type(... could either be an annotation constructor invocation or an annotated expression
    [$.constructor_invocation, $._unescaped_annotation],

    [$.platform_modifier, $._soft_keywords],
    [$._type_modifier, $._soft_keywords],
    [$.class_modifier, $._soft_keywords],
    [$.function_modifier, $._soft_keywords],
    [$.member_modifier, $._soft_keywords],
    [$.visibility_modifier, $._soft_keywords],
    [$.property_modifier, $._soft_keywords],
    [$.inheritance_modifier, $._soft_keywords],
    [$.parameter_modifier, $._soft_keywords],
    [$.class_modifier, $._soft_keywords],
    [$.use_site_target, $._soft_keywords],
    [$.import_header, $._soft_keywords],
    [$.variance_modifier, $._soft_keywords],
    [$.reification_modifier, $._soft_keywords],
    [$.getter, $._soft_keywords],
    [$.setter, $._soft_keywords],

    [$.type_parameter_modifiers],
    [$.type_projection_modifiers],
    [$.modifiers],

    // ambiguity between multiple user types and class property/function declarations
    [$.user_type],
    [$.user_type, $.anonymous_function],

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

    [$._simple_user_type, $._common_primary_expression],

    [$._simple_user_type],

    [$.annotated_expression, $.modifiers],
    [$.annotated_expression, $.when_subject],
    [$.annotated_expression, $.value_argument],
    [$.annotation],
    [$.variable_declaration],
    [$.class_parameter],
    [$.collection_literal],
    // becuase of $._NL after simple_identifier, all can reduce
    [$.value_argument, $._common_primary_expression],
    [$.variable_declaration, $._common_primary_expression],
    [$.variable_declaration, $._simple_user_type],
    [$.value_argument],
    [$.property_declaration],
    [$._enum_entries],
    [$.type_constraints],
    [$.class_declaration, $.type_constraints],
    [$.type_constraints, $.property_declaration],
    [$.do_while_statement],
    [$._delegation_specifiers],
    [$.explicit_delegation, $._primary_expression],
    [$.function_value_parameters],
    [$.class_parameters],
  ],

  extras: $ => [
    $.line_comment,
    $.multiline_comment,
    NON_NL_WHITESPACE
  ],

  externals: $ => [
    $._bin_min,
    $._bin_plus,
    $._safe_dot,
    $._elvis,
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
      repeat($._NL),
      optional($.shebang_line),
      repeat($.file_annotation),
      optional($.package_header),
      repeat(seq($.import_header, optional($._semi))),
      // In principle, we either parse a Kotlin file (.kt) or a Kotlin script (.kts).
      // Statements cannot appear as top-level constructs in Kotlin files, only in scripts.
      // However, here, we're allowing parsing of both statements and declarations as top level.
      repeat(choice($._top_level_object, alias($._top_level_statement, $.statement)))
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

    import_header: $ => prec.left(seq(
      "import",
      field('name', $.identifier),
      optional(choice(seq(".", $.wildcard_import), $._import_alias)),
    )),

    wildcard_import: $ => seq(repeat($._NL), "*"),

    _import_alias: $ => seq("as", field('alias', $.simple_identifier)),

    _top_level_object: $ => seq($._declaration, optional($._semi)),

    _top_level_statement: $ => seq(
      choice(
        $.assignment,
        $._loop_statement,
        $.expression
      ), 
      $._semi,
    ),

    type_alias: $ => prec.right(seq(
      optional(field('modifiers', $.modifiers)),
      "typealias",
      alias($.simple_identifier, $.type_identifier),
      optional($.type_parameters),
      repeat($._NL),
      $._ASSIGNMENT,
      $._type
    )),

    _declaration: $ => choice(
      $.class_declaration,
      $.enum_class_declaration,
      $.object_declaration,
      $.function_declaration,
      $.property_declaration,
      $.type_alias
    ),

    // ==========
    // Classes
    // ==========

    class_declaration: $ =>seq(
      optional(field('modifiers', $.modifiers)),
      choice("class", seq(optional(seq("fun", repeat($._NL))), "interface")),
      repeat($._NL),
      field('name', $.simple_identifier),
      optional($.type_parameters),
      optional($.primary_constructor),
      optional(seq(repeat($._NL), ":", repeat($._NL),  $._delegation_specifiers)),
      repeat($._NL),
      optional($.type_constraints),
      optional(field('body', $.class_body)),
    ),

    enum_class_declaration: $ => prec.right(seq(
      optional(field('modifiers', $.modifiers)),
      seq("enum", "class"),
      field('name', $.simple_identifier),
      optional($.type_parameters),
      optional($.primary_constructor),
      optional(seq(":", repeat($._NL), $._delegation_specifiers)),
      optional($.type_constraints),
      optional(field('body', $.enum_class_body))
    )),

    primary_constructor: $ => seq(
      repeat($._NL),
      optional(seq(optional(field('modifiers', $.modifiers)), "constructor", repeat($._NL))),
      $.class_parameters
    ),

    class_body: $ => seq(
      "{",
      repeat($._NL),
      optional($._class_member_declarations),
      "}"
    ),

    class_parameters: $ => seq(
      "(",
      repeat($._NL),
      optional(sep1($.class_parameter, repeat($._NL), ",", repeat($._NL))),
      optional(","),
      repeat($._NL),
      ")"
    ),

    class_parameter: $ => seq(
      optional(field('modifiers', $.modifiers)),
      optional(field('binding_pattern', $.binding_pattern_kind)),
      field('name', $.simple_identifier),
      ":",
      field('type', $._type),
      optional(seq(repeat($._NL), $._ASSIGNMENT, field('initializer', $.expression)))
    ),

    binding_pattern_kind: $ => choice("val", "var"),

    _delegation_specifiers: $ => seq(
      sep1(
        $.delegation_specifier, 
        repeat($._NL),
        ",",
        repeat($._NL),
      )
    ),

    delegation_specifier: $ => prec.right(choice(
      $.constructor_invocation,
      $.explicit_delegation,
      $.user_type,
      $.function_type
    )),

    constructor_invocation: $ => seq(
      $.user_type,
      $.value_arguments
    ),

    _annotated_delegation_specifier: $ => seq(repeat($.annotation), repeat($._NL), $.delegation_specifier),

    explicit_delegation: $ => seq(
      choice(
        $.user_type,
        $.function_type
      ),
      "by",
      repeat($._NL),
      choice($._common_primary_expression, alias($.simple_call_expression, $.call_expression))
    ),

    type_parameters: $ => seq(
      repeat($._NL),
      "<", 
      sep1($.type_parameter, ","), 
      ">"
    ),

    type_parameter: $ => seq(
      optional($.type_parameter_modifiers),
      repeat($._NL),
      alias($.simple_identifier, $.type_identifier),
      optional(seq(repeat($._NL), ":", repeat($._NL), $._type))
    ),

    type_constraints: $ => seq(repeat($._NL), "where", repeat($._NL), sep1($.type_constraint, repeat($._NL), ",", repeat($._NL))),

    type_constraint: $ => seq(
      repeat($.annotation),
      alias($.simple_identifier, $.type_identifier),
      repeat($._NL),
      ":",
      repeat($._NL),
      $._type
    ),

    // ==========
    // Class members
    // ==========

    _class_member_declarations: $ => repeat1(seq(
      $._class_member_declaration, 
      optional($._semi)
    )),

    _class_member_declaration: $ => choice(
      $._declaration,
      $.companion_object,
      $.anonymous_initializer,
      $.secondary_constructor
    ),

    anonymous_initializer: $ => seq("init", $.block),

    companion_object: $ => prec.right(seq(
      optional(field('modifiers', $.modifiers)),
      "companion",
      "object",
      optional(field('name', $.simple_identifier)),
      optional(seq(":", repeat($._NL), $._delegation_specifiers)),
      optional(field('body', $.class_body)),
      repeat($._NL)
    )),

    function_value_parameters: $ => seq(
      "(",
      repeat($._NL),
      optional(sep1($.function_value_parameter, repeat($._NL), ",", repeat($._NL))),
      optional(","),
      repeat($._NL),
      ")"
    ),

    function_value_parameter: $ => seq(
      optional(field('modifiers', $.parameter_modifiers)),
      field('parameter', $.parameter),
      optional(seq($._ASSIGNMENT, field('initializer', $.expression)))
    ),

    receiver_type: $ => seq(
      optional(field('modifiers', $.type_modifiers)),
      choice(
        $._type_reference,
        $.parenthesized_type,
        $.nullable_type
      )
    ),

    function_declaration: $ => prec.right(seq(
      optional(field('modifiers', $.modifiers)),
      "fun",
      optional($.type_parameters),
      optional(seq(repeat($._NL), field('receiver_type', $.receiver_type), repeat($._NL), optional('.'))),
      field('name', $.simple_identifier),
      field('parameters', $.function_value_parameters),
      optional(seq(repeat($._NL), ":", repeat($._NL), field('type', $._type))),
      optional($.type_constraints),
      repeat($._NL),
      optional(field('body', $.function_body))
    )),

    function_body: $ => choice(
      $.block, 
      seq($._ASSIGNMENT, field('expression', $.expression))
    ),

    variable_declaration: $ => seq(
      // repeat($.annotation), TODO
      field('id', $.simple_identifier),
      optional(field('type', seq(repeat($._NL), ":", repeat($._NL), $._type)))
    ),

    property_declaration: $ => seq(
      optional(field('modifiers', $.modifiers)),
      $.binding_pattern_kind,
      optional(field('type_parameters', $.type_parameters)),
      optional(seq(field('receiver_type', $.receiver_type), '.')),
      field('var_decl', choice($.variable_declaration, $.multi_variable_declaration)),
      optional(field('type_constraints', $.type_constraints)),
      optional(choice(
        seq($._ASSIGNMENT, field('initializer', $.expression)),
        $.property_delegate
      )),
      repeat($._NL),
      optional(';'),
      choice(
        seq(optional($.getter), optional(seq(repeat($._NL), $.setter))),
        seq(optional($.setter), optional(seq(repeat($._NL), $.setter)))
      )
    ),

    property_delegate: $ => seq("by", repeat($._NL), $.expression),

    getter: $ => prec.right(seq(
      optional(field('modifiers', $.modifiers)),
      "get",
      optional(seq(
        "(",
        repeat($._NL),
        ")",
        optional(seq(":", $._type)),
        $.function_body
      ))
    )),

    setter: $ => prec.right(seq(
      optional(field('modifiers', $.modifiers)),
      "set",
      optional(seq(
        "(",
        repeat($._NL),
        $.parameter_with_optional_type,
        ")",
        optional(seq(":", $._type)),
        $.function_body
      ))
    )),

    parameters_with_optional_type: $ => seq(
      "(",
      repeat($._NL),
      sep1($.parameter_with_optional_type, ","), 
      ")"
    ),

    parameter_with_optional_type: $ => seq(
      optional($.parameter_modifiers),
      $.simple_identifier,
      optional(seq(":", $._type))
    ),

    parameter: $ => seq(
      field('name', $.simple_identifier), 
      repeat($._NL),
      ":", 
      repeat($._NL),
      field('type', $._type)
    ),

    object_declaration: $ => prec.right(seq(
      optional(field('modifiers', $.modifiers)),
      "object",
      field('name', $.simple_identifier),
      optional(seq(":", repeat($._NL), $._delegation_specifiers)),
      optional(field('body', $.class_body))
    )),

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

    enum_class_body: $ => seq(
      "{",
      repeat($._NL),
      optional($._enum_entries),
      optional(seq(";", repeat($._NL), optional($._class_member_declarations))),
      "}"
    ),

    _enum_entries: $ => seq(
      $.enum_entry,
      repeat(seq(repeat($._NL), ",", repeat($._NL), $.enum_entry)),
      repeat($._NL),
      optional(","),
      repeat($._NL),
    ),

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

    // Give type reference a higher precedence to resolve conflict with parenthesized expression
    _type_reference: $ => prec(PREC.TYPE_REFERENCE, choice(
      field('type', $.user_type),
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
      repeat1("?")
    ),

    user_type: $ => seq(
      $._simple_user_type, repeat(seq(choice($._DOT, "."), repeat($._NL), $._simple_user_type))
    ),

    _simple_user_type: $ => seq(
      alias($.simple_identifier, $.type_identifier),
      optional($.type_arguments)
    ),

    type_projection: $ => choice(
      seq(optional($.type_projection_modifiers), $._type),
      "*"
    ),

    type_projection_modifiers: $ => repeat1($._type_projection_modifier),

    _type_projection_modifier: $ => $.variance_modifier,

    function_type: $ => seq(
      optional(seq($.user_type, choice($._DOT, "."))), // TODO: Support "real" types
      $.function_type_parameters,
      $._ARROW,
      repeat($._NL),
      $._type
    ),

    // A higher-than-default precedence resolves the ambiguity with 'parenthesized_type'
    function_type_parameters: $ => prec.left(1, seq(
      "(",
      repeat($._NL),
      optional(sep1(choice($.parameter, $._type), ",")),
      ")"
    )),

    parenthesized_type: $ => seq("(", repeat($._NL), $._type, ")"),

    parenthesized_user_type: $ => seq(
      "(",
      repeat($._NL),
      choice($.user_type, $.parenthesized_user_type),
      ")"
    ),

    // ==========
    // Statements
    // ==========

    statements: $ => seq(
      sep1($.statement, $._semis),
      optional($._semis)
    ),

    statement: $ => choice(
      $._declaration,
      $.assignment,
      $._loop_statement,
      $.expression
    ),

    label: $ => token(seq(
      /[a-zA-Z_][a-zA-Z_0-9]*/,
      "@"
    )),

    control_structure_body: $ => choice($.block, $.statement),

    block: $ => prec(PREC.BLOCK, seq(
      "{",
      repeat($._NL),
      optional($.statements),
      "}")
    ),

    _loop_statement: $ => choice(
      $.for_statement,
      $.while_statement,
      $.do_while_statement
    ),

    for_statement: $ => prec.right(seq(
      "for",
      "(",
      repeat($._NL),
      repeat($.annotation),
      field('var_decl', choice($.variable_declaration, $.multi_variable_declaration)),
      "in",
      field('expression', $.expression),
      ")",
      repeat($._NL),
      optional(field('body', $.control_structure_body))
    )),

    while_statement: $ => seq(
      "while",
      "(",
      repeat($._NL),
      $.expression,
      ")",
      repeat($._NL),
      choice(";", $.control_structure_body)
    ),

    do_while_statement: $ => prec.right(seq(
      "do",
      repeat($._NL),
      optional($.control_structure_body),
      repeat($._NL),
      "while",
      "(",
      repeat($._NL),
      $.expression,
      ")",
    )),

    assignment: $ =>
      prec.left(PREC.ASSIGNMENT, seq(
        field('left', $._directly_assignable_expression),
        field('op', choice($._ASSIGNMENT, $._assignment_and_operator)),
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
      $.jump_expression,
      $.annotated_expression,
      $.labeled_expression,
      $.if_expression
    ),

    postfix_expression: $ => prec(PREC.POSTFIX, seq(field('expression', $.expression), field('operator', $.postfix_unary_operator))),

    dot_qualified_expression: $ => prec(PREC.DOT, seq(
      field('receiver', choice($._primary_expression, $.postfix_expression)),
      choice(choice($._DOT, "."), $._safe_dot),
      repeat($._NL),
      field('selector', choice(
        $.simple_identifier,
        $.parenthesized_expression,
        "class"
      ))
    )),

    call_expression: $ => seq(
      field('expression', $._primary_expression), 
      optional($.type_arguments),
      $._call_arguments
    ),

    // It's not possible to resolve the ambiguity between explicit delegation and calls with last lambda argument.
    // For example:
    //   class Derived(b: Base) : Base by b {
    //     override fun printMessage() { print("abc") }
    //   }
    // Here, b {}, can be parsed as both a call expression or a delegation followed by the class body. In this case,
    // we need the second interpreation. But in funciton calls like `with (s) {}`, we want `{}` to be parsed as part
    // of the call. That means that based on the context a call_expression happens in, we either want left or right
    // associativity for the arguments, which is not posssible to specify with tree-sitter. 
    // Creating a separate nonterminal, is the cleanest way to move forward.
    // 
    simple_call_expression: $=> prec(10, seq(
      field('expression', $._primary_expression), 
      optional($.type_arguments),
      field('args', $.value_arguments)
    )),

    // Right precedence here to extend the call to the right, i.e., `with (s) { s }`
    // should be parsed as a flat list of arguments. 
    _call_arguments: $ => prec.right(choice(
      seq(optional(field('args', $.value_arguments)), field('lambda_arg', $.annotated_lambda)),
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

    annotated_expression: $ => seq($.annotation, $.expression),

    labeled_expression: $ => seq($.label, $.expression),

    prefix_expression: $ => prec(PREC.PREFIX, 
      seq(field('op', $.prefix_unary_operator), field('expression', $.expression))),

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

    additive_expression: $ => prec.left(PREC.ADDITIVE, seq($.expression, $._additive_operator, repeat($._NL), $.expression)),

    range_expression: $ => prec.left(PREC.RANGE, seq($.expression, $._range_opeartor, $.expression)),

    _range_opeartor: $ => choice("..", "..<"),

    infix_expression: $ => prec.left(PREC.INFIX, seq($.expression, $.simple_identifier, repeat($._NL), $.expression)),

    elvis_expression: $ => prec.left(PREC.ELVIS, seq($.expression, $._elvis, repeat($._NL), $.expression)),

    check_expression: $ => prec.left(PREC.CHECK, seq($.expression, choice(
      seq($._in_operator, $.expression),
      seq($._is_operator, $._type)))),

    comparison_expression: $ => prec.left(PREC.COMPARISON, seq($.expression, $._comparison_operator, $.expression)),

    equality_expression: $ => prec.left(PREC.EQUALITY, seq($.expression, $._equality_operator, $.expression)),

    conjunction_expression: $ => prec.left(PREC.CONJUNCTION, seq(
      field('left', $.expression), 
      $._CONJ,
      repeat($._NL),
      field('right', $.expression))
    ),

    disjunction_expression: $ => prec.left(PREC.DISJUNCTION, seq(
      field('left', $.expression), 
      $._DISJ,
      repeat($._NL),
      field('right', $.expression))
    ),

    // Suffixes

    annotated_lambda: $ => seq(
      repeat($.annotation),
      optional($.label),
      $.lambda_literal
    ),

    // Here we're defining a dynamic precedence for type arguments to resolve the ambiguity
    // between the comparison and generic call syntax.
    // Any expression such as a<b>(c) should be resolved in favor of the generic call.
    type_arguments: $ => prec.dynamic(PREC.GENERIC, seq("<", sep1($.type_projection, ","), ">")),

    value_arguments: $ => seq(
      "(",
      repeat($._NL),
      optional(
        seq(
          sep1($.value_argument, repeat($._NL), ",", repeat($._NL)),
          optional(seq(repeat($._NL), ",")),
          repeat($._NL),
        )
      ),
      ")"
    ),

    value_argument: $ => seq(
      optional($.annotation),
      repeat($._NL),
      optional(seq($.simple_identifier, repeat($._NL), $._ASSIGNMENT)),
      optional("*"),
      repeat($._NL),
      $.expression
    ),

    _primary_expression: $ => choice(
      $._common_primary_expression,
      $.call_expression,
    ),

    _common_primary_expression: $ => choice(
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
      $.index_access_expression
    ),

    parenthesized_expression: $ => seq("(", repeat($._NL), $.expression, repeat($._NL), ")"),

    collection_literal: $ => seq(
      "[", 
      repeat($._NL),
      optional(seq(
        sep1($.expression, repeat($._NL), ",", repeat($._NL)),
        repeat($._NL),
        optional(","),
      )),
      repeat($._NL),
      "]"
    ),

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
      seq("${", repeat($._NL), alias($.expression, $.interpolated_expression), repeat($._NL), "}"),
      seq("$", alias($.simple_identifier, $.interpolated_identifier))
    ),

    lambda_literal: $ => seq(
      "{",
      repeat($._NL),
      optional(seq(optional(field('parameters', $.lambda_parameters)), $._ARROW, repeat($._NL))),
      optional(field('body', $.statements)),
      "}"
    ),

    multi_variable_declaration: $ => seq(
      "(",
      repeat($._NL),
      $.variable_declaration, 
      repeat(seq(repeat($._NL), ',', repeat($._NL), $.variable_declaration)),
      optional(seq(repeat($._NL), ",")),
      repeat($._NL),
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
      optional(seq(":", repeat($._NL), $._delegation_specifiers)),
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
      repeat($._NL),
      "(",
      repeat($._NL),
      field('condition', $.expression), 
      repeat($._NL),
      ")",
      repeat($._NL),
      choice(
        field('consequence', $._if_block),
        seq(
          optional(field('consequence', $._if_block)),
          $._ELSE,
          field('alternative', $._if_block)
        )
      )
    )),

    _if_block: $ => choice(
      $.expression, 
      $.assignment, 
      $.block,
      ";"
    ),

    when_subject: $ => seq(
      "(",
      repeat($._NL),
      optional(seq(
        repeat($.annotation),
        "val",
        $.variable_declaration,
        $._ASSIGNMENT
      )),
      $.expression,
      ")",
    ),

    when_expression: $ => seq(
      "when",
      repeat($._NL),
      optional($.when_subject),
      "{",
      repeat($._NL),
      repeat($.when_entry),
      "}"
    ),

    when_entry: $ => seq(
      choice(
        seq(
          sep1($.when_condition, repeat($._NL), ",", repeat($._NL)), 
          repeat($._NL),
          optional(","),
        ),
        $._ELSE
      ),
      $._ARROW,
      repeat($._NL),
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

    try_expression: $ => prec.right(seq(
      "try",
      repeat($._NL),
      $.block,
      choice(
        seq(repeat($._NL), repeat1($.catch_block), optional($.finally_block)),
        seq(repeat($._NL), $.finally_block)
      )
    )),

    catch_block: $ => seq(
      "catch",
      "(",
      repeat($._NL),
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

    callable_reference: $ => prec(PREC.DOT, seq(
      optional(choice(seq($.user_type, optional("?")), $.this_expression)),
      "::",
      choice($.simple_identifier, "class")
    )),

    _assignment_and_operator: $ => choice("+=", "-=", "*=", "/=", "%="),

    _equality_operator: $ => choice("!=", "!==", "==", "==="),

    _comparison_operator: $ => choice("<", ">", "<=", ">="),

    _in_operator: $ => choice("in", "!in"),

    _is_operator: $ => choice("is", "!is"),

    _additive_operator: $ => choice($._bin_plus, $._bin_min),

    _multiplicative_operator: $ => choice("*", "/", "%"),

    _as_operator: $ => choice("as", "as?"),

    prefix_unary_operator: $ => choice("++", "--", "-", "+", "!"),

    postfix_unary_operator: $ => choice("++", "--", "!!"),

    _directly_assignable_expression: $ => prec(
      PREC.ASSIGNMENT,
      choice(
        $.dot_qualified_expression,
        $.index_access_expression,
        $.simple_identifier,
        $.postfix_expression,
      )
    ),

    // ==========
    // Modifiers
    // ==========

    modifiers: $ => repeat1(choice($.annotation, $._modifier)),

    parameter_modifiers: $ => prec.right(repeat1(choice($.annotation, $.parameter_modifier))),

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

    annotation: $ => seq(
      choice($._single_annotation, $._multi_annotation),
      repeat($._NL)
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
      repeat($._NL),
      ":"
    ),

    _unescaped_annotation: $ => choice(
      $.constructor_invocation,
      $.user_type
    ),

    // ==========
    // Identifiers
    // ==========

    simple_identifier: $ => choice($._lexical_identifier, $._soft_keywords),

    _soft_keywords: $ => choice(
      "by",
      "catch",
      "constructor",
      "delegate",
      "dyanamic",
      "field",
      "file",
      "finally",
      "get",
      "import",
      "init",
      "param",
      "property",
      "receiver",
      "set",
      "setparam",
      "value",
      "where",
      // modifier keywords
      "abstract",
      "actual",
      "annotation",
      "companion",
      "const",
      "crossline",
      "data",
      "enum",
      "expect",
      "data",
      "inner",
      "internal",
      "lateinit",
      "noinline",
      "open",
      "operator",
      "out",
      "override",
      "private",
      "protected",
      "public",
      "reified",
      "sealed",
      "suspend",
      "tailrec",
      "vararg"),

    identifier: $ => sep1($.simple_identifier, "."),

    // ====================
    // Lexical grammar
    // ====================

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

    _DOT: $ => /\s+\./,
    _ELVIS: $ => /\s*\?:/,
    _CONJ: $ => /\s*&&/,
    _DISJ: $ => /\s*\|\|/,
    _NL: $ => /\r?\n/,
    _ELSE: $ => token(prec(1, /\s*else\s*/)),
    _ARROW: $ => /\s*->/,
    _ASSIGNMENT: $ => /=\s*/,
    _semi: $ => seq(choice(";", $._NL), repeat($._NL)),
    _semis: $ => repeat1(choice(";", $._NL)),

    line_comment: $ => token(seq('//', /[^\r\n]*/)),

    // We need to consume all the newlines after the commend, otherwise, the comments
    // may be inserted in unwanted places. Comments (and other extra) nodes are inserted
    // after tokens.
    multiline_comment: $ => seq(
      token("/*"), 
      repeat(choice($._NL, /./)), 
      token("*/"),
    )
  }
});

function sep1(rule, ...separator) {
  return seq(rule, repeat(seq(...separator, rule)));
}
