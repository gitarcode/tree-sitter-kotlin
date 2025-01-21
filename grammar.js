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
  GENERIC: 3,
  STRING_CONTENT: 1,
};
const DEC_DIGITS = token(sep1(/[0-9]+/, /_+/));
const HEX_DIGITS = token(sep1(/[0-9a-fA-F]+/, /_+/));
const BIN_DIGIT = /[01]/;
const BIN_DIGIT_OR_SEPARATOR = /[01_]/;
const REAL_EXPONENT = token(seq(/[eE]/, optional(/[+-]/), DEC_DIGITS));

const uni_character_literal = token(seq(
  "\\u",
  /[0-9a-fA-F]{4}/
));

const escaped_identifier = /\\[tbrn'"\\$]/;

// Here, we should only match the '$' character if it's not followed by an alpha character
// If it is, it should be matched as part of the _interpolation rule.
const DOLLAR_IN_STRING_CONTENT = choice(
  token(/\$[^\p{L}_{"]/),
  token(seq("$", escaped_identifier))
);

const QUOTE_IN_MULTI_LINE_STRING_CONTENT = token(/"[^"]|""[^"]/);

const NON_NL_WHITESPACE = /[ \t\v\f\u00a0\u2000\u2001\u2002\u2003\u2004\u2005\u2006\u2007\u2008\u2009\u200a\u200b\u2028\u2029\u3000]/;

module.exports = grammar({
  name: "kotlin",

  conflicts: $ => [
    [$.function_declaration],
    [$.class_declaration_no_body, $.class_declaration],

    // @Type(... could either be an annotation constructor invocation or an annotated expression
    [$.constructor_invocation, $._unescaped_annotation],

    [$.type_parameter_modifiers],
    [$.type_projection_modifiers],

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
    //   - 'import'  (identifier  simple_identifier)  •  "."  …
    // By defining a conflict here, we let the parser to continue. The second path
    // eventually dies if there is no "."
    [$.identifier],

    [$._simple_user_type],

    [$.annotated_expression, $.when_subject],
    [$._annotation],
    [$.variable_declaration],
    [$.class_parameter],
    [$.collection_literal],
    [$.variable_declaration, $._simple_user_type],
    [$.value_argument],
    [$.property_declaration],
    [$._enum_entries],
    [$.type_constraints],
    [$.do_while_statement],
    [$.function_value_parameters],
    [$.class_parameters],
    [$.source_file],
    [$._type_reference, $.parenthesized_user_type],
    [$.type_arguments, $._comparison_operator],
    [$.call_expression, $.prefix_expression, $.comparison_expression],
    [$.call_expression, $.elvis_expression, $.comparison_expression],
    [$.call_expression, $.range_expression, $.comparison_expression],
    [$.call_expression, $.in_expression, $.comparison_expression],
    [$.call_expression, $.additive_expression, $.comparison_expression],
    [$.call_expression, $.multiplicative_expression, $.comparison_expression],
    [$.call_expression, $.infix_expression, $.comparison_expression],
    [$.annotated_lambda, $.modifiers],
    [$.function_type_parameters, $.parenthesized_type],
    [$._setter_getter],
    [$.if_expression],
    [$.secondary_constructor],
    [$.type_parameter],
    [$.explicit_delegation, $._unary_expression],
    [$.parameter, $._simple_user_type],
    [$.function_type_parameters],
    [$.enum_class_body],
    [$.package_header],
    [$.when_entry],
    [$.receiver_type, $._type],
    [$.receiver_type],
    [$.statements],
    // There is an inherent ambiguity between annotated expressions and annotated function types:
    // `@Annotation() f` should be parsed as an annotated expression while
    // `@Annotation () -> Int` should be parsed as a function type
    [$.annotated_expression, $.value_argument],
    [$._annotated_delegation_specifier, $._type_modifier],
    [$.call_expression, $.labeled_expression, $.comparison_expression],
    [$.class_body, $.enum_class_body],
    [$._when_entry],
    [$.when_entries],
    [$.when_expression],
    [$.variable_declaration, $._type_modifier],
    [$.variable_declaration, $.annotated_expression],
    [$.lambda_parameters],

    [$.import_header, $._soft_keywords],
    [$.function_modifier, $._soft_keywords],
    [$.class_modifier, $._soft_keywords],
    [$.member_modifier, $._soft_keywords],
    [$.visibility_modifier, $._soft_keywords],
    [$.property_modifier, $._soft_keywords],
    [$.inheritance_modifier, $._soft_keywords],
    [$.parameter_modifier, $._soft_keywords],
    [$.platform_modifier, $._soft_keywords],
    [$.use_site_target, $._soft_keywords],
    [$._type_modifier, $._soft_keywords],
    [$.variance_modifier, $._soft_keywords],
    [$.reification_modifier, $._soft_keywords],
    [$.secondary_constructor, $._soft_keywords],
    [$.anonymous_initializer, $._soft_keywords],
    [$.getter, $._soft_keywords],
    [$.setter, $._soft_keywords],

    [$.dot_qualified_expression, $.labeled_expression, $.as_expression, $.multiplicative_expression, $.additive_expression, $.range_expression, $.infix_expression, $.elvis_expression, $.in_expression, $.is_expression, $.comparison_expression, $.equality_expression, $.conjunction_expression, $.disjunction_expression],
    [$.dot_qualified_expression, $.prefix_expression, $.as_expression, $.multiplicative_expression, $.additive_expression, $.range_expression, $.infix_expression, $.elvis_expression, $.in_expression, $.is_expression, $.comparison_expression, $.equality_expression, $.conjunction_expression, $.disjunction_expression],
    [$.dot_qualified_expression, $.as_expression, $.multiplicative_expression, $.additive_expression, $.range_expression, $.infix_expression, $.elvis_expression, $.in_expression, $.is_expression, $.comparison_expression, $.equality_expression, $.conjunction_expression, $.disjunction_expression],

    [$._simple_user_type, $._non_call_primary_expression],
    [$.variable_declaration, $._non_call_primary_expression],
    // In `f(a)` it's not clear if a should be reduced to an expression and be the argument value
    // or the name of the argument.
    [$.value_argument, $._non_call_primary_expression],
    [$.explicit_delegation, $._primary_expression],

    [$._annotated_delegation_specifier],
    [$._top_level_statements],
    [$.statements, $._semi],

    [$.delegation_specifier, $.constructor_invocation],
    [$.delegation_specifier, $.explicit_delegation],

    [$.simple_call_expression, $._call_arguments],
    [$.annotated_expression, $.modifiers],
    [$.variable_declaration, $.annotated_expression, $.modifiers],
    [$._non_call_primary_expression, $.callable_reference],
    [$.delegation_specifier, $.constructor_invocation, $.explicit_delegation],
  ],

  precedences: $ => [
    ["index",
      "call",
      "dot",
      "postfix",
      "prefix",
      "label",
      "as",
      "mul",
      "add",
      "range",
      "infix",
      "elvis",
      "check",
      "comparison",
      "equality",
      "conjunction",
      "disjunction",
      "annotation",
      "jump",
      "assignment",
      "control",
      "function_body",
      "property_declaration",
      "property_delegate",
      "top_level"
     ],
  ],

  extras: $ => [
    $.line_comment,
    $.multiline_comment,
    NON_NL_WHITESPACE
  ],

  externals: $ => [
    $._ELVIS,
    $._external_nl,
    $.multiline_comment,
    "(",
    ")",
    "{",
    "}",
    "[",
    "]",
    "${"
  ],

  supertypes: $ => [
    $.expression,
    $.jump_expression,
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
      repeat($._NL),
      optional(field("package", $.package_header)),
      repeat($._NL),
      repeat(seq($.import_header, optional(seq($._semi, repeat($._NL))))),
      // In principle, we either parse a Kotlin file (.kt) or a Kotlin script (.kts).
      // Statements cannot appear as top-level constructs in Kotlin files, only in scripts.
      // However, here, we're allowing parsing of both statements and declarations as top level.
      repeat(choice($._top_level_object, $._top_level_statements))
    ),

    shebang_line: $ => seq("#!", /[^\r\n]*/),

    file_annotation: $ => seq(
      "@", "file", ":",
      choice(
        seq("[", repeat1($._unescaped_annotation), "]"),
        $._unescaped_annotation
      ),
      $._semi,
    ),

    package_header: $ => seq("package", $.identifier, optional(seq(repeat($._NL), ";"))),

    import_header: $ => prec.left(seq(
      "import",
      field("name", $.identifier),
      optional(choice(seq(".", $.wildcard_import), $._import_alias)),
    )),

    wildcard_import: $ => seq(repeat($._NL), "*"),

    _import_alias: $ => seq("as", field("alias", $.simple_identifier)),

    _top_level_object: $ => seq($._declaration, repeat($._semi)),

    _top_level_statements: $ => seq(
      sep1(alias($._top_level_statement, $.statement), repeat1($._semi)),
      repeat($._semi)
    ),

    _top_level_statement: $ => prec("top_level", $.expression), 

    type_alias: $ => prec.right(seq(
      optional(field("modifiers", $.modifiers)),
      "typealias",
      alias($.simple_identifier, $.type_identifier),
      optional($.type_parameters),
      repeat($._NL),
      "=",
      repeat($._NL),
      $._type
    )),

    _declaration: $ => choice(
      $.class_declaration,
      alias($.class_declaration_no_body, $.class_declaration),
      $.object_declaration,
      $.function_declaration,
      $.property_declaration,
      $.type_alias
    ),

    // ==========
    // Classes
    // ==========

    class_declaration_no_body: $ => prec.dynamic(-1, seq(
      optional(field("modifiers", $.modifiers)),
      choice("class", seq(optional(seq("fun", repeat($._NL))), "interface")),
      repeat($._NL),
      field("name", $.simple_identifier),
      optional(seq(repeat($._NL), $.type_parameters)),
      optional(seq(repeat($._NL), field("primary_constructor", $.primary_constructor))),
      optional(seq(repeat($._NL), ":", repeat($._NL),  $._delegation_specifiers)),
      optional(seq(repeat($._NL), $.type_constraints)),
    )),
    
    class_declaration: $ => seq(
      optional(field("modifiers", $.modifiers)),
      choice("class", seq(optional(seq("fun", repeat($._NL))), "interface")),
      repeat($._NL),
      field("name", $.simple_identifier),
      optional(seq(repeat($._NL), $.type_parameters)),
      optional(seq(repeat($._NL), field("primary_constructor", $.primary_constructor))),
      optional(seq(repeat($._NL), ":", repeat($._NL),  $._delegation_specifiers)),
      optional(seq(repeat($._NL), $.type_constraints)),
      repeat($._NL), 
      field("body", choice($.class_body, alias($.enum_class_body, $.class_body))),
    ),

    primary_constructor: $ => seq(
      optional(seq(optional(field("modifiers", $.modifiers)), "constructor", repeat($._NL))),
      $.class_parameters
    ),

    class_body: $ => seq(
      "{",
      repeat($._NL),
      repeat($._class_member_declaration),
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
      optional(field("modifiers", $.modifiers)),
      optional(field("binding_pattern", $.binding_pattern_kind)),
      field("name", $.simple_identifier),
      ":",
      field("type", $._type),
      optional(seq(repeat($._NL), "=", repeat($._NL), field("initializer", $.expression)))
    ),

    binding_pattern_kind: $ => choice("val", "var"),

    _delegation_specifiers: $ => sep1(
      $._annotated_delegation_specifier, 
      ",",
      repeat($._NL),
    ),

    _annotated_delegation_specifier: $ => prec.left(seq(repeat($._annotation), repeat($._NL), $.delegation_specifier, repeat($._NL))),

    delegation_specifier: $ => choice(
      $.constructor_invocation,
      $.explicit_delegation,
      $.user_type,
      $.function_type
    ),

    constructor_invocation: $ => seq(
      field("type", $.user_type),
      repeat($._NL),
      field("args", $.value_arguments)
    ),

    explicit_delegation: $ => seq(
      choice(
        $.user_type,
        $.function_type
      ),
      repeat($._NL),
      "by",
      repeat($._NL),
      choice(
        $._non_call_primary_expression,
        alias($.simple_call_expression, $.call_expression),
        $.as_expression
      )
    ),

    type_parameters: $ => seq(
      "<", 
      repeat($._NL),
      sep1($.type_parameter, repeat($._NL), ",", repeat($._NL)), 
      repeat($._NL),
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
      repeat($._annotation),
      alias($.simple_identifier, $.type_identifier),
      repeat($._NL),
      ":",
      repeat($._NL),
      $._type
    ),

    // ==========
    // Class members
    // ==========

    _class_member_declaration: $ => seq(choice(
      $._declaration,
      $.companion_object,
      $.anonymous_initializer,
      $.secondary_constructor,
    ), repeat($._semi)),

    anonymous_initializer: $ => seq("init", $.block),

    companion_object: $ => prec.right(seq(
      optional(field("modifiers", $.modifiers)),
      "companion",
      "object",
      optional(field("name", $.simple_identifier)),
      optional(seq(":", repeat($._NL), $._delegation_specifiers)),
      optional(field("body", $.class_body)),
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
      optional(field("modifiers", $.parameter_modifiers)),
      field("parameter", $.parameter),
      optional(seq("=", repeat($._NL), field("initializer", $.expression)))
    ),

    receiver_type: $ => seq(
      optional(field("modifiers", $.type_modifiers)),
      choice(
        $._type_reference,
        $.parenthesized_type,
        $.nullable_type
      )
    ),

    function_declaration: $ => seq(
      optional(field("modifiers", $.modifiers)),
      "fun",
      optional($.type_parameters),
      optional(seq(repeat($._NL), field("receiver_type", $.receiver_type), repeat($._NL), optional("."))),
      field("name", $.simple_identifier),
      field("parameters", $.function_value_parameters),
      optional(seq(repeat($._NL), ":", repeat($._NL), field("type", $._type))),
      optional(seq(repeat($._NL), $.type_constraints)),
      optional(seq(repeat($._NL), field("body", prec.dynamic(100, $.function_body))))
    ),

    function_body: $ => prec("function_body", choice(
      $.block, 
      seq("=", repeat($._NL), field("expression", $.expression))
    )),

    variable_declaration: $ => seq(
      repeat($._annotation),
      repeat($._NL),
      field("id", $.simple_identifier),
      optional(field("type", seq(repeat($._NL), ":", repeat($._NL), $._type)))
    ),

    property_declaration: $ => prec("property_declaration", seq(
      optional(field("modifiers", $.modifiers)),
      field("binding_pattern", $.binding_pattern_kind),
      optional(field("type_parameters", $.type_parameters)),
      optional(seq(field("receiver_type", $.receiver_type), ".")),
      field("var_decl", choice($.variable_declaration, $.multi_variable_declaration)),
      optional(field("type_constraints", $.type_constraints)),
      optional(seq(
        repeat($._NL),
        choice(
          seq("=", repeat($._NL), field("initializer", $.expression)),
          field("delegate", $.property_delegate)
      ))),
      optional(
        seq(      
          repeat($._NL),
          optional(";"),
          repeat($._NL),
          $._setter_getter
        )
      )
    )),

    property_delegate: $ => prec("property_delegate", 
      seq(
        field("by", $.by), 
        repeat($._NL), 
        $.expression
      )
    ),
    
    by: $ => "by",

    _setter_getter: $ => choice(
      seq($.getter, optional(seq(repeat($._NL), $.setter))),
      seq($.setter, optional(seq(repeat($._NL), $.getter)))
    ),

    getter: $ => prec.right(seq(
      optional(field("modifiers", $.modifiers)),
      prec.dynamic(1, "get"),
      optional(seq(
        "(",
        repeat($._NL),
        ")",
        optional(seq(":", $._type)),
        $.function_body
      ))
    )),

    setter: $ => prec.right(seq(
      optional(field("modifiers", $.modifiers)),
      prec.dynamic(1, "set"),
      optional(seq(
        "(",
        repeat($._NL),
        $.parameter_with_optional_type,
        ")",
        optional(seq(":", $._type)),
        repeat($._NL),
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
      field("name", $.simple_identifier), 
      repeat($._NL),
      ":", 
      repeat($._NL),
      field("type", $._type)
    ),

    object_declaration: $ => prec.right(seq(
      optional(field("modifiers", $.modifiers)),
      "object",
      field("name", $.simple_identifier),
      optional(seq(":", repeat($._NL), $._delegation_specifiers)),
      optional(field("body", $.class_body))
    )),

    secondary_constructor: $ => seq(
      optional(field("modifiers", $.modifiers)),
      "constructor",
      repeat($._NL),
      field("parameters", $.function_value_parameters),
      optional(seq(repeat($._NL), ":", repeat($._NL), $.constructor_delegation_call)),
      optional(field("block", $.block))
    ),

    constructor_delegation_call: $ => seq(choice("this", "super"), $.value_arguments),

    // ==========
    // Enum classes
    // ==========

    enum_class_body: $ => seq(
      "{",
      repeat($._NL),
      optional($._enum_entries),
      repeat($._NL),
      optional(","),
      repeat($._NL),
      optional(seq(";", repeat($._NL), repeat($._class_member_declaration))),
      "}"
    ),

    _enum_entries: $ => sep1(
      $.enum_entry, repeat($._NL), ",", repeat($._NL),
    ),

    enum_entry: $ => seq(
      optional(field("modifiers", $.modifiers)),
      field("name", $.simple_identifier),
      optional(field("arguments", $.value_arguments)),
      optional(field("body", $.class_body))
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
    _type_reference: $ => choice(
      field("type", $.user_type),
      "dynamic"
    ),

    not_nullable_type: $ => seq(
      optional($.type_modifiers),
      choice($.user_type, $.parenthesized_user_type),
      "&",
      optional($.type_modifiers),
      choice($.user_type, $.parenthesized_user_type),
    ),

    nullable_type: $ => seq(
      choice($._type_reference, $.parenthesized_type),
      repeat1("?")
    ),

    user_type: $ => seq(
      $._simple_user_type, repeat(seq(".", repeat($._NL), $._simple_user_type))
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
      optional(seq($.receiver_type, ".", repeat($._NL))), 
      $.function_type_parameters,
      repeat($._NL),
      "->",
      repeat($._NL),
      $._type
    ),

    function_type_parameters: $ => seq(
      "(",
      repeat($._NL),
      optional(sep1(choice($.parameter, $._type), repeat($._NL), ",", repeat($._NL))),
      repeat($._NL),
      ")"
    ),

    parenthesized_type: $ => seq("(", repeat($._NL), $._type, repeat($._NL), ")"),

    parenthesized_user_type: $ => seq(
      "(",
      repeat($._NL),
      choice($.user_type, $.parenthesized_user_type),
      repeat($._NL),
      ")"
    ),

    // ==========
    // Statements
    // ==========

    _statements: $ => seq($.statements, repeat($._semi)),

    statements: $ => seq(sep1($.statement, repeat1($._semi)), optional(seq(repeat($._NL), ";"))),

    statement: $ => choice(
      $._declaration,
      $.expression
    ),

    label: $ => token(seq(
      /[a-zA-Z_][a-zA-Z_0-9]*/,
      "@"
    )),

    block: $ => seq(
      "{",
      repeat($._NL),
      optional($._statements),
      "}"
    ),

    _loop_statement: $ => choice(
      $.for_statement,
      $.while_statement,
      $.do_while_statement
    ),

    for_statement: $ => prec.right(seq(
      "for",
      repeat($._NL),
      "(",
      repeat($._NL),
      $._in_expression,
      repeat($._NL),
      ")",
      repeat($._NL),
      optional(field("body", $._control_body_structure))
    )),

    _in_expression: $ => seq(
      repeat($._annotation),
      field("var_decl", choice($.variable_declaration, $.multi_variable_declaration)),
      repeat($._NL),
      "in",
      repeat($._NL),
      field("expression", $.expression),
    ),

    while_statement: $ => seq(
      "while",
      "(",
      repeat($._NL),
      $.expression,
      repeat($._NL),
      ")",
      repeat($._NL),
      choice(";", $._control_body_structure)
    ),

    do_while_statement: $ => prec.right(seq(
      "do",
      repeat($._NL),
      optional($._control_body_structure),
      repeat($._NL),
      "while",
      "(",
      repeat($._NL),
      $.expression,
      ")",
    )),

    assignment: $ => prec.left("assignment", seq(
      field("left", $.expression),
      field("operator", $._assignment_operator),
      repeat($._NL),
      field("right", $.expression)
    )),

    // _directly_assignable_expression: $ => choice(
    //   $.dot_qualified_expression,
    //   $.index_access_expression,
    //   $.simple_identifier,
    //   $.postfix_expression,
    //   $.this_expression
    // ),

    // ==========
    // Expressions
    // ==========

    expression: $ => choice(
      $._unary_expression,
      $._binary_expression,
      $._primary_expression,
      $.jump_expression,
      $.assignment,
      $._loop_statement
    ),

    // Unary expressions

    _unary_expression: $ => choice(
      $.prefix_expression,
      $.postfix_expression,
      $.as_expression,
      $.is_expression,
    ),

    jump_expression: $ => choice(
      $.return_expression,
      $.throw_expression,
      $.continue_expression,
      $.break_expression
    ),

    postfix_expression: $ => prec("postfix", seq(
      field("expression", $.expression), field("operator", $._postfix_unary_operator)
    )),

    dot_qualified_expression: $ => prec("dot", seq(
      field("receiver", $.expression),
      repeat($._external_nl),
      choice(".", "?."),
      repeat($._NL),
      field("selector", choice(
        $.simple_identifier,
        $.parenthesized_expression,
        "class"
      ))
    )),

    call_expression: $ => prec("call", seq(
      field("expression", $.expression), 
      optional($.type_arguments),
      $._call_arguments
    )),

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
    // A simple_call_expression is the one without the trailing lambda argument.
    simple_call_expression: $=> prec("call", seq(
      field("expression", $.expression), 
      optional($.type_arguments),
      field("args", $.value_arguments)
    )),

    // Right precedence here to extend the call to the right, i.e., `with (s) { s }`
    // should be parsed as a flat list of arguments. 
    _call_arguments: $ => prec.right(choice(
      seq(optional(field("args", $.value_arguments)), field("lambda_arg", $.annotated_lambda)),
      field("args", $.value_arguments)
    )),
    
    index_access_expression: $ => prec("index", seq(
      field("expression", $.expression), 
      field("open_bracket", $.open_bracket),
      repeat($._NL),
      sep1(field("index", $.expression), repeat($._NL), ",", repeat($._NL)),
      repeat($._NL),
      optional(seq(",", repeat($._NL))),
      field("close_bracket", $.close_bracket),)
    ),

    open_bracket: $ => "[",

    close_bracket: $ => "]",

    annotated_expression: $ => prec("annotation", seq(
      $._annotation, $.expression
    )),

    labeled_expression: $ => prec("label", seq(
      field("label", $.label), 
      repeat($._NL), 
      field("expression", $.expression)
    )),

    prefix_expression: $ => prec("prefix", seq(
      field("operator", $._prefix_unary_operator), 
      repeat($._NL),
      field("expression", $.expression))
    ),

    as_expression: $ => prec("as", seq(
      $.expression, repeat($._external_nl), $._as_operator, repeat($._NL), $._type
    )),

    // Binary expressions

    _binary_expression: $ => choice(
      $.multiplicative_expression,
      $.additive_expression,
      $.range_expression,
      $.infix_expression,
      $.elvis_expression,
      $.comparison_expression,
      $.equality_expression,
      $.conjunction_expression,
      $.disjunction_expression,
      $.in_expression,
    ),

    multiplicative_expression: $ => prec.left("mul", seq(
      field("left", $.expression), 
      repeat($._external_nl), 
      field("operator", $._multiplicative_operator), 
      repeat($._NL), 
      field("right", $.expression)
    )),

    additive_expression: $ => prec.left("add", seq(
      field("left", $.expression), 
      repeat($._external_nl), 
      field("operator", $._additive_operator), 
      repeat($._NL), 
      field("right", $.expression)
    )),

    range_expression: $ => prec.left("range", seq(
      field("left", $.expression), 
      repeat($._external_nl), 
      field("operator", $._range_operator), 
      repeat($._NL), 
      field("right", $.expression)
    )),

    infix_expression: $ => prec.left("infix", seq(
      field("left", $.expression), 
      repeat($._external_nl), 
      field("operator", $.simple_identifier), 
      repeat($._NL), 
      field("right", $.expression)
    )),

    elvis_expression: $ => prec.left("elvis", seq(
      field("left", $.expression), 
      repeat($._external_nl), 
      field("operator", $._ELVIS), 
      repeat($._NL), 
      field("right", $.expression)
    )),

    in_expression: $ => prec.left("check", seq(
      field("left", $.expression), 
      repeat($._external_nl), 
      field("operator", $._in_operator), 
      repeat($._NL), 
      field("right", $.expression)
    )),

    is_expression: $ => prec("check", seq(
      field("left", $.expression), 
      repeat($._external_nl), 
      field("operator", $._is_operator), 
      repeat($._NL), 
      field("right", $._type)
    )),

    comparison_expression: $ => prec.left("comparison", seq(
      field("left", $.expression), 
      repeat($._external_nl), 
      field("operator", $._comparison_operator), 
      repeat($._NL), 
      field("right", $.expression)
    )),

    equality_expression: $ => prec.left("equality", seq(
      field("left", $.expression), 
      repeat($._external_nl), 
      field("operator", $._equality_operator), 
      repeat($._NL), 
      field("right", $.expression)
    )),

    conjunction_expression: $ => prec.left("conjunction", seq(
      field("left", $.expression), 
      repeat($._external_nl),
      field("operator", "&&"),
      repeat($._NL),
      field("right", $.expression))
    ),

    disjunction_expression: $ => prec.left("disjunction", seq(
      field("left", $.expression), 
      repeat($._external_nl),
      field("operator", "||"),
      repeat($._NL),
      field("right", $.expression))
    ),

    // Suffixes

    annotated_lambda: $ => seq(
      repeat($._annotation),
      optional($.label),
      field("lambda_literal", $.lambda_literal)
    ),

    // Here we're defining a dynamic precedence for type arguments to resolve the ambiguity
    // between the comparison and generic call syntax.
    // Any expression such as a<b>(c) should be resolved in favor of the generic call.
    type_arguments: $ => prec.dynamic(PREC.GENERIC, seq(
      "<", 
      repeat($._NL),
      sep1($.type_projection, repeat($._NL), ",", repeat($._NL)), 
      repeat($._NL),
      ">"
    )),

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
      optional($._annotation),
      repeat($._NL),
      // dynamic precedence so that tree-sitter doesn't parse f(a = b) as an assignment...
      optional(seq(field("name", $.simple_identifier), repeat($._NL), prec.dynamic(10, "="), repeat($._NL))),
      optional("*"),
      repeat($._NL),
      field("expression", $.expression)
    ),

    _non_call_primary_expression: $ => choice(
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
      $.index_access_expression,
      $.if_expression,
      $.annotated_expression,
      $.parenthesized_expression,
      $.labeled_expression
    ),

    _primary_expression: $ => choice(
      $.call_expression,
      $._non_call_primary_expression
    ),

    // we need to give parenthesized expression a lower dymanic precedence to resolve ambiguities like
    // @Annotation() in favor of a single annotation rather than an annotation of a parenthesized expression.
    parenthesized_expression: $ => prec.dynamic(-1, seq("(", repeat($._NL), $.expression, repeat($._NL), ")")),

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
      optional(field("content", alias($.line_string_content, $.string_content))),
      '"'
    ),

    line_string_content: $ => choice(
      seq(
        repeat1(choice(
          $.line_string_content_part, 
          DOLLAR_IN_STRING_CONTENT,
          $._interpolation
        )),
        // Need to consume the last '$' character here, and create a node in the tree
        optional("$")
      ), 
      "$"
    ),

    line_string_content_part: $ => token.immediate(prec(PREC.STRING_CONTENT, choice(
      /[^\\"$]+/,
      repeat1(uni_character_literal),
      escaped_identifier
    ))),

    _multi_line_string_literal: $ => prec.right(seq(
      '"""',
      optional(field("content", alias($.multi_line_string_content, $.string_content))),
      '"""',
      repeat('"')
    )),

    multi_line_string_content: $ => choice(
      seq(
        repeat1(choice(
          $.multi_line_string_content_part, 
          DOLLAR_IN_STRING_CONTENT, 
          QUOTE_IN_MULTI_LINE_STRING_CONTENT,
          $._interpolation,
        )),
        // Need to consume the last '$' character here, and create a node in the tree
        optional("$"),
      ),
      "$"
    ),

    multi_line_string_content_part: $ => token(prec(PREC.STRING_CONTENT, /[^"$]+/)),

    _interpolation: $ => choice(
      seq("${", repeat($._NL), alias($.expression, $.interpolated_expression), repeat($._NL), "}"),
      seq("$", alias($.simple_identifier, $.interpolated_identifier))
    ),

    lambda_literal: $ => prec(-1, seq(
      "{",
      repeat($._NL),
      optional(seq(optional(field("parameters", $.lambda_parameters)), repeat($._NL), "->", repeat($._NL))),
      optional(field("body", $._statements)),
      "}"
    )),

    multi_variable_declaration: $ => seq(
      "(",
      repeat($._NL),
      $.variable_declaration, 
      repeat(seq(repeat($._NL), ",", repeat($._NL), $.variable_declaration)),
      optional(seq(repeat($._NL), ",")),
      repeat($._NL),
      ")"
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

    if_expression: $ => seq(
      "if",
      repeat($._NL),
      $._if_condition,
      repeat($._NL),
      choice(
        field("consequence", $._control_body_structure),
        seq(
          optional(field("consequence", $._control_body_structure)),
          repeat($._NL),
          // We need to give else a higher precedence to extend the if-expression to the right
          prec.dynamic(1, "else"),
          repeat($._NL),
          field("alternative", $._control_body_structure)
        )
      )
    ),

    _if_condition: $ => seq(
      "(",
      repeat($._NL),
      field("condition", $.expression), 
      repeat($._NL),
      ")",
    ),

    _control_body_structure: $ => choice(
      prec("control", $.expression), 
      $.block,
    ),

    when_subject: $ => seq(
      "(",
      repeat($._NL),
      optional(seq(
        repeat($._annotation),
        "val",
        $.variable_declaration,
        "=",
        repeat($._NL),
      )),
      $.expression,
      repeat($._NL),
      ")",
    ),

    when_expression: $ => seq(
      "when",
      repeat($._NL),
      optional($.when_subject),
      "{",
      repeat($._NL),
      optional($.when_entries),
      repeat($._NL),
      "}"
    ),

    when_entries: $ => sep1($._when_entry, repeat($._NL)),

    _when_entry: $ => seq(
      choice(
        $.when_entry,
        $.else_entry
      ),
      optional(seq(repeat($._NL), ";"))
    ),

    when_entry: $ => seq(
      sep1($.when_condition, repeat($._NL), ",", repeat($._NL)), 
      optional(seq(repeat($._NL), optional(","))),
      repeat($._NL),
      "->",
      repeat($._NL),
      $._control_body_structure,
    ),

    else_entry: $ => seq(
      "else",
      repeat($._NL),
      "->",
      repeat($._NL),
      $._control_body_structure,
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
      repeat($._NL),
      "(",
      repeat($._NL),
      repeat($._annotation),
      field("name", $.simple_identifier),
      ":",
      field("type", $._type),
      ")",
      repeat($._NL),
      field("body", $.block),
    ),

    finally_block: $ => seq("finally", repeat($._NL), $.block),

    return_expression: $ => prec.right(seq(
      choice("return", seq("return@", $._lexical_identifier)), 
      optional($.expression)
    )),

    throw_expression: $ => prec("jump", seq(
      "throw", $.expression,
    )),

    continue_expression: $ => choice(
      "continue",
      seq("continue@", $._lexical_identifier)
    ),

    break_expression: $ => choice(
      "break",
      seq("break@", $._lexical_identifier)
    ),

    callable_reference: $ => seq(
      optional(choice(
        seq(field("receiver", $.user_type), optional(choice("?", "!!"))), 
        field("receiver", $.this_expression)
      )),
      "::",
      choice(field("name", $.simple_identifier), "class")
    ),

    _assignment_operator: $ => choice("=", "+=", "-=", "*=", "/=", "%="),

    _equality_operator: $ => choice("!=", "!==", "==", "==="),
    
    _comparison_operator: $ => choice("<", ">", "<=", ">="),

    _in_operator: $ => choice("in", "!in"),

    _is_operator: $ => choice("is", "!is"),

    _additive_operator: $ => choice("+", "-"),

    _multiplicative_operator: $ => choice("*", "/", "%"),

    _as_operator: $ => choice("as", "as?"),

    _prefix_unary_operator: $ => choice("++", "--", "-", "+", "!"),

    _postfix_unary_operator: $ => choice("++", "--", "!!"),

    _range_operator: $ => choice("..", "..<"),

    // ==========
    // Modifiers
    // ==========

    modifiers: $ => prec.right(repeat1(choice($._annotation, $._modifier))),

    parameter_modifiers: $ => prec.right(repeat1(choice($._annotation, $.parameter_modifier))),

    _modifier: $ => seq(choice(
      $.class_modifier,
      $.member_modifier,
      $.visibility_modifier,
      $.function_modifier,
      $.property_modifier,
      $.inheritance_modifier,
      $.parameter_modifier,
      $.platform_modifier
    ), repeat($._NL)),

    type_modifiers: $ => repeat1($._type_modifier),

    _type_modifier: $ => choice($._annotation, "suspend"),

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
      $._annotation
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

    _annotation: $ => seq(
      $.annotation,
      repeat($._NL)
    ),

    annotation: $ => choice($._single_annotation, $._multi_annotation),

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
      "vararg"
    ),

    identifier: $ => sep1($.simple_identifier, "."),

    // ====================
    // Lexical grammar
    // ====================

    // ==========
    // Keywords
    // ==========

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

    bin_literal: $ => token(choice(
      seq("0", /[bB]/, BIN_DIGIT, repeat(BIN_DIGIT_OR_SEPARATOR), BIN_DIGIT),
      seq("0", /[bB]/, BIN_DIGIT)
    )),

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

    _NL: $ => /\n/,
    _semi: $ => choice(";", $._NL),

    line_comment: $ => token(seq("//", /[^\r\n]*/)),
  }
});

function sep1(rule, ...separator) {
  return seq(rule, repeat(seq(...separator, rule)));
}
