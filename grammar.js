const KW = {
  AND: reserve('and'),
  ASYMETRIC: reserve('asymetric'),
  BETWEEN: reserve('between'),
  BY: reserve('by'),
  DEFAULT: reserve('default'),
  DISTINCT: reserve('distinct'),
  DOCUMENT_P: reserve('document_p'),
  EXPLAIN: reserve('explain'),
  FOR: reserve('for'),
  FROM: reserve('from'),
  GROUP: reserve('group'),
  ILIKE: reserve('ilike'),
  IN: reserve('in'),
  INTERVAL: reserve('interval'),
  IS: reserve('is'),
  ISNULL: reserve('isnull'),
  JOIN: reserve('join'),
  LIKE: reserve('like'),
  LIMIT: reserve('limit'),
  NOT: reserve('not'),
  NOTNULL: reserve('notnull'),
  OR: reserve('or'),
  ORDER: reserve('order'),
  SIMILAR: reserve('similar'),
  SYMMETRIC: reserve('symmetric'),
  TO: reserve('to'),
  USING: reserve('using'),
  VERBOSE: reserve('verbose'),
  WHERE: reserve('where'),
};

const PREC = {
  DEFAULT: 0,
  LOGICAL_OR: 1,
  LOGICAL_AND: 2,
  LOGICAL_NOT: 3,
  COMPARE: 4,
  EQUAL: 4,
  BITWISE_OR: 5,
  BITWISE_XOR: 6,
  BITWISE_AND: 7,
  SHIFT: 8,
  ADD: 9,
  SUBTRACT: 9,
  MUL: 10,
  DIV: 10,
  CONCAT: 10,
  AT: 11,
  BITWISE_NOT:12,
  UMINUS: 12,
  UPLUS: 12,
  CAST: 13,
  CALL: 15,
  PRIMARY: 16, // for field, array subscript, and json subscript
};

module.exports = grammar({
  name: 'UniverSQL',

  word: $ => $._unquoted_ident,

  inline: $ => [
    $._string_lit_or_param,
    $._array_construct_prefix,
  ],

  extras: $ => [
    /[\s\p{Zs}\uFEFF\u2060\u200B]/,
    $.comment,
  ],

  supertypes: $ => [
    $.expr,
    $._stmt,
    $._raw_type,
    $.func_expr_common_subexpr,
  ],

  rules: {
    /* Start Rule */
    // SQL can be definned as "stmt", "stmt;", "stmt;stmt" or "stmt; stmt;"
    multistmt: $ => seq(
      $._stmt,
      repeat(seq(';', $._stmt)),
      optional(';')
    ),
    _stmt: $ => choice(
      $._select_stmt,
      $.explain,
    /*  $.alter_statement,
        $.analyze_statement,
        $.assert_statement,
        $.aux_load_data_statement,
        $.clone_data_statement,
        $.dml_statement,
        $.merge_statement,
        $.truncate_statement,
        $.begin_statement,
        $.set_statement,
        $.commit_statement,
        $.start_batch_statement,
        $.run_batch_statement,
        $.abort_batch_statement,
        $.create_constant_statement,
        $.create_database_statement,
        $.create_function_statement,
        $.create_procedure_statement,
        $.create_role_statement, // Postgres
        $.create_index_statement,
        $.create_privilege_restriction_statement,
        $.create_row_access_policy_statement,
        $.create_external_table_statement,
        $.create_external_table_function_statement,
        $.create_model_statement,
        $.create_schema_statement,
        $.create_snapshot_table_statement,
        $.create_table_function_statement,
        $.create_table_statement,
        $.create_view_statement,
        $.create_entity_statement,
        $.define_table_statement,
        $.describe_statement,
        $.execute_immediate,
        $.execute_statement,
        $.explain_statement,
        $.export_data_statement,
        $.export_model_statement,
        $.fetch_statment,
        $.grant_statement,
        $.grant_role_statement, // Postgres
        $.rename_statement,
        $.revoke_statement,
        $.rollback_statement,
        $.show_statement,
        $.drop_all_row_access_policies_statement,
        $.drop_statement,
        $.call_statement,
        $.import_statement,
        $.module_statement,
        */
    ),

    /*==== Keywwords ====*/
    _kw_as: _ => token(choice(
      'as',
      'AS',
      'aS',
      'As'
    )),
    _kw_and: _ => token(prec(1, KW.AND)),
    _kw_or: _ => token(prec(1, KW.OR)),
    _kw_order: _ => token(prec(1, KW.ORDER)),
    _kw_limit: _ => token(prec(1, KW.LIMIT)),
    _kw_analyze: _ => reserveMany('analyze', 'analyse'),
    _kw_not: _ => token(prec(1, KW.NOT)),
    _kw_like_ilike: _ => token(prec(1, reserveMany('like', 'ilike'))),
    _kw_is: _ => token(prec(1, reserve('is'))),

    /*==== Literals ====*/
    null: _ => token(prec(1, reserve('null'))),
    unknow: _ => reserve('unknow'),
    boolean_literal: _ => reserveMany('true', "false"),
    
    numeric_literal: $ => seq(
      reserveMany('numeric', 'decimal'),
      $.string_literal
    ),
    bignumeric_literal: $ => seq(
      reserveMany('bigdecimal', 'bigdecimal'),
      $.string_literal
    ),
    date_or_time_literal: $ => seq(
      reserveMany('date', 'datetime', 'time', 'timestamp'),
      $.string_literal
    ),
    integer_literal: _ => choice(/\d+/, /0x[\da-fA-F]+/),

    _exp: _ => token.immediate(/e[+-]?\d+/),
    float_literal: $ => choice(
      seq(/\d+\.\d*/, optional($._exp)),
      seq(/\.\d+/, optional($._exp)),
      seq(/\d+/, $._exp)
    ),

    /* String and Bytes Literal */
    string_literal: $ => choice(
      seq(/[rR]?'/, $._single_quote_text),
      seq(/[rR]?"/, $._double_quote_text),
      seq(/[rR]?'''/, $._single_quote_3_text),
      seq(/[rR]?"""/, $._double_quote_3_text)
    ),
    bytes_literal: $ => choice(
      seq(/([bB]|[bB][rR]|[rR][bB])?'/, $._single_quote_text),
      seq(/([bB]|[bB][rR]|[rR][bB])?"/, $._double_quote_text),
      seq(/([bB]|[bB][rR]|[rR][bB])?'''/, $._single_quote_3_text),
      seq(/([bB]|[bB][rR]|[rR][bB])?"""/, $._double_quote_3_text)
    ),
    _single_quote_text: $ => seq(
      repeat(
        choice(token.immediate(prec(1, /[^\\\n\r']/)), $.escape_sequence)
      ),
      token.immediate("'")
    ),
    _double_quote_text: $ => seq(
      repeat(
        choice(token.immediate(prec(1, /[^\\\n\r"]/)), $.escape_sequence)
      ),
      token.immediate('"')
    ),
    _single_quote_3_text: $ => seq(
      repeat(
        seq(
          token.immediate(/('|'')?/),
          choice(token.immediate(prec(1, /[^\\']/)), $.escape_sequence)
        )
      ),
      token.immediate("'''")
    ),
    _double_quote_3_text: $ => seq(
      repeat(
        seq(
          token.immediate(/("|"")?/),
          choice(token.immediate(prec(1, /[^\\"]/)), $.escape_sequence)
        )
      ),
      token.immediate('"""')
    ),
    escape_sequence: _ => token(token.immediate(/\\(.|\n|\r|\r\n)/)),

    interval_literal: $ => choice(
      seq(KW.INTERVAL, $.string_literal, optional($.interval_end)),
      seq(KW.INTERVAL, '(', $.integer_literal, ')', $.string_literal),
    ),
    interval_end: $ => choice(
      $.interval_second,
      reserveMany('year', 'month', 'day', 'hour', 'minute'),
      reserveSeq('year', 'to', 'month'),
      reserveSeq('day', 'to', 'hour'),
      reserveSeq('day', 'to', 'minute'),
      reserveSeq('hour', 'to', 'minute'),
      seq(reserve('day'), KW.TO, $.interval_second),
      seq(reserve('hour'), KW.TO, $.interval_second),
      seq(reserve('minute'), KW.TO, $.interval_second)
    ),
    interval_second: $ => seq(reserve('second'), optional(seq('(', $.integer_literal, ')'))),
    range_literal: $ => seq(reserve('range'), '<', $.type, '>', $.string_literal),
    json_literal: $ => seq(reserve('json'), $.string_literal),

    /* Identifer */
    identifier: $ => choice($._unquoted_ident, $._backtick_text),
    general_identifier: $ => choice($._unquoted_general_ident, $._backtick_text),
    _unquoted_ident: _ => /[a-zA-Z_][a-zA-Z_0-9]*/,
    _unquoted_general_ident: _ => /[a-zA-Z_0-9]+/,
    _backtick_text: $ => seq(
      '`',
      repeat(choice(token.immediate(/[^\\`\r\n]/), $.escape_sequence)),
      token.immediate('`')
    ),

    /* Type */
    // Unlike other type names, 'INTERVAL' is a reserved keyword.
    _type_name: $ => choice($.path_expr, KW.INTERVAL),
    array_type: $ => seq(token(prec(1, reserve('array'))), '<', $.type, '>'),
    struct_field: $ => seq(optional($.identifier), $.type),
    //struct_field_list: $ => commaSep1($.struct_field),
    struct_type: $ => seq(token(prec(1, reserve('struct'))), '<', optional(commaSep1($.struct_field)), '>'), 
    _raw_type: $ => choice($.array_type, $.struct_type, $._type_name),
    _type_param_field: $ => choice(
      $.integer_literal,
      $.boolean_literal,
      $.string_literal,
      $.bytes_literal,
      $.float_literal,
      reserve('max'),
    ),
    type_param: $ => seq('(', $._type_param_field, repeat(seq(',', $._type_param_field)), ')'),
    type: $ => seq($._raw_type, optional($.type_param), optional($.collate_clause)),

    /*==== Clause ====*/
    _string_lit_or_param: $ => choice(
      $.string_literal,
      $.parameter_expr,
      $.system_variable_expr
    ),
    collate_clause: $ => seq(reserve('collate'), $._string_lit_or_param),

    /* Expression grammar
     * Take inpiration from Postgres Expression Grammar
     * Divides into a_expr, b_expr and c_expr
     * '(' a_expr ')' is a b_expr
     * a_expr will be renamed to expr
     */
    expr: $ => choice(
      $._expr_primary,
      alias($.logical_unary_expr, $.unary_expr),
      alias($.logical_binary_expr, $.binary_expr),
      $.compare_expr,
      // prec(PREC.COMPARE, seq($._expr_primary, optNot(), KW.IN, $.in_expr)),
      // DEFAULT
    ),
    logical_unary_expr: $ => prec(PREC.LOGICAL_NOT, seq($._kw_not, $.expr)),
    logical_binary_expr: $ => choice(
      prec.left(PREC.LOGICAL_AND, seq($.expr, $._kw_and, $.expr)),
      prec.left(PREC.LOGICAL_OR, seq($.expr, $._kw_or, $.expr)),
    ),
    compare_expr: $ => choice(
      prec(PREC.COMPARE, seq($._expr_primary, optNot(), $._kw_like_ilike, $._expr_primary)),
      prec(PREC.COMPARE, seq($._expr_primary, optNot(), reserve('similar'), KW.TO, $._expr_primary)),
      prec(PREC.COMPARE, seq($._expr_primary, $._kw_is, optional(KW.NOT),
        choice($.null, $.boolean_literal, $.unknow))),
      prec(PREC.COMPARE, seq($._expr_primary, choice(KW.NOTNULL, KW.ISNULL))),
      prec(PREC.COMPARE, seq($._expr_primary, optional(KW.ASYMETRIC), KW.BETWEEN, $._expr_primary, KW.AND, $._expr_primary)),
      prec(PREC.COMPARE, seq($._expr_primary, optNot(), KW.BETWEEN, KW.SYMMETRIC, $._expr_primary, KW.AND, $._expr_primary)),
      prec(PREC.COMPARE, seq($._expr_primary, $._kw_is, optNot(), optional($.unicode_normal_form), reserve('normalized'))),
    ),
    _expr_primary: $ => choice(
      $._c_expr,
      $.at_expr,
      $.pg_cast_expr,
      $.unary_expr,
      $.binary_expr,
    ),
    at_expr: $ => prec.left(PREC.AT, seq($._expr_primary, $._at_time_zone)),
    _at_time_zone: $ => seq(reserve('at'), reserve('time'), reserve('zone'), $._expr_primary),
    pg_cast_expr:  $ => prec(PREC.CAST, seq($._expr_primary, '::', $.type)),// Postgres cast "::"
    unary_expr: $ => prec(PREC.UMINUS, seq(/~|\+|-/, $._expr_primary)),
    binary_expr: $ => choice(
      prec.left(PREC.ADD, seq($._expr_primary, /\+|-/, $._expr_primary)),
      prec.left(PREC.MUL, seq($._expr_primary, /\*|\/|%|\|\|/, $._expr_primary)),
      prec.left(PREC.SHIFT, seq($._expr_primary, /<<|>>/, $._expr_primary)),
      prec.left(PREC.BITWISE_AND, seq($._expr_primary, /&/, $._expr_primary)),
      prec.left(PREC.BITWISE_OR, seq($._expr_primary, /\|/, $._expr_primary)),
      prec.left(PREC.BITWISE_XOR, seq($._expr_primary, /\^/, $._expr_primary)),
      prec.left(PREC.COMPARE, seq($._expr_primary, /=|<=?|>=?|!=|<>/, $._expr_primary)),
      prec.left(PREC.PRIMARY, seq($._expr_primary, '[', $._expr_primary, ']')),
    ),

    /*
     * Productions that can be used in both a_expr and b_expr.
     *
     * Note: productions that refer recursively to a_expr or b_expr mostly
     * cannot appear here.	However, it's OK to refer to a_exprs that occur
     * inside parentheses, such as function arguments; that cannot introduce
     * ambiguity to the b_expr syntax.
     */
    _c_expr: $ => choice(
      $.path_expr,
      $.dot_identifier,
      //$.dot_field,
      $._const_expr,
      $.paren_expr,
      $.case_expr,
      $.func_expr,
      $.array_expr
    ),
    paren_expr: $ => seq('(', $.expr ,')'),

    /* Constants */
    _const_expr: $ => choice(
      $.null,
      $.boolean_literal,
      $.integer_literal,
      $.float_literal,
      $.string_literal,
      $.bytes_literal,
      $.numeric_literal,
      $.interval_literal, // Postgres
      $.date_or_time_literal
    ),
    //TODO: add /* generic type 'literal' syntax */, and /* generic syntax with a type modifier */
    _numeric_only: $ => choice(
      $.float_literal,
      seq(/\+|-/, $.float_literal),
      $.integer_literal,
      seq(/\+|-/, $.interval_literal)
    ),
    dot_identifier: $ => prec(PREC.PRIMARY, seq($._expr_primary, '.', $.identifier)),
    dot_field: $ => prec(PREC.PRIMARY, seq($.expr, '.', '(', $.path_expr, ')')),

    /* Function Expression */
    func_expr: $ => choice(
      seq($.func_app, optional($.within_group_clause), optional($.filter_clause), optional($._over_clause)),
      $.func_expr_common_subexpr
    ),
    func_app: $ => seq(field('name', $.path_expr), '(', optional(choice(
      seq(alias($.func_arg_list_variadic, $.func_arg_list), optOrderBy($)),
      seq($.func_arg_variadic, optOrderBy($)),
      seq($.set_quantifier, $.func_arg_list, optOrderBy($)),
    )), ')'),
    func_arg_variadic: $ => seq(reserve('variadic'), $._func_arg_expr),
    // The first argument may be a "*" instead of an expression
    // This is valid for COUNT(*), which has no other arguments
    // and ANON_COUNT(*), which has multiple other arguments.
    func_arg_list: $ => commaSep1($._func_arg_expr),
    func_arg_list_variadic: $ => seq(
      choice($._func_arg_expr, $.star), repeat(seq(',', $._func_arg_expr)), optional(seq(',', $.func_arg_variadic))
    ),
    _func_arg_expr: $ => choice(
      $.expr,
      seq($.path_expr, ':=', $.expr),
      seq($.path_expr, '=>', $.expr)
    ),

    /* Special expressions that are considered to be functions.*/
    func_expr_common_subexpr: $ => choice(
      $.parenless_func_expr,
      $.collation_expr,
      $.current_time_expr,
      $.cast_expr,
      $.extract_expr,
      $.substr_expr,
      $.trim_expr,
      $.normalize_expr,
      $.overlay_expr,
      $.position_expr,
      $.treat_expr,
      $.xml_expr,
    ),

    /*
     * As func_expr but does not accept WINDOW functions directly
     * (but they can still be contained in arguments for functions etc).
     * Use this when window expressions are not allowed, where needed to
     */
    func_expr_windowless: $ => choice($.func_app, $.func_expr_common_subexpr),

    /*
     * func_alias_clause can include both an Alias and a coldeflist, so we make it
     * return a 2-element list that gets disassembled by calling production.
     */
    func_alias_clause: $ => choice(
			$._alias_clause,
			seq($._kw_as, optional($.identifier), '(', $.table_func_element_list, ')'),
			seq($.identifier, '(', $.table_func_element_list, ')'),
    ),

    /* column_ref: $ => choice(
      $.path_expr,
      seq($.path_expr, '.', '*'),
    ), */
    overlay_list: $ => seq(
      $.expr, reserve('placing'), $.expr, KW.FROM, $.expr,
      optional(seq(KW.FOR, $.expr))
    ),
    case_expr: $ => seq(
      reserve('case'), optional($.expr),
      repeat1(seq(reserve('when'), $.expr, reserve('then'), $.expr)),
      optional(seq(reserve('else'), $.expr)),
      reserve('end')
    ),
    unicode_normal_form: _ => reserveMany('nfc', 'nfd', 'nfkc', 'nfkd'), 
    path_expr: $ => prec.right(seq($.identifier, repeat(seq('.', $.identifier)))),
    unnest_expr: $ => seq(reserve('unnest'), '(', $.expr, ')'),
    // convert_time_zone: $ => prec.left(, seq($.expr, $._at_time_zone)),
    // _at_time_zone: $ => seq(reserve('at'), reserve('time'), reserve('zone'), $._expr_primary),
    _format: $ => seq(reserve('format'), $.expr),
    cast_expr: $ => seq(
      token(prec(1, reserveMany('cast', 'safe_cast'))),
      '(', $.expr, $._kw_as, $.type, optional(field('format', $._format)), ')'
    ),
    extract_expr: $ => seq(
      reserve('extract'), '(', $.expr, KW.FROM, $.expr, optional($._at_time_zone), ')'
    ),
    substr_list: $ => choice(
      seq($.expr, KW.FROM, $.expr, optional(seq(KW.FOR, $.expr))),
      seq($.expr, KW.FOR, $.expr, KW.FROM,  $.expr),
      seq($.expr, reserve('similar'), $.expr, reserve('escape'), $.expr)
    ),
    substr_expr: $ => seq(reserve('substring'), '(', choice(
      $.substr_list, optional($.func_arg_list)
    ), ')'),
    trim_list: $ => choice(
      seq($.expr, KW.FROM, $.expr_list),
      seq(KW.FROM, $.expr_list),
      $.expr_list
    ),
    trim_expr: $ => seq(reserve('trim'), '(', optional(reserveMany('both', 'leading', 'trailing')), $.trim_list, ')'),
    normalize_expr: $ => seq(reserve('normalize'), '(', $.expr, optional(seq(',', $.unicode_normal_form)),')'),
    overlay_expr: $ => seq(reserve('overlay'), '(', optional(choice($.overlay_list, $.func_arg_list)), ')'),
    position_expr: $ => seq(reserve('position'), '(', $._expr_primary, KW.IN, $._expr_primary, ')'),
    treat_expr: $ => seq(reserve('treat'), '(', $.expr, $._kw_as, $.type, ')'),
    current_time_expr: $ =>  seq(
      reserveMany('current_time', 'current_timestamp', 'localtime', 'localtimestamp'),
      optional(seq('(', $.integer_literal, ')'))
    ),
    collation_expr: $ => seq(reserve('collation'), KW.FOR, '(', $.expr, ')'),
    parenless_func_expr: _ => reserveMany(
      'current_date',
      'current_role', 'current_user', 'session_user', 'system_user', 'user',
      'current_catalog', 'current_schema'
    ),
    // Postgres XML function
    xml_expr: $ => choice(
      seq(reserve('xmlconcat'), '(', $.expr_list, ')'),
      seq(reserve('xmlelement'), '(', reserve('name'), $.path_expr, optional(choice(
        seq(',', $.xml_attributes),
        seq(',', $.expr_list),
        seq(',', $.xml_attributes, ',', $.expr_list)
      )), ')'),
      seq(reserve('xmlexists'), '(', $._c_expr, $.xmlexists_arg, ')'),
      seq(reserve('xmlforest'), '(', $.xml_attribute_list, ')'),
      seq(reserve('xmlparse'), '(', $.document_or_content, $.expr, optional($.xml_whitespace_option), ')'),
      seq(reserve('xmlpi'), '(', reserve('name'), $.path_expr, optional(seq(',', $.expr)), ')'),
      seq(reserve('xmlroot'), '(', $.expr ,',', $.xml_root_version, optional($.xml_root_standalone), ')'),
      seq(reserve('xmlserialize'), '(', $.document_or_content, $.expr, $._kw_as, $.type, ')')
    ),

    xml_root_version: $ => choice(
      seq(reserve('version'), $.string_literal),
      seq(reserve('version'), reserve('no'), reserve('value'))
    ),
    xml_root_standalone: _ => seq(',', reserve('standalone'), choice(
      reserve('yes'),
      reserve('no'),
      reserveSeq('no', 'value')
    )),
    xml_attributes: $ => seq(reserve('xmlattributes') ,'(', $.xml_attribute_list, ')'),
    xml_attribute_list:	$ => seq($.xml_attribute_el, optional(seq(',', $.xml_attribute_el))),
    xml_attribute_el: $ => seq($.expr, optional(seq($._kw_as, $.path_expr))),
    document_or_content:  _ => reserveMany('document', 'content'),
    xml_whitespace_option: _ => seq(reserveMany('preserve', 'strip'), reserve('whitespace')),
    // We allow several variants for SQL and other compatibility.
    xmlexists_arg: $ => seq(reserve('passing'), choice(
      $._c_expr,
      seq($._c_expr, $.xml_passing_mech),
      seq($.xml_passing_mech, $._c_expr),
      seq($.xml_passing_mech, $._c_expr, $.xml_passing_mech)
    )),
    xml_passing_mech: _ => seq(reserve('by'), reserveMany('ref', 'value')),

    /* Postgres XML Table */
    xmltable: $ => seq(
      reserve('xmltable'), '(',
      optional(seq(reserve('xmlnamespaces', '(', $.xml_namespace_list, ')', ','))),
      $._c_expr, $.xmlexists_argument, reserve('columns'), $.xmltable_column_list,
      ')'
    ),
    xml_namespace_list: $ => commaSep1($.xml_namespace_element),
    xml_namespace_element: $ => choice(
      seq($._expr_primary, $._kw_as, $.identifier),
      seq(KW.DEFAULT, $._expr_primary),
    ),
    xmlexists_argument: $ => seq(reserve('passing'), choice(
      seq($._c_expr, optional($.xml_passing_mech)),
      seq($.xml_passing_mech, $._c_expr, $.xml_passing_mech)
    )),
    xmltable_column_list: $ => commaSep1($.xmltable_column_element),
    xmltable_column_element: $ => choice(
      seq($.identifier, $._type_name, $.xmltable_column_option_list),
      seq($.identifier, KW.FOR, reserve('ordinality')),
    ),
    xmltable_column_option_list: $ => repeat1($.xmltable_column_option_element),
    xmltable_column_option_element: $ => choice(
      seq($.identifier, $._expr_primary),
			seq(KW.DEFAULT, $._expr_primary),
      reserveSeq('not', 'null'),
      reserve('null')
    ),

    /* Common Clause */
    _order_by_clause: $ => field('order', seq($._kw_order, KW.BY, $.order_by_list)),
    order_by_list: $ => commaSep1($._order_by_item),
    _order_by_item:	$ => seq($.expr, optional(reserveMany('asc', 'desc')), optional($.nulls_order)),
    nulls_order: _ => seq(reserve('nulls'), reserveMany('first', 'last')),
    within_group_clause: $ => seq(reserve('within'), reserve('group'),'(', $._order_by_clause, ')'),
    filter_clause: $ => seq(reserve('filter'), '(', KW.WHERE, $.expr,')'),
    _over_clause: $ => seq(reserve('over'), field('window', $.window_spec)),
    _partition_clause: $ => seq(reserve('partition'), KW.BY, field('partition', $.expr_list)),
    window_spec: $ => choice(
      $.identifier,
      seq('(',
        optional($.identifier), optional($._partition_clause),
        optOrderBy($), optional($.window_frame_clause),
        ')'
      )
    ),
    window_frame_clause: $ => seq(
      reserveMany('range', 'rows', 'groups'),
      choice(
        $.window_frame_bound,
        seq(reserve('between'), $.window_frame_bound, reserve('and'), $.window_frame_bound)
      ),
      optional($.window_exclusion_clause)
    ),
    window_frame_bound: $ => choice(
      seq(reserve('unbounded'), reserveMany('preceding', 'following')),
      seq(reserve('current'), reserve('row')),
      seq($.expr, reserveMany('preceding', 'following'))
    ),
    window_exclusion_clause: _ => seq(reserve('exclude'), choice(
      reserve('group'),
      reserve('ties'),
      seq(reserve('current'), reserve('row')),
      seq(reserve('no'), reserve('others'))
    )),
    expr_list: $ => seq($.expr, repeat(seq(',', $.expr))),

    _array_construct_prefix: $ => choice(
      reserve('array'),
      $.array_type
    ),
    array_expr: $ => seq(optional($._array_construct_prefix), '[', optional($.expr_list), ']'),
    /*
    _format: $ => seq(reserve('format'), $.expression, optional($._at_time_zone)),
    cast_expr: $ => seq(
      reserve_many('cast', 'safe_cast'), '(', $.expression, reserve('as'), $.type, optional($._format), ')'
    ),
    */
    
    // bigquery GA interval expr
    // interval_expr: $ => seq(KW.INTERVAL, $.expression, $.identifier, optional(seq(KW.TO, $.identifier))), 
    parameter_expr: $ => choice($.named_parameter_expr, '?'),
    named_parameter_expr: $ => seq('@', $.identifier),
    system_variable_expr: $ => seq('@@', $.path_expr),

    /*=================================
     *		QUERY:                      *
     *				SELECT STATEMENTS       *
     =================================*/
    // take inspiration from Postgres
    _select_stmt: $ => choice($.select, $.select_in_parens),
    select_in_parens: $ => seq('(', choice($.select, $.select_in_parens), ')'),
    // select_no_parens
    select: $ => choice(
      $._simple_select,
      $.select_set,
      seq($._select_clause, $._order_by_clause),
      seq($._select_clause, optOrderBy($), $.for_locking_clause, optional($._select_limit)),
      seq($._select_clause, optOrderBy($), $._select_limit, optional($.for_locking_clause)),
      seq($._with_clause, $._select_clause, optional($._order_by_clause)),
      seq($._with_clause, $._select_clause, optional($._order_by_clause), $.for_locking_clause, optional($._select_limit)),
      seq($._with_clause, $._select_clause, optional($._order_by_clause), $._select_limit, optional($.for_locking_clause)),
    ),
    // select_clause ~ <query expression body> in foundation grammar
    _select_clause: $ => choice(
      $._simple_select,
      $.select_in_parens,
      $.select_set,
    ),
    // simple_select ~ <simple table> in foundation grammar
    // SELECT statements that can appear within set operations,
    // including UNION, INTERSECT and EXCEPT.  '(' and ')' can be used to specify
    // the ordering of the set operations.	Without '(' and ')' we want the
    // operations to be ordered per the precedence specs at the head of this file.
    _simple_select: $ => choice(
      $.select_spec,
      $.values_clause,
      seq(reserve('table'), $._relation_expr),
    ),
    // set query
    select_set: $ => prec.left(0, seq($._select_clause, $.set_operator, optional($.set_quantifier), $._select_clause)),
    
    // select_spec ~ 7.16 <query specification> in foundation grammar
    select_spec: $ => seq(
      token(prec(1, reserve('select'))), optional($.set_quantifier),
      field('select_list', $.select_list),
      field('into', optional($.into_clause)),
      field('from', optional($._from_clause)),
      field('where', optional($._where_clause)),
      field('group', optional(seq($._group_by_clause, optional($.having_clause)))),
      field('window', optional($.window_clause)),
      field('pivot', optional(choice($.pivot_clause, $.unpivot_clause))),
    ),

    set_operator: _ => token(prec(1, reserveMany('union', 'intersect', 'except'))),
    set_quantifier: _ => token(prec(1, reserveMany('all', 'distinct'))),

    into_clause: $ => seq(reserve('into'), $.into_temp_table),
    into_temp_table: $ => choice(
      seq(optional(reserveMany('local', 'global')), reserve('temporary'), optional(reserve('table')), $.path_expr),
			seq(optional(reserveMany('local', 'global')), reserve('temp'), optional(reserve('table')), $.path_expr),
			seq(reserve('unlogged'), optional(reserve('table')), $.path_expr),
			seq(reserve('table'), $.path_expr),
      $.path_expr
    ),

    // Specify a table or a grouped table.
    /* table_expr: $ => seq(
      $.from_clause,
      optional($.where_clause),
      optional($.group_by_clause, optional($.having_clause)),
      $.window_clause
    ), */

    // Specify a table derived from one or more tables.
    // ~ <from clause> in foundation grammar
    _from_clause: $ => seq(token(prec(1, KW.FROM)), $.from_list),
    from_list: $ => commaSep1($.table_ref),
    // Reference a table ~ <table reference> in foundation grammar.
    table_ref: $ =>  choice(
      seq($._relation_expr, optional($._alias_clause), field('sample', optional($.sample_clause))),
      seq(reserveOpt('lateral'), $.func_table, optional($.func_alias_clause)),
      seq(reserveOpt('lateral'), $.xmltable, optional($._alias_clause)),
      seq(reserveOpt('lateral'), alias($.select_in_parens, $.subquery_table), optional($._alias_clause)),
      $.joined_table,
      seq('(', $.joined_table , ')', $._alias_clause),
    ),

    joined_table: $ => choice(
      seq('(', $.joined_table, ')'),
      prec.left(0, seq($.table_ref, reserve('cross'), KW.JOIN, $.table_ref)), // Cross Join
      prec.left(0, seq($.table_ref, $.join_type, KW.JOIN, $.table_ref, $.join_spec)),// Qualified Join
      prec.left(0, seq($.table_ref, reserve('natural'), optional($.join_type), KW.JOIN, $.table_ref)) // Natural Join
    ),
    join_spec: $ => choice(
      seq(reserve('using'), '(', $.identifier_list, ')'),
      seq(reserve('on'), $.expr)
    ),
    identifier_list: $ => commaSep1($.identifier),
    join_type: _ => choice(
      seq(reserveMany('left', 'right', 'full'), reserveOpt('outer')),
      reserve('inner')
    ),

    _alias_clause: $ => choice(
      seq(field('alias', $.identifier), optional(field('column_alias', $._column_alias))),
      seq($._kw_as, field('alias', $.identifier), optional(field('column_alias', $._column_alias))),
      seq($._kw_as, field('column_alias', $._column_alias))
    ),
    _column_alias: $ => seq('(', $.identifier_list, ')'),
    values_clause: $ => seq(reserve('values'), commaSep1(seq('(', $.expr_list, ')'))),

    /* func_table in Postgres ~ tvf in zetqsql */
    func_table: $ => choice(
      seq($.func_expr_windowless, optional(reserveSeq('with', 'ordinality'))),
      seq(reserveSeq('rows', 'from'), '(', commaSep1($.rowsfrom_item), ')', optional(reserve('ordinality')))
    ),
    rowsfrom_item: $ => seq($.func_expr_windowless, optional(seq($._kw_as, '(', $.table_func_element_list, ')'))),
    table_func_element_list: $ => commaSep1($.table_func_element),
    table_func_element: $ => seq($.identifier, $.type),

    _select_limit: $ => choice(
      seq(field('limit', $.limit_clause), optional(field('offset', $.offset_clause))),
      seq(field('offset', $.offset_clause), optional(field('limit', $.limit_clause)))
    ),
    limit_clause: $ => choice(
      seq($._kw_limit, $.select_limit_val),
      seq(
        reserve('fetch'), reserveMany('first', 'next'),
        optional($.select_fetch_first_val),
        reserveMany('row', 'rows'),
        choice(reserve('only'), reserveSeq('with', 'ties'))
      ),
    ),
    offset_clause: $ => seq(reserve('offset'), choice(
      $.expr,
      seq($.select_fetch_first_val, reserveMany('row', 'rows'))
    )),
    select_limit_val: $ => choice(reserve('all'), $.expr,),
    select_fetch_first_val: $ => choice(
      $._c_expr,
      seq(/\+|-/, $.integer_literal),
      seq(/\+|-/, $.float_literal)
    ),

    /* TABLESAMPLE decoration in a FROM item */
    // TODO use path_expr instead of func_name, revise later
    sample_clause: $ => seq(
      reserve('tablesample'), $.path_expr, '(', $.expr_list, ')',
      optional(seq(reserve('repeatable'), '(', $.expr, ')'))
    ),

    for_locking_clause: $ => choice(
      reserveSeq('for', 'read', 'only'),
      repeat1($.for_locking_item)
    ),
    for_locking_item: $ => seq($.for_locking_strength, optional($.locked_rels_list), optional($.nowait_or_skip)),
    for_locking_strength: _ => choice(
      reserveSeq('for', 'update'),
      reserveSeq('for', 'no', 'key', 'update'),
      reserveSeq('for', 'share'),
      reserveSeq('for', 'key', 'share')
    ),
    locked_rels_list: $ => seq(reserve('of'), $.identifier_list),
    nowait_or_skip: _ => choice(reserve('nowait'), reserveSeq('skip', 'locked')),
    
    /* target_list on postgres, select_column in zetasql */
    select_list: $ => seq($.select_item, repeat(seq(',', $.select_item)), optional(',')),
    select_item: $ => choice(
      $.expr,
      $.star,
      $._expr_opt_as_alias,
      seq($.star, $._star_modifiers),
      seq($.expr, field('star', $.dot_star)), //optional($._star_modifiers)),
    ),
    _expr_opt_as_alias: $ => seq($.expr, optional($._kw_as), field('alias', $.identifier)),
    star: $ => field('star', '*'),
    dot_star: $ => token(prec(1, seq('.', '*'))),
    _star_modifiers: $ => choice(
      $._star_except,
      $._star_replace,
      seq($._star_except, $._star_replace)
    ),
    _star_except: $ => seq(token(prec(2, reserve('except'))), '(', field('except', $.identifier_list), ')'),
    _star_replace: $ => seq(reserve('replace'), '(', field('replace', $.star_replace_list), ')'),
    star_replace_list: $ => seq($.star_replace_item, repeat(seq(',', $.star_replace_item))),
    star_replace_item: $ => seq($.expr, $._kw_as, $.identifier),

    // Postgres relation_expr
    // TODO: revise  and check with zetasql
    _relation_expr: $ => choice(
      $.path_expr,
      seq($.path_expr,  '*'),
      seq(reserve('only'), $.path_expr ),
      seq(reserve('only'), '(', $.path_expr, ')')
    ),

    // Bigquery table_path_expr:
    /* table_path_expr: $ => seq(
      $.table_path_expr_base, optional($.join_hint),
      choice(
        optional($.pivot_or_unpivot_clause_and_alias),
        optional($.with_offset_and_alias),
      ),
      optional($.at_system_time),
      optional($.sample_clause)
    ),
    join_hint: reserveMany('hash', 'lookup'), */

    /* Where clause in select */
    _where_clause: $ => seq(token(prec(1, reserve('where'))), $.expr),
    /* Group by and having clause */
    _group_by_clause: $ => seq(KW.GROUP, KW.BY, optional($.set_quantifier), $.group_by_list),
    group_by_list: $ => commaSep1($._group_by_item),
    _group_by_item: $ => choice(
      $.expr,
      seq('(', ')'),
      seq(reserve('cube'), '(', $.expr_list, ')'),
      seq(reserve('rollup'), '(', $.expr_list, ')'),
      $.grouping_sets_clause
    ),
    grouping_sets_clause: $ => seq(reserve('grouping'), reserve('sets'), '(', $.group_by_list, ')'),
    having_clause: $ => seq(reserve('having'), $.expr),

    /* Window Definition */
    window_clause: $ => seq(reserve('window'), commaSep1($.window_definition)),
    window_definition: $ => seq($.identifier, $._kw_as, $.window_spec),
    
    pivot_value: $ => $._expr_opt_as_alias,
    pivot_list: $ => commaSep1($.pivot_value),
    pivot_clause: $ => seq(reserve('pivot'),
      '(' , $.pivot_list, KW.FOR, $.expr, KW.IN, '(', $.pivot_list, ')', ')'
    ),
    unpivot_nulls_filter: $ => seq(reserveMany('exclude', 'include'), reserve('nulls')),
    path_expr_list_opt_parens: $ => choice(
      seq('(', commaSep1($.path_expr), ')'),
      $.path_expr
    ),
    unpivot_in_item: $ => seq(
      $.path_expr_list_opt_parens,
      optional(seq(optional($._kw_as), choice($.integer_literal, $.string_literal)))
    ),
    unpivot_in_list: $ => commaSep1($.unpivot_in_item),
    unpivot_clause: $ => seq(
      reserve('unpivot'), optional($.unpivot_nulls_filter), '(',
        $.path_expr_list_opt_parens, KW.FOR, $.path_expr, KW.IN, '(', $.unpivot_in_list, ')',
      ')'
    ),
    
    
    /*
     * Common Table Expression
     *
     * SQL standard WITH clause looks like:
     *
     * WITH [ RECURSIVE ] <query name> [ (<column>,...) ]
     *		AS (query) [ SEARCH or CYCLE clause ]
     */

    _with_clause: $ => seq(reserve('with'), reserveOpt('recursive'), field('with', $.with_list)),
    with_list: $ => commaSep1($.common_table_expr),
    common_table_expr: $ => seq(
      $.identifier, optional($.identifier_list), $._kw_as, optional(seq(optNot(), reserve('materialized'))),
      '(', $._preparable_stmt, ')', optional($.search_clause), optional($.cycle_clause)
    ),
    _preparable_stmt: $ => choice($._select_stmt),//TOO: add more kind of statements later.
    search_clause: $ => seq(
      reserve('search'), reserveMany('depth', 'breadth'), reserve('first'), reserve('by'),
      $.identifier_list, reserve('set'), $.identifier // TODO: change to column_list and column_id later
    ),
    cycle_clause: $ => choice(
      seq(reserve('cycle'), $.identifier_list, reserve('set') , $.identifier, ),
      seq(reserve('cycle'), $.identifier_list, reserve('set'), $.identifier, choice(
        seq(KW.USING, $.identifier),
        seq(reserve('to'), $._const_expr, KW.DEFAULT, $._const_expr, KW.USING, $.identifier)
      ))
    ), // TODO: change to column_list and column_id later
    
    /*****************************************************************************
     *
     *		QUERY:
     *				EXPLAIN [ANALYZE] [VERBOSE] query
     *				EXPLAIN ( options ) query
     *****************************************************************************/
    _explainable_stmt: $ => choice(
      $._select_stmt,
      //TODO: add other statements
    ),
    utility_option_list: $ => commaSep1($.utility_option_item),
    utility_option_item: $ => seq($.utility_option_name, optional($.utility_option_arg)),
    utility_option_name: $ => choice($._kw_analyze, $.identifier), //TODO: change identifier to NonReservedWord
    utility_option_arg: $ => choice($.boolean_literal, $.string_literal, $._numeric_only),
    explain: $ => seq(KW.EXPLAIN, choice(
      $._explainable_stmt,
      seq($._kw_analyze, optional(KW.VERBOSE), $._explainable_stmt),
      seq(KW.VERBOSE, $._explainable_stmt),
      seq('(', $.utility_option_list, ')', $._explainable_stmt)
    )),

    // http://stackoverflow.com/questions/13014947/regex-to-match-a-c-style-multiline-comment/36328890#36328890
    comment: $ => token(choice(
      seq(/--|#/, /(\\(.|\r?\n)|[^\\\n])*/),
      seq(
        '/*',
        /[^*]*\*+([^/*][^*]*\*+)*/,
        '/'
      )
    )),
  }
});


/* 
 * Helper functions
 */

//generate b_expr
function generate_exprs($, expr_rule, ...extend_rules) {
  base_exprs = [
    $._c_expr,
    prec(PREC.CAST, seq(expr_rule, '::', $.type)),
    prec(PREC.UMINUS, seq(/~|\+|-/, expr_rule)),
    prec.left(PREC.ADD, seq(expr_rule, /\+|-/, expr_rule)),
    prec.left(PREC.MUL, seq(expr_rule, /\*|\/|%|\|\|/, expr_rule)),
    prec.left(PREC.SHIFT, seq(expr_rule, /<<|>>/, expr_rule)),
    prec.left(PREC.BITWISE_AND, seq(expr_rule, /&/, expr_rule)),
    prec.left(PREC.BITWISE_OR, seq(expr_rule, /\|/, expr_rule)),
    prec.left(PREC.BITWISE_XOR, seq(expr_rule, /\^/, expr_rule)),
    prec.left(PREC.COMPARE, seq(expr_rule, /=|<=?|>=?|!=|<>/, expr_rule)),
    prec.left(PREC.PRIMARY, seq(expr_rule, '[', expr_rule, ']')),
    // prec.left(PREC.COMPARE, seq(expr_rule, KW.IS, optNot(), KW.DISTINCT, KW.FROM, expr_rule)),
    // prec.left(PREC.COMPARE, seq(expr_rule, KW.IS, optNot(), KW.DOCUMENT_P))
  ];

  exprs = base_exprs.concat(extend_rules);
  return choice(...exprs);
}

function optNot() {
  return optional(KW.NOT);
}

function optOrderBy($) {
  return optional($._order_by_clause)
}

function commaSep (rule) {
  return optional(commaSep1(rule))
}

function commaSep1 (rule) {
  return seq(rule, repeat(seq(',', rule)))
}

function reserveOpt(keyword) {
  return optional(reserve(keyword));
}

function reserveSeq(...keywords) {
  let keys = keywords.map(keyword => reserve(keyword));
  return seq(...keys);
}

function reserveChoice(...keywords) {
  let keys = keywords.map(keyword => reserve(keyword));
  return choice(...keys);
}

function reserve(keyword) {
  //return keyword;
  return new RegExp(caseInsensitive(keyword));
}

function reserveMany(...keywords) {
  return new RegExp(
    keywords
    .map(keyword => caseInsensitive(keyword))
    .join("|")
  )
}

function caseInsensitive(keyword) {
  return keyword
    .split('')
    .map(letter => (letter !== ' ' && letter !== '_') ? `[${letter}${letter.toUpperCase()}]` : letter)
    .join('')
}

