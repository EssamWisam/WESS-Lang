# pylint: disable=W,C,R
from Lexer import Lexer
from ply import yacc
import sys
'''
table.parent -> parentTable
table['var2'].kind -> [VAR, CONST, FUNC]
table['var1'].type -> [INT, FLOAT, STR, None] None == cureently unknown or doesn't exist
'''
'''
kind  -> **attributes

VAR   -> name, line, [type], [value], [enum (enum symbole for unassigned enum vars)]

CONST -> name, line, type, value

FUNC  -> name, line, type, params

ENUM_DECLARATION  -> name, line, params (list of values that the enum can take)

ENUM_MEMBER -> name, enum (enum symbol)

CASE  -> line, type
'''
'''
EXPRESSION -> type, value
PARAMETER_LIST -> regular list of PARAMETER's
ENUM_MEMBER_LIST -> regular list of ENUM_MEMBER's
'''
class object:
  pass

def error_handler(error):
  global success
  success = False
  error = f"Error: {error}"
  output_result.append(error)
  raise SyntaxError ("invalid thing")

def warning_handler(warning):
  warning = f"Warning: {warning}"
  output_result.append(warning)

class Symbol:
    def __init__(self, symbol_info):
      # NOTE: Info are the attributes (e.g. name, type, value, params, etc.)
      self.type = None
      self.value = None
      self.used = False
      for key in symbol_info:
        setattr(self, key, symbol_info[key])

class SymbolTable:
    def __init__(self):
        self.symbols = {}
        self.parent = None

    def insert_symbol(self, symbol):
        if symbol.name in self.symbols:
            error_handler(f"{symbol.name} already declared, at line {symbol.line}")
        self.symbols[symbol.name] = symbol

    def lookup_symbol(self, name, line):
        if name in self.symbols:
            self.symbols[name].used = True
            return self.symbols[name]
        elif self.parent:
            return self.parent.lookup_symbol(name, line)
        else:
            error_handler(f"usage of undeclared identifier {name}, at line {line}")

    def create_child(self):
        child = SymbolTable()
        child.parent = self
        return child

current_table = SymbolTable()
output_result = []

class Parser(object):

  tokens = Lexer.tokens

  def p_program(self, p):  # Top-level rule
    '''
        PROGRAM : STATEMENT_LIST
        '''

  def p_statement_list(self, p):
    '''
        STATEMENT_LIST : STATEMENT
                       | STATEMENT STATEMENT_LIST
        '''

  def p_statement(self, p):
    '''
        STATEMENT : DECLARATION SEMICOLON
                  | ASSIGNMENT SEMICOLON
                  | IF_STATEMENT
                  | LOOP
                  | FUNCTION_DECLARATION
                  | EXPRESSION SEMICOLON
                  | NEW_SCOPE BLOCK
                  | SWITCH_STATEMENT
                  | RETURN_STATEMENT SEMICOLON
                  | CONTINUE_STATEMENT
                  | BREAK_STATEMENT
        '''

  def p_declaration(self, p):# NOTE: ENUM_VAR_DECLARATION is variable instantiation
    '''
        DECLARATION : VAR_DECLARATION
                    | CONST_DECLARATION
                    | ENUM_DECLARATION
                    | ENUM_VAR_DECLARATION
        '''

  def p_var_declaration(self, p):
    '''
        VAR_DECLARATION : VAR IDENTIFIER 
                        | VAR IDENTIFIER ASSIGN EXPRESSION
        '''
    if len(p) == 3:
      symbol_info = {'kind': 'VAR', 'name': p[2], 'line': p.lineno(1)}
      current_table.insert_symbol(Symbol(symbol_info))
    if len(p) == 5:
      symbol_info = {'kind': 'VAR', 'name': p[2], 'line': p.lineno(1), 'type': p[4].symbol.type, 'value': p[4].symbol.value}
      current_table.insert_symbol(Symbol(symbol_info))

  def p_const_declaration(self, p):
    '''
        CONST_DECLARATION : CONST IDENTIFIER ASSIGN EXPRESSION
        '''
    symbol_info = {'kind': 'CONST', 'name': p[2], 'type': p[4].symbol.type, 'line': p.lineno(1), 'value': p[4].symbol.value}
    current_table.insert_symbol(Symbol(symbol_info))

  def p_enum_declaration(self, p):
    '''
        ENUM_DECLARATION : ENUM IDENTIFIER LBRACE ENUM_MEMBER_LIST RBRACE
        '''
    symbol_info = {'kind': 'ENUM_DECLARATION', 'name': p[2], 'params': p[4], 'line': p.lineno(1)}
    current_table.insert_symbol(Symbol(symbol_info))
    for param in p[4]:
      symbol_info = {'kind': 'ENUM_MEMBER', 'type': p[2], 'name': param, 'line': p.lineno(1)}
      current_table.insert_symbol(Symbol(symbol_info))

  def p_enum_member_list(self, p):
    '''
        ENUM_MEMBER_LIST : IDENTIFIER
                         | IDENTIFIER COMMA ENUM_MEMBER_LIST
        '''
    if len(p) == 2:
        p[0] = [p[1]]
    if len(p) == 4:
      if p[1] in p[3]:
        error_handler(f"{p[1]} is a repeated enum value, at line {p.lineno(1)}")
      p[0] = [p[1]] + p[3]

  def p_enum_var_declaration(self, p):
    '''
        ENUM_VAR_DECLARATION : IDENTIFIER IDENTIFIER ASSIGN IDENTIFIER
                             | IDENTIFIER IDENTIFIER
        '''
    enum_symbol = current_table.lookup_symbol(p[1], p.lineno(1))
    if len(p) == 5:
      if p[4] not in enum_symbol.params:
        error_handler(f"Enum value is not found, at line {p.lineno(4)}")
      symbol_info = {'name': p[2], 'kind': 'VAR', 'type': p[1], 'line': p.lineno(2)}
      current_table.insert_symbol(Symbol(symbol_info))
    if len(p) == 3:
      symbol_info = {'name': p[2], 'kind': 'VAR', 'type': p[1], 'line': p.lineno(2)}
      current_table.insert_symbol(Symbol(symbol_info))

  def p_assignment(self, p):
    '''
        ASSIGNMENT : IDENTIFIER ASSIGN EXPRESSION
        '''
    print(p.lineno(1))
    symbol = current_table.lookup_symbol(p[1], p.lineno(1))
    symbol.value = p[3].symbol.value
    if symbol.kind != 'VAR':
      error_handler(f"Cannot assign to non-variable, at line {p.lineno(1)}")
    if symbol.type and p[3].symbol.type != 'UNK' and symbol.type != p[3].symbol.type:
      error_handler(f"Type mismatch, at line {p.lineno(1)}")
    if not symbol.type:
      symbol.type = p[3].symbol.type

  def p_if_statement(self, p):
    '''
        IF_STATEMENT : IF NEW_SCOPE LPAREN EXPRESSION RPAREN BLOCK
                     | IF NEW_SCOPE LPAREN EXPRESSION RPAREN BLOCK ELSE NEW_SCOPE BLOCK
        '''
    if p[4].symbol.type != 'BOOL' and p[4].symbol.type != 'UNK':
      error_handler(f"Non-boolean expression found in the if statement, at line {p.lineno(3)}")
    if p[4].symbol.value == False:
      warning_handler(f"Unreachable code, at line {p.lineno(3)}")

  def p_loop(self, p):
    '''
        LOOP : WHILE NEW_SCOPE LPAREN EXPRESSION RPAREN BLOCK
             | FOR NEW_SCOPE LPAREN ASSIGNMENT SEMICOLON EXPRESSION SEMICOLON ASSIGNMENT RPAREN BLOCK
             | FOR NEW_SCOPE LPAREN VAR_DECLARATION SEMICOLON EXPRESSION SEMICOLON ASSIGNMENT RPAREN BLOCK
             | DO NEW_SCOPE BLOCK WHILE LPAREN EXPRESSION RPAREN SEMICOLON
        '''
    if len(p) == 7 and p[4].symbol.type != 'BOOL' and p[4].symbol.type != 'UNK':
      error_handler(f"Non-boolean expression found in the while loop, at line {p.lineno(3)}")
    if len(p) == 11  and p[6].symbol.type != 'BOOL' and p[6].symbol.type != 'UNK':
      error_handler(f"Non-boolean expression found in the for loop, at line {p.lineno(7)}")
    if len(p) == 9 and p[6].symbol.type != 'BOOL' and p[6].symbol.type != 'UNK':
      error_handler(f"Non-boolean expression found in the do while loop, at line {p.lineno(5)}")

  def p_function_declaration(self, p):
    '''
        FUNCTION_DECLARATION : FUNCTION IDENTIFIER NEW_SCOPE LPAREN PARAMETER_LIST RPAREN BLOCK
        '''
    symbol_info = {'name': p[3], 'kind': 'FUNCTION', 'type': 'UNK', 'params': p[5], 'line': p.lineno(1)}
    current_table.insert_symbol(Symbol(symbol_info))

  def p_parameter_list(self, p):
    '''
        PARAMETER_LIST : IDENTIFIER
                       | IDENTIFIER COMMA PARAMETER_LIST
                       | epsilon
        '''
    if len(p) == 2 and p[1] != None:
        p[0] = [p[1]]
        symbol_info = {'name': p[1], 'kind': 'VAR', 'line': p.lineno(1)}
        current_table.insert_symbol(Symbol(symbol_info))
    if len(p) == 4:
        p[0] = [p[1]] + p[3]
    if len(p) == 2 and p[1] == None:
        p[0] = []

  def p_block(self, p):
    '''
        BLOCK : LBRACE STATEMENT_LIST RBRACE
              | LBRACE RBRACE
        '''
    global current_table
    current_table = current_table.parent # NOTE: Excuted at the end of the block

  def p_new_scope(self, p):# Dummy rule for creating a new scope semantic action
    '''
        NEW_SCOPE :
    '''
    global current_table
    current_table = current_table.create_child()

  def p_return_statement(self, p):
    '''
        RETURN_STATEMENT : RETURN EXPRESSION
        '''

  def p_switch_statement(self, p):
    '''
        SWITCH_STATEMENT : SWITCH LPAREN EXPRESSION RPAREN NEW_SCOPE LBRACE CASE_LIST RBRACE
        '''
    for case in p[7]:
      if case.kind == 'CASE' and p[3].symbol.type!= 'UNK' and case.type != p[3].symbol.type:
        error_handler(f"Non-boolean expression found in the switch statement, at line {case.line}")

  def p_case_list(self, p):
    '''
        CASE_LIST : CASE_CLAUSE
                  | CASE_CLAUSE CASE_LIST
      '''
    if len(p) == 2:
      p[0] = [p[1]]
    if len(p) == 3:
      p[0] = [p[1]] + p[2]

  def p_case_clause(self, p):
    '''
        CASE_CLAUSE : CASE EXPRESSION COLON STATEMENT_LIST
                    | DEFAULT COLON STATEMENT_LIST
      '''
    if len(p) == 5:
      symbol_info = {'kind': 'CASE', 'type': p[2].symbol.type, 'line': p.lineno(1)}
    if len(p) == 4:
      symbol_info = {'kind': 'DEFAULT'}
    p[0] = object()
    p[0].symbol = Symbol(symbol_info)

  def p_expression(self, p):
    '''
        EXPRESSION : LOGICAL_EXPR
                   | STRING
        '''
    p[0] = object()
    p[0].symbol = Symbol({})
    if not p[1].symbol.type:
      p[0].symbol.type = 'STRING'
    else:
      p[0].symbol.type = p[1].symbol.type
      p[0].symbol.value = p[1].symbol.value

  def p_logical_expr(self, p):  # (y<0) and (2*x+1>0) or (x==0)
    '''
        LOGICAL_EXPR : COMPARISON_EXPR
                     | COMPARISON_EXPR BINARY_LOGICAL_OPERATOR LOGICAL_EXPR
                     | NOT COMPARISON_EXPR
                     | NOT COMPARISON_EXPR BINARY_LOGICAL_OPERATOR LOGICAL_EXPR
        '''
    p[0] = object()
    p[0].symbol = Symbol({})
    if len(p) == 2:
      p[0].symbol.type = p[1].symbol.type
      p[0].symbol.value = p[1].symbol.value
    if len(p) == 4:
      p[0].symbol.type = 'BOOL'
    if len(p) == 3:
      p[0].symbol.type = 'BOOL'
    if len(p) == 5:
      p[0].symbol.type = 'BOOL'

  def p_comparison_expr(self, p):  # 2*x+1<2*y+1
    '''
        COMPARISON_EXPR : ADDITIVE_EXPR
                        | ADDITIVE_EXPR COMPARISON_OPERATOR COMPARISON_EXPR
        '''
    p[0] = object()
    p[0].symbol = Symbol({})
    if len(p) == 2:
      p[0].symbol.type = p[1].symbol.type
      p[0].symbol.value = p[1].symbol.value
    if len(p) == 4:
      p[0].symbol.type = 'BOOL'

  def p_additive_expr(self, p):  # 5+2*x
      '''
          ADDITIVE_EXPR : MULTIPLICATIVE_EXPR
                        | MULTIPLICATIVE_EXPR ADDITIVE_OPERATOR ADDITIVE_EXPR
          '''
      p[0] = object()
      p[0].symbol = Symbol({})
      if len(p) == 2:
          p[0].symbol.type = p[1].symbol.type
          p[0].symbol.value = p[1].symbol.value

      if len(p) == 4:
          numbers_types = ['INT', 'FLOAT']
          if p[1].symbol.type not in numbers_types or p[3].symbol.type not in numbers_types:
              print(p.lineno(2))
              error_handler(f"Can't add or subtract non-number types, at line {p.lineno(2)}")
          p[0].symbol.type = 'INT'
          if p[1].symbol.type == 'FLOAT' or p[3].symbol.type == 'FLOAT':
              p[0].symbol.type = 'FLOAT'

  def p_multiplicative_expr(self, p):
    '''
        MULTIPLICATIVE_EXPR : TERM
                            | MINUS TERM
                            | TERM MULTIPLICATIVE_OPERATOR MULTIPLICATIVE_EXPR
        ''' 

    p[0] = object()
    p[0].symbol = Symbol({})
    if len(p) == 2:
        p[0].symbol.type = p[1].symbol.type
        p[0].symbol.value = p[1].symbol.value

    if len(p) == 3:
        numbers_types = ['INT', 'FLOAT', 'UNK']
        if p[2].symbol.type not in numbers_types:
          error_handler(f"Can't negate non-number types, at line {p.lineno(1)}")
        p[0].symbol.type = p[2].symbol.type

    if len(p) == 4:
        if p[2].symbol.type == 'UNK' or p[3].symbol.type == 'UNK':
          p[0].symbol.type = 'UNK'
        else:
          numbers_types = ['INT', 'FLOAT']
          if p[1].symbol.type not in numbers_types or p[3].symbol.type not in numbers_types:
              error_handler(f"Can't multiple, divide or mod non-number types, at line {p.lineno(2)}")
          p[0].symbol.type = 'INT'

          if p[2]=='/':
            p[0].symbol.type = 'FLOAT'

          if p[1].symbol.type == 'FLOAT' or p[3].symbol.type == 'FLOAT':
              p[0].symbol.type = 'FLOAT'

          if p[2] == '%':
              if p[1].symbol.type != 'INT' or p[3].symbol.type != 'INT':
                  error_handler(f"Can't mod non-integer types, at line {p.lineno(2)}")
              p[0].symbol.type = 'INT'

          elif p[2] == '//':
              p[0].symbol.type = 'INT'

  def p_term(self, p):  # x
    '''
        TERM : IDENTIFIER
             | NUMBER
             | FUNCTION_CALL
             | LPAREN EXPRESSION RPAREN
             | TRUE
             | FALSE
        '''
    p[0] = object()
    p[0].symbol = Symbol({})
    if len(p) == 4:
        p[0].symbol.type = p[2].symbol.type

    if len(p) == 2:
        if p[1] == 'True':
            p[0].symbol.type = 'BOOL'

        elif p[1] == 'False':
            p[0].symbol.type = 'BOOL'
            p[0].symbol.value = False

        elif type(p[1]) == str:
            symbol = current_table.lookup_symbol(p[1], p.lineno(1))
            p[0].symbol.type = symbol.type
            p[0].symbol.value = symbol.value

        else:
            # check if int or float by checking if there is a decimal point
            if type(p[1]) == float:
                p[0].symbol.type = 'FLOAT'
            else:
                p[0].symbol.type = 'INT'

  def p_function_call(self, p):
    '''
        FUNCTION_CALL : IDENTIFIER LPAREN ARGUMENT_LIST RPAREN
        '''
    symbol = current_table.lookup(p[1])
    if len(symbol.params) != len(p[3]):
      error_handler(f"Length mismatch in function call, at line {p.lineno(1)}")
    symbol_info = {'name': p[1], 'kind': 'FUNCTION', 'type': symbol.type, 'params': p[3], 'line': p.lineno(1)}
    p[0] = object()
    p[0].symbol = Symbol(symbol_info)

  def p_argument_list(self, p):
    '''
        ARGUMENT_LIST : EXPRESSION
                      | EXPRESSION COMMA ARGUMENT_LIST
                      | epsilon
        '''
    if len(p) == 2 and p[1] != None:
        p[0] = [p[1]]
    if len(p) == 4:
        p[0] = [p[1]] + p[3]
    if len(p) == 2 and p[1] == None:
        p[0] = []

  def p_epsilon(self, p):
    '''
        epsilon :
        '''

  def p_logical_operator(self, p):
    '''
        BINARY_LOGICAL_OPERATOR : AND
                                | OR
        '''

  def p_comparison_operator(self, p):
    '''
        COMPARISON_OPERATOR : EQ
                            | NE
                            | LT
                            | LE
                            | GT
                            | GE
        '''

  def p_additive_operator(self, p):
    '''
        ADDITIVE_OPERATOR : PLUS
                          | MINUS
        '''

  def p_multiplicative_operator(self, p):
    '''
        MULTIPLICATIVE_OPERATOR : TIMES
                                | DIVIDE
                                | MOD
                                | INT_DIVIDE
        '''
  def p_continue_statement(self, p):
    '''
        CONTINUE_STATEMENT : CONTINUE SEMICOLON
        '''
  def p_break_statement(self, p):
    '''
        BREAK_STATEMENT : BREAK SEMICOLON
        '''


  def p_error(self, p):
    global success
    success=False
    output_result.append("Error: Syntax error")

  def __init__(self):
    self.lexer = Lexer()
    self.parser = yacc.yacc(module=self)
P = Parser()
success = True
def parse_gui(code):
  global output_result, current_table, P,success
  success=True
  current_table = SymbolTable()
  output_result = []
  P = Parser()
  parse = P.parser.parse(code)
  for symbol in current_table.symbols.values():
    if (symbol.kind == 'VAR' or symbol.kind == 'CONST') and symbol.used == False:
      warning_handler(f"Variable {symbol.name} declared but not used, at line {symbol.line}")
  if success:
    output_result.append("Compiled Successfully")
  print("In parser",output_result)
  return output_result


print(parse_gui(
    """
  var x = 5;
  var y;
  z = y + 3;
  """
  ))