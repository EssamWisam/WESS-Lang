# pylint: disable=W,C,R
from Lexer import Lexer
from ply import yacc

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
'''
'''
EXPRESSION -> type, value
PARAMETER_LIST -> regular list of PARAMETER's
ENUM_MEMBER_LIST -> regular list of ENUM_MEMBER's
'''

class Symbol:
    def __init__(self, symbol_info):
      # NOTE: Info are the attributes (e.g. name, type, value, params, etc.)
      for key in symbol_info:
        setattr(self, key, symbol_info[key])

class SymbolTable:
    def __init__(self):
        self.symbols = {}
        self.parent = None

    def insert_symbol(self, symbol):
        if symbol.name in self.symbols:
            raise Exception(f"{symbol.name} already declared, at line {symbol.line}")
        self.symbols[symbol.name] = symbol

    def lookup_symbol(self, name, line):
        if name in self.symbols:
            return self.symbols[name]
        elif self.parent:
            return self.parent.lookup_symbol(name, line)
        else:
            raise Exception(f"usage of undeclared identifier {name}, at line {line}")

    def create_child(self):
        child = SymbolTable()
        child.parent = self
        return child

current_table = SymbolTable()

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
      symbol_info = {'kind': 'VAR', 'name': p[2], 'line': p.lineno(2)}
      current_table.insert_symbol(Symbol(symbol_info))
    if len(p) == 5:
      symbol_info = {'kind': 'VAR', 'name': p[2], 'line': p.lineno(2), 'type': p[4].type, 'value': p[4].value}
      current_table.insert_symbol(Symbol(symbol_info))

  def p_const_declaration(self, p):
    '''
        CONST_DECLARATION : CONST IDENTIFIER ASSIGN EXPRESSION
        '''
    symbol_info = {'kind': 'CONST', 'name': p[2], 'type': p[4].type, 'value': p[4].value, 'line': p.lineno(2)}
    current_table.insert_symbol(Symbol(symbol_info))

  def p_enum_declaration(self, p):
    '''
        ENUM_DECLARATION : ENUM IDENTIFIER LBRACE ENUM_MEMBER_LIST RBRACE
        '''
    symbol_info = {'kind': 'ENUM_DECLARATION', 'name': p[2], 'params': p[4], 'line': p.lineno(2)}
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
        raise Exception(f" {p[1]} Repeated enum value, at line {p.lineno(1)}")
      p[0] = [p[1]] + p[3]

  def p_enum_var_declaration(self, p):
    '''
        ENUM_VAR_DECLARATION : ENUM IDENTIFIER IDENTIFIER ASSIGN IDENTIFIER
                             | ENUM IDENTIFIER IDENTIFIER
        '''
    if len(p) == 6:
      enum_symbol = current_table.lookup_symbol(p[2], p.lineno(2))
      if p[5] not in enum_symbol.params:
        raise Exception(f"Enum value not found, at line {p.lineno(5)}")
      symbol_info = {'name': p[3], 'kind': 'VAR', 'type': 'INT', 'value': enum_symbol.params.index(p[5]), 'line': p.lineno(3)}
      current_table.insert_symbol(Symbol(symbol_info))
    if len(p) == 4:
      enum_symbol = current_table.lookup_symbol(p[2], p.lineno(2))
      symbol_info = {'name': p[2], 'kind': 'VAR', 'type': 'INT', 'enum': enum_symbol, 'line': p.lineno(3)}
      current_table.insert_symbol(Symbol(symbol_info))

  def p_assignment(self, p):
    '''
        ASSIGNMENT : IDENTIFIER ASSIGN EXPRESSION
        '''
    symbol = current_table.lookup_symbol(p[1], p.lineno(1))
    if symbol.kind != 'VAR':
      raise Exception(f"Cannot assign to non-variable, at line {p.lineno(1)}")
    if symbol.type and symbol.type != p[3].type:
      raise Exception(f"Type mismatch, at line {p.lineno(1)}")
    if not symbol.type:
      symbol.type = p[3].type
    symbol.value = p[3].value

  def p_if_statement(self, p):
    '''
        IF_STATEMENT : IF NEW_SCOPE LPAREN EXPRESSION RPAREN BLOCK
                     | IF NEW_SCOPE LPAREN EXPRESSION RPAREN BLOCK ELSE NEW_SCOPE BLOCK
        '''
    if p[4].type != 'BOOL':
      raise Exception(f"Type mismatch, at line {p.lineno(4)}")

  def p_loop(self, p):
    '''
        LOOP : WHILE NEW_SCOPE LPAREN EXPRESSION RPAREN BLOCK
             | FOR NEW_SCOPE LPAREN ASSIGNMENT SEMICOLON EXPRESSION SEMICOLON ASSIGNMENT RPAREN BLOCK
             | FOR NEW_SCOPE LPAREN VAR_DECLARATION SEMICOLON EXPRESSION SEMICOLON ASSIGNMENT RPAREN BLOCK
             | DO NEW_SCOPE BLOCK WHILE LPAREN EXPRESSION RPAREN SEMICOLON
        '''
    if len(p) == 7 and p[4].type != 'BOOL':
      raise Exception(f"Type mismatch, at line {p.lineno(4)}")
    if len(p) == 11  and p[6].type != 'BOOL':
      raise Exception(f"Type mismatch, at line {p.lineno(6)}")
    if len(p) == 9 and p[6].type != 'BOOL':
      raise Exception(f"Type mismatch, at line {p.lineno(6)}")

  def p_function_declaration(self, p):
    '''
        FUNCTION_DECLARATION : FUNCTION NEW_SCOPE IDENTIFIER LPAREN PARAMETER_LIST RPAREN BLOCK
        '''
    symbol_info = {'name': p[3], 'kind': 'FUNCTION', 'type': None, 'params': p[5], 'line': p.lineno(3)}
    current_table.insert_symbol(Symbol(symbol_info))

  def p_parameter_list(self, p):
    '''
        PARAMETER_LIST : IDENTIFIER
                       | IDENTIFIER COMMA PARAMETER_LIST
                       | epsilon
        '''
    if len(p) == 2 and p[1] != None: # TODO: not sure !epsilon
        p[0] = [p[1]]
        symbol_info = {'name': p[1], 'kind': 'VAR', 'type': None, 'value': None, 'line': p.lineno(1)}
        current_table.insert_symbol(Symbol(symbol_info))
    if len(p) == 4:
        p[0] = [p[1]] + p[3]
    if len(p) == 2 and p[1] == None: # TODO: not sure epsilon
        p[0] = []

  def p_block(self, p):
    '''
        BLOCK : LBRACE STATEMENT_LIST RBRACE
              | LBRACE RBRACE
        '''
    current_table = current_table.parent # NOTE: Excuted at the end of the block

  def p_new_scope(p):# Dummy rule for creating a new scope semantic action
    '''
        NEW_SCOPE : 
    '''
    current_table = current_table.create_child()

  def p_return_statement(self, p):
    '''
        RETURN_STATEMENT : RETURN EXPRESSION
        '''

  def p_switch_statement(self, p):
    '''
        SWITCH_STATEMENT : SWITCH LPAREN EXPRESSION RPAREN LBRACE CASE_LIST RBRACE
        '''

  def p_case_list(self, p):
    '''
        CASE_LIST : CASE_CLAUSE
                  | CASE_CLAUSE CASE_LIST
      '''

  def p_case_clause(self, p):
    '''
        CASE_CLAUSE : CASE EXPRESSION COLON STATEMENT_LIST
                    | DEFAULT COLON STATEMENT_LIST
      '''

  def p_expression(self, p):
    '''
        EXPRESSION : LOGICAL_EXPR
                   | STRING
        '''

  def p_logical_expr(self, p):  # (y<0) and (2*x+1>0) or (x==0)
    '''
        LOGICAL_EXPR : COMPARISON_EXPR
                     | COMPARISON_EXPR BINARY_LOGICAL_OPERATOR LOGICAL_EXPR
                     | NOT COMPARISON_EXPR
                     | NOT COMPARISON_EXPR BINARY_LOGICAL_OPERATOR LOGICAL_EXPR
        '''

  def p_comparison_expr(self, p):  # 2*x+1<2*y+1
    '''
        COMPARISON_EXPR : ADDITIVE_EXPR
                        | ADDITIVE_EXPR COMPARISON_OPERATOR COMPARISON_EXPR
        '''

  def p_additive_expr(self, p):  # 5+2*x
    '''
        ADDITIVE_EXPR : MULTIPLICATIVE_EXPR
                      | MULTIPLICATIVE_EXPR ADDITIVE_OPERATOR ADDITIVE_EXPR
        '''

  def p_multiplicative_expr(self, p):  # x*2
    '''
        MULTIPLICATIVE_EXPR : TERM
                            | MINUS TERM
                            | TERM MULTIPLICATIVE_OPERATOR MULTIPLICATIVE_EXPR
        '''

  def p_term(self, p):  # x
    '''
        TERM : IDENTIFIER
             | NUMBER
             | FUNCTION_CALL
             | LPAREN EXPRESSION RPAREN
             | TRUE
             | FALSE
        '''

  def p_function_call(self, p):
    '''
        FUNCTION_CALL : IDENTIFIER LPAREN ARGUMENT_LIST RPAREN
        '''
  def p_epsilon(self, p):
    '''
        epsilon :
        '''
    
  def p_argument_list(self, p):
    '''
        ARGUMENT_LIST : EXPRESSION
                      | EXPRESSION COMMA ARGUMENT_LIST
                      | epsilon
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
    
    
    # if the previous toke is a float and the current toke is MOD or INT_DIVIDE then raise an error (TODO:)
    


  def p_error(self, p):
    print("Syntax error in input!")
    #print(p)

  def __init__(self):
    self.lexer = Lexer()
    self.parser = yacc.yacc(module=self)


if __name__ == "__main__":
  ### Test the parser
  P = Parser()

  code = \
"""
switch (expression) {
  case value1:
    var x=1;
    break;
  case value2:
    var x=2;
    break;
  default:
    var x=3;
}
"""
  # # get the code from the code.txt file
  # with open("code.txt", "r") as f:
  #   code = f.read()

  parse = P.parser.parse(code)

  #print(parse)
  # if parse is not None:
  #   print("Parsing Successful!")

