# pylint: disable=W,C,R
from Lexer import Lexer
from ply import yacc


class Parser(object):

  tokens = Lexer.tokens

  def p_program(self, p):  ### Top-level rule
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
                  | BLOCK
                  | SWITCH_STATEMENT
                  | RETURN_STATEMENT SEMICOLON
        '''

  def p_declaration(self, p):# ENUM_VAR_DECLARATION is variable instantiation
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

  def p_const_declaration(self, p):
    '''
        CONST_DECLARATION : CONST IDENTIFIER ASSIGN EXPRESSION
        '''

  def p_enum_declaration(self, p):
    '''
        ENUM_DECLARATION : ENUM IDENTIFIER LBRACE ENUM_MEMBER_LIST RBRACE
        '''

  def p_enum_member_list(self, p):
    '''
        ENUM_MEMBER_LIST : IDENTIFIER
                         | IDENTIFIER COMMA ENUM_MEMBER_LIST
        '''

  def p_enum_var_declaration(self, p):
    '''
        ENUM_VAR_DECLARATION : ENUM IDENTIFIER IDENTIFIER ASSIGN IDENTIFIER
                             | ENUM IDENTIFIER IDENTIFIER
        '''

  def p_assignment(self, p):
    '''
        ASSIGNMENT : IDENTIFIER ASSIGN EXPRESSION
        '''

  def p_if_statement(self, p):
    '''
        IF_STATEMENT : IF LPAREN EXPRESSION RPAREN BLOCK
                     | IF LPAREN EXPRESSION RPAREN BLOCK ELSE BLOCK
        '''

  def p_loop(self, p):
    '''
        LOOP : WHILE LPAREN EXPRESSION RPAREN BLOCK
             | FOR LPAREN ASSIGNMENT SEMICOLON EXPRESSION SEMICOLON ASSIGNMENT RPAREN BLOCK
             | FOR LPAREN VAR_DECLARATION SEMICOLON EXPRESSION SEMICOLON ASSIGNMENT RPAREN BLOCK
             | DO BLOCK WHILE LPAREN EXPRESSION RPAREN SEMICOLON
        '''

  def p_function_declaration(self, p):
    '''
        FUNCTION_DECLARATION : FUNCTION IDENTIFIER LPAREN PARAMETER_LIST RPAREN BLOCK
        '''

  def p_parameter_list(self, p): #TODO add epsilon
    '''
        PARAMETER_LIST : IDENTIFIER
                       | IDENTIFIER COMMA PARAMETER_LIST
        '''

  def p_block(self, p):
    '''
        BLOCK : LBRACE STATEMENT_LIST RBRACE
              | LBRACE RBRACE
        '''

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
                        | ADDITIVE_EXPR COMPARISON_OPERATOR ADDITIVE_EXPR
        '''

  def p_additive_expr(self, p):  # 5+2*x
    '''
        ADDITIVE_EXPR : MULTIPLICATIVE_EXPR
                      | MULTIPLICATIVE_EXPR ADDITIVE_OPERATOR ADDITIVE_EXPR
        '''

  def p_multiplicative_expr(self, p):  # x*2
    '''
        MULTIPLICATIVE_EXPR : TERM
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

  def p_argument_list(self, p):# TODO add epsilon
    '''
        ARGUMENT_LIST : EXPRESSION
                      | EXPRESSION COMMA ARGUMENT_LIST
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
        '''

  def p_error(self, p):
    print("Syntax error in input!")

  def __init__(self):
    self.lexer = Lexer()
    self.parser = yacc.yacc(module=self)


if __name__ == "__main__":
  ### Test the parser
  P = Parser()

#   code = \
# """
# var x;
# x = 5;
# const pi = 3.14;
# if (x*pi > 10) {
#   x = "Hello";
# } else {
#   x = "World";
# }
# """
  # get the code from the code.txt file
  with open("code.txt", "r") as f:
    code = f.read()

  parse = P.parser.parse(code)

  # print(parse)
  # if parse is not None:
  #   print("Parsing Successful!")

