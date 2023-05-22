# pylint: disable=W,C,R
from Lexer import Lexer
from ply import yacc

class Quadruple:
  temp_count = 0
  result = ""

  def __init__(self, op, arg1, arg2):
      self.op = op
      self.arg1 = arg1
      self.arg2 = arg2
      Quadruple.update_temp()
      self.result = Quadruple.result
      
  @classmethod
  def update_temp(cls):
      cls.result = f"t{cls.temp_count}"
      cls.temp_count += 1

  def __str__(self):
      # dont print Nones
      return f"{self.result} = {self.arg1} {self.op if self.op else ''} {self.arg2 if self.arg2 else ''}"


class Parser(object):

  tokens = Lexer.tokens
  Quadruples = []

  ### Top-level rules
  def p_program(self, p):  
    '''
        PROGRAM : STATEMENT_LIST
        '''
    p[0] = p[1]
  def p_statement_list(self, p):
    '''
        STATEMENT_LIST : STATEMENT
                       | STATEMENT STATEMENT_LIST
        '''
    if len(p) == 2:
      p[0] = p[1]
    elif len(p) == 3:
      p[0] = p[2]

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
                  | CONTINUE_STATEMENT
                  | BREAK_STATEMENT
        '''
    p[0] = p[1]
    
  def p_block(self, p):
    '''
        BLOCK : LBRACE STATEMENT_LIST RBRACE
              | LBRACE RBRACE
        '''

  def p_epsilon(self, p):
    '''
        epsilon :
        '''
        
  ### Declarations
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

  def p_function_declaration(self, p):
    '''
        FUNCTION_DECLARATION : FUNCTION IDENTIFIER LPAREN PARAMETER_LIST RPAREN BLOCK
        '''

  def p_parameter_list(self, p): #TODO add epsilon
    '''
        PARAMETER_LIST : IDENTIFIER
                       | IDENTIFIER COMMA PARAMETER_LIST
                       | epsilon
        '''


  ### Statements
  def p_return_statement(self, p):
    '''
      RETURN_STATEMENT : RETURN EXPRESSION
      '''
  
  def p_function_call(self, p):
    '''
        FUNCTION_CALL : IDENTIFIER LPAREN ARGUMENT_LIST RPAREN
        '''

    
  def p_argument_list(self, p):# TODO add epsilon
    '''
        ARGUMENT_LIST : EXPRESSION
                      | EXPRESSION COMMA ARGUMENT_LIST
                      | epsilon
        '''
        
  def p_assignment(self, p):
    '''
        ASSIGNMENT : IDENTIFIER ASSIGN EXPRESSION
        '''
    self.Quadruples.append(Quadruple(op=None, arg1=p[3], arg2=None))
    p[0] = Quadruple.result

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

  def p_continue_statement(self, p):
    '''
        CONTINUE_STATEMENT : CONTINUE SEMICOLON
        '''
  def p_break_statement(self, p):
    '''
        BREAK_STATEMENT : BREAK SEMICOLON
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

  ### Expressions
  def p_expression(self, p):
    '''
        EXPRESSION : LOGICAL_EXPR
                   | STRING
        '''
    p[0] = p[1]

  def p_logical_expr(self, p):  # (y<0) and (2*x+1>0) or (x==0)
    '''
        LOGICAL_EXPR : COMPARISON_EXPR
                     | COMPARISON_EXPR BINARY_LOGICAL_OPERATOR LOGICAL_EXPR
                     | NOT COMPARISON_EXPR
                     | NOT COMPARISON_EXPR BINARY_LOGICAL_OPERATOR LOGICAL_EXPR
        '''
    if len(p) == 2:
      p[0] = p[1]

  def p_comparison_expr(self, p):  # 2*x+1<2*y+1
    '''
        COMPARISON_EXPR : ADDITIVE_EXPR
                        | ADDITIVE_EXPR COMPARISON_OPERATOR ADDITIVE_EXPR
        '''
    if len(p) == 2:
      p[0] = p[1]
    
    
  def p_additive_expr(self, p):  # 5+2*x
    '''
        ADDITIVE_EXPR : MULTIPLICATIVE_EXPR
                      | MULTIPLICATIVE_EXPR ADDITIVE_OPERATOR ADDITIVE_EXPR
        '''
    if len(p) == 4:
      self.Quadruples.append(Quadruple(op=p[2], arg1=p[1], arg2=p[3]))
      p[0] = Quadruple.result
    
    elif len(p) == 2:
      p[0] = p[1]
    
    
  def p_multiplicative_expr(self, p):  # x*2
    '''
        MULTIPLICATIVE_EXPR : TERM
                            | MINUS TERM
                            | TERM MULTIPLICATIVE_OPERATOR MULTIPLICATIVE_EXPR
        '''
    if len(p) == 4:
      self.Quadruples.append(Quadruple(op=p[2], arg1=p[1], arg2=p[3]))
      p[0] = Quadruple.result
      
    if len(p) == 3:
      self.Quadruples.append(Quadruple(op=p[1], arg1=p[2], arg2=None))
      p[0] = Quadruple.result
    
    elif len(p) == 2:
      p[0] = p[1]

  def p_term(self, p):  # x
    '''
        TERM : IDENTIFIER
             | NUMBER
             | FUNCTION_CALL
             | LPAREN EXPRESSION RPAREN
             | TRUE
             | FALSE
        '''
    if len(p) == 4:
      p[0] = p[2]
    
    if len(p) == 2:
      p[0] = p[1]      
    


  ### Operators
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
    p[0] = p[1]

  def p_multiplicative_operator(self, p):
    '''
        MULTIPLICATIVE_OPERATOR : TIMES 
                                | DIVIDE
                                | MOD
                                | INT_DIVIDE
        '''
    p[0] = p[1]
    
    
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
     var y = 3
     x = 7 - 3 + (y) + 2 - (3 * 4);
    """

  root = P.parser.parse(code)  # returns the value of the root node

  for i in P.Quadruples:
    print(i)

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
  #with open("code.txt", "r") as f:
  #  code = f.read()

  #parse = P.parser.parse(code)

  # print(parse)
  # if parse is not None:
  #   print("Parsing Successful!")