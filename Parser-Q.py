# pylint: disable=W,C,R
from Lexer import Lexer
from ply import yacc

class object():
    pass

class Quadruple:
  temp_count = 0
  reg_result = ""

  def __init__(self, op, arg1, arg2):
      self.op = op
      self.arg1 = arg1
      self.arg2 = arg2
      Quadruple.update_temp()
      self.reg_result = Quadruple.reg_result
      
  @classmethod
  def update_temp(cls):
      cls.reg_result = f"t{cls.temp_count}"
      cls.temp_count += 1

  def __str__(self):
      # dont print Nones
      return f"{self.reg_result} = {self.arg1} {self.op if self.op else ''} {self.arg2 if self.arg2 else ''}"


class Label:
  lbl_count = 0
  def __init__(self, label=None):
      self.label = label
      Label.lbl_count += 1
      if not self.label:
          self.label = f"L{Label.lbl_count}"
  
  
  
  def __str__(self):
      return f"{self.label}:"






class Parser(object):

  tokens = Lexer.tokens
  Quadruples = []
  arg_counter = 0
  Q_len = 0
  Q2_len = 0
  ### Top-level rules
  def p_program(self, p):  
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
                  | CONTINUE_STATEMENT
                  | BREAK_STATEMENT
        '''

  def p_block(self, p):
    '''
        BLOCK : LBRACE STATEMENT_LIST RBRACE
              | LBRACE RBRACE
        '''
  
  ### ADDED RULE
  def p_fblock(self, p):
    '''
        FBLOCK : LBRACE STATEMENT_LIST RBRACE
               | LBRACE RBRACE'''
  
  ### ADDED RULE
  def p_pop(self, p):
    '''
        POP : epsilon
        '''
    self.Quadruples.append("POP IPC \n")
  
  # will be back for code generation (hmmm.)

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
    if len(p) == 3:
      p[0] = object()
      p[0].reg = p[2]
      self.Quadruples.append(Quadruple(op=None, arg1="undefined", arg2=None))
      # Todo: add the assigned register Quadruple.result (of identifier) to the symbol table
    if len(p) == 5:
      p[0] = object()
      p[0].reg = p[4].reg
      self.Quadruples.append(Quadruple(op=None, arg1=p[4].reg, arg2=None))
      # Todo: add the assigned register Quadruple.result to the symbol table
    
  def p_const_declaration(self, p):
    '''
        CONST_DECLARATION : CONST IDENTIFIER ASSIGN EXPRESSION
        '''
    p[0] = object()
    p[0].reg = p[4].reg
    self.Quadruples.append(Quadruple(op=None, arg1=p[4].reg, arg2=None))
    # Todo: add the assigned register Quadruple.result to the symbol table

  def p_enum_declaration(self, p):
    '''
        ENUM_DECLARATION : ENUM IDENTIFIER LBRACE ENUM_MEMBER_LIST RBRACE
        '''
    # Skip Enum
  def p_enum_member_list(self, p):
    '''
        ENUM_MEMBER_LIST : IDENTIFIER
                         | IDENTIFIER COMMA ENUM_MEMBER_LIST
        '''
    # Skip Enum
  def p_enum_var_declaration(self, p):
    '''
        ENUM_VAR_DECLARATION : ENUM IDENTIFIER IDENTIFIER ASSIGN IDENTIFIER
                             | ENUM IDENTIFIER IDENTIFIER
        '''
    # Skip Enum
    
    
  ### MODIFIED RULE
  def p_function_declaration(self, p):
    '''
        FUNCTION_DECLARATION : FUNCTION IDENTIFIER LPAREN PARAMETER_LIST RPAREN LBL PUSH FBLOCK POP
        '''
    
    ## TODO add the function's label to the symbol table (perhaps) 

  ### ADDED RULE
  def p_label(self, p):
    '''
        LBL : epsilon
        '''
    self.Quadruples.append(Label(p[-4]))

  ### ADDED RULE
  def p_PUSH(self, p):
      '''
          PUSH : epsilon
          '''
      self.Quadruples.append("PUSH IPC")



  def p_parameter_list(self, p): #TODO add epsilon
    '''
        PARAMETER_LIST : IDENTIFIER
                       | IDENTIFIER COMMA PARAMETER_LIST
                       | epsilon
        '''
    #p[0] = object()
    if len(p) == 2:
      self.Quadruples.append(f"param {p[1]}")
    if len(p) == 4:
      self.Quadruples.append(f"param {p[1]}")

  ### Statements
  def p_return_statement(self, p):
    '''
      RETURN_STATEMENT : RETURN EXPRESSION
      '''
    self.Quadruples.append(f"return {p[2].reg}")
  
  def p_function_call(self, p):
    '''
        FUNCTION_CALL : IDENTIFIER LPAREN ARGUMENT_LIST RPAREN
        '''
    self.Quadruples.append(f"call {p[1]}, {self.arg_counter} \n")
    self.arg_counter = 0
    
  def p_argument_list(self, p):# TODO add epsilon
    '''
        ARGUMENT_LIST : EXPRESSION
                      | EXPRESSION COMMA ARGUMENT_LIST
                      | epsilon
        '''
    self.Quadruples.append(f"arg {p[1].reg}")
    self.arg_counter += 1
    
  def p_assignment(self, p):
    '''
        ASSIGNMENT : IDENTIFIER ASSIGN EXPRESSION
        '''
    self.Quadruples.append(Quadruple(op=None, arg1=p[3].reg, arg2=None))
    p[0] = object()
    p[0].reg = p[1]
    # Todo: add the assigned register to the symbol table
    
  ### Modified
  def p_if_statement(self, p):
    '''
        IF_STATEMENT : IF LPAREN EXPRESSION JMPF RPAREN BLOCK IF_LBL
                     | IF LPAREN EXPRESSION JMPF RPAREN BLOCK JMP ELSE IFELSE_LBL BLOCK ELSE_LBL
        '''
    if len(p) == 6:
      self.Quadruples.append(p[4].label)

    
  ### Added
  def p_jmpf(self, p):
    '''
        JMPF : epsilon
        '''
    p[0] = object()
    p[0].label = Label()
    self.Quadruples.append(f"JMPF {p[-1].reg} {p[0].label}")


  ### Added
  def p_jmp(self, p):
      '''
          JMP : epsilon
          '''
      p[0] = object()
      p[0].label = Label()
      self.Quadruples.append(f"JMP {p[0].label}")
    
  ### Added
  def p_else_lbl(self, p):
    '''
        ELSE_LBL : epsilon
        '''
    self.Quadruples.append(p[-4].label)
      
  ### Added
  def p_if_lbl(self, p):
    '''
        IF_LBL : epsilon
        '''
    self.Quadruples.append(p[-3].label)
    
  ### Added
  def p_ifelse_lbl(self, p):
    '''
        IFELSE_LBL : epsilon
        '''
    self.Quadruples.append(p[-5].label)


  ### Modified
  ### todo: use the same register for the while condition using the symbol table 
  def p_loop(self, p):
    '''
        LOOP : WHILE_LBL_2 WHILE LPAREN EXPRESSION JMPF_WHILE_1 RPAREN BLOCK JMP_WHILE_2 WHILE_LBL_1
             | FOR LPAREN ASSIGNMENT SEMICOLON EXPRESSION SEMICOLON SPY1 ASSIGNMENT RPAREN SPY2 BLOCK 
             | FOR LPAREN VAR_DECLARATION SEMICOLON EXPRESSION SEMICOLON SPY1 ASSIGNMENT RPAREN SPY2 BLOCK
             | WHILE_LBL_2 DO BLOCK WHILE LPAREN EXPRESSION RPAREN SEMICOLON JMPT_WHILE
        '''
    if p.slice[1].value == 'for':
      num_new = len(self.Quadruples) - self.Q_len
      block_instructions = self.Quadruples[-num_new:]
      self.Quadruples = self.Quadruples[:-num_new]
      
      num_of_assignments_quadruples = self.Q_len - self.Q2_len
      new_assignments = self.Quadruples[-num_of_assignments_quadruples:]
      self.Quadruples = self.Quadruples[:-num_of_assignments_quadruples]
      
      lbl1, lbl2 = Label(), Label()
      self.Quadruples.append(lbl1)
      self.Quadruples.append(f"JMPF {p[5].reg} {lbl2}")
      
      self.Quadruples.extend(block_instructions)
      
      self.Quadruples.extend(new_assignments)
      
      self.Quadruples.append(f"JMP {lbl1}")
      self.Quadruples.append(lbl2)
      #for inst in block_instructions:
      #    print(inst)
    
    
    '''
    for (var x = 0; x < 10; x = x + 1) {
      var x = 5;
      var y = 4;
    }
    
    var x = 0;
    while (x < 10) {
      var x = 5;
      var y = 7;
      x = x + 1;
      }
      
      var x = 0; <<<
      t2 = x < 10 <<<
      L1:
      JMPF t2 L2
      x = x + 1; <<<
      var x = 5; <<<
      var y = 7; <<<
      JMP L1
      L2:

    '''

  
  ### Added1
  def p_SPY_1(self, p):
    '''
        SPY1 : epsilon
        '''
    self.Q2_len = len(self.Quadruples)

  ### Added
  def p_SPY_2(self, p):
    '''
        SPY2 : epsilon
        '''
    self.Q_len = len(self.Quadruples)
  
  ### Added
  def p_while_lbl_1(self, p):
    '''
        WHILE_LBL_1 : epsilon
        '''
    self.Quadruples.append(p[-4].label)

  ### Added
  def p_while_lbl_2(self, p):
    '''
        WHILE_LBL_2 : epsilon
        '''
    p[0] = object()
    p[0].label = Label()
    self.Quadruples.append(p[0].label)
    
  ### Added
  def p_while_jmpf(self, p):
    '''
        JMPF_WHILE_1 : epsilon
        '''
    p[0] = object()
    p[0].label = Label()
    self.Quadruples.append(f"JMPF {p[-1].reg} {p[0].label}")
    
  ### Added
  def p_while_jmp(self, p):
    '''
        JMP_WHILE_2 : epsilon
        '''    
    self.Quadruples.append(f"JMP {p[-7].label}")
    
    
  ### Added
  def p_while_jmpt(self, p):
    '''
        JMPT_WHILE : epsilon
        '''
    p[0] = object()
    p[0].label = Label()
    self.Quadruples.append(f"JMPT {p[-3].reg} {p[-8].label}")
    
    
  ### Canceled 
  def p_continue_statement(self, p):
    '''
        CONTINUE_STATEMENT : CONTINUE SEMICOLON
        '''
  ### Canceled
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
    p[0] = object()
    p[0].reg = p[1].reg

  ### Remove last rule
  def p_logical_expr(self, p):  # (y<0) and (2*x+1>0) or (x==0)
    '''
        LOGICAL_EXPR : COMPARISON_EXPR
                     | COMPARISON_EXPR BINARY_LOGICAL_OPERATOR LOGICAL_EXPR
                     | NOT COMPARISON_EXPR
                     | NOT COMPARISON_EXPR BINARY_LOGICAL_OPERATOR LOGICAL_EXPR
        '''
    if len(p) == 2:
      p[0] = object()
      p[0].reg = p[1].reg
    
    if len(p) == 4:
      self.Quadruples.append(Quadruple(op=p[2], 
                                       arg1=p[1].reg, 
                                       arg2=p[3].reg))
      p[0] = object()
      p[0].reg = Quadruple.reg_result
      
    if len(p) == 3:
      self.Quadruples.append(Quadruple(op=p[1], 
                                       arg1=p[2].reg, 
                                       arg2=None))
      p[0] = object()
      p[0].reg = Quadruple.reg_result
      

  def p_comparison_expr(self, p):  # 2*x+1<2*y+1
    '''
        COMPARISON_EXPR : ADDITIVE_EXPR
                        | ADDITIVE_EXPR COMPARISON_OPERATOR ADDITIVE_EXPR
        '''
    if len(p) == 2:
     p[0] = object()
     p[0].reg = p[1].reg
    
    if len(p) == 4:
      self.Quadruples.append(Quadruple(op=p[2], 
                                       arg1=p[1].reg, 
                                       arg2=p[3].reg))
      p[0] = object()
      p[0].reg = Quadruple.reg_result
    
  def p_additive_expr(self, p):  # 5+2*x
    '''
        ADDITIVE_EXPR : MULTIPLICATIVE_EXPR
                      | MULTIPLICATIVE_EXPR ADDITIVE_OPERATOR ADDITIVE_EXPR
        '''
    
    if len(p) == 4:
      self.Quadruples.append(Quadruple(op=p[2], 
                                       arg1=p[1].reg 
                                       , 
                                       arg2=p[3].reg 
                                       ))
      p[0] = object()
      p[0].reg = Quadruple.reg_result
    
    elif len(p) == 2:
      p[0] = object()
      p[0].reg = p[1].reg
    
    
  def p_multiplicative_expr(self, p):  # x*2
    '''
        MULTIPLICATIVE_EXPR : TERM
                            | MINUS TERM
                            | TERM MULTIPLICATIVE_OPERATOR MULTIPLICATIVE_EXPR
        '''
    if len(p) == 4:
      self.Quadruples.append(Quadruple(op=p[2],
                                       arg1=p[1].reg,
                                       arg2=p[3].reg))
      p[0] = object()
      p[0].reg = Quadruple.reg_result
      
    if len(p) == 3:
      self.Quadruples.append(Quadruple(op=p[1], 
                                       arg1=p[2].reg, 
                                       arg2=None))
      p[0] = object()
      p[0].reg = Quadruple.reg_result
    
    elif len(p) == 2:
      p[0] = object()
      p[0].reg = p[1].reg

  def p_term(self, p):  
    '''
        TERM : IDENTIFIER
             | NUMBER
             | FUNCTION_CALL
             | LPAREN EXPRESSION RPAREN
             | TRUE
             | FALSE
        '''
    if len(p) == 4:     # 4th rule (exception)
      p[0] = object()
      p[0].reg = p[2].reg 
  
    # from lexer
    if len(p) == 2:
      p[0] = object()
      p[0].reg = p[1] 

      

  ### Operators
  # from lexer
  def p_logical_operator(self, p):
    '''
        BINARY_LOGICAL_OPERATOR : AND
                                | OR
        '''
    p[0] = p[1]
        
  # from lexer
  def p_comparison_operator(self, p):
    '''
        COMPARISON_OPERATOR : EQ
                            | NE
                            | LT
                            | LE
                            | GT
                            | GE
        '''
    p[0] = p[1]
    
  # from lexer
  def p_additive_operator(self, p):
    '''
        ADDITIVE_OPERATOR : PLUS
                          | MINUS
        '''
    p[0] = p[1]

  # from lexer
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


  code_exprs = \
  """
     const y = 7;
     var x;
     y = 2;
     x = 5 + y;
    """

  code_funcs = \
  """
      function foo(x, y, z) {
        x = 4;
        y = 5 + z;
        return x+y;
        }
        
        foo(1, 2, 3);
        
        bar(1, 2, 3, 6, 7, x+y);

  """
  
  
  code_ifs = \
  """
      if (x > 5 and x < 10) {
        var x = 3;
      }
      else{
        var u = 5;
      }
        
        
  """
  
  
  
  code_whiles = \
    """
    #while (x > 5 and x < 10) {
    #  var x = 3;
    #}
    
    #do {
    #  var x = 5;
    #  var y = 6;
    #}
    #while(x > 5 and x < 10);
    
    for (var x=0; x<10; x=x+1){
      y =5;
      z =4;
    }
    
    """
  root = P.parser.parse(code_whiles)  # returns the value of the root node


  for i in P.Quadruples:
   print(i)



