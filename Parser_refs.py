# pylint: disable=W,C,R
from Lexer_ref import Lexer
from ply import yacc
from tree import draw_tree



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
      return f"{self.result} = {self.arg1} {self.op} {self.arg2}"


class Parser(object):
  tokens = Lexer.tokens
  Quadruples = []

  def p_expression(self, p):
      '''
      expression : expression PLUS term
                  | expression MINUS term
                  | term
      '''
      if len(p) == 4:
          self.Quadruples.append(Quadruple(op=p[2], arg1=p[1], arg2=p[3]))
          p[0] = Quadruple.result

      elif len(p) == 2:
          p[0] = p[1]

  def p_term(self, p):
      '''
      term : term TIMES factor
            | term DIVIDE factor
            | factor
      '''
      if len(p) == 4:
          self.Quadruples.append(Quadruple(op=p[2], arg1=p[1], arg2=p[3]))
          p[0] = Quadruple.result

      elif len(p) == 2:
          p[0] = p[1]

  def p_factor_num(self, p):
      '''
      factor : LPAREN expression RPAREN
              | NUMBER
      '''
      if len(p) == 4:  # Matching first rule
          p[0] = p[2]  # Return the value of the expression inside parentheses
      elif len(p) == 2:  # Matching second rule
          p[0] = p[1]  # Return the value of the number

  def p_error(self, p):
    print("Syntax error in input!")

  def __init__(self):
    self.lexer = Lexer()
    self.parser = yacc.yacc(module=self)


if __name__ == "__main__":
  ### Test the parser
  P = Parser()

  code = \
  """
    1 + 2 + 3 + 4 * 6
    """

  root = P.parser.parse(code)  # returns the value of the root node


  #draw_tree(root)
  # print P.Quadruples with each line on a separate line
  for i in P.Quadruples:
    print(i)
