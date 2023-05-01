# pylint: disable=W,C,R
from ply import lex


class Lexer(object):

  def __init__(self):
    self.lexer = lex.lex(module=self)

  ### List of token names

  reserved = {
      'if': 'IF',
      'else': 'ELSE',
      'while': 'WHILE',
      'for': 'FOR',
      'int': 'INT',
      'float': 'FLOAT',
      'return': 'RETURN',
      'bool': 'BOOL',
  }

  tokens = [
      'NUMBER',  # e.g., 234
      'ID',  # Identifiers
      'PLUS',  # +
      'MINUS',  # -
      'TIMES',  # *
      'DIVIDE',  # /
      'LPAREN',  # (
      'RPAREN',  # )
  ] + list(reserved.values())  # add reserved words to tokens list

  # All of these can be defined as functions like t_NUMBER below but we don't need to any actions so this works.
  # Defining as funtions allows to control precedence otherwise larger ones are matched first.
  t_PLUS = r'\+'
  t_MINUS = r'-'
  t_TIMES = r'\*'
  t_DIVIDE = r'/'
  t_LPAREN = r'\('
  t_RPAREN = r'\)'

  def t_NUMBER(self, t):
    r'[0-9]+'
    # recall, any token read is associated with type(NUMBER), value(assigned as matched string unless overridden here),
    # lineno(from lexer.lineno below), lexpos(position relative to the start of the input text)
    # all four are printed with each token when you run the file.

    # create an entry in the symbol table once the token is created and put the value in it
    t.value = int(t.value)
    return t

  def t_ID(self, t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = Lexer.reserved.get(
        t.value, 'ID'
    )  # If its reserved (and found), it should rather take type of reserved word
    return t

  # A string containing ignored characters as strings
  t_ignore = ' ' + '\t'

  def t_COMMENT(self, t):
    r'\#.*'
    pass  # in other words, discard.

  ### Now each token has a rule

  # Define a rule so we can track line numbers
  def t_newline(self, t):
    r'\n+'
    t.lexer.lineno += len(t.value)

  # Error handling rule
  def t_error(self, t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)  # skip one character (this one)


if __name__ == "__main__":  # so that code below isn't invoked if the module is imported.
  ### Test the lexer
  L = Lexer()

  code = \
  """
    if else
    3 + 2
    # This is a comment!
    7 - 2 + 1
    1 + 1 
    """

  L.lexer.input(code)
  while True:
    tok = L.lexer.token()  # get next token
    if not tok:  # no more tokens
      break
    #print(tok)
