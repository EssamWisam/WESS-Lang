from Lexer import Lexer
import ply.yacc as yacc
from tree import draw_tree



class Parser(object):

    tokens = Lexer.tokens
    
        
    def p_expression(self, p):                  ### Top-level rule 
        '''
        expression : expression PLUS term
                   | expression MINUS term
                   | term
        '''
        if len(p) == 4:                                 # Must be matching first or second rule
            p[0] = ('expr',  p[1], p[2], p[3])           
        
        elif len(p) == 2:                               # Must be the third rule
            p[0] = p[1]                                 # Ignored in the AST


    def p_term(self, p):
        '''
        term : term TIMES factor
        term : term DIVIDE factor
        term : factor
        '''
        if len(p) == 4:                                 # Must be matching first or second rule
            p[0] = ('term',  p[1], p[2], p[3])
        
        elif len(p) == 2:                             # Must be the third rule
            p[0] = p[1]
        


    def p_factor_num(self, p):
        '''
        factor : LPAREN expression RPAREN
        factor : NUMBER
        '''
        if len(p) == 4:
            p[0] = ('Grouped', p[2])
        elif len(p) == 2:
            p[0] = ('Number', p[1])


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
    7 - 3 + (5)
    """
    
    root = P.parser.parse(code)           # returns the value of the root node
    
    
    draw_tree(root)
