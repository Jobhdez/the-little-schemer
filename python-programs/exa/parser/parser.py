import ply.lex as lex
import ply.yacc as yacc
reserved = {
    'if': 'IF',
    'let': 'LET',
    'while': 'WHILE',
    'begin': 'BEGIN',
    'and': 'AND',
    'or': 'OR',
    'set': 'SET',
    'not': 'NOT',
    'eq?': 'EQ',
    }
tokens = [
    'LPAREN', 'RPAREN', 'PLUS', 'MINUS',
    'LESS', 'GREATER', 'LESSEQ',
    'GREATEREQ', 'INTEGER',
    'TRUE', 'FALSE', 'NAME',
    ] + list(reserved.values())

t_PLUS = r'\+'
t_MINUS = r'-'
t_IF = r'if'
t_LET = r'let'
t_BEGIN = r'begin'
t_AND = r'and'
t_OR = r'or'
t_TRUE = r'\#t'
t_FALSE = r'\#f'
t_LESS = r'<'
t_EQ = r'eq?'
t_LESSEQ = r'<='
t_GREATER = r'>'
t_GREATEREQ = r'>='
t_NOT = r'not'
t_SET = r'set'
t_WHILE = r'while'
t_LPAREN  = r'\('
t_RPAREN  = r'\)'

def t_NAME(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    t.type = reserved.get(t.value, 'NAME')
    return t


def t_INTEGER(t):
    r'-?[0-9]+'
    t.value = int(t.value)
    return t

t_ignore = '\t\n'

def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)
    
lexer = lex.lex()

def p_program(p):
    "expressions : expression expressions"
    p[0] = Exps(p[1], p[2])

def p_program_empty(p):
    "expressions : expression"
    p[0] = Exp(p[1])
    
def p_expression_int(p):
    "expression : INTEGER"
    p[0] = Int(p[1])

def p_expression_bool(p):
    """expression : TRUE
                  | FALSE"""
    p[0] = Bool(p[1])

def p_expression_var(p):
    "expression : NAME"
    p[0] = Var(p[1])

    
def p_expression_prim(p):
    "expression : LPAREN op expression expression RPAREN"
    p[0] = Prim(p[2], p[3], p[4])
    
def p_expression_if(p):
    "expression : LPAREN IF expression expression expression RPAREN"
    p[0] = If(p[3], p[4], p[5])

def p_expression_while(p):
    "expression : LPAREN WHILE expression expression RPAREN"
    p[0] = While(p[3], p[4])

def p_expression_set(p):
    "expression : LPAREN SET expression expression RPAREN"
    p[0] = SetBang(p[3], p[4])

def p_expression_let(p):
    "expression : LPAREN LET binding expressions RPAREN"
    p[0] = Let(p[3], p[4])

def p_expression_begin(p):
    "expression : LPAREN BEGIN expressions RPAREN"
    p[0] = Begin(p[3])


def p_op(p):
    """op : PLUS
        | MINUS
        | AND
        | OR
        | EQ
        | LESS
        | LESSEQ
        | GREATER
        | GREATEREQ
        | NOT"""
    p[0] = Op(p[1])

def p_binding(p):
    "binding : LPAREN LPAREN NAME expression RPAREN RPAREN"
    p[0] = Binding(p[3], p[4])

def p_error(p):
    print("Syntax error at '%s'" % p.value)
                 
parser = yacc.yacc()
### Parse Nodes

class Program:
    "PROGRAM node."
    def __init__(self, expressions):
        self.expressions = expressions

    def __repr__(self):
        return f'(Program {self.expressions})'

class Nil:
    pass
    
class Exps:
    "PROGRAM node."
    def __init__(self, exp,  expressions):
        self.exp = exp
        self.expressions = expressions

    def __repr__(self):
        return f'(Exps {self.exp}  {self.expressions})'

class Exp:
    "PROGRAM node."
    def __init__(self, exp):
        self.exp = exp
      

    def __repr__(self):
        return f'(Exp {self.exp})'

class Prim:
    def __init__(self, op, exp1, exp2):
        self.op = op
        self.exp1 = exp1
        self.exp2 = exp2

    def __repr__(self):
        return f'(Prim {self.op} {self.exp1} {self.exp2})'
    
class If:
    "IF node."
    def __init__(self, condition, scmthen, scmelse):
        self.condition = condition
        self.scmthen = scmthen
        self.scmelse = scmelse

    def __repr__(self):
        return f'(IF (Condition {self.condition}) (Then {self.scmthen}) (Else {self.scmelse}))'

class Bool:
    def __init__(self, boolscm):
        self.boolscm = boolscm

    def __repr__(self):
        return f'(Bool {self.boolscm})'

class Begin:
    def __init__(self, exps):
        self.exps = exps

    def __repr__(self):
        return f'(Begin {self.exps})'

class While:
    def __init__(self, condition, expr):
        self.condition = condition
        self.expr = expr

    def __repr__(self):
        return f'(While {self.condition} {self.expr})'

class Let:
    def __init__(self, binding, expr):
        self.binding = binding
        self.expr = expr

    def __repr__(self):
        return f'(Let {self.binding} {self.expr})'

class SetBang:
    def __init__(self, var, expr):
        self.var = var
        self.expr = expr

    def __repr__(self):
        return f'(SetBang {self.var} {self.expr})'

class Int:
    "INT node."
    def __init__(self, num):
        self.num = num

    def __repr__(self):
        return f'(Int {self.num})'

class Op:
    def __init__(self, op):
        self.op = op

    def __repr__(self):
        return f'(OP {self.op})'


class Binding:
    "BINDING node."
    def __init__(self, var, exp):
        self.var = var
        self.exp = exp

        
    def __repr__(self):
        return f'(Binding {self.var} {self.exp})'

class Var:
    def __init__(self, var):
        self.var = var

    def __repr__(self):
        return f'(Var {self.var})'
