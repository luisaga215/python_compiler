import ply.lex as lex
import ply.yacc as yacc
import re
import codecs
import os
import sys

reservadas = [
    'PROGRAMA',
    'FUNCION',
    'FINFUNC',
    'INICIO',
    'FIN',
    'SI',
    'FIN_SI',
    'SINO',
    'MIENTRAS',
    'FIN_MIENTRAS',
    'LEER',
    'ESCRIBIR',
    'LLAMAR',
    'ENTERO',
    'FLOTANTE',
    'AND',
    'OR'
]
 #Definicion de tokens

tokens = reservadas+[
    'NUM_ENTERO',
    'NUM_FLOTANTE',
    'PUNTOYCOMA',
    'ID',
    'SUMA',
    'RESTA',
    'DIVISION',
    'MULTIPLICACION',
    'CORCHETE1',
    'CORCHETE2',
    'DOS_PUNTOS',
    'GATO',
    'IGUAL',
    'STRING',
    'DOBLE_IGUAL',
    'MENOR',
    'MAYOR',
    'MENOR_IGUAL',
    'MAYOR_IGUAL',
    'COMA',
]

t_PROGRAMA = 'PROGRAMA'
t_ignore = '\t |" "'
t_SUMA = r'\+'
t_RESTA = r'\-'
t_MULTIPLICACION = r'\*'
t_DIVISION = r'\/'
t_DOBLE_IGUAL = r'\=='
t_MAYOR_IGUAL = '\=>'
t_MENOR_IGUAL = '\<='
t_PUNTOYCOMA = '\;'
t_DOS_PUNTOS = '\:'
t_IGUAL = r'\='
t_MENOR = r'\<'
t_MAYOR = r'\>'
t_CORCHETE1 = r'\['
t_CORCHETE2 = r'\]'
t_COMA = r'\,'
t_GATO = r'\#'
t_AND = r'\&&'
t_OR = r'\!!'

#Analizador de lexico

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)
    
def t_ID(t):
    r'[a-zA-Z][a-zA-Z0-9_]*'
    if t.value.upper() in reservadas:
       t.value = t.value.upper()
       t.type = t.value
    return t

def t_NUM_FLOTANTE(t):
    r'\d\.\d+'
    t.value = float(t.value)
    return t
       
def t_NUM_ENTERO(t):
    r'[0-9]+'
    t.value = int(t.value)
    return t

def t_STRING(t):
     r'\%.*\n*.*%'
     t.value = str(t.value)
     return t
    
def t_error(t):
    print ("caracter ilegal '%s'" % t.value[0])
    t.lexer.skip(1)


lexer = lex.lex()

#lexer.input('%abc_DEF.%okok56 5')

#Precedencia que probablemente no sea necesaria debido a que dentro de la sintaxis ya se manejan prioridades
precedence = (
    ('right','ID','LLAMAR','INICIO','SI','MIENTRAS'),
    ('right', 'ENTERO','FLOTANTE'),
    ('right', 'IGUAL'),
    ('left','MENOR','MAYOR','MENOR_IGUAL','MAYOR_IGUAL'),
    ('left','SUMA','RESTA'),
    ('left','MULTIPLICACION','DIVISION'),
    ('right','CORCHETE1','CORCHETE2')
    )

#Analizador sintactico

def p_programa(p):
    '''programa : PROGRAMA ID GATO variables funciones inicio'''
    #print("programa")

def p_variables1(p):
    '''
    variables : tipo comas GATO variables
    '''
    
    #print("variables1")
    
def p_variables2(p):
    '''
    variables : tipo ID CORCHETE1 NUM_ENTERO CORCHETE2 comas GATO variables
    '''
    #print("variables2")
    addArreglo(p[2],p[4])
    
def p_variables3(p):
    '''
    variables : tipo ID CORCHETE1 NUM_ENTERO CORCHETE2 CORCHETE1 NUM_ENTERO CORCHETE2 comas GATO variables
    '''
    #print("variables3")
    addMatrices(p[2],p[4],p[7])

    
def p_variablesEmpty(p):
    '''
    variables : empty
    '''
    #print("variablesEmpty")
    
def p_tipo(p):
    '''
    tipo : FLOTANTE
         | ENTERO
    '''
    #print("tipo")
    p[0]=p[1]
    
def p_funciones(p):
    '''
    funciones : funciones FUNCION idfunc DOS_PUNTOS repeat finfuncion
    '''
    #print("funciones")
    cuadReturn(p[6])
def p_idfunc(p):
    '''
    idfunc : ID
    '''
    matFunc(p[1])

def p_finfuncion(p):
    '''
    finfuncion : FINFUNC
    '''
    p[0]=p[1]
    
def p_funcionesEmpty(p):
    '''
    funciones : empty
    '''
    #print("funcionesEmpty")
    
def p_inicio(p):
    '''
    inicio : iniciox repeat FIN
    '''
    #print("inicio")
    cuadFin(p[3])

def p_iniciox(p):
    '''
    iniciox : INICIO
    '''
    cuadInicio(p[1])
    
def p_inicioEmpty(p):
    '''
    inicio : empty
    '''
    #print("inicioEmpty")
    
def p_repeat(p):
    '''
    repeat : repeat sentencia
    '''
    #print("gatito")
    p[0]=cuadruplosExp(p[2])
    
def p_gatitoEmpty(p):
    '''
    repeat : empty
    '''
    #print("gatitoEmpty")
    
def p_comas1(p):
    '''
    comas : ID comas
    '''
    #print("comas1")
    addMatriz(p[1])
    
def p_comas(p):
    '''
    comas : COMA comas
    '''
    
def p_comas2(p):
    '''
    comas : COMA ID CORCHETE1 NUM_ENTERO CORCHETE2 comas
    '''
    #print("comas2")
    addArreglo(p[2],p[4])
    
def p_comas3(p):
    '''
    comas : COMA ID CORCHETE1 NUM_ENTERO CORCHETE2 CORCHETE1 NUM_ENTERO CORCHETE2 comas
    '''
    #print("comas3")
    addMatrices(p[2],p[4],p[7])
    
def p_comasEmpty(p):
    '''
    comas : empty
    '''
    #print("comasEmpty")
    
def p_sentencia1_f(p):
    '''
    condicionx : condicion
    '''
    saltosIF(p[1])
    
def p_sentencia1(p):
    '''
    sentencia : SI condicionx repeat else findelsi 
    '''
    #print("sentencia1")

def p_findelsi(p):
    '''
    findelsi : FIN_SI
    '''
    saltosFinIf(p[1])
    
def p_elseEmpty(p):
    '''
    else : empty
    '''
    #print("elseEmpty")

def p_else(p):
    '''
    else : sinox repeat 
    '''
    #print("else")
    
def p_sinox(p):
    '''
    sinox : SINO
    '''
    saltosElse(p[1])
    
def p_sentencia2(p):
    '''
    sentencia : mientrass condiciony repeat fin_mientra
    '''
    #print("sentencia2")
    p[0]=(p[1],p[2])

def p_fin_mientra(p):
    '''
    fin_mientra : FIN_MIENTRAS
    '''
    saltosFinWhile(p[1])
    
def p_mientrass(p):
    '''
    mientrass : MIENTRAS
    '''
    saltosWhilepush(p[1])
    
def p_condiciony(p):
    '''
    condiciony : condicion
    '''
    saltosWhile(p[1])
    
def p_sentencia3(p):
    '''
    sentencia : ID IGUAL expresion PUNTOYCOMA
    '''
    #print("sentencia3")
    p[0]=(p[2],p[1],cuadruplosExp(p[3]))
    
    
def p_sentencia4(p):
    '''
    sentencia : ID CORCHETE1 NUM_ENTERO CORCHETE2 IGUAL expresion PUNTOYCOMA
              | ID CORCHETE1 ID CORCHETE2 IGUAL expresion PUNTOYCOMA
    '''
    #print("sentencia4")
    p[0]=(p[2],p[5],p[1],p[3],cuadruplosExp(p[6]))

    
def p_sentencia5(p):
    '''
    sentencia : ID CORCHETE1 NUM_ENTERO CORCHETE2 CORCHETE1 NUM_ENTERO CORCHETE2 IGUAL expresion PUNTOYCOMA
              | ID CORCHETE1 ID CORCHETE2 CORCHETE1 ID CORCHETE2 IGUAL expresion PUNTOYCOMA
              | ID CORCHETE1 NUM_ENTERO CORCHETE2 CORCHETE1 ID CORCHETE2 IGUAL expresion PUNTOYCOMA
              | ID CORCHETE1 ID CORCHETE2 CORCHETE1 NUM_ENTERO CORCHETE2 IGUAL expresion PUNTOYCOMA
    '''
    #print("sentencia5")
    p[0]=(p[4],p[8],p[1],p[3],p[6],cuadruplosExp(p[9]))
    
def p_sentencia6(p):
    '''
    sentencia : LEER ID PUNTOYCOMA
    '''
    #print("sentencia6")
    Read(p[1],p[2])
    
def p_sentencia7(p):
    '''
    sentencia : LEER ID CORCHETE1 NUM_ENTERO CORCHETE2 PUNTOYCOMA
              | LEER ID CORCHETE1 ID CORCHETE2 PUNTOYCOMA
    '''
    #print("sentencia7")
    Read2(p[1],p[2],p[4])
    
    
def p_sentencia8(p):
    '''
    sentencia : LEER ID CORCHETE1 NUM_ENTERO CORCHETE2 CORCHETE1 NUM_ENTERO CORCHETE2 PUNTOYCOMA
              | LEER ID CORCHETE1 ID CORCHETE2 CORCHETE1 ID CORCHETE2 PUNTOYCOMA
              | LEER ID CORCHETE1 NUM_ENTERO CORCHETE2 CORCHETE1 ID CORCHETE2 PUNTOYCOMA
              | LEER ID CORCHETE1 ID CORCHETE2 CORCHETE1 NUM_ENTERO CORCHETE2 PUNTOYCOMA
    '''
    #print("sentencia8")
    Read3(p[1],p[2],p[4],p[7])
    
def p_sentencia9(p):
    '''
    sentencia : ESCRIBIR ID PUNTOYCOMA
    '''
    #print("sentencia9")
    Write(p[1],p[2])
    
def p_sentencia10(p):
    '''
    sentencia : ESCRIBIR ID CORCHETE1 NUM_ENTERO CORCHETE2 PUNTOYCOMA
              | ESCRIBIR ID CORCHETE1 ID CORCHETE2 PUNTOYCOMA
    '''
    #print("sentencia10")
    Write2(p[1],p[2],p[4])

    
def p_sentencia11(p):
    '''
    sentencia : ESCRIBIR ID CORCHETE1 NUM_ENTERO CORCHETE2 CORCHETE1 NUM_ENTERO CORCHETE2 PUNTOYCOMA
              | ESCRIBIR ID CORCHETE1 ID CORCHETE2 CORCHETE1 ID CORCHETE2 PUNTOYCOMA
              | ESCRIBIR ID CORCHETE1 NUM_ENTERO CORCHETE2 CORCHETE1 ID CORCHETE2 PUNTOYCOMA
              | ESCRIBIR ID CORCHETE1 ID CORCHETE2 CORCHETE1 NUM_ENTERO CORCHETE2 PUNTOYCOMA
    '''
    #print("sentencia11")
    Write3(p[1],p[2],p[4],p[7])
    
def p_sentencia12(p):
    '''
    sentencia : ESCRIBIR STRING PUNTOYCOMA
    '''
    #print("sentencia12")
    Write(p[1],p[2])
    
def p_sentencia13(p):
    '''
    sentencia : LLAMAR ID PUNTOYCOMA
    '''
    #print("sentencia13")
    callFunc(p[1],p[2])
    
def p_condicion1(p):
    '''
    condicion : tb
    '''
    #print("condicion1")
    p[0]=cuadruplosExp(p[1])

def p_condicion3(p):
    '''
    condicion : condicion2
    '''
    p[0]=cuadruplosExp(p[1])
    
def p_condicion2(p):
    '''
    condicion2 : tb OR condicion
    '''
    #print("codicion2")
    p[0]= (p[2],cuadruplosExp(p[1]),cuadruplosExp(p[3]))
    
def p_tb1(p):
    '''
    tb : fb
    '''
    #print("tb1")
    p[0]=cuadruplosExp(p[1])
    
def p_tb2(p):
    '''
    tb : fb AND tb
    '''
    #print("tb2")
    p[0]= (p[2],cuadruplosExp(p[1]),cuadruplosExp(p[3]))
    
def p_fb(p):
    '''
    fb : expresion DOBLE_IGUAL expresion
       | expresion MAYOR expresion
       | expresion MENOR expresion
       | expresion MAYOR_IGUAL expresion
       | expresion MENOR_IGUAL expresion
    '''
    #print("fb")
    p[0]= (p[2],cuadruplosExp(p[1]),cuadruplosExp(p[3]))
    

def p_expresion(p):
    '''
    expresion : termino SUMA termino
              | termino RESTA termino
    '''
    #print("expresion")
    p[0]=(p[2],cuadruplosExp(p[1]),cuadruplosExp(p[3]))

def p_expresion2(p):
    '''
    expresion : termino
    '''
    p[0]=p[1]
    
def p_termino1(p):
    '''
    termino : factor MULTIPLICACION factor
            | factor DIVISION factor
    '''
    p[0]=(p[2],cuadruplosExp(p[1]),cuadruplosExp(p[3]))

def p_termino(p):
    '''
    termino : factor
    '''
    p[0]=p[1]
    
def p_factor_int_float(p):
    '''
    factor : NUM_ENTERO
           | NUM_FLOTANTE
    '''
    #print("factor_int_float")
    p[0]=p[1]
    
def p_factor_ID(p):
    '''
    factor : ID
    '''
    #print("expresion_ID")
    p[0]=('var',p[1])

def p_factor_exp(p):
    '''
    factor : expresion
    '''
    p[0]=p[1]

def p_expresion_arreglo(p):
    '''
    factor :    ID CORCHETE1 NUM_ENTERO CORCHETE2
              | ID CORCHETE1 ID CORCHETE2
    '''
    #print("expresion_arreglo")
    p[0]=('arreglo',p[1],p[3])
    
def p_expresion_matriz(p):
    '''
    factor :    ID CORCHETE1 NUM_ENTERO CORCHETE2 CORCHETE1 NUM_ENTERO CORCHETE2
              | ID CORCHETE1 ID CORCHETE2 CORCHETE1 ID CORCHETE2
              | ID CORCHETE1 NUM_ENTERO CORCHETE2 CORCHETE1 ID CORCHETE2
              | ID CORCHETE1 ID CORCHETE2 CORCHETE1 NUM_ENTERO CORCHETE2
    '''
    #print("expresion_matriz")
    p[0]=('matriz',p[1],p[3],p[6])

    
def p_empty(p):
    ''' empty :'''
    p[0]= None
    #print("empty")
    
    
def p_error(p):
    print("Error de Sintaxis")
    print("Error en la linea: "+str(p.lineno), ", columna: "+str(p.lexpos))


#Declaracion de matrices para cuadruplos, arreglos, matrices y funciones

cuadruplos = [[None for y in range(5)] for x in range(200)]
matrices = [[None for y in range(3)] for x in range(1000)]
arreglos = [[None for y in range(3)] for x in range(1000)]
funciones=[[None for y in range(2)] for x in range(50)]


variables={}            #diccionario de variables inicializadas a 0, es como las matrices pero se me hizo mas facil hacerlo en un diccionario
contArr=0
contMatrices=0
def addMatriz(p):       #agregar variables no dimensionadas al diccionario (no arreglos, no matrices)
    global variables
    global contMATR
    variables[p]=0
    
    
def addArreglo(p1,p2):                          #agregar arreglos a una matriz de arreglos
    global contArr
    arreglos[contArr][0]=p1
    arreglos[contArr][1]=p2
    contArr=contArr+1
    arreglos[contArr][0]='['
    contArr=contArr+1
    c=1
    while (c<=p2):
        arreglos[contArr][0]=c
        arreglos[contArr][1]=0
        contArr=contArr+1
        c=c+1
    arreglos[contArr][0]=']'
    contArr=contArr+1
        
def addMatrices(p1,p2,p3):                  #agregar matrices a una matriz de matrices
    global contMatrices
    matrices[contMatrices][0]=p1
    matrices[contMatrices][1]=p2
    matrices[contMatrices][2]=p3
    contMatrices=contMatrices+1
    matrices[contMatrices][0]='['
    contMatrices=contMatrices+1
    c=1
    while(c<=(p2*p3)):
        matrices[contMatrices][0]=c
        matrices[contMatrices][1]=0
        contMatrices=contMatrices+1
        c=c+1
    matrices[contMatrices][0]=']'
    contMatrices=contMatrices+1


temporales={}
numTempo=0                       #Diccionario de temporales
while (numTempo<100):
    tx='t'+str(numTempo)
    temporales[tx]= None
    numTempo=numTempo+1

saltos=[]
operandos=[]
operadores=[]
i=1
j=0
tnum=0
def rellenar(direccion,dir_salto):                          #Funcion de rellenar para cuadruplos pendientes
    cuadruplos[direccion][3]=dir_salto
                                                            #De aqui en adelante estan las funciones para la generacion de cuadruplos a partir
def saltosWhile(p):                                         #de la sintaxis establecida anteriormente
    global i
    cuadruplos[i][j]='gotoF'
    cuadruplos[i][j+1]=p
    saltos.append(i)
    i=i+1

def saltosWhilepush(p):
    global i
    saltos.append(i)
    
def saltosFinWhile(p):
    global i
    dir1=saltos.pop()
    dir2=saltos.pop()
    rellenar(dir1,i+1)
    cuadruplos[i][j]='goto'
    cuadruplos[i][3]=dir2
    i=i+1
    
def saltosIF(p):
    global i
    cuadruplos[i][j]='gotoF'
    cuadruplos[i][j+1]=p
    saltos.append(i)
    i=i+1


def saltosElse(p):
    global i
    dir1=saltos.pop()
    nexti=i+1
    rellenar(dir1,nexti)
    cuadruplos[i][j]='goto'
    saltos.append(i)
    i=i+1
    
def saltosFinIf(p):
    global i
    dir1=saltos.pop()
    rellenar(dir1,i)

def cuadruplosExp(p):
    global i
    global j
    global tnum
    global operandos
    global operadores
    global variables
    global temporales
    tx='t'+str(tnum)
    if (type(p)!=tuple):
          if(p!=None):
              operandos.append(p)
              return p
          
    if(type(p) == tuple):
        if (p[0]=='var'):
            operandos.append(p[1])
            return('$'+p[1])
        
        if (p[0]=='arreglo'):
            operandos.append(p[1])
            return('ARREGLO',p[1],p[2])
        
        if (p[0]=='matriz'):
            operandos.append(p[1])
            return('MATRIZ',p[1],p[2],p[3])
            
        elif(p[0]=='SI'):
            cuadruplos[i][j]='gotoF'
            cuadruplos[i][j+1]=p[1]
            saltos.append(i)
            i=i+1
             
        elif(p[0]=='!!'):
            #print(p, 'OR')
            cuadruplos[i][j]=p[0]
            j=j+1
            cuadruplos[i][j]=p[1]
            j=j+1
            cuadruplos[i][j]=p[2]
            j=j+1
            cuadruplos[i][j]=tx
            tant=tx
            tnum=tnum+1
            operandos.append(tx)
            i=i+1
            j=0
            return (tx)
        
        elif(p[0]=='&&'):
            #print(p, 'AND')
            cuadruplos[i][j]=p[0]
            j=j+1
            cuadruplos[i][j]=p[1]
            j=j+1
            cuadruplos[i][j]=p[2]
            j=j+1
            cuadruplos[i][j]=tx
            tant=tx
            tnum=tnum+1
            operandos.append(tx)
            i=i+1
            j=0
            return (tx)
        
        elif(p[0]=='<='):
            #print(p, 'menor igual')
            cuadruplos[i][j]=p[0]
            j=j+1
            cuadruplos[i][j]=p[1]
            j=j+1
            cuadruplos[i][j]=p[2]
            j=j+1
            cuadruplos[i][j]=tx
            tant=tx
            tnum=tnum+1
            operandos.append(tx)
            i=i+1
            j=0
            return (tx)

        elif(p[0]=='=>'):
            #print(p, 'mayor igual')
            cuadruplos[i][j]=p[0]
            j=j+1
            cuadruplos[i][j]=p[1]
            j=j+1
            cuadruplos[i][j]=p[2]
            j=j+1
            cuadruplos[i][j]=tx
            tant=tx
            tnum=tnum+1
            operandos.append(tx)
            i=i+1
            j=0
            return (tx)
        
        elif(p[0]=='>'):
            #print(p, 'mayor que')
            cuadruplos[i][j]=p[0]
            j=j+1
            cuadruplos[i][j]=p[1]
            j=j+1
            cuadruplos[i][j]=p[2]
            j=j+1
            cuadruplos[i][j]=tx
            tant=tx
            tnum=tnum+1
            operandos.append(tx)
            i=i+1
            j=0
            return (tx)
        
        elif(p[0]=='<'):
            #print(p, 'menor que')
            cuadruplos[i][j]=p[0]
            j=j+1
            cuadruplos[i][j]=p[1]
            j=j+1
            cuadruplos[i][j]=p[2]
            j=j+1
            cuadruplos[i][j]=tx
            tant=tx
            tnum=tnum+1
            operandos.append(tx)
            i=i+1
            j=0
            return (tx)
        
        elif(p[0]=='=='):
            #print(p, 'comparacion')
            cuadruplos[i][j]=p[0]
            j=j+1
            cuadruplos[i][j]=p[1]
            j=j+1
            cuadruplos[i][j]=p[2]
            j=j+1
            cuadruplos[i][j]=tx
            tant=tx
            tnum=tnum+1
            operandos.append(tx)
            i=i+1
            j=0
            return (tx)
        
        elif(p[0]=='='):
            #print(p,'igual')
            cuadruplos[i][j]=p[0]
            j=j+1
            cuadruplos[i][j]=p[1]
            j=j+2
            cuadruplos[i][j]=p[2]
            i=i+1
            j=0
            
        elif(p[0]=='['):                    #igual para arreglo
            cuadruplos[i][j]=p[1]
            j=j+1
            cuadruplos[i][j]=('ARREGLO',p[2],p[3])
            j=j+2
            cuadruplos[i][j]=p[4]
            i=i+1
            j=0

        elif(p[0]==']'):
            cuadruplos[i][j]=p[1]
            j=j+1
            cuadruplos[i][j]=('MATRIZ',p[2],p[3],p[4])
            j=j+2
            cuadruplos[i][j]=p[5]
            i=i+1
            j=0
            
        elif(p[0]=='+'):
            #print(p, 'suma')
            cuadruplos[i][j]=p[0]
            j=j+1
            cuadruplos[i][j]=p[1]
            j=j+1
            cuadruplos[i][j]=p[2]
            j=j+1
            cuadruplos[i][j]=tx
            tant=tx
            tnum=tnum+1
            operandos.append(tx)
            i=i+1
            j=0
            return (tx)
         
        elif(p[0]=='-'):
            #print(p,'resta')
            cuadruplos[i][j]=p[0]
            j=j+1
            cuadruplos[i][j]=p[1]
            j=j+1
            cuadruplos[i][j]=p[2]
            j=j+1
            cuadruplos[i][j]=tx
            cuadruplos[i][j]=tx
            tant=tx
            tnum=tnum+1
            operandos.append(tx)
            i=i+1
            j=0
            return (tx)
            
           
        elif(p[0]=='*'):
            #print(p, 'multiplicacion')
            cuadruplos[i][j]=p[0]
            j=j+1
            cuadruplos[i][j]=p[1]
            j=j+1
            cuadruplos[i][j]=p[2]
            j=j+1
            cuadruplos[i][j]=tx
            cuadruplos[i][j]=tx
            tant=tx
            tnum=tnum+1
            operandos.append(tx)
            i=i+1
            j=0
            return (tx)
            
           
        elif(p[0]=='/'):
            #print(p,'division')
            cuadruplos[i][j]=p[0]
            j=j+1
            cuadruplos[i][j]=p[1]
            j=j+1
            cuadruplos[i][j]=p[2]
            j=j+1
            cuadruplos[i][j]=tx
            tant=tx
            tnum=tnum+1
            operandos.append(tx)
            i=i+1
            j=0
            return (tx)
        

def Write(p1,p2):
    global i
    cuadruplos[i][0]=p1   #escribir
    if(p2[0]!='%'):
        cuadruplos[i][1]='ID'
    else:
        cuadruplos[i][1]='STRING'
    cuadruplos[i][3]=p2
    i=i+1

def Write2(p1,p2,p3):
    global i
    cuadruplos[i][0]=p1
    cuadruplos[i][1]=('ARREGLO',p2,p3)
    cuadruplos[i][2]=p3
    cuadruplos[i][3]=p2
    i=i+1

def Write3(p1,p2,p3,p4):
    global i
    cuadruplos[i][0]=p1
    cuadruplos[i][1]=('MATRIZ',p2,p3,p4)
    cuadruplos[i][2]=(p3,p4)
    cuadruplos[i][3]=p2
    i=i+1

def Read(p1,p2):
    global i
    cuadruplos[i][0]=p1
    cuadruplos[i][1]='VARIABLE'
    cuadruplos[i][3]=p2
    i=i+1

def Read2(p1,p2,p3):
    global i
    cuadruplos[i][0]=p1
    cuadruplos[i][1]=('ARREGLO',p2,p3)
    i=i+1

def Read3(p1,p2,p3,p4):
    global i
    cuadruplos[i][0]=p1
    cuadruplos[i][1]=('MATRIZ',p2,p3,p4)
    i=i+1

#llenar mi matriz de funciones
contFunc=0
def matFunc(p):
    global contFunc
    global i
    funciones[contFunc][0]=p
    funciones[contFunc][1]=i
    contFunc=contFunc+1

pilaReturn=[]
def callFunc(p1,p2):
    global contFunc
    global i
    n=0
    while (n<=contFunc):
        if(funciones[n][0]==p2):
            cuadruplos[i][0]='goto'
            cuadruplos[i][3]=funciones[n][1]
        n=n+1
    i=i+1
    dirDelReturn=pilaReturn.pop(0)
    cuadruplos[dirDelReturn][3]=i

def cuadReturn(p):
    global i
    cuadruplos[i][0]='goto'
    pilaReturn.append(i)
    i=i+1

def cuadInicio(p):
    cuadruplos[0][0]='goto'
    cuadruplos[0][1]=p
    cuadruplos[0][3]=i

def cuadFin(p):
    global i
    cuadruplos[i][0]=p


#PARSER, es un "modulo" del paquete PLY, sirve para "parsear" la sintaxis, en este caso estoy leyendo de un documento txt y le aplico la sintaxis establecida

ejemplo = '/Users/Luis Gonzalez/Desktop/compilador/ejemplo.txt'
parser = yacc.yacc()
test_file = open(ejemplo,'r')
Entrada = test_file.read()
test_file.close()
result = parser.parse(Entrada)

'''
lexer.input(Entrada)
while True:
    tok = lexer.token()
    if not tok:
        break
    print(tok.type)
'''
'''
while True:
    try:
        s= input('>> ')
    except EOFError:
        break
    parser.parse(s)
'''
#tok -> .type   . value    .lineon .lexpos

"""
#Funcion para imprimir cuadruplos
print('Cuadruplos:')
for x in range(i+1):
        print(cuadruplos[x][0],cuadruplos[x][1],cuadruplos[x][2],cuadruplos[x][3])
"""

######EJECUCION######
#Aqui se ejecutan todos los cuadruplos generados en el parser
def ejecucion():
    global i
    global variables
    PC=0
    while(PC<i):
        if(cuadruplos[PC][0]=='goto'):                                  #GOTO
            PC=cuadruplos[PC][3]
            
        elif(cuadruplos[PC][0]=='='):                                       #IGUALDAD
            if(type(cuadruplos[PC][1])!=tuple):
                cuad3=cuadruplos[PC][3]
                if(type(cuadruplos[PC][3])==int or type(cuadruplos[PC][3])==float):
                    variables[cuadruplos[PC][1]]=cuadruplos[PC][3]
                    PC=PC+1
                elif(type(cuad3)!=tuple and type(cuad3)!=int and type(cuad3)!=float and cuad3[0]=='t'):
                    variables[cuadruplos[PC][1]]=temporales[cuadruplos[PC][3]]
                    PC=PC+1
                elif(type(cuad3)==tuple):
                    variables[cuadruplos[PC][1]]=ArryMat(cuadruplos[PC][3])
                    PC=PC+1
                elif(str(cuad3[0])=='$' and type(cuad3)!=int):
                    variables[cuadruplos[PC][1]]=variables[str(cuad3[1:len(cuad3)])]
                    print(cuad3)
                    PC=PC+1
            
            elif(type(cuadruplos[PC][1])==tuple):
                cuad3=cuadruplos[PC][3]
                cuad1=cuadruplos[PC][1]
                direccion=dirMatyArr(cuad1)
                
                if(cuad1[0]=='MATRIZ'):
                    if(type(cuadruplos[PC][3])==int or type(cuadruplos[PC][3])==float):
                        matrices[direccion][1]=cuadruplos[PC][3]
                        PC=PC+1
                    elif(type(cuad3)!=tuple and type(cuad3)!=int and type(cuad3)!=float and cuad3[0]=='t'):
                         matrices[direccion][1]=temporales[cuadruplos[PC][3]]
                         PC=PC+1
                    elif(type(cuad3)==tuple):
                        matrices[direccion][1]=ArryMat(cuadruplos[PC][3])
                        PC=PC+1
                    elif(type(cuad3!=int)):
                        matrices[direccion][1]=variables[str(cuad3[1:len(cuad3)])]
                        PC=PC+1
                        
                elif(cuad1[0]=='ARREGLO'):
                    if(type(cuadruplos[PC][3])==int or type(cuadruplos[PC][3])==float):
                        arreglos[direccion][1]=cuadruplos[PC][3]
                        PC=PC+1
                    elif(type(cuad3)!=tuple and type(cuad3)!=int and type(cuad3)!=float and cuad3[0]=='t'):
                         arreglos[direccion][1]=temporales[cuadruplos[PC][3]]
                         PC=PC+1
                    elif(type(cuad3)==tuple):
                        arreglos[direccion][1]=ArryMat(cuadruplos[PC][3])
                        PC=PC+1
                    elif(type(cuad3)!=int):
                        arreglos[direccion][1]=variables[(cuad3[1:len(cuad3)])]
                        PC=PC+1  
            #print(temporales)
        elif(cuadruplos[PC][0]=='+'):
            cuad1=cuadruplos[PC][1]
            cuad2=cuadruplos[PC][2]
            if(type(cuadruplos[PC][1])==int or type(cuadruplos[PC][1])==float and type(cuadruplos[PC][1])!=tuple):
                a=cuadruplos[PC][1]
            elif(cuad1[0]=='$'):
                a=variables[cuad1[1:]]
            elif(cuad1[0]=='t'):
                a=temporales[cuad1]
            if(type(cuadruplos[PC][2])==int or type(cuadruplos[PC][2])==float and type(cuadruplos[PC][2])!=tuple):
                b=cuadruplos[PC][2]
            elif(cuad2[0]=='$'):
                b=variables[cuad2[1:]]
            elif(cuad2[0]=='t'):
                b=temporales[cuad2]
            if(type(cuadruplos[PC][1])==tuple):
                a=ArryMat(cuadruplos[PC][1])
            if(type(cuadruplos[PC][2])==tuple):
                b=ArryMat(cuadruplos[PC][2])
            temporales[cuadruplos[PC][3]]=(a+b)
            PC=PC+1
        elif(cuadruplos[PC][0]=='-'):
            cuad1=cuadruplos[PC][1]
            cuad2=cuadruplos[PC][2]
            if(type(cuadruplos[PC][1])==int or type(cuadruplos[PC][1])==float and type(cuadruplos[PC][1])!=tuple):
                a=cuadruplos[PC][1]
            elif(cuad1[0]=='$'):
                a=variables[cuad1[1:]]
            elif(cuad1[0]=='t'):
                a=temporales[cuad1]
            if(type(cuadruplos[PC][2])==int or type(cuadruplos[PC][2])==float and type(cuadruplos[PC][2])!=tuple):
                b=cuadruplos[PC][2]
            elif(cuad2[0]=='$'):
                b=variables[cuad2[1:]]
            elif(cuad2[0]=='t'):
                b=temporales[cuad2]
            if(type(cuadruplos[PC][1])==tuple):
                a=ArryMat(cuadruplos[PC][1])
            if(type(cuadruplos[PC][2])==tuple):
                b=ArryMat(cuadruplos[PC][2])
            temporales[cuadruplos[PC][3]]=(a-b)
            PC=PC+1
        elif(cuadruplos[PC][0]=='/'):
            cuad1=cuadruplos[PC][1]
            cuad2=cuadruplos[PC][2]
            if(type(cuadruplos[PC][1])==int or type(cuadruplos[PC][1])==float and type(cuadruplos[PC][1])!=tuple):
                a=cuadruplos[PC][1]
            elif(cuad1[0]=='$'):
                a=variables[cuad1[1:]]
            elif(cuad1[0]=='t'):
                a=temporales[cuad1]
            if(type(cuadruplos[PC][2])==int or type(cuadruplos[PC][2])==float and type(cuadruplos[PC][2])!=tuple):
                b=cuadruplos[PC][2]
            elif(cuad2[0]=='$'):
                b=variables[cuad2[1:]]
            elif(cuad2[0]=='t'):
                b=temporales[cuad2]
            if(type(cuadruplos[PC][1])==tuple):
                a=ArryMat(cuadruplos[PC][1])
            if(type(cuadruplos[PC][2])==tuple):
                b=ArryMat(cuadruplos[PC][2])
            temporales[cuadruplos[PC][3]]=(a/b)
            PC=PC+1
        elif(cuadruplos[PC][0]=='*'):
            cuad1=cuadruplos[PC][1]
            cuad2=cuadruplos[PC][2]
            if(type(cuadruplos[PC][1])==int or type(cuadruplos[PC][1])==float and type(cuadruplos[PC][1])!=tuple):
                a=cuadruplos[PC][1]
            elif(cuad1[0]=='$'):
                a=variables[cuad1[1:]]
            elif(cuad1[0]=='t'):
                a=temporales[cuad1]
            if(type(cuadruplos[PC][2])==int or type(cuadruplos[PC][2])==float and type(cuadruplos[PC][2])!=tuple):
                b=cuadruplos[PC][2]
            elif(cuad2[0]=='$'):
                b=variables[cuad2[1:]]
            elif(cuad2[0]=='t'):
                b=temporales[cuad2]
            if(type(cuadruplos[PC][1])==tuple):
                a=ArryMat(cuadruplos[PC][1])
            if(type(cuadruplos[PC][2])==tuple):
                b=ArryMat(cuadruplos[PC][2])
            temporales[cuadruplos[PC][3]]=(a*b)
            PC=PC+1
        elif(cuadruplos[PC][0]=='&&'):
            cuad1=cuadruplos[PC][1]
            cuad2=cuadruplos[PC][2]
            if(type(cuadruplos[PC][1])==int or type(cuadruplos[PC][1])==float and type(cuadruplos[PC][1])!=tuple):
                a=cuadruplos[PC][1]
            elif(cuad1[0]=='$'):
                a=variables[cuad1[1:]]
            elif(cuad1[0]=='t'):
                a=temporales[cuad1]
            if(type(cuadruplos[PC][2])==int or type(cuadruplos[PC][2])==float and type(cuadruplos[PC][2])!=tuple):
                b=cuadruplos[PC][2]
            elif(cuad2[0]=='$'):
                b=variables[cuad2[1:]]
            elif(cuad2[0]=='t'):
                b=temporales[cuad2]
            if(type(cuadruplos[PC][1])==tuple):
                a=ArryMat(cuadruplos[PC][1])
            if(type(cuadruplos[PC][2])==tuple):
                b=ArryMat(cuadruplos[PC][2])
            if(a and b):
                temporales[cuadruplos[PC][3]]=0
            else:
                temporales[cuadruplos[PC][3]]=1
            PC=PC+1
        elif(cuadruplos[PC][0]=='!!'):
            cuad1=cuadruplos[PC][1]
            cuad2=cuadruplos[PC][2]
            if(type(cuadruplos[PC][1])==int or type(cuadruplos[PC][1])==float and type(cuadruplos[PC][1])!=tuple):
                a=cuadruplos[PC][1]
            elif(cuad1[0]=='$'):
                a=variables[cuad1[1:]]
            elif(cuad1[0]=='t'):
                a=temporales[cuad1]
            if(type(cuadruplos[PC][2])==int or type(cuadruplos[PC][2])==float and type(cuadruplos[PC][2])!=tuple):
                b=cuadruplos[PC][2]
            elif(cuad2[0]=='$'):
                b=variables[cuad2[1:]]
            elif(cuad2[0]=='t'):
                b=temporales[cuad2]
            if(type(cuadruplos[PC][1])==tuple):
                a=ArryMat(cuadruplos[PC][1])
            if(type(cuadruplos[PC][2])==tuple):
                b=ArryMat(cuadruplos[PC][2])
            if(a or b):
                temporales[cuadruplos[PC][3]]=0
            else:
                temporales[cuadruplos[PC][3]]=1
            PC=PC+1
        elif(cuadruplos[PC][0]=='>'):
            cuad1=cuadruplos[PC][1]
            cuad2=cuadruplos[PC][2]
            if(type(cuadruplos[PC][1])==int or type(cuadruplos[PC][1])==float and type(cuadruplos[PC][1])!=tuple):
                a=cuadruplos[PC][1]
            elif(cuad1[0]=='$'):
                a=variables[cuad1[1:]]
            elif(cuad1[0]=='t'):
                a=temporales[cuad1]
            if(type(cuadruplos[PC][2])==int or type(cuadruplos[PC][2])==float and type(cuadruplos[PC][2])!=tuple):
                b=cuadruplos[PC][2]
            elif(cuad2[0]=='$'):
                b=variables[cuad2[1:]]
            elif(cuad2[0]=='t'):
                b=temporales[cuad2]
            if(type(cuadruplos[PC][1])==tuple):
                a=ArryMat(cuadruplos[PC][1])
            if(type(cuadruplos[PC][2])==tuple):
                b=ArryMat(cuadruplos[PC][2])
            if(a>b):
                temporales[cuadruplos[PC][3]]=0
            else:
                temporales[cuadruplos[PC][3]]=1
            PC=PC+1
        elif(cuadruplos[PC][0]=='<'):
            cuad1=cuadruplos[PC][1]
            cuad2=cuadruplos[PC][2]
            if(type(cuadruplos[PC][1])==int or type(cuadruplos[PC][1])==float and type(cuadruplos[PC][1])!=tuple):
                a=cuadruplos[PC][1]
            elif(cuad1[0]=='$'):
                a=variables[cuad1[1:]]
            elif(cuad1[0]=='t'):
                a=temporales[cuad1]
            if(type(cuadruplos[PC][2])==int or type(cuadruplos[PC][2])==float and type(cuadruplos[PC][2])!=tuple):
                b=cuadruplos[PC][2]
            elif(cuad2[0]=='$'):
                b=variables[cuad2[1:]]
            elif(cuad2[0]=='t'):
                b=temporales[cuad2]
            if(type(cuadruplos[PC][1])==tuple):
                a=ArryMat(cuadruplos[PC][1])
            if(type(cuadruplos[PC][2])==tuple):
                b=ArryMat(cuadruplos[PC][2])
            if(a<b):
                temporales[cuadruplos[PC][3]]=0
            else:
                temporales[cuadruplos[PC][3]]=1
                   
            PC=PC+1
        elif(cuadruplos[PC][0]=='=>'):
            cuad1=cuadruplos[PC][1]
            cuad2=cuadruplos[PC][2]
            if(type(cuadruplos[PC][1])==int or type(cuadruplos[PC][1])==float and type(cuadruplos[PC][1])!=tuple):
                a=cuadruplos[PC][1]
            elif(cuad1[0]=='$'):
                a=variables[cuad1[1:]]
            elif(cuad1[0]=='t'):
                a=temporales[cuad1]
            if(type(cuadruplos[PC][2])==int or type(cuadruplos[PC][2])==float and type(cuadruplos[PC][2])!=tuple):
                b=cuadruplos[PC][2]
            elif(cuad2[0]=='$'):
                b=variables[cuad2[1:]]
            elif(cuad2[0]=='t'):
                b=temporales[cuad2]
            if(type(cuadruplos[PC][1])==tuple):
                a=ArryMat(cuadruplos[PC][1])
            if(type(cuadruplos[PC][2])==tuple):
                b=ArryMat(cuadruplos[PC][2])
            if(a>=b):
                temporales[cuadruplos[PC][3]]=0
            else:
                temporales[cuadruplos[PC][3]]=1
            PC=PC+1
        elif(cuadruplos[PC][0]=='<='):
            cuad1=cuadruplos[PC][1]
            cuad2=cuadruplos[PC][2]
            if(type(cuadruplos[PC][1])==int or type(cuadruplos[PC][1])==float and type(cuadruplos[PC][1])!=tuple):
                a=cuadruplos[PC][1]
            elif(cuad1[0]=='$'):
                a=variables[cuad1[1:]]
            elif(cuad1[0]=='t'):
                a=temporales[cuad1]
            if(type(cuadruplos[PC][2])==int or type(cuadruplos[PC][2])==float and type(cuadruplos[PC][2])!=tuple):
                b=cuadruplos[PC][2]
            elif(cuad2[0]=='$'):
                b=variables[cuad2[1:]]
            elif(cuad2[0]=='t'):
                b=temporales[cuad2]
            if(type(cuadruplos[PC][1])==tuple):
                a=ArryMat(cuadruplos[PC][1])
            if(type(cuadruplos[PC][2])==tuple):
                b=ArryMat(cuadruplos[PC][2])
            if(a<=b):
                temporales[cuadruplos[PC][3]]=0
            else:
                temporales[cuadruplos[PC][3]]=1
            PC=PC+1

        elif(cuadruplos[PC][0]=='gotoF'):                                                   #GOTOf
            wat=temporales[cuadruplos[PC][1]]
            if(wat==1):
                PC=cuadruplos[PC][3]
            else:
                PC=PC+1
        elif(cuadruplos[PC][0]=='ESCRIBIR'):
            cuad1=cuadruplos[PC][1]
            cuad=cuadruplos[PC][3]
            if(cuadruplos[PC][1]=='STRING'):
                print(cuad[1:(len(cuad)-1)])
            elif(cuadruplos[PC][1]=='ID'):
                print(variables[cuad])
            elif(type(cuadruplos[PC][1])==tuple):
                print(ArryMat(cuad1))
            PC=PC+1
        elif(cuadruplos[PC][0]=='LEER'):
            cuad1=cuadruplos[PC][1]
            cuad3=cuadruplos[PC][3]
            if(type(cuad1)!=tuple):
                x=input('-> ')
                numero=int(x)
                try:
                    number=int(x)
                except ValueError:
                    print('No es un numero')
                variables[cuad3]=number
                PC=PC+1
            elif(type(cuad1)==tuple):
                x=input('-> ')
                numero=int(x)
                try:
                    number=int(x)
                except ValueError:
                    print('No es un numero')
                direccion=dirMatyArr(cuad1)
                if(cuad1[0]=='ARREGLO'):
                    arreglos[direccion][1]=number
                    PC=PC+1
                elif(cuad1[0]=='MATRIZ'):
                    matrices[direccion][1]=number
                    PC=PC+1
        #print(PC)
                    
#Funciones que se llaman dentro de la funcion de ejecucion, sirven para obtener valores o direcciones de las matrices de matrices o arreglos
def ArryMat(lista):
    global contMatrices
    global contArr
    if(lista[0]=='MATRIZ'):
        if(type(lista[2])==int or type(lista[2])==float):
            a=lista[2]
        elif(type(lista[2])!=int and type(lista[2])!=float):
            a=variables[lista[2]]
        if(type(lista[3])==int or type(lista[3])==float):
            b=lista[3]
        elif(type(lista[3])!=int and type(lista[3])!=float):
            b=variables[lista[3]]
        c=0
        while(c<contMatrices):
            if(matrices[c][0]==lista[1]):
                dire=((a*matrices[c][2])+b+c+1)
                return matrices[int(dire)][1]
            else:
                c=c+1
                
    if(lista[0]=='ARREGLO'):
        if(type(lista[2])==int or type(lista[2])==float):
            a=lista[2]
        elif(type(lista[2])!=int and type(lista[2])!=float):
            a=variables[lista[2]]
        c=0
        while(c<contArr):
            if(arreglos[c][0]==lista[1]):
                dire=(a+c+1)
                return arreglos[int(dire)][1]
            else:
                c=c+1

def dirMatyArr(lista):
    global contMatrices
    global contArr
    if(lista[0]=='MATRIZ'):
        if(type(lista[2])==int or type(lista[2])==float):
            a=lista[2]
        elif(type(lista[2])!=int and type(lista[2])!=float):
            a=variables[lista[2]]
        if(type(lista[3])==int or type(lista[3])==float):
            b=lista[3]
        elif(type(lista[3])!=int and type(lista[3])!=float):
            b=variables[lista[3]]
        c=0
        while(c<contMatrices):
            if(matrices[c][0]==lista[1]):
                dire=int((a*matrices[c][2])+b+c+1)
                return dire
            else:
                c=c+1

    if(lista[0]=='ARREGLO'):
        if(type(lista[2])==int or type(lista[2])==float):
            a=lista[2]
        elif(type(lista[2])!=int and type(lista[2])!=float):
            a=variables[lista[2]]
        c=0
        while(c<contArr):
            if(arreglos[c][0]==lista[1]):
                dire=int(a+c+1)
                return dire
            else:
                c=c+1
    
'''
#Imprimir mi matriz de funciones 
for x in range(10):
    print(funciones[x][0],funciones[x][1])
'''
'''
#Imprimir matriz de arreglos
for x in range(100):
    print(arreglos[x][0],arreglos[x][1],arreglos[x][2])
for x in range(400):
    print(matrices[x][0],matrices[x][1],matrices[x][2])
'''
#SE LLAMA AL EJECUTOR QUE ES PARA REALIZAR EL PROGRAMA EN EL DOCUMENTO QUE SE ESTA LEYENDO
ejecucion()

"""
#Imprimir mi matriz de matrices
for x in range(400):
    print(matrices[x][0],matrices[x][1],matrices[x][2])
"""
