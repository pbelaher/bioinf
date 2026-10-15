### Strings ###

my_string = 'Mi String'
my_other_string = "Mi otro String"

print(len(my_string))
print(len(my_other_string))

print(my_string + ' ' + my_other_string)

'''\n: new line
   \t: Tab means(8 spaces)
   \\: Back slash
   \': Single quote (')
   \": Double quote (")'''

my_new_line_string = 'Este es un string \n con salto de línea'
print(my_new_line_string)

my_new_tab_string = '\t Este es un string con tabulación'
print(my_new_tab_string)

my_scape_string = '\\ Este es un string \\n escapado'
print(my_scape_string) #\ Este es un string \n escapado

multiline_string = '''Soy bioinformatica y quiero
aprender Python lo mejor posible.'''
print(multiline_string)

# Formateo antiguo (casi que mejor)

'''%s - String (or any object with a string representation, like numbers)
   %d - Integers
   %f - Floating point numbers
   %.number of digitsf" - Floating point numbers with fixed precision'''

name, surname, age = 'Paula', 'Belaza', 22
print('Mi nombre es %s %s y mi edad es %d' %(name, surname, age))

# Formateo nuevo

name, surname, age = 'Paula', 'Belaza', 22
print('Mi nombre es {} {} y mi edad es {}' .format(name, surname, age))

print(f'Mi nombre es {name} {surname} y mi edad es {age}') #inferencia preferida

# Desempaquetado de caracteres

#language = 'Python' #aqui tengo 6 caracteres
#a, b = language #aqui solo estoy creando 2 variables, tendria que crear c, d, e y f

language = 'python'
a, b, c, d, e, f = language
print(a) #P
print(b) #Y 

# División 

language_slice = language[1:3]
print(language_slice) # output: yt--> P(0) Y(1) T(2) H(3) O(4) N(5) nos ha codigo las letras de la 1 a la 3 sin contar la 3

language_slice = language[1:] # de la 1 al final
print(language_slice) #output: ython 

language_slice = language[-2] # P=0 --> PYTHONPYTHON--> O(-2) N(-1) P(0)
print(language_slice) # output: o 

language_slice = language[0:6:2] # texto[inicio:fin:paso/avanza de _ en _]
print(language_slice) #output: pto

# Reverse

reversed_language = language[::-1]
print(reversed_language) # nohtyP

# Funciones

print(language.capitalize()) # Python --> pone en mayuscula la primera
print(language.upper()) # PYTHON --> todo en mayúscula
print(language.count('t')) 
print(language.isnumeric()) # False
print('1'.isnumeric()) # True
print(language.lower()) #python
print(language.startswith('py')) # True

print(language.upper().isupper()) # True
