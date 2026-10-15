# Variables

MyVariable = 'my string variable'
print(MyVariable) #esto no me daría error, pero es preferible así:

myvariable = 'my string variable'
print(myvariable)
# ó 
my_variable = 'my string variable'
print(my_variable)


my_int_variable = 5
print(my_int_variable)

my_bool_variable = True
print(my_bool_variable)

my_int_to_str_variable= str(my_int_variable)
print(my_int_to_str_variable)
print(type(my_int_to_str_variable))

# Concatenacion de variables en un print
print(my_variable, my_int_variable, my_bool_variable)
print('Este es el valor de:', my_bool_variable)

# Algunas funciones del sistema
print(len(my_variable))  #me imprime la longitud 

# Variables en una sola línea (Ciudado con abusar de esta sintaxis!)
name , surname, alias, age = 'Paula', 'Belaza', 'paulilur', 22
print(name, surname, alias, age) #tambien puedo cambiar el orden 
print('Me llamo:', name, surname, '.Mi edad es:', age, 'y mi alias es', alias) 

# Inputs
name = input('¿Cuál es tu nombre? ')
age = input('¿Cuántos años tienes? ')
print(name)
print(age)

# Cambiamos su tipo
name = 35
age = "Brais"
print(name)
print(age)

# ¿Forzamos el tipo?
address: str = "Mi dirección"
address = True
address = 5
address = 1.2
print(type(address))
