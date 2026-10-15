### Operadores Aritmeticos ###

6 + 10 #si no pongo print no me sale el resultado
print(3 + 4)
print(3 - 4) 
print(3 * 4)
print(16 / 4)
print (10 % 3) #modulo --> nos muestra el resto
print (10 // 3) #floor division --> aproxima la división a un numero entero
print (3**2)
print(2**3 + 3 - 7 / 1 // 4) #puedo combinar los signos sin prob

print('Hola' + 'Python') #output: HolaPython
print('Hola' + 'Python' + '¿Que tal?') #output: HolaPython¿Que tal?
print('Hola' + str(5)) #output: Hola5
print('Hola' * 5) #output: HolaHolaHolaHolaHola

print('Hola' * (2.5)) #error
print('Hola' * (2.5 * 2)) #error porque me saca 5.0 (float, no int)
#solucion
my_float= 2.5 * 2
print('Hola ' * int(my_float))

### Operadores Comparativos ###

print(3 > 4)
print(3 < 4)
print(3 >= 4)
print(3 <= 4)
print(3 == 4) #igualdad, digo que son iguales
print(3 != 4) #es distinto
print(3 > 4 == 2) #output: false

print('Hola' > 'Python') 
print('Hola' < 'Python')
print('Hola' >= 'Python')
print('Python' >= 'Python')
print('Hola' <= 'Python')
print('Hola' == 'Hola')
print('Hola' != 'Python') 

'''Al principio pensaríamos que cuenta caracteres, pero
esto no es así. Sigue una ordenación alfabética por ASCII '''
print('Hola' >= 'Zola') #output:false
print('aaaa' >= 'aaaa') #output:true
print('aaaa' >= 'abaa') #output:false --> En Python, una letra que aparece antes en el abecedario se considera menor, no mayor.
#si quisieramos contar caracteres:
print(len('aaaa') >= len('aaaa')) #output:true

### Operadores Lógicos ###

print(3 > 4 and 'Hola' > 'Python') #output:false --> "y"
print(3 > 4 or 'Hola' > 'Python') #output:false --> "o"
print(3 < 4 and 'Hola' < 'Python') #output:true
print(3 < 4 or 'Hola' < 'Python') #output:true
print(3 < 4 or 'Hola' > 'Python') #output: true (porque estoy diciendo: o true o false)
print(3 < 4 or 'Hola' > 'Python' and 4 == 4) #output:true

print(not(3 > 4)) #el not me sirve para negar toda la condición--> no (falso) = true 
