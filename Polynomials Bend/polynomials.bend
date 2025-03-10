# Adaptado de Sedgewick & Wayne,
# http://introcs.cs.princeton.edu/java/92symbolic/Polynomial.java.html

# Esta clase representa una descripción genérica de cualquier polinomio de una
# variable con coeficientes enteros, y proporciona métodos que permiten 
# manipulación simbólica de dicho polinomio, o evaluarlo dado un valor
# para su variable

# Representación del polinomio:
# - Un polinomio se representa como un objeto con dos campos:
#   - coef: Una lista de coeficientes enteros
#   - deg: El grado del polinomio (0 para el polinomio cero)

object Polynomial { coef, deg }

# Construye un nuevo término polinomial de la forma a * x^b
def Polynomial/new(a: i24, b: i24) -> Polynomial:
  # Creamos una lista de coeficientes de tamaño b+1 llena de ceros
  coef = fill_zeros(b + 1)
  # Colocamos 'a' en la posición b
  coef = set_coefficient(coef, b, a)
  # Calculamos el grado real basado en los coeficientes
  deg = actual_degree(coef)
  return Polynomial { coef: coef, deg: deg }

# Función auxiliar para llenar una lista con ceros hasta n
def fill_zeros(n: i24) -> List(i24):
  bend i = 0, result = []:
    when i < n:
      result = List/Cons(0, result)
      fork(i + 1, result)
    else:
      result

# Función auxiliar para establecer un coeficiente en una posición específica
def set_coefficient(coef: List(i24), pos: i24, val: i24) -> List(i24):
  fold coef:
    case List/Nil:
      return List/Nil
    case List/Cons:
      if pos == 0:
        return List/Cons(val, coef.tail)
      else:
        return List/Cons(coef.head, set_coefficient(coef.tail, pos - 1, val))

# Función para obtener un coeficiente en una posición específica
def get_coefficient(coef: List(i24), pos: i24) -> i24:
  fold coef:
    case List/Nil:
      return 0  # Si pos está fuera de rango, devuelve 0
    case List/Cons:
      if pos == 0:
        return coef.head
      else:
        return get_coefficient(coef.tail, pos - 1)

# Función auxiliar para calcular el grado actual basado en los coeficientes
def actual_degree(coef: List(i24)) -> i24:
  # Calcula la posición del último coeficiente no cero
  bend i = 0, pos = List/length(coef) - 1:
    when pos >= 0:
      if get_coefficient(coef, pos) != 0:
        i = pos
        fork(i, 0)  # Encontramos la posición, terminamos
      else:
        fork(i, pos - 1)  # Seguimos buscando
    else:
      i  # Devuelve el grado encontrado (o 0 si no hay coeficientes no nulos)

# Devuelve el grado de este polinomio (0 para el polinomio cero)
def Polynomial/degree(poly: Polynomial) -> i24:
  return poly.deg

# Devuelve la suma de dos polinomios (este objeto más el argumento), c = a + b
def Polynomial/plus(a: Polynomial, b: Polynomial) -> Polynomial:
  # Determinamos el grado máximo entre los dos polinomios
  max_deg = if a.deg > b.deg: a.deg else: b.deg
  
  # Creamos un nuevo polinomio de grado max_deg con coeficientes 0
  c = Polynomial/new(0, max_deg)
  
  # Sumamos los coeficientes de a
  c_coef = c.coef
  bend i = 0, c_coef = c_coef:
    when i <= a.deg:
      a_coef = get_coefficient(a.coef, i)
      c_coef = set_coefficient(c_coef, i, a_coef)
      fork(i + 1, c_coef)
    else:
      c_coef
  
  # Sumamos los coeficientes de b
  bend i = 0, c_coef = c_coef:
    when i <= b.deg:
      b_coef = get_coefficient(b.coef, i)
      c_coef_i = get_coefficient(c_coef, i)
      c_coef = set_coefficient(c_coef, i, c_coef_i + b_coef)
      fork(i + 1, c_coef)
    else:
      c_coef
  
  # Actualizamos el grado
  deg = actual_degree(c_coef)
  return Polynomial { coef: c_coef, deg: deg }

# Devuelve la diferencia de dos polinomios (este objeto menos el argumento), c = a - b
def Polynomial/minus(a: Polynomial, b: Polynomial) -> Polynomial:
  # Determinamos el grado máximo entre los dos polinomios
  max_deg = if a.deg > b.deg: a.deg else: b.deg
  
  # Creamos un nuevo polinomio de grado max_deg con coeficientes 0
  c = Polynomial/new(0, max_deg)
  
  # Sumamos los coeficientes de a
  c_coef = c.coef
  bend i = 0, c_coef = c_coef:
    when i <= a.deg:
      a_coef = get_coefficient(a.coef, i)
      c_coef = set_coefficient(c_coef, i, a_coef)
      fork(i + 1, c_coef)
    else:
      c_coef
  
  # Restamos los coeficientes de b
  bend i = 0, c_coef = c_coef:
    when i <= b.deg:
      b_coef = get_coefficient(b.coef, i)
      c_coef_i = get_coefficient(c_coef, i)
      c_coef = set_coefficient(c_coef, i, c_coef_i - b_coef)
      fork(i + 1, c_coef)
    else:
      c_coef
  
  # Actualizamos el grado
  deg = actual_degree(c_coef)
  return Polynomial { coef: c_coef, deg: deg }

# Devuelve el producto de dos polinomios (este objeto por el argumento), c = a * b
def Polynomial/times(a: Polynomial, b: Polynomial) -> Polynomial:
  # Creamos un nuevo polinomio de grado a.deg + b.deg con coeficientes 0
  c = Polynomial/new(0, a.deg + b.deg)
  c_coef = c.coef
  
  # Multiplicamos cada término de a por cada término de b
  bend i = 0, c_coef = c_coef:
    when i <= a.deg:
      a_coef_i = get_coefficient(a.coef, i)
      
      bend j = 0, c_coef = c_coef:
        when j <= b.deg:
          b_coef_j = get_coefficient(b.coef, j)
          c_coef_ij = get_coefficient(c_coef, i + j)
          c_coef = set_coefficient(c_coef, i + j, c_coef_ij + a_coef_i * b_coef_j)
          fork(j + 1, c_coef)
        else:
          c_coef
      
      fork(i + 1, c_coef)
    else:
      c_coef
  
  # Actualizamos el grado
  deg = actual_degree(c_coef)
  return Polynomial { coef: c_coef, deg: deg }

# Devuelve la composición de dos polinomios (este objeto "de" el argumento), c = a(b(x))
# Calculado usando el método de Horner
def Polynomial/compose(a: Polynomial, b: Polynomial) -> Polynomial:
  # Comenzamos con un polinomio constante 0
  c = Polynomial/new(0, 0)
  
  # Usamos el método de Horner para la composición
  bend i = a.deg, c = c:
    when i >= 0:
      a_coef_i = get_coefficient(a.coef, i)
      term = Polynomial/new(a_coef_i, 0)
      c = Polynomial/plus(term, Polynomial/times(b, c))
      fork(i - 1, c)
    else:
      c
  
  return c

# Devuelve el resultado de evaluar este objeto en el valor del argumento,
# obtenido usando el método de Horner
def Polynomial/evaluate(poly: Polynomial, x: i24) -> i24:
  # Aplicamos el método de Horner para evaluar el polinomio en x
  bend i = poly.deg, p = 0:
    when i >= 0:
      p = get_coefficient(poly.coef, i) + x * p
      fork(i - 1, p)
    else:
      p

# Devuelve la derivada de este objeto
def Polynomial/differentiate(poly: Polynomial) -> Polynomial:
  if poly.deg == 0:
    return Polynomial/new(0, 0)  # La derivada de una constante es cero
  else:
    # Creamos un nuevo polinomio de grado poly.deg - 1
    deriv = Polynomial/new(0, poly.deg - 1)
    deriv_coef = deriv.coef
    
    # Calculamos los coeficientes de la derivada
    bend i = 0, deriv_coef = deriv_coef:
      when i < poly.deg:
        poly_coef_i1 = get_coefficient(poly.coef, i + 1)
        deriv_coef = set_coefficient(deriv_coef, i, (i + 1) * poly_coef_i1)
        fork(i + 1, deriv_coef)
      else:
        deriv_coef
    
    return Polynomial { coef: deriv_coef, deg: poly.deg - 1 }

# Devuelve la representación en cadena de este polinomio
def Polynomial/to_string(poly: Polynomial) -> String:
  if poly.deg == 0:
    return i24/to_string(get_coefficient(poly.coef, 0))
  elif poly.deg == 1:
    # Construimos la cadena para el término de primer grado
    term1 = i24/to_string(get_coefficient(poly.coef, 1)) + "x"
    
    # Agregamos el término constante si es necesario
    coef0 = get_coefficient(poly.coef, 0)
    if coef0 > 0:
      return term1 + " + " + i24/to_string(coef0)
    elif coef0 < 0:
      return term1 + " - " + i24/to_string(-coef0)
    else:
      return term1
  else:
    # Comenzamos con el término de mayor grado
    result = i24/to_string(get_coefficient(poly.coef, poly.deg)) + "x^" + i24/to_string(poly.deg)
    
    # Agregamos los términos restantes
    bend i = poly.deg - 1, result = result:
      when i >= 0:
        coef_i = get_coefficient(poly.coef, i)
        if coef_i > 0:
          result = result + " + " + i24/to_string(coef_i)
        elif coef_i < 0:
          result = result + " - " + i24/to_string(-coef_i)
        
        # Agregamos la variable si el exponente es mayor que 0
        if coef_i != 0:
          if i == 1:
            result = result + "x"
          elif i > 1:
            result = result + "x^" + i24/to_string(i)
        
        fork(i - 1, result)
      else:
        result
    
    return result

# Programa principal para probar la funcionalidad
def main() -> _:
  # Creamos los polinomios de prueba como en el ejemplo de Java
  zero = Polynomial/new(0, 0)
  
  # Construimos p(x) = 4x^3 + 3x^2 + 2x + 1
  p1 = Polynomial/new(4, 3)
  p2 = Polynomial/new(3, 2)
  p3 = Polynomial/new(1, 0)
  p4 = Polynomial/new(2, 1)
  p = Polynomial/plus(Polynomial/plus(Polynomial/plus(p1, p2), p3), p4)
  
  # Construimos q(x) = 3x^2 + 5
  q1 = Polynomial/new(3, 2)
  q2 = Polynomial/new(5, 0)
  q = Polynomial/plus(q1, q2)
  
  # Realizamos las operaciones
  r = Polynomial/plus(p, q)
  s = Polynomial/times(p, q)
  t = Polynomial/compose(p, q)
  
  # Mostramos los resultados
  with IO:
    * <- IO/print("zero(x)     = " + Polynomial/to_string(zero) + "\n")
    * <- IO/print("p(x)        = " + Polynomial/to_string(p) + "\n")
    * <- IO/print("q(x)        = " + Polynomial/to_string(q) + "\n")
    * <- IO/print("p(x) + q(x) = " + Polynomial/to_string(r) + "\n")
    * <- IO/print("p(x) * q(x) = " + Polynomial/to_string(s) + "\n")
    * <- IO/print("p(q(x))     = " + Polynomial/to_string(t) + "\n")
    * <- IO/print("0 - p(x)    = " + Polynomial/to_string(Polynomial/minus(zero, p)) + "\n")
    p_at_3 = Polynomial/evaluate(p, 3)
    * <- IO/print("p(3)        = " + i24/to_string(p_at_3) + "\n")
    p_prime = Polynomial/differentiate(p)
    * <- IO/print("p'(x)       = " + Polynomial/to_string(p_prime) + "\n")
    p_prime_prime = Polynomial/differentiate(p_prime)
    * <- IO/print("p''(x)      = " + Polynomial/to_string(p_prime_prime) + "\n")
    return wrap(0)

# Función auxiliar para obtener la longitud de una lista
def List/length(list: List(T)) -> i24:
  fold list:
    case List/Nil:
      return 0
    case List/Cons:
      return 1 + List/length(list.tail)