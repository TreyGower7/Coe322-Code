Program NamedVar
  implicit none

  type var
     character(len=20) :: id
     integer :: value
  end type var
  type(var) :: x,y,z,a

  x = var("x", 1 )
  print *,x
  y = var("y", 2 )
  print *,y
  z = varadd(x,y)
  print *,z
  a = varmult(x,z)  
  print *,a

contains
type(var) function varadd (p, q)
implicit none
type(var), intent(in) :: p, q
type(var) :: d
integer :: sum

sum = p%value + q%value 
d%value = sum
d%id = trim(p%id) // " + " // trim(q%id)

varadd = d 

end function varadd

type(var) function varmult(i, j)
implicit none
type(var), intent(in) :: i
type(var) :: f, j
integer :: sum

sum = i%value * j%value
f%value = sum
f%id = "" // trim(i%id) // "("  // trim(j%id) // ")" 

varmult = f

end function varmult

end Program NamedVar
