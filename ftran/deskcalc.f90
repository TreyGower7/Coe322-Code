! Names: Trey Gower, Jeet Patel, Jordan Burton EIDs: Tag3334 & JCP4345 & Jeb5645
module varHandling
use inputHandling

 type var
     character (len=20) :: id = "."
  character :: expr
     integer :: value
  end type var
  type(var) :: x,y,z,a

 type(var),dimension(10) :: stack
  integer :: top = 0

type stackstr
type(var),dimension(10) :: data
integer :: top=0
contains
procedure,public :: display, push, name, op 
end type stackstr

contains
 integer function char_to_int(char)
    implicit none
    character,intent(in) :: char
    char_to_int = iachar(char)-iachar("0")
  end function char_to_int
    subroutine display(me)
      implicit none
      class(stackstr) :: me
 integer :: istck
if (me%top.eq.0) return
print '( 10( a,a1, a,a, a,i0,"; ") )', ( & "id:",me%data(istck)%expr, &
" expr=",trim(me%data(istck)%id), & " val=",me%data(istck)%value, &
istck=1,me%top )
    end subroutine display
 subroutine push(me,char)
      implicit none
      class(stackstr) :: me
      character,intent(in) :: char
      integer :: value 
      value = char_to_int(char)
      print *,"Value: ",value
      me%top = me%top + 1
      me%data(me%top)%value = value
 me%data(me%top)%id = trim(char)
    end subroutine push
subroutine op(me,input)
implicit none
      class(stackstr) :: me
character,intent(in) :: input
type(var) :: val
if(input .eq. "+") then
val = varadd(me%data(me%top), me%data(me%top-1))
print *, val%id
else
val = varmult(me%data(me%top), me%data(me%top-1))
end if


me%top = me%top-1
me%data(me%top) = val


end subroutine op

 subroutine name(me, input)
      class(stackstr) :: me
character, intent(in) :: input
integer :: inc

do
me%data(me%top)%expr = input
if(input .eq. me%data(inc)%expr) then
me%data(me%top) = me%data(inc)
exit
end if
inc = inc+1
end do
end subroutine name

type(var) function varadd (p, q)
implicit none
type(var), intent(in) :: p, q
type(var) :: d
integer :: sum

sum = p%value + q%value
d%value = sum
d%id =  trim(p%id) // " + " // trim(q%id)

varadd = d

end function varadd

type(var) function varmult(i, j)
implicit none
type(var), intent(in) :: i
type(var) :: f, j
integer :: sum
sum = i%value * j%value
f%value = sum
f%id =  trim(i%id) // "*" //  trim(j%id)

varmult = f

end function varmult


end module varHandling

Module inputHandling

contains
    logical function isdigit(char)
    implicit none
    character,intent(in) :: char
    integer :: code,code_0,code_9

    code = iachar(char)
    code_0 = iachar("0")
    code_9 = iachar("9")
    isdigit = code.ge.code_0 .and. code.le.code_9
  end function isdigit

  logical function islower(char)
    implicit none
    character,intent(in) :: char
    integer :: code,code_a,code_z

    code = iachar(char)
    code_a = iachar("a")
    code_z = iachar("z")
    islower = code.ge.code_a .and. code.le.code_z

  end function islower
  logical function isop(char)
    implicit none
    character,intent(in) :: char
   isop = (char=="+") .or. (char=="*")
  end function isop

End Module inputHandling

Program lowerchar
  use varHandling
use inputHandling
  implicit none
  character :: char
type(stackstr) :: thestack

  do 
call thestack%display()
     read *,char 
     if (char=="0") then
exit
end if
     if ( islower(char) ) then
        ! name the top with this character
        call thestack%name(char)
     else if ( isdigit(char) ) then
        ! create var in the vars array
        ! increase top
        ! put number in the top var
call thestack%push(char)
else if (isop(char)) then
call thestack%op(char)
end if

end do

End Program lowerchar
