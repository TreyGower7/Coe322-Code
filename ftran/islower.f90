module stackstuff
use charstuff
type(var), dimension(10) :: vars
integer :: top = 0, i, value
end module stackstuff

module charstuff

type var
character :: id = "."
character(len=20) :: expression
integer :: value
end type var

contains
logical function islower(char)
    implicit none
    character,intent(in) :: char
    integer :: code,code_a,code_z

    code = iachar(char)
    code_a = iachar("a")
    code_z = iachar("z")
    islower = code.ge.code_a .and. code.le.code_z

  end function islower

logical function isdigit(char)
    implicit none
    character,intent(in) :: char
    integer :: code,code_0,code_9

    code = iachar(char)
    code_0 = iachar("0")
    code_9 = iachar("9")
    isdigit = code.ge.code_0 .and. code.le.code_9

  end function isdigit

integer function char_to_int(char)
implicit none 
character, intent(in) :: char

char_to_int = iachar(char)-iachar("0")
end function char_to_int
end module charstuff

Program lowerchar
use charstuff
use stackstuff
  implicit none
  character :: char



do
print *, (vars(i), i =1, top)
read *, char
if(char == "0") then
exit
end if
if(islower(char)) then
print *, "id: ",char
vars(top)%id = char
else if(isdigit(char)) then
top = 1+top
value = char_to_int(char)
print *, "Value: ", value
vars(top)%value = value

else
print *, char, "is something else"
end if 
end do

  
End Program lowerchar
