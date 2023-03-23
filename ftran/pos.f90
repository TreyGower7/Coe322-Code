program readpos
implicit none
real(4) :: userinput
print *,"Type a positive number:"
userinput = read_positive()
print *,"Thank you for",userinput
contains
real(4) function read_positive()
implicit none
integer:: in
read*, in
if(in<0) then
do while(in <0)
print*, in, "is not positive"
read*, in
if(in >= 0) then
read_positive= in
exit
end if 
end do
end if
end function read_positive
end program readpos
