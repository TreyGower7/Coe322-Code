Program ArrayPoint

  implicit none

  !call random_number(array)

  real,dimension(10),target :: array &
       = [1.1, 2.2, 3.3, 4.4, 5.5, &
          9.9, 8.8, 7.7, 6.6, 0.0]
  real,pointer :: biggest_element

  print '(10f5.2)',array
  call SetPointer(array,biggest_element)
  print *,"Biggest element is",biggest_element
  print *,"checking pointerhood:",&
       associated(biggest_element)
  biggest_element = 0
  print '(10f5.2)',array

contains

  subroutine SetPointer(ar,pt)
    implicit none
real, dimension(10), target, intent(inout) :: ar
real, pointer,intent(inout) :: pt
integer :: i=2
pt => ar(1)
    do while (i <= size(ar))
if (pt < ar(i)) then
pt => ar(i)
end if
i = i + 1
end do
    
  end subroutine SetPointer
  
end Program ArrayPoint
