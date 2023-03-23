!Names: Trey Gower & Jeet Patel & Jordan Burton  EID: Tag3334 & Jcp4345 & Jeb5645
Program LinkedList

  implicit none

  type node
     integer :: value
     type(node),pointer :: next
  end type node

  type list
     type(node),pointer :: head
  end type list
  
  integer, parameter :: listsize =8
  type(list) :: the_list
  integer,dimension(listsize) :: inputs 
  integer :: input,input_value

nullify(the_list%head)
  do input=1,listsize
read *, inputs(input)
  input_value = inputs(input) 
     call insert(the_list,input_value)
     !call attach(the_list,input_value)
    ! print *,"|list| = ",length(the_list)
call print(the_list)
  end do

  call print(the_list)

contains
  
  subroutine attach( the_list,new_value )
    implicit none
    ! parameters
    type(list),intent(inout) :: the_list
    integer,intent(in) :: new_value
    type(node),pointer :: current_node

    ! local

    ! if the list has no head node, attached the new node
!    if (.not.associated(the_list%head)) then
 !      allocate( the_list%head )
  !     the_list%head%value = new_value
   ! else
       !call node_attach( the_list%head,new_value )
       current_node => the_list%head
       do while( associated(current_node%next) )
          current_node => current_node%next
       end do
       allocate(current_node%next)
       current_node%next%value = new_value
   ! end if
    
  end subroutine attach

 subroutine insert( the_list,new_value )
    implicit none
    ! parameters                                                                                                              
    type(list),intent(inout) :: the_list
    integer,intent(in) :: new_value
    type(node),pointer :: current_node
integer :: val, nval
    ! local                                                                                                      

   if (.not.associated(the_list%head)) then
       allocate( the_list%head )
      the_list%head%value = new_value

   else
 current_node => the_list%head
nval = new_value
       !call node_attach( the_list%head,new_value )                            
do while( associated(current_node%next) )
          current_node => current_node%next
if(nval < current_node%value) then
val = current_node%value                                                  
current_node%value = nval
nval = val
end if

       end do
 allocate(current_node%next)
       current_node%next%value = nval
 
current_node => the_list%head
do while( associated(current_node%next) )
          current_node => current_node%next

if(current_node%value < the_list%head%value) then
val = the_list%head%value
the_list%head%value = current_node%value
current_node%value = val
end if

       end do

    end if

  end subroutine insert
  
  integer function length( the_list )
    implicit none
    type(list),intent(in),target :: the_list
    ! local
    type(node),pointer :: current

    length = 0
    current => the_list%head
    if ( associated(current) ) then
       do while( associated(current%next) )
          length = length+1
          !print *,current%value
          current => current%next
       end do
    end if

  end function length

  subroutine print(the_list)
    implicit none
    type(list),intent(in) :: the_list
    type(node),pointer :: current

    write(*,'("List: [ ")',advance="no")
    if (associated(the_list%head)) then
       current => the_list%head
       do while (associated(current))
          write(*,'(i0",")',advance="no") current%value
          if (.not.associated(current%next)) exit
          current => current%next
       end do
    end if
    write(*,'(x"]")')

  end subroutine print
  
end Program LinkedList
