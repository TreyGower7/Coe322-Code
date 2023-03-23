!Name: Trey Gower   EID: Tag3334
program checkprimenums
  implicit none 
integer :: how_many, num_of_primes, number

print*, "How many prime numbers?: "
read*, how_many

number = 1
num_of_primes = 0
do while (num_of_primes < how_many) 
if(isprime(number)) then
num_of_primes = num_of_primes + 1
print*, number
number = number + 1
else 
number = number + 1
end if 

end do 

contains
logical function isprime(x) 
implicit none
logical :: prime
integer :: count, x
prime = .true.
if(x < 2) then
prime = .false.
end if

do count=2, x/2 
if((mod(x,count)) .eq. 0) then
prime = .false.
exit
end if 
end do 
isprime = prime
end function isprime


end program Checkprimenums
