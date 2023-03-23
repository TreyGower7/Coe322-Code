// Name: Trey Gower   EID: TAG3334
#include <iostream>

using std::cin;
using std::cout;
using std::endl;
 
bool isprime( int n ) {
  bool is_prime = true;
  if(n == 0 || n   == 1) {
    is_prime = false;
  }
  for(int i = 2; i<=n; i++) {
    if(n % i ==0 && i != n)
      {
        is_prime = false;
        break;
      }
  }
  return is_prime;
}

class primegenerator {
 private:
  //members to count prime numbers and get excluded number  
int numprimes = 0, nextp =0;
  bool prime = isprime(nextp);
 public:

int number_of_primes_found() { 
if(prime)
    return numprimes++;
}

  int nextprime() {    
    nextp++;
    prime = isprime(nextp);
    while(prime  == false) {
      prime = isprime(nextp);        
    if(prime){ 
break;    
}
    else 
      nextp++;
  }
        
      return nextp;
  }
};

int main() {
  int nprimes;
  cin >> nprimes;
  primegenerator sequence;
  while(sequence.number_of_primes_found()<nprimes-1) { 
  int number = sequence.nextprime();
    cout << "Number " << number << "is prime" << endl;
  }
  return 0;
}

