// Name: Trey Gower   EID: TAG3334
#include <iostream>

using std::cin;
using std::cout;

 
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
  int numprimes = 0, nextp =0, p, r,  q;
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
  void goldbach() {
    int num;
    if(nextp > 4)
      {
	p=0;	
	r= nextp;
	num = 2*r;
    for(q = 2; q >= 2; q++)
      {
	p = num-q;
	  if(isprime(p) && isprime(q))
	    {
	      cout <<"p: " << p <<" q: " << q << " r: " << r << '\n';

	      break;
	    }
	  p++;	
}
      }
      }


};

int main() {
  int nprimes, number, p, q, r;
  cin >> nprimes;
  primegenerator sequence;

  while(sequence.number_of_primes_found()<nprimes-1) { 
  number = sequence.nextprime();
	sequence.goldbach();

       

  }
  return 0;
}

