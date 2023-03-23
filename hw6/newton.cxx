// Name: Trey Gower and Jordan Burton  EID: Tag3334 and Jeb5645
#include <iostream>
using std::cin;
using std::cout;
using std::endl;
#include <cmath>

#include <functional>
using std::function;


double newtons_method ( function< double(double) > f, function< double(double) > fprime) {
  double x{1.};
   while ( true ) {
    auto fx = f(x);
    if (std::abs(fx)<1.e-10 ) break;
    x = x - fx/fprime(x); 
 }
  return x;
}

double find_zero ( function< double(double) > f ) {
  double h =1.e-6;
  auto gradient = [h,f] (double x) -> double {return ((f(x+h)-f(x))/h);};

 return newtons_method(f, gradient);
  
}


int main() {

  int n;
  cin >> n; 
   auto f = [n] (double x) -> double { return x*x-n; };
 
  cout << "root(" << n << ") =" << find_zero(f) << '\n';

  return 0;
}
