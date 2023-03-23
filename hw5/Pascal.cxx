// Names: Trey Gower and Jeet Patel     EID: Tag3334
#include <iostream>
#include <vector>

using std::vector;
using std::cout;
using std::cin;

class Pascals {
private:
  int rows;
  vector<vector<int>> pascal;
public:
  //constructer takes one input m that makes matrix of size n x n
  Pascals (int n) 
    : rows(n), pascal(rows) {}; //passed n into rows for code clarity 

  vector<vector<int>> calculaterow() {   
 
    for(int i =0; i < rows; i++) {
      pascal.at(i).resize(i+1);
      pascal.at(i).at(0) = pascal.at(i).at(i) = 1;
	for(int j =1; j< i; j++) {
	 
	  pascal.at(i).at(j) = pascal.at(i-1).at(j) + pascal.at(i-1).at(j-1);

      }     
   
      
    }
    return pascal;  
  }
  //prints pascals triangle with numbers
  void print() {

       for(int i=0;i<rows;i++){
	 cout << "row " << i+1 << ": ";
     for(int k=1;k<=rows-i;k++){
       cout << "  ";
     }
     for(int j =0; j<= i; j++) {
       cout << "    " << pascal.at(i).at(j);
     }
       cout << '\n';
     
       }

   }
  //print pascals triangle with stars
  void print(int m) {

              for(int i=0;i<rows;i++){
	    for(int k = 1; k<= rows-i; k++) {
	      cout << " ";

	    }     
      for(int j=0;j<= i;j++){
	if(pascal.at(i).at(j) %m !=0)
	cout << " " << "*";
	else
	  cout<< "  ";
      }
            cout << '\n';
	  }
    
    
  }      

};


int main()  { 

  int numrows; 
  vector<int> mod(3);
  cin >>numrows;
  cout << '\n';

  //for storing modulos 
  for(int i =0; i< mod.size(); i++) {
    cin >> mod.at(i);
cout  << '\n';
  }
 
 Pascals triangle(numrows);
 triangle.calculaterow();
  triangle.print();
  for(int j = 0; j < mod.size(); j++){
    triangle.print(mod.at(j));  
  }
  return 0;
}
