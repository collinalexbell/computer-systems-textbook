#include <stdio.h>
#include <math.h>

using namespace std;


int test_twos_complement_overflow(){
    int a = 2147483647;
    long b = a;
    printf("Testing 2's complement multiplication overflow\n %d * 2 should overflow; RESULT:  %d\n", a, a*2);
    printf("And %d * 2 mod 2\^32 = %d\n", b, (int)(fmod(b*2, (long)pow(2, 32))));
}


int main(){
    test_twos_complement_overflow();
    return 0;
}
