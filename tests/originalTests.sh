#!/bin/bash

echo '__________________1+(3-2);_________________________________' >> testOutput.txt
echo '{1+(3-2);}' | ./cimple >> testOutput.txt
echo '__________________1+3-2;___________________________________' >> testOutput.txt
echo '{1+3-2;}' | ./cimple >> testOutput.txt
echo '__________________a=2;____________________________________' >> testOutput.txt
echo '{a=1;}' | ./cimple >> testOutput.txt
echo '__________________a=2;____________________________________' >> testOutput.txt
echo '{a=2;}' | ./cimple >> testOutput.txt
echo '__________________a+=1;_____________________________________' >> testOutput.txt
echo '{a+=1;}' | ./cimple >> testOutput.txt
echo '__________________a-=1;_____________________________________' >> testOutput.txt
echo '{a-=1;}' | ./cimple >> testOutput.txt
echo '__________________a*=1;_____________________________________' >> testOutput.txt
echo '{a*=1;}' | ./cimple >> testOutput.txt
echo '__________________a/=1;_____________________________________' >> testOutput.txt
echo '{a/=1;}' | ./cimple >> testOutput.txt
echo '__________________a<<=1;_____________________________________' >> testOutput.txt
echo '{a<<=1;}' | ./cimple >> testOutput.txt
echo '__________________a>>=1;_____________________________________' >> testOutput.txt
echo '{a>>=1;}' | ./cimple >> testOutput.txt
echo '__________________a&=1;_____________________________________' >> testOutput.txt
echo '{a&=1;}' | ./cimple >> testOutput.txt
echo '__________________a|=1;_____________________________________' >> testOutput.txt
echo '{a|=1;}' | ./cimple >> testOutput.txt
echo '__________________a^=1;_____________________________________' >> testOutput.txt
echo '{a^=1;}' | ./cimple >> testOutput.txt
echo '__________________int a; a = 1;________________________________' >> testOutput.txt
echo '{int a; a = 1;}' | ./cimple >> testOutput.txt
echo '__________________{int a; int b; int c; a = 1; b = 2; c = 3;}_____________' >> testOutput.txt
echo '{int a; int b; int c; a = 1; b = 2; c = 3;}' | ./cimple >> testOutput.txt
echo '__________________int helloWorld(int a, int b) { a = 1; b = 2; }_________' >> testOutput.txt
echo 'int helloWorld(int a, int b) { a = 1; b = 2; }' | ./cimple >> testOutput.txt
