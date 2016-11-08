#!/bin/bash

echo '__________________1+(3-2);_________________________________' > testOutput.txt
echo '1+(3-2);' | ./cimple >> testOutput.txt
echo '__________________1+3-2;___________________________________' >> testOutput.txt
echo '1+3-2;' | ./cimple >> testOutput.txt
echo '__________________a=2;____________________________________' >> testOutput.txt
echo 'a=1;' | ./cimple >> testOutput.txt
echo '__________________a=2;____________________________________' >> testOutput.txt
echo 'a=2;' | ./cimple >> testOutput.txt
echo '__________________a+=1;_____________________________________' >> testOutput.txt
echo 'a+=1;' | ./cimple >> testOutput.txt
echo '__________________a-=1;_____________________________________' >> testOutput.txt
echo 'a-=1;' | ./cimple >> testOutput.txt
echo '__________________a*=1;_____________________________________' >> testOutput.txt
echo 'a*=1;' | ./cimple >> testOutput.txt
echo '__________________a/=1;_____________________________________' >> testOutput.txt
echo 'a/=1;' | ./cimple >> testOutput.txt
echo '__________________a<<=1;_____________________________________' >> testOutput.txt
echo 'a<<=1;' | ./cimple >> testOutput.txt
echo '__________________a>>=1;_____________________________________' >> testOutput.txt
echo 'a>>=1;' | ./cimple >> testOutput.txt
echo '__________________a&=1;_____________________________________' >> testOutput.txt
echo 'a&=1;' | ./cimple >> testOutput.txt
echo '__________________a|=1;_____________________________________' >> testOutput.txt
echo 'a|=1;' | ./cimple >> testOutput.txt
echo '__________________a^=1;_____________________________________' >> testOutput.txt
echo 'a^=1;' | ./cimple >> testOutput.txt
