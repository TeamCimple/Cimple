#include <stdio.h>

#include <stdlib.h>

#include <math.h>

struct _virtualMountainBike{
void(*applyBreak)(void*, int);
int(*getGear)(void*);
void(*setCadence)(void*, int);
void(*setGear)(void*, int);
void(*setHeight)(void*, int);
void(*speedUp)(void*, int);
}
;
struct _virtualBicycle{
void(*applyBreak)(void*, int);
int(*getGear)(void*);
void(*setCadence)(void*, int);
void(*setGear)(void*, int);
void(*speedUp)(void*, int);
}
;
struct _structMountainBike{
struct _virtualMountainBike* _virtual;
int cadence;
int gear;
int speed;
int seatHeight;
}
;
struct _structBicycle{
struct _virtualBicycle* _virtual;
int cadence;
int gear;
int speed;
}
;

void _constructor_MountainBike ( struct _structMountainBike**, int, int, int, int, int)

;
void _constructor_Bicycle ( struct _structBicycle**, int, int, int, int)

;
void _destructor_MountainBike ( struct _structMountainBike**, int)

;
void _destructor_Bicycle ( struct _structBicycle**, int)

;
void _structMountainBike_setHeight ( void*, int)

;
void _structMountainBike_setGear ( void*, int)

;
int _structBicycle_getGear ( void*)

;
void _structBicycle_speedUp ( void*, int)

;
void _structBicycle_applyBreak ( void*, int)

;
void _structBicycle_setGear ( void*, int)

;
void _structBicycle_setCadence ( void*, int)

;
int main ( int, char**)

;

void _constructor_MountainBike ( struct _structMountainBike** _this, int start_gear, int start_speed, int start_cadence, int start_height, int _needs_malloc){
int cadence;
int gear;
int speed;
int seatHeight;

if(_needs_malloc)(*_this)=malloc(sizeof(struct _structMountainBike));

else ;

(*_this)->_virtual=malloc(sizeof(struct _virtualMountainBike));
_constructor_Bicycle((struct _structBicycle**)(_this),start_gear,start_speed,start_cadence,0);
cadence=(*_this)->cadence;
gear=(*_this)->gear;
speed=(*_this)->speed;
;
seatHeight=start_height;
(*_this)->cadence=cadence;
(*_this)->gear=gear;
(*_this)->speed=speed;
(*_this)->seatHeight=seatHeight;
(*_this)->_virtual->applyBreak=_structBicycle_applyBreak;
(*_this)->_virtual->getGear=_structBicycle_getGear;
(*_this)->_virtual->setCadence=_structBicycle_setCadence;
(*_this)->_virtual->setGear=_structMountainBike_setGear;
(*_this)->_virtual->setHeight=_structMountainBike_setHeight;
(*_this)->_virtual->speedUp=_structBicycle_speedUp;

}



void _constructor_Bicycle ( struct _structBicycle** _this, int start_gear, int start_speed, int start_cadence, int _needs_malloc){
int cadence;
int gear;
int speed;

if(_needs_malloc)(*_this)=malloc(sizeof(struct _structBicycle));

else ;

(*_this)->_virtual=malloc(sizeof(struct _virtualBicycle));
gear=start_gear;
cadence=start_cadence;
speed=start_speed;
(*_this)->cadence=cadence;
(*_this)->gear=gear;
(*_this)->speed=speed;
(*_this)->_virtual->applyBreak=_structBicycle_applyBreak;
(*_this)->_virtual->getGear=_structBicycle_getGear;
(*_this)->_virtual->setCadence=_structBicycle_setCadence;
(*_this)->_virtual->setGear=_structBicycle_setGear;
(*_this)->_virtual->speedUp=_structBicycle_speedUp;

}



void _destructor_MountainBike ( struct _structMountainBike** _this, int _needs_free){
int cadence;
int gear;
int speed;
int seatHeight;

cadence=(*_this)->cadence;
gear=(*_this)->gear;
speed=(*_this)->speed;
seatHeight=(*_this)->seatHeight;
_destructor_Bicycle((struct _structBicycle**)(_this),0);
printf("MountainBike destructor");
if(_needs_free){

free((*_this)->_virtual);
free((*_this));

}

}



void _destructor_Bicycle ( struct _structBicycle** _this, int _needs_free){
int cadence;
int gear;
int speed;

cadence=(*_this)->cadence;
gear=(*_this)->gear;
speed=(*_this)->speed;
printf("Bicycle destructor");
if(_needs_free){

free((*_this)->_virtual);
free((*_this));

}

}



void _structMountainBike_setHeight ( void* _body, int newValue){
struct _structMountainBike* mb = (struct _structMountainBike*)(_body);

mb->seatHeight=newValue;
return ; 
}



void _structMountainBike_setGear ( void* _body, int newValue){
struct _structMountainBike* mb = (struct _structMountainBike*)(_body);

mb->gear=2*newValue;
return ; 
}



int _structBicycle_getGear ( void* _body){
struct _structBicycle* b = (struct _structBicycle*)(_body);

return b->gear; 
}



void _structBicycle_speedUp ( void* _body, int increment){
struct _structBicycle* b = (struct _structBicycle*)(_body);

b->speed=b->speed+increment;
return ; 
}



void _structBicycle_applyBreak ( void* _body, int decrement){
struct _structBicycle* b = (struct _structBicycle*)(_body);

b->speed=b->speed-decrement;
return ; 
}



void _structBicycle_setGear ( void* _body, int newValue){
struct _structBicycle* b = (struct _structBicycle*)(_body);

b->gear=newValue;
return ; 
}



void _structBicycle_setCadence ( void* _body, int newValue){
struct _structBicycle* b = (struct _structBicycle*)(_body);

b->cadence=newValue;
return ; 
}



int main ( int argc, char** argv){
struct _structMountainBike* mb;
struct _structBicycle** b;
int j;
int k;

b=malloc(10*sizeof(struct _structBicycle *));
for(j=0; j<5; j++){

_constructor_Bicycle((struct _structBicycle**)(&(b[j])),10,10,10,1);

}
for(j=5; j<10; j++){

_constructor_MountainBike((struct _structMountainBike**)(&(b[j])),20,10,10,10,1);

}
for(k=0; k<10; k++){

b[k]->_virtual->setGear((void*)(b[k]),40);

}
for(k=0; k<10; k++){

printf("%d\n",b[k]->_virtual->getGear((void*)(b[k])));

}
return 0; 
}



