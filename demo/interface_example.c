#include <stdio.h>

#include <stdlib.h>

#include <math.h>

struct _virtualSquare{
int(*getArea)(void*);
int(*getPerimeter)(void*);
}
;
struct _virtualRectangle{
int(*getArea)(void*);
int(*getPerimeter)(void*);
}
;
struct _interfaceShape{
void* body;
int(*getPerimeter)(void*);
int(*getArea)(void*);
}
;
struct _structSquare{
struct _interfaceShape _Shape_Square;
struct _virtualSquare* _virtual;
int x;
}
;
struct _structRectangle{
struct _interfaceShape _Shape_Rectangle;
struct _virtualRectangle* _virtual;
int length;
int width;
}
;

void _constructor_Square ( struct _structSquare**, int, int)

;
void _constructor_Rectangle ( struct _structRectangle**, int, int, int)

;
void _destructor_Square ( struct _structSquare**, int)

;
void _destructor_Rectangle ( struct _structRectangle**, int)

;
int _structSquare_getPerimeter ( void*)

;
int _structSquare_getArea ( void*)

;
int _structRectangle_getPerimeter ( void*)

;
int _structRectangle_getArea ( void*)

;
int getArea ( struct _interfaceShape*)

;
int getPerimeter ( struct _interfaceShape*)

;
int main ( int, char**)

;

void _constructor_Square ( struct _structSquare** _this, int side, int _needs_malloc){
int x;
struct _interfaceShape __this_interfaceShape;

if(_needs_malloc)(*_this)=malloc(sizeof(struct _structSquare));

else ;

(*_this)->_virtual=malloc(sizeof(struct _virtualSquare));
if(side<0)
{

x=1;

}
else{

x=side;

}
(*_this)->x=x;
(*_this)->_virtual->getArea=_structSquare_getArea;
(*_this)->_virtual->getPerimeter=_structSquare_getPerimeter;
__this_interfaceShape.getPerimeter=_structSquare_getPerimeter;
__this_interfaceShape.getArea=_structSquare_getArea;
__this_interfaceShape.body=(void*)((*_this));
(*_this)->_Shape_Square=__this_interfaceShape;

}



void _constructor_Rectangle ( struct _structRectangle** _this, int new_width, int new_length, int _needs_malloc){
int length;
int width;
struct _interfaceShape __this_interfaceShape;

if(_needs_malloc)(*_this)=malloc(sizeof(struct _structRectangle));

else ;

(*_this)->_virtual=malloc(sizeof(struct _virtualRectangle));
length=new_length;
width=new_width;
(*_this)->length=length;
(*_this)->width=width;
(*_this)->_virtual->getArea=_structRectangle_getArea;
(*_this)->_virtual->getPerimeter=_structRectangle_getPerimeter;
__this_interfaceShape.getPerimeter=_structRectangle_getPerimeter;
__this_interfaceShape.getArea=_structRectangle_getArea;
__this_interfaceShape.body=(void*)((*_this));
(*_this)->_Shape_Rectangle=__this_interfaceShape;

}



void _destructor_Square ( struct _structSquare** _this, int _needs_free){
int x;

x=(*_this)->x;
if(_needs_free){

free((*_this)->_virtual);
free((*_this));

}

}



void _destructor_Rectangle ( struct _structRectangle** _this, int _needs_free){
int length;
int width;

length=(*_this)->length;
width=(*_this)->width;
if(_needs_free){

free((*_this)->_virtual);
free((*_this));

}

}



int _structSquare_getPerimeter ( void* _body){
struct _structSquare* s = (struct _structSquare*)(_body);

return 4*s->x; 
}



int _structSquare_getArea ( void* _body){
struct _structSquare* s = (struct _structSquare*)(_body);

return s->x*s->x; 
}



int _structRectangle_getPerimeter ( void* _body){
struct _structRectangle* r = (struct _structRectangle*)(_body);

return 2*r->length+2*r->width; 
}



int _structRectangle_getArea ( void* _body){
struct _structRectangle* r = (struct _structRectangle*)(_body);

return r->length*r->width; 
}



int getArea ( struct _interfaceShape* s){

return s->getArea(s->body); 
}



int getPerimeter ( struct _interfaceShape* s){

return s->getPerimeter(s->body); 
}



int main ( int argc, char** argv){
struct _structSquare* s;
struct _structRectangle* r;

_constructor_Square((struct _structSquare**)(&(s)),5,1);
_constructor_Rectangle((struct _structRectangle**)(&(r)),7,6,1);
printf("square area %d\n",getArea(&(s->_Shape_Square)));
printf("rectangle area %d\n",getArea(&(r->_Shape_Rectangle)));
printf("square perimeter %d\n",getPerimeter(&(s->_Shape_Square)));
printf("rectangle perimeter %d\n",getPerimeter(&(r->_Shape_Rectangle)));
return 0; 
}



