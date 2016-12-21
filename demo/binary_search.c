#include <stdio.h>

#include <stdlib.h>

#include <math.h>

struct _virtualBinaryTree{
int(*addNode)(void*, int);
int(*find_max)(void*);
int(*find_min)(void*);
int(*hasValue)(void*, int);
}
;
struct _structBinaryTree{
struct _virtualBinaryTree* _virtual;
struct _structBinaryTree* left;
struct _structBinaryTree* right;
int val;
}
;

void _constructor_BinaryTree ( struct _structBinaryTree**, int, int)

;
void _destructor_BinaryTree ( struct _structBinaryTree**, int)

;
int _structBinaryTree_find_min ( void*)

;
int _structBinaryTree_hasValue ( void*, int)

;
int _structBinaryTree_find_max ( void*)

;
int _structBinaryTree_addNode ( void*, int)

;
int main ( )

;

void _constructor_BinaryTree ( struct _structBinaryTree** _this, int val_to_set, int _needs_malloc){
struct _structBinaryTree* left;
struct _structBinaryTree* right;
int val;

if(_needs_malloc)(*_this)=malloc(sizeof(struct _structBinaryTree));

else ;

(*_this)->_virtual=malloc(sizeof(struct _virtualBinaryTree));
val=val_to_set;
left=NULL;
right=NULL;
(*_this)->left=left;
(*_this)->right=right;
(*_this)->val=val;
(*_this)->_virtual->addNode=_structBinaryTree_addNode;
(*_this)->_virtual->find_max=_structBinaryTree_find_max;
(*_this)->_virtual->find_min=_structBinaryTree_find_min;
(*_this)->_virtual->hasValue=_structBinaryTree_hasValue;

}



void _destructor_BinaryTree ( struct _structBinaryTree** _this, int _needs_free){
struct _structBinaryTree* left;
struct _structBinaryTree* right;
int val;

left=(*_this)->left;
right=(*_this)->right;
val=(*_this)->val;
if(left!=NULL){

_destructor_BinaryTree((struct _structBinaryTree**)(&(left)),1);

}
if(right!=NULL){

_destructor_BinaryTree((struct _structBinaryTree**)(&(right)),1);

}
printf("cleaning node %d\n",val);
if(_needs_free){

free((*_this)->_virtual);
free((*_this));

}

}



int _structBinaryTree_find_min ( void* _body){
struct _structBinaryTree* b = (struct _structBinaryTree*)(_body);

if(b->left==NULL)
{

return b->val; 
}
else{

return b->left->_virtual->find_min((void*)(b->left)); 
}

}



int _structBinaryTree_hasValue ( void* _body, int toFind){
struct _structBinaryTree* b = (struct _structBinaryTree*)(_body);

if(b->val==toFind)
{

return 1; 
}
else{

if(b->left!=NULL){

if(b->left->_virtual->hasValue((void*)(b->left),toFind)>0){

return 1; 
}

}

else if(b->right!=NULL){

return b->right->_virtual->hasValue((void*)(b->right),toFind); 
}


}
return 0; 
}



int _structBinaryTree_find_max ( void* _body){
struct _structBinaryTree* b = (struct _structBinaryTree*)(_body);

if(b->right==NULL)
{

return b->val; 
}
else{

return b->right->_virtual->find_max((void*)(b->right)); 
}

}



int _structBinaryTree_addNode ( void* _body, int val){
struct _structBinaryTree* b = (struct _structBinaryTree*)(_body);

if(val<b->val){

if(b->left==NULL)
{

_constructor_BinaryTree((struct _structBinaryTree**)(&(b->left)),val,1);

}
else{

b->left->_virtual->addNode((void*)(b->left),val);

}

}

else if(val>b->val)
{

if(b->right==NULL)
{

_constructor_BinaryTree((struct _structBinaryTree**)(&(b->right)),val,1);

}
else{

b->right->_virtual->addNode((void*)(b->right),val);

}

}
else{

b->_virtual->addNode((void*)(b),val+1);

}

return 0; 
}



int main ( ){
struct _structBinaryTree* b;
int i;

_constructor_BinaryTree((struct _structBinaryTree**)(&(b)),100,1);
for(i=0; i<200; i++){

b->_virtual->addNode((void*)(b),i);

}
b->_virtual->find_max((void*)(b));
printf("has 200 %d\n",b->_virtual->hasValue((void*)(b),200));
printf("hass 100 %d\n",b->_virtual->hasValue((void*)(b),100));
_destructor_BinaryTree((struct _structBinaryTree**)(&(b)),1);
return 0; 
}



