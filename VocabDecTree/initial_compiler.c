#include <assert.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
  void *mem;
  int elems;  
  int capacity;
  size_t elemsize;  
} vec;

/** Returns a new vec */
vec* vec_init(size_t elemsize, int base_capacity) {
  vec *ret = (vec*)malloc(sizeof(vec));
  ret->elemsize=elemsize;
  ret->elems = 0;
  ret->capacity = base_capacity;
  ret->mem = malloc(elemsize*base_capacity);
  return ret;
}

int vec_push(vec *v, void *item) {
  if (v->elems == v->capacity) {
    v->mem = realloc(v->mem, v->elemsize*v->capacity*2);
    if (!v->mem) {
      return -1;
    }
    v->capacity = v->capacity*2;
  }
  memcpy(v->mem+v->elemsize*v->elems, item, v->elemsize);
  v->elems++;
  return v->elems;
}

void* vec_get(vec *v, int index) {
  assert (index < v->elems);
  return v->mem + index*v->elemsize;
}

int main(int argc, char *argv[]) {
  vec *vector = vec_init(sizeof(int), 10);
  
  for (int x=0; x<66; x++) {
    if (!vec_push(vector, &x)) {
      printf("Error adding element");
      exit(-1);
    }
    printf("Adding %i to a vec of size %i\n", x, vector->elems);
  }
  for (int x=0; x < vector->elems; x++) {
    printf("%i, %d\n", x, *(int*)vec_get(vector, x));
  }
}
