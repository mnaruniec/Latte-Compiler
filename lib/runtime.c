#include <stdlib.h>
#include <stdio.h>
#include <string.h>


void printInt(int n) {
    printf("%d\n", n);
}

void printString(char *s) {
    printf("%s\n", s);
}

void error() {
    printString("runtime error\n");
    exit(1);
}

int readInt() {
    int n;
    scanf("%d", &n);
    return n;
}

char *readString() {
    char *target = NULL;
    size_t n = 0;
    getline(&target, &n, stdin);
    return target;
}

char *concat(char *s1, char *s2) {
    char *t = malloc(strlen(s1) + strlen(s2) + 1);
    return strcat(strcpy(t, s1), s2);
}

char *copy(char *src) {
    char *dest = malloc(strlen(src) + 1);
    return strcpy(dest, src);
}

