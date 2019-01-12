#include <stdlib.h>
#include <stdio.h>
#include <string.h>


void printInt(int n);

void printString(char *s);

void error();

int readInt();

char *readString();

char *concat(char *s1, char *s2);

char *copy(char *src);



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
    readString();
    return n;
}

char *readString() {
    char *target = NULL;
    size_t n = 0;
    size_t got = getline(&target, &n, stdin);
    if (got >= 1 && target[got - 1] == '\n') {
        target[got - 1] = (char)0;
    }
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

