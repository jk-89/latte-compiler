#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

void printInt(int x) {
    printf("%d\n", x);
    fflush(stdout);
}

int readInt() {
    int x;
    scanf("%d ", &x);
    return x;
}

void printString(char *s) {
    printf("%s\n", s);
    fflush(stdout);
}

char *readString() {
    char *line = NULL;
    size_t len = 0;
    ssize_t read = getline(&line, &len, stdin);
    if (read == -1) {
        line = malloc(1 * sizeof(char));
        if (!line) {
            printf("Malloc error.\n");
            exit(1);
        }
        line[0] = 0;
    }
    if (read > 0) { 
        if (line[read - 1] == '\n') {
            line[read - 1] = 0;
        }
    }
    return line;
}

void error() {
    printf("runtime error\n");
    exit(1);
}

char *_concatenateStrings(char *s1, char *s2) {
    size_t len1 = strlen(s1);
    size_t len2 = strlen(s2);
    size_t len_total = len1 + len2 + 1;

    char *allocated = malloc(len_total * sizeof(char));
    if (!allocated) {
        printf("Malloc error.\n");
        exit(1);
    }
    allocated[0] = 0;
    allocated = strcat(allocated, s1);
    allocated = strcat(allocated, s2);
    return allocated;
}

bool _stringsEqual(char *s1, char *s2) {
    return strcmp(s1, s2) == 0;
}

bool _stringsNotEqual(char *s1, char *s2) {
    return !_stringsEqual(s1, s2);
}
