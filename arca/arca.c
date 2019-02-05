/* arca.c
 * Andrew A. Cashner
 * 2019/02/04
 * Tests toward a digital version of Kircher's Arca musarithmica
 */

#include <stdio.h>

#define MAX_COL 6
#define MAX_ROW 10
#define MAX_VV  4
#define MAX_SYL 6

int col[MAX_ROW][MAX_VV][MAX_SYL] = {
    { 
        {5, 5, 5, 3, 5, 5}, 
        {8, 8, 7, 8, 7, 8},
        {3, 3, 2, 2, 2, 3}, 
        {8, 8, 5, 6, 5, 1}
    },
    {
        {4, 4, 3, 2, 2, 2},
        {2, 2, 8, 7, 6, 7},
        {6, 6, 5, 5, 4, 5},
        {2, 2, 3, 5, 2, 5}
    },
    {
        {3, 2, 3, 5, 4, 5},
        {8, 7, 8, 2, 8, 2},
        {5, 5, 5, 2, 6, 7},
        {8, 5, 8, 7, 6, 5}
    },
    {   
        {8, 8, 2, 3, 2, 3},
        {6, 5, 6, 8, 7, 8},
        {4, 5, 4, 5, 5, 5},
        {4, 3, 2, 1, 5, 1}
    },
    {
        {6, 5, 6, 5, 4, 3},
        {8, 8, 8, 8, 8, 8},
        {4, 3, 4, 5, 6, 5},
        {4, 8, 4, 3, 4, 1}
    }
};

typedef int *col_ptr[MAX_ROW][MAX_VV][MAX_SYL];
col_ptr pinax[6];

/*
typedef struct {
    int id;
    int syl;
    int voices[4][6];
} perm;

void perm_init(perm *p);
void set_voices(perm *p, int new[4][6]);
void print_voices(perm *p);
*/

int main(void) {

    int c, v, s;
    pinax[6] = &col;
    for (c = 6; c < MAX_COL; ++c) {
        for (v = 0; v < MAX_VV; ++v) {
            for (s = 0; s < MAX_SYL; ++s) {
                printf("%d ", pinax[c][v][s]);
            }
            printf("\n");
        }
        printf("* * *\n");
    }

/*   
    perm p1;
    perm_init(&p1);
    p1.id = 1;
    p1.syl = 6;
    set_voices(&p1, col[0]);
    print_voices(&p1);
    */
    

    return(0);
}
/*
void perm_init(perm *p) {
    int i, j;
    p->id = 0;
    p->syl = 0;
    for (i = 0; i < 4; ++i) {
        for (j = 0; j < 6; ++j) {
            p->voices[i][j] = 0;
        }
    }
    return;
}

void set_voices(perm *p, int new[4][6]) {
    int i, j;
    for (i = 0; i < 4; ++i) {
        for (j = 0; j < p->syl; ++j) {
            p->voices[i][j] = new[i][j];
        }
    }
    return;
}

void print_voices(perm *p){
    int i, j;
    for (i = 0; i < 4; ++i) {
        for (j = 0; j < p->syl; ++j) {
            printf("%d ", p->voices[i][j]);
        }
        printf("\n");
    }
    return;
}
*/
