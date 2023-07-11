// #include <R.h>
#include <Rdefines.h>
// #include <stdio.h>
// #include <string.h>

#include "wagner.h"

extern "C"{

  SEXP exchange(SEXP s, SEXP coeff, SEXP skpen){
    //first line is for test  int a=20;char aback1[20];char aback2[20];strcpy(aback1,"tu");strcpy(aback2,"tuF");printf("look at the returns:\n");printf("%s %s %d\n",aback1,aback2,a);

    // now we have to deal with the input, give the string value to two vectors
    //char s1[20], s2[20];
    // manually increase the length of accepted vectors
    char s1[200], s2[200];
    
    
    // printf("coeff -1: %d\n", INTEGER(coeff)[-1]);
    // printf("coeff 0: %d\n", INTEGER(coeff)[0]);
    // printf("coeff 1: %d\n", INTEGER(coeff)[1]);

    // copy the input strings
    strcpy(s1,CHAR(STRING_ELT(s,0)));
    strcpy(s2,CHAR(STRING_ELT(s,1)));
    
    int vec[13]; 
    
    for(int i=0;i<13;i++) {
      vec[i]=INTEGER(coeff)[i]; //printf("%d\n",vec[11]);
      // printf("%d\n", INTEGER(coeff)[i]);
    }

    int SkipPen=INTEGER(skpen)[0];     //printf("the skip penalty input:%d\n",SkipPen);

        int a=0;
        int *score;
        score=&a;
        //int vec[12]={1,2,3,4,5,6,7,8,9,0,11,12};
        char aback1[500];
        char aback2[500];

        Word::applyRedundancyRules(); // sets vowel height values
        
        Word* w1 = new Word(s1); // construct word 1
        Word* w2 = new Word(s2); // construct word 2
        
        // w1->showAll();
        // w2->showAll();
        
        align( w1, w2,score, aback1, aback2, vec, SkipPen);
        
        delete w1;
        delete w2;

        //printf("look at the outputs from character functions:\n");printf("%s\n",aback1);printf("%s\n",aback2); printf("The score: %d\n",a);
        a=*score;


    // This is the output part, where a list is now successfully returned!
    
    // create vectors for alignment1, alignment2, and simscore
    SEXP alignment1=PROTECT(allocVector(STRSXP,1));
    SEXP alignment2=PROTECT(allocVector(STRSXP,1));
    SEXP simscore=PROTECT(allocVector(REALSXP,1));
    
    // create wrapper vector
    SEXP result=PROTECT(allocVector(VECSXP,3)); 
    
    // fill in alignment1, aligment2, and simscore
    SET_STRING_ELT(alignment1,0,mkChar(aback1));
    SET_STRING_ELT(alignment2,0,mkChar(aback2));
    REAL(simscore)[0]=a;
    
    // fill result in with alignment1, alignment2, and simscore
    SET_VECTOR_ELT(result,0,alignment1);
    SET_VECTOR_ELT(result,1,alignment2);
    SET_VECTOR_ELT(result,2,simscore);
    UNPROTECT(4);
    
    // return
    return(result);
  }


}

float gP::ScoreMargin = 1.0;
int gP::MaxScore = conv(35);
int gP::MaxCmpScore = conv(45);
//int gP::SkipCost = conv(10);
int gP::VwlDecr = conv(10);
//int gP::SalCoef[FT_LEN] = { 5, 40, 50, 10, 10, 10, 10, 5, 1, 5, 5, 5, 10 };
