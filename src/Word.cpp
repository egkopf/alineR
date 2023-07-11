// Copyright (c) 2015 by Sean Downey
// Authors: Sean Downey (sean@codexdata.com) and Guowei Sun (gwsun@umd.edu)
// This software is distributed under GPL-3.
// Includes modified C++ code originally developed by Greg Kondrak (c) 2000

// Copyright (c) 2000 by Grzegorz Kondrak
//
// Permission is granted for anyone to copy, use, or modify this program
// for purposes of research or education, provided this copyright notice
// is retained, and note is made of any changes that have been made.
//
// This program is distributed without any warranty, express or implied.
// As this program was written for research purposes only, it has not been
// tested to the degree that would be advisable in any important application.
// All use of this program is entirely at the user's own risk.

// This file contains routines for computing similarity between
// individual phonetic segments based on their feature representation.

#include "Word.h"
#include <cstring>
#include <stdlib.h>
#define min(a,b) (((a)<(b))?(a):(b)))

// feature values
const int FSyl = 0;
	const fval syl = 100;
	const fval nsl = 0;
const int FPlace = 1;
	      const fval glo = 10;
        const fval pha = 30;
        const fval uvu = 50;
        const fval vel = 60;
        const fval pal = 70;
        const fval pav = 75;
        // const fval rfl = 80;
        const fval alv = 85;
        const fval dnt = 90;
        const fval lbd = 95;
	const fval blb = 100;
const int FStop  = 2;
        const fval lvl = 0;
        const fval lmv = 13;
        const fval hmv = 27;
        const fval hvl = 40;
        const fval vwl = hvl;
        const fval apr = 60;
        const fval frc = 80;
        // const fval afr = 90;
        const fval stp = 100;
const int FVoice = 3;
  const fval imp = 100;
	const fval vce = 70;
	const fval vls = 30;
	const fval ejt = 0;
const int FNasal = 4;
	const fval nas = 100;
//	const fval nns = 0;
const int FRetro = 5;
	const fval ret = 100;
//	const fval nrt = 0;
const int FLat  = 6;
	const fval lat = 100;
//	const fval nlt = 0;
const int FAsp  = 7;
	const fval asp = 100;
//	const fval nap = 0;
const int FLong = 8;
	const fval lng = 100;
//	const fval sht = 0;
const int FHigh = 9;
	const fval hgh = 100;
	const fval hmi = 67;
	const fval lmi = 33;
	const fval low = 0;
const int FBack = 10;
	const fval frt = 100;
	const fval cnt = 50;
	const fval bak = 0;
const int FRound = 11;
	const fval rnd = 100;
	const fval nrd = 0;
const int FDouble = 12;

#define FVW syl, pal, vwl, vce, 0, 0, 0, 0, 0
#define BVW syl, vel, vwl, vce, 0, 0, 0, 0, 0
#define FGL nsl, pal, vwl, vce, 0, 0, 0, 0, 0
#define BGL nsl, vel, vwl, vce, 0, 0, 0, 0, 0
#define CNS nsl

FVec Word::FCon[NSEG] = {
    { FVW, low, frt }, // a
    { CNS, blb, stp, vce }, // b
    { CNS, pal, stp, vls }, // c
    { CNS, alv, stp, vce }, // d
    { FVW, hmi, frt }, // e
    { CNS, lbd, frc, vls }, // f
    { CNS, vel, stp, vce }, // g
    { CNS, glo, frc, vls }, // h
    { FVW, hgh, frt }, // i
    { FGL, hgh, frt }, // j
    { CNS, vel, stp, vls }, // k
    { CNS, alv, apr, vce, 0, 0, lat }, // l
    { CNS, blb, stp, vce, nas }, // m
    { CNS, alv, stp, vce, nas }, // n
    { BVW, hmi, bak, rnd }, // o
    { CNS, blb, stp, vls }, // p
    { CNS, uvu, stp, vls }, // q
    { CNS, alv, apr, vce }, // r // ACTUALLY an (upside down r)
    { CNS, alv, frc, vls }, // s
    { CNS, alv, stp, vls }, // t
    { BVW, hgh, bak, rnd }, // u
    { CNS, lbd, frc, vce }, // v
    { BGL, hgh, bak, rnd, blb }, // w
    { CNS, vel, frc, vls }, // x
    { FVW, hgh, frt, rnd }, // y
    { CNS, alv, frc, vce } // z
};

// true if segment is a vowel
inline bool vowel( FP p )
{
    return ( p[FStop] <= vwl );
}

// this is to make FCon more transparent
// sets vowel height values?
void Word::applyRedundancyRules()
{
    for ( int i = 0; i < NSEG; i++ )
    {
        if ( vowel(FCon[i]) )
        {
                 if ( FCon[i][FHigh] == hgh ) FCon[i][FStop] = hvl;
            else if ( FCon[i][FHigh] == hmi ) FCon[i][FStop] = hmv;
            else if ( FCon[i][FHigh] == lmi ) FCon[i][FStop] = lmv;
            else if ( FCon[i][FHigh] == low ) FCon[i][FStop] = lvl;
            else assert( false );
        }

        if ( !vowel(FCon[i]) )		// harmless
        {
            FCon[i][FHigh] = hgh;
        }
    }
}

// the sole constructor
Word::Word( const char *w )
{
    outlen = strlen( w );
    // printf("the outlen of %s:%d\n",w,outlen);
    assert( outlen > 0 && outlen < ELEN ); // length check
    assert( w[0] >= BASE_LOW ); // at least 'a'
    strcpy( out, w ); //out is "external (mixed) representation"
    fconvert(); // fill feature matrix
}

// Display external representation sequence corresponding to
// an individual phoneme.
int Word::flush( short wi, char aback[50], int abackcount )
//void Word::flush( short wi )
{
    assert( wi >= 0 && wi < len );
    short o1 = ind[wi];               // pointers to w.out
    short o2 = ind[wi+1];
    for ( int t = 0; t < TAB; t++ )
    {
        if ( o1 != o2 )
            {
            //printf( "%c", out[o1++] );
            o1++;
            aback[abackcount]=out[o1-1]; abackcount++;
            }             //This palce could easily have some mistakes!
        else
            {
            //printf( " " );
            aback[abackcount]=' '; abackcount++;
            }
    }
    return(abackcount);
}

// Display whole record.
void Word::showAll()
{
    printf( "    out: |%s|\n", out );
    printf( " outlen: %d\n", outlen );
    printf( "         " );
    for ( int i = 0; i <= len; i++ )
    {
        printf( "%d ", ind[i] );
    }
    printf( "\n" );
    for ( int i = 0; i < len; i++ )
    {
        printf( "         " );
        for ( int f = 0; f < FT_LEN; f++ )
        {
            printf( "%4.1d", phon[i][f] );
        }
        printf( "\n" );
    }
    printf( "    len: %d\n", len );
}

// Convert the external string representation into a feature matrix.
void Word::fconvert()
{
    int j = -1; // character in phon counter
    for ( int seg = 0; seg < outlen; seg++ ) // character in input string counter
    {
        char c = out[seg];
        if ( c >= BASE_LOW ) // lowercase
        {
            j++; // increment phon character index
            for ( int f = 0; f < FT_LEN; f++ ) // for each feature
            {
                phon[j][f] = FCon[ c - BASE_LOW ][f]; // look up feature values in FCon and write to phon 
            }
            ind[j] = seg; // mapping of input string rep chars to output string rep chars ?
        }
        else // if it's uppercase
        {
            modify( phon[j], c ); // perform the modification specified by the uppercase letter
        }
    }
    ind[j+1] = outlen; // final character endloc
    len = j+1; // num output characters
    assert( j+1 < PLEN ); // size req
}

// modifies feature matrix
void Word::modify( FP p, char c )
{
    switch ( c )
    {
        case 'A': p[FAsp] = asp;        // "Aspirated"
            break;
        case 'B': p[FLat] = lat;        // "lateral_B"
            break;
        case 'C': p[FBack] = cnt;       // "Central"
            break;
        case 'D': p[FPlace] = dnt;      // "Dental"
            break;
        case 'E': p[FVoice] = imp;      // "implosivE"
            break;
        case 'F': p[FBack] = frt;       // "Front"
            break;
        case 'G': p[FPlace] = glo;      // "Glottal"
            break;
        case 'H': p[FLong] = lng;       // "loHng"
            break;
        case 'I':                       // "open-mId"
            p[FPlace] = lmv;
            p[FHigh] = lmi;
            break;
        case 'J': p[FVoice] = ejt;
            break;
        case 'K': p[FBack] = bak;       // "bacK"
            break;
        case 'L':                       // "Lax" (ignored)
            break;
        case 'M': p[FStop] = apr;       // "approxiMant"
            break;
        case 'N': p[FNasal] = nas;      // "Nasal"
            break;
        case 'O': p[FRound] = rnd;      // "rOund"
            break;
        case 'P': p[FPlace] = pal;      // "Palatal"
            break;
        case 'S': p[FStop] = frc;       // "fricative_S"
            break;
        case 'T': p[FPlace] = pav;      // "posT_alveolar
            break;
        case 'U': p[FRound] = nrd;      // "Unrounded"
            break;
        case 'V': p[FVoice] = vce;      // "Voiced"
            break;
        case 'X': p[FRetro] = ret;      // "retrofleX" added by SD 8-31-06
            break;
        case 'Y': p[FPlace] = pha;      // "pharYngeal"
            break;
        default:
           // cout << "I don't know " << c << endl;
            assert( false );
    }
}

// deal with double articulations
int places( FP p1, FP p2, int vec[12] )
{
    //for(int test=0;test<12;test++) {printf("%d",coeff[test]);}; printf("\n"); This means the values are also in here!

    int pd = abs( p1[FPlace] - p2[FPlace] );

    if ( p1[FDouble] ) {
    
    	if ( pd > abs( p1[FDouble] - p2[FPlace]) ) {
        	pd = abs( p1[FDouble] - p2[FPlace]);
        }
        
        }

    if ( p2[FDouble] )
//        pd = min( pd,  abs( p1[FPlace] - p2[FDouble] ) );

			if ( pd > abs( p1[FPlace] - p2[FDouble] ) ){
				pd = abs( p1[FPlace] - p2[FDouble] );
			}

    if ( p1[FDouble] && p2[FDouble] )
//        pd = min( pd,  abs( p1[FDouble] - p2[FDouble] ) );
		if ( pd > abs( p1[FDouble] - p2[FDouble] ) ){
				pd = abs( p1[FDouble] - p2[FDouble] );
			}

    //cout<<gP::SalCoef[FPlace]<<endl; //This shows that SalCoef is jsut like a vector
    
    //return pd * gP::SalCoef[FPlace];
    return pd * vec[FPlace];
}

// difference between segments for a given feature
#define diff(f)		( ( d = p1[f] - p2[f] ) > 0 ? d : -d )

// compute difference wrt specific feature
//#define Consider(f)	dist += diff(f) * gP::SalCoef[f]

//reproduce a function would be too complicated, let's put the code back in the functinos. OK!?

// compute distance between two feature vectors
int ladDist( FP p1, FP p2, int vec[12] )
{
    //for(int test=0;test<12;test++) {printf("%d",coeff[test]);}; printf("\n"); This means the value is also in here!
    
    int dist = 0;
    fval d;

    if ( vowel(p1) && vowel(p2) )
    {
        //Consider(FSyl);
        dist += diff(FSyl) * vec[FSyl];
        // printf("%d\n", dist);
        //Consider(FNasal);
        dist += diff(FNasal) * vec[FNasal];
        // printf("%d\n", dist);
        //Consider(FRetro);
        dist += diff(FRetro) * vec[FRetro];
        // printf("%d\n", dist);
        //Consider(FHigh);
        dist += diff(FHigh) * vec[FHigh];
        // printf("%d\n", dist);
        //Consider(FBack);
        dist += diff(FBack) * vec[FBack];
        // printf("%d\n", dist);
        //Consider(FRound);
        dist += diff(FRound) * vec[FRound];
        // printf("%d\n", dist);
        //Consider(FLong);
        dist += diff(FLong) * vec[FLong];
        // printf("%d\n", dist);
    }
    else
    {
        //Consider(FSyl);
        dist += diff(FSyl) * vec[FSyl];
        // printf("%d\n", dist);
        
        dist += places( p1, p2, vec );		// was: Consider(FPlace);
        // printf("%d\n", dist);
        
        //Consider(FStop);
        dist += diff(FStop) * vec[FStop];
        // printf("%d\n", dist);
        //Consider(FVoice);
        dist += diff(FVoice) * vec[FVoice];
        // printf("%d\n", dist);
        //Consider(FNasal);
        dist += diff(FNasal) * vec[FNasal];
        // printf("%d\n", dist);
        //Consider(FRetro);
        dist += diff(FRetro) * vec[FRetro];
        // printf("%d\n", dist);
        //Consider(FLat);
        dist += diff(FLat) * vec[FLat];
        // printf("%d\n", dist);
        //Consider(FAsp);
        dist += diff(FAsp) * vec[FAsp];
        // printf("%d\n", dist);
        
    }
    
   
    // printf("dist between %hu and %hu is %d\n", *p1, *p2, dist);

    return dist;
}

// compute score for substitution
int sigmaSub( Word *wA, short iA, Word *wB, short iB, int vec[12] )
{
    if(iA==0 || iB==0) return NoScore; //bug fix
    FP pA = wA->phon[wA->len-iA];
    FP pB = wB->phon[wB->len-iB];
    
    //printf("\npA:"); for (int i = 0; i < 12; i++) {printf(" %d", pA[i]);}printf("\npB:"); for (int i = 0; i < 12; i++) {printf(" %d", pB[i]);}printf("\n");
    
    int score = gP::MaxScore - ladDist( pA, pB, vec );

    if ( vowel(pA) )
        score -= gP::VwlDecr;
    if ( vowel(pB) )
        score -= gP::VwlDecr;
    
    return score;
}

// compute score for expansion/compresion
int sigmaExp( Word *wA, short iA, Word *wB, short i1B, short i2B, int vec[12] )
{
    // bug fix 
    
    //for(int test=0;test<12;test++) {printf("%d",coeff[test]);}; printf("\n");
    if(iA==0 || i2B==0) return NoScore; //bug fix
    
    if ( i1B == 0 )
        return NoScore;

    FP pA  = wA->phon[wA->len-iA];
    FP p1B = wB->phon[wB->len-i1B];
    FP p2B = wB->phon[wB->len-i2B];

    int d1 = ladDist( p1B, pA, vec );
    int d2 = ladDist( p2B, pA, vec );

    if ( ( d1 == 0 ) || ( d2 == 0 ) )
        return NoScore;

    int score = gP::MaxCmpScore - (d1 + d2);

    if ( vowel(p1B) || vowel(p2B) )
        score -= gP::VwlDecr;
    if ( vowel(pA) )
        score -= gP::VwlDecr;
    return score;
}

// compute score for insertion/deletion
int sigmaSkip( Word *w, short i , int skippen)
{
   // FP p = w->phon[w->len-i];

    //return -gP::SkipCost;
    return skippen;
}

// check if they are identical
int sigmaIdent(  Word *wA, short iA, Word *wB, short iB )
{
    FP pA = wA->phon[wA->len-iA];
    FP pB = wB->phon[wB->len-iB];

    for ( int f = 0; f < FT_LEN; f++ )
    {
        if ( pA[f] != pB[f] )
            return 0;
    }

    return 1;
}

