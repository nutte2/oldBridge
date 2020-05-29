#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define LEKANT  4    /* 4 Lekar ger bra utskrift */
#define BREDD 8
#define BUDRONDER 4

typedef enum { kl, ru, hj, sp } farger;
char FET[4]; /* Laser = { 27,69,0 };*/
char NOR[4]; /* = { 27,70,0 };*/
char valtab[]  = { '2','3','4','5','6','7','8','9','T','J','Q','K','A' };
char koltab[]  = "CDHS";//{ 5,4,3,6 }; /* Grafik f”r kortf„rger */
char p[4][6]   = { "None", "NS  ","EW  ","All "};
short zt[16]   = { 0,1,2,3,1,2,3,0,2,3,0,1,3,0,1,2 }; /* zoner */
char hnd[8][8] = { "North","East","South","West","Bidder","Generou",
	"Schema","Statist" };
char motbud[LEKANT][4][10];
int  lek[LEKANT][52]; /* 52 kort. No 51 = Sp A, 50 = Sp K etc */
char mess[81];
FILE *fp[9];          /* 4+2+1 utfiler  */
int  giv,pongs[5][5];
long blandade;
int  saab,fords[4],forde[5][5],slaskf[4];	/* F”rdelningar */
int  maxN=40,minN=0,minS=0,minTot=0,minTot1=0,sprid=1;
int  maxKl=13,minKl=0,maxRu=13,minRu=0,maxHj=13,minHj=0,maxSp=13,minSp=0;
int  nfords[4],nford[4];
float slask; /* F”r LIB! */

struct typtab {
	int type;
	int no;
       } fstaty[40];
int lastaty;

int numeric(const void *p1,const void *p2)
{
	return(*(int*)p1 - *(int*)p2);
}

/*int numtyp(struct typtab *a,struct typtab *b)*/
int numtyp(const void *a,const void *b)
{
	return(((struct typtab*)b)->no-((struct typtab*)a)->no);
}
void addtyp(int typ)
{
	int i;
	for (i=0;i<lastaty;i++)
		if (fstaty[i].type==typ)
		{
			fstaty[i].no++;
			return;
		}

	if (lastaty<40)
	{
		fstaty[lastaty].type = typ;
		fstaty[lastaty++].no=1;
		return;
	}
}

float br(int i)
{
	switch(i)
	{
	case 4432:return(21.55);
	case 5332:return(15.52);
	case 5431:return(12.93);
	case 5422:return(10.58);
	case 4333:return(10.54);
	case 6322:return(5.64);
	case 6421:return(4.70);
	case 5521:return(3.17);
	case 6331:return(3.44);
	case 4441:return(2.99);
	default:  return(0.0);

	}
}
void lityp(int givar)
{
	int i;
	qsort(fstaty,lastaty,sizeof(fstaty[0]),numtyp);
	fprintf(fp[7],"%s\n  Dist AbsFreq  RelFreq(%) Rel\n  ",mess);
	for(i=0;i<lastaty;i++)

		if (br(fstaty[i].type))
		fprintf(fp[7],"%4d     %4d%8.2f   %8.2f\n  ",
			fstaty[i].type,fstaty[i].no,
			100.0*fstaty[i].no/4.0/givar,br(fstaty[i].type));
		else fprintf(fp[7],"%4d     %4d%8.2f   \n  ",
			fstaty[i].type,fstaty[i].no,
			100.0*fstaty[i].no/4.0/givar);
}

void blanda(int no)
{	int i,tkort,byt,*lekno,serie;
	float rnd;
	blandade++;
        lekno = &lek[no][0];
	for (i = 0;i < 52;i++)
		lek[no][i] = i;
     for(serie=0;serie<1;serie++)
	for (i = 51; i >= 0; i--)
	{	byt = rand() % (i+1);
		tkort = lek[no][byt];
		lek[no][byt] = lek[no][i];
		lek[no][i] = tkort;
	}
}


void sortera(int no)
{ 	int i;
        for (i = 0 ; i < 52 ; i += 13 )
		qsort(lek[no]+i,13,sizeof(int),numeric);
}

farger farg(int kort)
{	if      (kort >= 39) return(sp);
	else if (kort >= 26) return(hj);
        else if (kort >= 13) return(ru);
        else		     return(kl);
}
int ford(int hnd,int sgiv,int an[4],int ans[4])
/* Ta reda p† f”rdelningen */
{	int i,j;
	for (i=0; i<4; i++)
		an[i] = 0;
	for (i=hnd; i<13+hnd; i++)
		an[farg(lek[sgiv][i])]++;
	for(i=0;i<4;i++)
		ans[i]=an[i];
	qsort(ans,4,sizeof(int),numeric);
	return(an[0]+an[1]*10+an[2]*100+an[3]*1000);
}

char valor(int kort)
{	return(valtab[kort % 13]);
}

int poeng(int no,int hand)
{   int po,lastk;
    po = 0;
    for (lastk = hand+13; hand < lastk; hand++)
    	switch (lek[no][hand] % 13)
    	{	case 12 : po += 4; break;
    	        case 11 : po += 3; break;
			case 10 : po += 2; break;
    		case  9 : po += 1; break;
        }
    return(po);
}

char *z(int no)
{
	return(p[zt[(no-1) % 16]]);
}

int ant[5][5];

void prifarg(int farg1,int sgiv,int i,int kort[LEKANT][4],int filno)
{
	int kol,bla;
			if (farg1==sp)
				ant[sgiv][i] = 13;
			fprintf(fp[filno],"%c",koltab[farg1]);
			kol = kort[sgiv][i];
			while ((farg(lek[sgiv][kort[sgiv][i]]) == farg1 ) &&
			  (ant[sgiv][i]-- > 0))

			{
			  fprintf(fp[filno]," %c",
					valor(lek[sgiv][kort[sgiv][i]--]));

			}
        		for (bla = 0 ; bla <= BREDD + kort[sgiv][i] - kol ; bla++)
				fprintf(fp[filno],"  ");

}


void skrivrad(int schema)
{
	int farg2,sgiv,farg1,kol,bla,fil,i;
int kort[LEKANT][4] = { {12,25,38,51},{12,25,38,51},
                                {12,25,38,51},{12,25,38,51}};
	for (farg1 = sp ; farg1 >= kl ; farg1--)
	{

       	   for (sgiv = 0;sgiv < LEKANT; sgiv++)
           {

		for (i = 0 ; i < 4 ; i++)
		{
			prifarg(farg1,sgiv,i,kort,i);
                 }
             }
             for (fil = 0; fil < LEKANT; fil++)
		  fprintf(fp[fil],"\n  ");
       	}

}

char zm(int givnu)
{
	switch(zt[givnu%16])
	{
	case 0:return('0'); break;
	case 1:return('|'); break;
	case 2:return('-'); break;
	case 3:return('+');break;
	}


}
void schema(int giv)
{
	int farg,sgiv,givnu;
int kort[LEKANT][4] = { {12,25,38,51},{12,25,38,51},
                                {12,25,38,51},{12,25,38,51}};

	for (sgiv = 0; sgiv < LEKANT; sgiv++)
	 {
	givnu = giv+sgiv;
	if (givnu%2==1) fprintf(fp[6],"%c\t\t%s",12,mess);
		else fprintf(fp[6],"\n\n\n\n  ");
	fprintf(fp[6],
	"\n  \t\tDeal %d %s/%s\n\n  ",givnu,hnd[(givnu-1)%4],p[zt[(givnu-1)%16]]);
	for(farg=sp;farg>= kl;farg--)
	{       fprintf(fp[6],"\n  \t\t\t  ");
		prifarg(farg,sgiv,0,kort,6);
	}
	fprintf(fp[6],"\n  ");
	for(farg=sp;farg>= kl;farg--)
	{
		fprintf(fp[6],"\n  \t");
		prifarg(farg,sgiv,3,kort,6);
		if (farg==sp)
		{
		fprintf(fp[6],"   %c           ",(givnu%4==1?'N':'*'));
		}
		else if(farg==hj)
		{
		fprintf(fp[6]," %c %c %c         ",
			(givnu%4==0?'W':'*'),zm(givnu-1),(givnu%4==2?'E':'*'));
		}
		else if(farg==ru)
		{
		fprintf(fp[6],"   %c           ",(givnu%4==3?'S':'*'));
		}
		else
		fprintf(fp[6],"               ");
		prifarg(farg,sgiv,1,kort,6);

	}
	fprintf(fp[6],"\n  ");
	for(farg=sp;farg>= kl;farg--)
	{       fprintf(fp[6],"\n  \t\t\t  ");
		prifarg(farg,sgiv,2,kort,6);
	}

    }

}
void skrivut2(int gno)
{	int i,fil,kol,slask[4];

			for (fil = 0; fil < 5; fil++)
	{
	     if (fil != 4) fprintf(fp[fil],"\n  ");
             fprintf(fp[fil],
"\n  Deal %s%2d%s N/%s(%2d) %s%2d%s E/%s(%2d)      %s%2d%s S/%s(%2d)      %s%2d%s W/%s(%2d) \n  ",
                  	FET,gno,  NOR,z(gno)  ,pongs[0][fil],
                        FET,gno+1,NOR,z(gno+1),pongs[1][fil],
                        FET,gno+2,NOR,z(gno+2),pongs[2][fil],
                        FET,gno+3,NOR,z(gno+3),pongs[3][fil]);
        }
        for(i=0;i<4;i++)
        {
		if (motbud[i][1][0] != 0) fprintf(fp[4],"E %7s",motbud[i][1]);
		else fprintf(fp[4],"  N  E  S");
		if (motbud[i][3][0] != 0) fprintf(fp[4],"W %7s ",motbud[i][3]);
		else fprintf(fp[4],"  W      ");
	}
		fprintf(fp[4],"\n\n\n\n  ");
/*		for (kol=0; kol<4;kol++)
		{
		fprintf(fp[4],"%c%c%c%c%c%c%c%c%c%c%c%c%c      ",
		218,196,196,194,196,196,194,196,196,194,196,196,191);
		}
		fprintf(fp[4],"\n  ");
	for (i=0;i<=BUDRONDER-2;i++)
	{
		for (kol=0; kol<4;kol++)
		{
		fprintf(fp[4],"%c%c%c%c%c%c%c%c%c%c%c%c%c      ",
		195,196,196,197,196,196,197,196,196,197,196,196,180);
		}
	   fprintf(fp[4],"\n  ");
	};
	   for (kol=0; kol<4;kol++)
		{
		fprintf(fp[4],"%c%c%c%c%c%c%c%c%c%c%c%c%c      ",
		192,196,196,193,196,196,193,196,196,193,196,196,217);
		}*/
	   fprintf(fp[4],"\n  ");
	skrivrad(0);
}
void skrivpairs(int gno)
{	int i,fil,sgiv,farg1,farg2,kol,bla,slask[4];
        int kort[LEKANT][4] = { {12,25,38,51},{12,25,38,51},
				{12,25,38,51},{12,25,38,51}};
	int ant[5][5];
/* Norths hand */

      	fil = 0;
/*	fprintf(fp[5],"\n  ");*/
	fprintf(fp[5],
"\n  North %s%2d%s N/%s(%2d) %s%2d%s E/%s(%2d)     %s%2d%s S/%s(%2d)     %s%2d%s W/%s(%2d) \n  ",
			FET,gno,  NOR,z(gno)  ,pongs[0][fil],
			FET,gno+1,NOR,z(gno+1),pongs[1][fil],
			FET,gno+2,NOR,z(gno+2),pongs[2][fil],
			FET,gno+3,NOR,z(gno+3),pongs[3][fil]);

	for (farg2 = sp ; farg2 >= kl ; farg2--)
	{

	   for (sgiv = 0;sgiv < LEKANT; sgiv++)
	   {

		farg1 = farg2;
		for (i = 0 ; i < 1 ; i++)
		{
			if (farg2 == sp)
				ant[sgiv][i]=13;
			fprintf(fp[5],"%c",koltab[farg1]);
			kol = kort[sgiv][i];
			while ((farg(lek[sgiv][kort[sgiv][i]]) == farg1 ) &&
			  (ant[sgiv][i]-- > 0))

				fprintf(fp[5]," %c",
					valor(lek[sgiv][kort[sgiv][i]--]));
			for (bla = 0 ; bla <= BREDD + kort[sgiv][i] - kol ; bla++)
				fprintf(fp[5],"  ");
		 }
	     }
		  fprintf(fp[5],"\n  ");
	}
/* Bidder */

	for(i=0;i<4;i++)
        {
		if (motbud[i][1][0] != 0) fprintf(fp[5],"E %7s",motbud[i][1]);
		else fprintf(fp[5],"  N  E  S");
		if (motbud[i][3][0] != 0) fprintf(fp[5],"W %7s ",motbud[i][3]);
		else fprintf(fp[5],"  W       ");
	}
		fprintf(fp[5],"\n\n\n\n  ");
/*	   for (kol=0; kol<4;kol++)
		{
		fprintf(fp[5],"%c%c%c%c%c%c%c%c%c%c%c%c%c      ",
		218,196,196,194,196,196,194,196,196,194,196,196,191);
		}
	   fprintf(fp[5],"\n  ");
	for (i=0;i<=BUDRONDER-2+3;i++)
	{
	   for (kol=0; kol<4;kol++)
		{
		fprintf(fp[5],"%c%c%c%c%c%c%c%c%c%c%c%c%c      ",
		195,196,196,197,196,196,197,196,196,197,196,196,180);
		}
	   fprintf(fp[5],"\n  ");
	};
	   for (kol=0; kol<4;kol++)
		{
		fprintf(fp[5],"%c%c%c%c%c%c%c%c%c%c%c%c%c      ",
		192,196,196,193,196,196,193,196,196,193,196,196,217);
		}*/
	   fprintf(fp[5],"\n  ");
/* South */
      	fil = 2;
        fprintf(fp[5],
"\n  South %s%2d%s N/%s(%2d) %s%2d%s E/%s(%2d)      %s%2d%s S/%s(%2d)      %s%2d%s W/%s(%2d) \n  ",
                  	FET,gno,  NOR,z(gno)  ,pongs[0][fil],
                        FET,gno+1,NOR,z(gno+1),pongs[1][fil],
								FET,gno+2,NOR,z(gno+2),pongs[2][fil],
								FET,gno+3,NOR,z(gno+3),pongs[3][fil]);

		for (farg2 = sp ; farg2 >= kl ; farg2--)
	{

				for (sgiv = 0;sgiv < LEKANT; sgiv++)
			  {

					 farg1 = farg2;
		for (i = 2 ; i < 3 ; i++)
			{
				fprintf(fp[5],"%c",koltab[farg1]);
				kol = kort[sgiv][i];
							while (farg(lek[sgiv][kort[sgiv][i]]) == farg1 )
					fprintf(fp[5]," %c",
						valor(lek[sgiv][kort[sgiv][i]--]));
			for (bla = 0 ; bla <= BREDD + kort[sgiv][i] - kol ; bla++)
					fprintf(fp[5],"  ");
					  }
				 }
	  fprintf(fp[5],"\n  ");
			}
}

void motbjud(int sgiv)
{	int i,j,fordeln[4],fords[4];
	for(i=1;i<4;i+=2)
	{
		ford(i*13,sgiv,fordeln,fords);
		strcpy(motbud[sgiv][i],"");
		for(j=3;j>=0;j--) /* B”rja med spader */
		{
/*			printf("ant %d:%d ,",j,fordeln[j]);*/
			 if(motbud[sgiv][i][0]==0) /* Inget bud hittills */
			if (fordeln[j] >= 6) /* S”k sexa eller mer */
			{
				if (pongs[sgiv][i]>5)
					sprintf(motbud[sgiv][i],
					"pre %c",koltab[j]);
				else if (*z(giv+sgiv)=='N') /* ozon */
					sprintf(motbud[sgiv][i],
					"pre %c",koltab[j]);
			}
			else if ((fordeln[j] == 5) && pongs[sgiv][i] >7)
				sprintf(motbud[sgiv][i],
				"bid %c",koltab[j]);
			else if ((fordeln[j] >= 4) && pongs[sgiv][i] >8)
				sprintf(motbud[sgiv][i],
				"opn %c",koltab[j]);
/*			printf("%s",motbud[sgiv][i]);*/
		}
	}
}

void dscanf(char *prmpt, int val, int *adr)
{
	char def[30];
	printf("%s [%d] ? ",prmpt,val);
	gets(def);
	if (def[0] == 0)
	  *adr = val;
	else *adr=atoi(def);
};

void fixford(void)
{
	int hnd,sgiv,i;
	for (hnd=0;hnd<52;hnd += 13)
		for(sgiv=0;sgiv<LEKANT;sgiv++)
		{
			for(i=0;i<4;i++)
				nford[i]=nfords[i]=0;
			ford(hnd,sgiv,nford,nfords);
			addtyp(nfords[3]*1000+nfords[2]*100+
				nfords[1]*10+nfords[0]);

		}

}
void main2(void)
{
	int hnds,sgiv,givar,utsk,slask,svar;
	int fordn[4];
   long now;
   char filnamn[128];
   int miK=0,maK=13,miR=0,maR=13,miH=0,maH=13,miS=0,maS=13,
       siK=0,saK=13,siR=0,saR=13,siH=0,saH=13,siS=0,saS=13;
   int minN2=0,maxN2=40,maxS=40,minS=0,minS2,maxS2;
   int siK2=0,saK2=13,siR2=0,saR2=13,siH2=0,saH2=13,siS2=0,saS2=13;
	printf("Bridgedeal generator with conditions. Version 4.00\n");
	printf("Copyright (c) 2016 Kalle Prorok\n");
	srand(time(&now) % 37);
	printf("Message to give (1 row) ? ");      gets(mess);

   dscanf("Number of deals",24,&givar);
   printf("\nMenu\n");
   printf("1. Simple deals\n");
   printf("2. One spec North\n");
   printf("3. One spec North & South\n");
   printf("4. Double spec Nord\n");
   printf("5. Double spec Nord & South\n");
   scanf("%d",&svar);
   getchar();
   if (svar != 1)
   {
   dscanf("Min pts N",0,&minN);
   dscanf("Max pts N ? ",40,&maxN);
   dscanf("Min pts S",0,&minS);
   dscanf("Max pts S ? ",40,&maxS);
	dscanf("Min pts Total (N+S)",0,&minTot1);
   dscanf("Variation in minimum of total points +-",
	0,&sprid);
   sprid *= 2;
   printf("Distribution NORTH hand.\n");

   dscanf("Min clubs    ",0,&minKl);
   dscanf("Max clubs    ",13,&maxKl);
   dscanf("Min diamonds ",0,&minRu);
   dscanf("Max diamonds ",13,&maxRu);
   dscanf("Min hearts   ",0,&minHj);
   dscanf("Max hearts   ",13,&maxHj);
   dscanf("Min spades   ",0,&minSp);
   dscanf("Max spades   ",13,&maxSp);

   if (svar>3)
   {
   printf("\nspecifikations NORTH hand (alt2).\n");

	dscanf("Min pts N",0,&minN2);
   dscanf("Max pts N ?  ",40,&maxN2);
   dscanf("Min clubs    ",0,&miK);
   dscanf("Max clubs    ",13,&maK);
   dscanf("Min diamonds ",0,&miR);
   dscanf("Max diamonds ",13,&maR);
   dscanf("Min hearts   ",0,&miH);
   dscanf("Max hearts   ",13,&maH);
   dscanf("Min spades   ",0,&miS);
	dscanf( "Max spades   ",13,&maS);
	} else
   { maK = -1; /* Om”jligt klara alt 2 */
   }
   if (svar>2)
   {
   printf("\nspecifikations SOUTH hand.\n");

   dscanf("Min antal Kl”ver ",0,&siK);
   dscanf("Max antal Kl”ver ",13,&saK);
   dscanf("Min antal Ruter  ",0,&siR);
   dscanf("Max antal Ruter  ",13,&saR);
   dscanf("Min antal Hj„rter",0,&siH);
   dscanf("Max antal Hj„rter",13,&saH);
   dscanf("Min antal Spader ",0,&siS);
   dscanf("Max antal Spader ",13,&saS);
   }
	if (svar>4)
   {
   printf("\nHar kommer specifikationer p† SYDS hand (alt2).\n");
   dscanf("Min po„ng S ? ",0,&minS2);
   dscanf("Max po„ng S ? ",40,&maxS2);

   dscanf("Min antal Kl”ver ",0,&siK2);
   dscanf("Max antal Kl”ver ",13,&saK2);
   dscanf("Min antal Ruter  ",0,&siR2);
   dscanf("Max antal Ruter  ",13,&saR2);
   dscanf("Min antal Hj„rter",0,&siH2);
   dscanf("Max antal Hj„rter",13,&saH2);
   dscanf("Min antal Spader ",0,&siS2);
   dscanf("Max antal Spader ",13,&saS2);

   } else
	{
		saK2 = -1;
	}
	}
   printf("Redeals!\nPoints Required Deal\n");
   for (utsk = 0; utsk < LEKANT + 1 + 1 + 1 + 1; utsk++)
	strcpy(filnamn, hnd[utsk]);
   strcat(filnamn, '.txt');
	if ((fp[utsk] = fopen(filnamn,"w"))==NULL)
	{
		printf("Can not open '%s'.\n",filnamn);
		exit(10);
	}
   minTot = minTot1;
	for (giv=1;giv<=givar;giv += LEKANT)
	{
	if ((giv % 32 ) == 1)
           for (utsk = 0; utsk < LEKANT+1; utsk++)
              fprintf(fp[utsk],"%c%ss hand. %s",
                 (giv>1 ? 12 : 32),hnd[utsk],mess);
        if ((giv % 12 ) == 1)
              fprintf(fp[5],"%c%ss hand. %s",
                 (giv>1 ? 12 : 32),hnd[5],mess);
        for (sgiv = 0; sgiv < LEKANT; sgiv++)
        {
		blanda(sgiv);
		if (sprid>0)
			minTot = minTot1 + rand() % sprid - sprid / 2;
		else minTot = minTot1;
                printf("(     )  %2d  %2d\r",minTot,giv+sgiv);
		while (( /* Alt 1 p† Nord */
			( (pongs[sgiv][0] = poeng(sgiv, 0)) < minN ) ||
                        ( (pongs[sgiv][0] = poeng(sgiv, 0)) > maxN ) ||
                        ( (pongs[sgiv][2] = poeng(sgiv,26)) < minS ) ||
                        (  pongs[sgiv][0] + pongs[sgiv][2]  < minTot ) ||
			( ((saab=ford(0,sgiv,fords,fordn)) && (fords[0] > maxKl)) ) ||
                        (                       fords[0] < minKl   ) ||
                        (                       fords[1] > maxRu   ) ||
			(                       fords[1] < minRu   ) ||
			(                       fords[2] > maxHj   ) ||
			(                       fords[2] < minHj   ) ||
			(                       fords[3] > maxSp   ) ||
			(                       fords[3] < minSp   )
		       ) &&
		       ( /* Alt 2 p† Nord */

			( (pongs[sgiv][0] = poeng(sgiv, 0)) < minN2 ) ||
			( (pongs[sgiv][0] = poeng(sgiv, 0)) > maxN2 ) ||
			(
			  (saab=ford(0,sgiv,fords,fordn)) &&
							 (fords[0] > maK)  ) ||
			(                       fords[0] < miK   ) ||
			(                       fords[1] > maR   ) ||
			(                       fords[1] < miR   ) ||
			(                       fords[2] > maH   ) ||
			(                       fords[2] < miH   ) ||
			(                       fords[3] > maS   ) ||
			(                       fords[3] < miS   )
		       ) ||
				 ( /* Alt 1 p† S */

			( ((saab=ford(26,sgiv,fords,fordn)) && (fords[0] > saK)) ) ||
			( (pongs[sgiv][2] = poeng(sgiv,26)) > maxS ) ||
			(  pongs[sgiv][2] < minS) ||
			(                       fords[0] < siK   ) ||
			(                       fords[1] > saR   ) ||
			(                       fords[1] < siR   ) ||
			(                       fords[2] > saH   ) ||
			(                       fords[2] < siH   ) ||
			(                       fords[3] > saS   ) ||
			(                       fords[3] < siS   )
		       ) &&
		       (/* Alt 2 p† S */

			( ((saab=ford(26,sgiv,fords,fordn)) && (fords[0] > saK2)) ) ||
			( (pongs[sgiv][2] = poeng(sgiv,26)) > maxS2 ) ||
			(  pongs[sgiv][2] < minS2) ||
			(                       fords[0] < siK2  ) ||
			(                       fords[1] > saR2  ) ||
			(                       fords[1] < siR2  ) ||
			(                       fords[2] > saH2  ) ||
			(                       fords[2] < siH2  ) ||
			(                       fords[3] > saS2  ) ||
			(                       fords[3] < siS2  )
		       )

		      ) {  printf("(%2d %2d\r",pongs[sgiv][0],pongs[sgiv][2]);
			   blanda(sgiv);
                        }
                pongs[sgiv][1] = poeng(sgiv,13);
                pongs[sgiv][3] = poeng(sgiv,39);
		motbjud(sgiv);
   	        sortera(sgiv);
        }
   	skrivut2(giv);
	skrivpairs(giv);
	schema(giv);
	fixford();
   }
/*   for (hnds = 0; hnds < 4; hnds++)
	fprintf(fp[hnds],"%c",12);*/
   lityp(givar);
   fcloseall();
   printf("\n\nReady. %ld shuffles done!\n",blandade);
}
