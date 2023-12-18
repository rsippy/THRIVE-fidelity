/************************************************/
/* Survey results                               */
/* Rachel Sippy                                 */
/*                                              */
/* 1: Site Implementation                       */
/* 2: Personal Implementation                   */
/* 3: Perfect Score                             */
/************************************************/

proc contents data=sip.survey;
run;

proc freq data=sip.survey;
table site*ImplYN;
run;

/************************************************/
/* 1: Site Implementation                       */
/*                                              */
/************************************************/

proc logistic data=sip.survey;
class site(ref='Warringt');
model ImplYN (event='1') = site;
contrast 'Bex vs Ctrls' site 1 -0.1 0 0 -0.1 0 -0.1 0 -0.1 -0.1 -0.1 -0.1 0 -0.1 -0.1 0 0 -0.1/e estimate = exp; 
contrast 'CamP vs Ctrls' site 0 -0.1 1 0 -0.1 0 -0.1 0 -0.1 -0.1 -0.1 -0.1 0 -0.1 -0.1 0 0 -0.1/e estimate = exp; 
contrast 'Cam vs Ctrls' site 0 -0.1 0 1 -0.1 0 -0.1 0 -0.1 -0.1 -0.1 -0.1 0 -0.1 -0.1 0 0 -0.1/e estimate = exp; 
contrast 'Her vs Ctrls' site 0 -0.1 0 0 -0.1 1 -0.1 0 -0.1 -0.1 -0.1 -0.1 0 -0.1 -0.1 0 0 -0.1/e estimate = exp; 
contrast 'Man vs Ctrls' site 0 -0.1 0 0 -0.1 0 -0.1 1 -0.1 -0.1 -0.1 -0.1 0 -0.1 -0.1 0 0 -0.1/e estimate = exp; 
contrast 'Sto vs Ctrls' site 0 -0.1 0 0 -0.1 0 -0.1 0 -0.1 -0.1 -0.1 -0.1 1 -0.1 -0.1 0 0 -0.1/e estimate = exp; 
contrast 'Tow vs Ctrls' site 0 -0.1 0 0 -0.1 0 -0.1 0 -0.1 -0.1 -0.1 -0.1 0 -0.1 -0.1 1 0 -0.1/e estimate = exp; 
contrast 'Wal vs Ctrls' site 0 -0.1 0 0 -0.1 0 -0.1 0 -0.1 -0.1 -0.1 -0.1 0 -0.1 -0.1 0 1 -0.1/e estimate = exp; 
contrast 'Bra vs Ctrls' site 0 1 0 0 -0.111 0 -0.111 0 -0.111 -0.111 -0.111 -0.111 0 -0.111 -0.111 0 0 -0.111/e estimate = exp; 
contrast 'Eas vs Ctrls' site 0 -0.111 0 0 1 0 -0.111 0 -0.111 -0.111 -0.111 -0.111 0 -0.111 -0.111 0 0 -0.111/e estimate = exp; 
contrast 'Lew vs Ctrls' site 0 -0.111 0 0 -0.111 0 1 0 -0.111 -0.111 -0.111 -0.111 0 -0.111 -0.111 0 0 -0.111/e estimate = exp; 
contrast 'Nen vs Ctrls' site 0 -0.111 0 0 -0.111 0 -0.111 0 1 -0.111 -0.111 -0.111 0 -0.111 -0.111 0 0 -0.111/e estimate = exp; 
contrast 'Nor vs Ctrls' site 0 -0.111 0 0 -0.111 0 -0.111 0 -0.111 1 -0.111 -0.111 0 -0.111 -0.111 0 0 -0.111/e estimate = exp; 
contrast 'Por vs Ctrls' site 0 -0.111 0 0 -0.111 0 -0.111 0 -0.111 -0.111 1 -0.111 0 -0.111 -0.111 0 0 -0.111/e estimate = exp; 
contrast 'Sou vs Ctrls' site 0 -0.111 0 0 -0.111 0 -0.111 0 -0.111 -0.111 -0.111 1 0 -0.111 -0.111 0 0 -0.111/e estimate = exp; 
contrast 'Sto vs Ctrls' site 0 -0.111 0 0 -0.111 0 -0.111 0 -0.111 -0.111 -0.111 -0.111 0 1 -0.111 0 0 -0.111/e estimate = exp; 
contrast 'Sun vs Ctrls' site 0 -0.111 0 0 -0.111 0 -0.111 0 -0.111 -0.111 -0.111 -0.111 0 -0.111 1 0 0 -0.111/e estimate = exp; 
contrast 'Wor vs Ctrls' site 0 -0.111 0 0 -0.111 0 -0.111 0 -0.111 -0.111 -0.111 -0.111 0 -0.111 -0.111 0 0 1/e estimate = exp; 
run;

/************************************************/
/* 2: Personal Implementation                   */
/*                                              */
/************************************************/

proc logistic data=sip.survey;
class site(ref='Warringt');
model PersYN (event='1') = site;
contrast 'Bex vs Ctrls' site 1 -0.1 0 0 -0.1 0 -0.1 0 -0.1 -0.1 -0.1 -0.1 0 -0.1 -0.1 0 0 -0.1/e estimate = exp; 
contrast 'CamP vs Ctrls' site 0 -0.1 1 0 -0.1 0 -0.1 0 -0.1 -0.1 -0.1 -0.1 0 -0.1 -0.1 0 0 -0.1/e estimate = exp; 
contrast 'Cam vs Ctrls' site 0 -0.1 0 1 -0.1 0 -0.1 0 -0.1 -0.1 -0.1 -0.1 0 -0.1 -0.1 0 0 -0.1/e estimate = exp; 
contrast 'Her vs Ctrls' site 0 -0.1 0 0 -0.1 1 -0.1 0 -0.1 -0.1 -0.1 -0.1 0 -0.1 -0.1 0 0 -0.1/e estimate = exp; 
contrast 'Man vs Ctrls' site 0 -0.1 0 0 -0.1 0 -0.1 1 -0.1 -0.1 -0.1 -0.1 0 -0.1 -0.1 0 0 -0.1/e estimate = exp; 
contrast 'Sto vs Ctrls' site 0 -0.1 0 0 -0.1 0 -0.1 0 -0.1 -0.1 -0.1 -0.1 1 -0.1 -0.1 0 0 -0.1/e estimate = exp; 
contrast 'Tow vs Ctrls' site 0 -0.1 0 0 -0.1 0 -0.1 0 -0.1 -0.1 -0.1 -0.1 0 -0.1 -0.1 1 0 -0.1/e estimate = exp; 
contrast 'Wal vs Ctrls' site 0 -0.1 0 0 -0.1 0 -0.1 0 -0.1 -0.1 -0.1 -0.1 0 -0.1 -0.1 0 1 -0.1/e estimate = exp; 
contrast 'Bra vs Ctrls' site 0 1 0 0 -0.111 0 -0.111 0 -0.111 -0.111 -0.111 -0.111 0 -0.111 -0.111 0 0 -0.111/e estimate = exp; 
contrast 'Eas vs Ctrls' site 0 -0.111 0 0 1 0 -0.111 0 -0.111 -0.111 -0.111 -0.111 0 -0.111 -0.111 0 0 -0.111/e estimate = exp; 
contrast 'Lew vs Ctrls' site 0 -0.111 0 0 -0.111 0 1 0 -0.111 -0.111 -0.111 -0.111 0 -0.111 -0.111 0 0 -0.111/e estimate = exp; 
contrast 'Nen vs Ctrls' site 0 -0.111 0 0 -0.111 0 -0.111 0 1 -0.111 -0.111 -0.111 0 -0.111 -0.111 0 0 -0.111/e estimate = exp; 
contrast 'Nor vs Ctrls' site 0 -0.111 0 0 -0.111 0 -0.111 0 -0.111 1 -0.111 -0.111 0 -0.111 -0.111 0 0 -0.111/e estimate = exp; 
contrast 'Por vs Ctrls' site 0 -0.111 0 0 -0.111 0 -0.111 0 -0.111 -0.111 1 -0.111 0 -0.111 -0.111 0 0 -0.111/e estimate = exp; 
contrast 'Sou vs Ctrls' site 0 -0.111 0 0 -0.111 0 -0.111 0 -0.111 -0.111 -0.111 1 0 -0.111 -0.111 0 0 -0.111/e estimate = exp; 
contrast 'Sto vs Ctrls' site 0 -0.111 0 0 -0.111 0 -0.111 0 -0.111 -0.111 -0.111 -0.111 0 1 -0.111 0 0 -0.111/e estimate = exp; 
contrast 'Sun vs Ctrls' site 0 -0.111 0 0 -0.111 0 -0.111 0 -0.111 -0.111 -0.111 -0.111 0 -0.111 1 0 0 -0.111/e estimate = exp; 
contrast 'Wor vs Ctrls' site 0 -0.111 0 0 -0.111 0 -0.111 0 -0.111 -0.111 -0.111 -0.111 0 -0.111 -0.111 0 0 1/e estimate = exp; 
run;

/************************************************/
/* 3: Perfect Score                             */
/* Warrington estimate                          */
/************************************************/

proc logistic data=sip.survey;
class site(ref='Warringt');
model THRIVEQ_CorrectAns (event='1') = site;
contrast 'Bex vs Ctrls' site 1 -0.1 0 0 -0.1 0 -0.1 0 -0.1 -0.1 -0.1 -0.1 0 -0.1 -0.1 0 0 -0.1/e estimate = exp; 
contrast 'CamP vs Ctrls' site 0 -0.1 1 0 -0.1 0 -0.1 0 -0.1 -0.1 -0.1 -0.1 0 -0.1 -0.1 0 0 -0.1/e estimate = exp; 
contrast 'Cam vs Ctrls' site 0 -0.1 0 1 -0.1 0 -0.1 0 -0.1 -0.1 -0.1 -0.1 0 -0.1 -0.1 0 0 -0.1/e estimate = exp; 
contrast 'Her vs Ctrls' site 0 -0.1 0 0 -0.1 1 -0.1 0 -0.1 -0.1 -0.1 -0.1 0 -0.1 -0.1 0 0 -0.1/e estimate = exp; 
contrast 'Man vs Ctrls' site 0 -0.1 0 0 -0.1 0 -0.1 1 -0.1 -0.1 -0.1 -0.1 0 -0.1 -0.1 0 0 -0.1/e estimate = exp; 
contrast 'Sto vs Ctrls' site 0 -0.1 0 0 -0.1 0 -0.1 0 -0.1 -0.1 -0.1 -0.1 1 -0.1 -0.1 0 0 -0.1/e estimate = exp; 
contrast 'Tow vs Ctrls' site 0 -0.1 0 0 -0.1 0 -0.1 0 -0.1 -0.1 -0.1 -0.1 0 -0.1 -0.1 1 0 -0.1/e estimate = exp; 
contrast 'Wal vs Ctrls' site 0 -0.1 0 0 -0.1 0 -0.1 0 -0.1 -0.1 -0.1 -0.1 0 -0.1 -0.1 0 1 -0.1/e estimate = exp; 
contrast 'Bra vs Ctrls' site 0 1 0 0 -0.111 0 -0.111 0 -0.111 -0.111 -0.111 -0.111 0 -0.111 -0.111 0 0 -0.111/e estimate = exp; 
contrast 'Eas vs Ctrls' site 0 -0.111 0 0 1 0 -0.111 0 -0.111 -0.111 -0.111 -0.111 0 -0.111 -0.111 0 0 -0.111/e estimate = exp; 
contrast 'Lew vs Ctrls' site 0 -0.111 0 0 -0.111 0 1 0 -0.111 -0.111 -0.111 -0.111 0 -0.111 -0.111 0 0 -0.111/e estimate = exp; 
contrast 'Nen vs Ctrls' site 0 -0.111 0 0 -0.111 0 -0.111 0 1 -0.111 -0.111 -0.111 0 -0.111 -0.111 0 0 -0.111/e estimate = exp; 
contrast 'Nor vs Ctrls' site 0 -0.111 0 0 -0.111 0 -0.111 0 -0.111 1 -0.111 -0.111 0 -0.111 -0.111 0 0 -0.111/e estimate = exp; 
contrast 'Por vs Ctrls' site 0 -0.111 0 0 -0.111 0 -0.111 0 -0.111 -0.111 1 -0.111 0 -0.111 -0.111 0 0 -0.111/e estimate = exp; 
contrast 'Sou vs Ctrls' site 0 -0.111 0 0 -0.111 0 -0.111 0 -0.111 -0.111 -0.111 1 0 -0.111 -0.111 0 0 -0.111/e estimate = exp; 
contrast 'Sto vs Ctrls' site 0 -0.111 0 0 -0.111 0 -0.111 0 -0.111 -0.111 -0.111 -0.111 0 1 -0.111 0 0 -0.111/e estimate = exp; 
contrast 'Sun vs Ctrls' site 0 -0.111 0 0 -0.111 0 -0.111 0 -0.111 -0.111 -0.111 -0.111 0 -0.111 1 0 0 -0.111/e estimate = exp; 
contrast 'Wor vs Ctrls' site 0 -0.111 0 0 -0.111 0 -0.111 0 -0.111 -0.111 -0.111 -0.111 0 -0.111 -0.111 0 0 1/e estimate = exp; 
run;

proc logistic data=sip.survey;
class site(ref='Waltham');
model THRIVEQ_CorrectAns (event='1') = site;
contrast 'War vs Ctrls' site 0 -0.1 0 0 -0.1 0 -0.1 0 -0.1 -0.1 -0.1 -0.1 0 -0.1 -0.1 0 1 -0.1/e estimate = exp;
run;