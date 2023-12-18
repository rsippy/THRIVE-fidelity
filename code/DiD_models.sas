/************************************************/
/* DiD models                                   */
/* Rachel Sippy                                 */
/*                                              */
/* 1: Simple DiD                                */
/* 2: 4-group DiD                               */
/* 3: Alternative 4-group DiD models            */
/************************************************/

proc contents data=sip.fidel;
run;

/************************************************/
/* 1: Simple DiD                                */
/* overall, macro, meso, micro                  */
/************************************************/

proc mixed data=sip.fidel method=ML;
class intervention (ref='Ctrl') time (ref='BL') site;
model total_fidelity = intervention time intervention*time/solution cl;
repeated / subject=site type=ar(1);
estimate 'DiD' intervention*time 1 -1 -1 1;
run;

proc mixed data=sip.fidel method=ML;
class intervention (ref='Ctrl') time (ref='BL') site;
model macro = intervention time intervention*time/solution cl;
repeated / subject=site type=ar(1);
estimate 'DiD' intervention*time 1 -1 -1 1;
run;

proc mixed data=sip.fidel method=ML;
class intervention (ref='Ctrl') time (ref='BL') site;
model meso = intervention time intervention*time/solution cl;
repeated / subject=site type=ar(1);
estimate 'DiD' intervention*time 1 -1 -1 1;
run;

proc mixed data=sip.fidel method=ML;
class intervention (ref='Ctrl') time (ref='BL') site;
model micro = intervention time intervention*time/solution cl;
repeated / subject=site type=ar(1);
estimate 'DiD' intervention*time 1 -1 -1 1;
run;

/************************************************/
/* 2: 4-group DiD                               */
/* overall, macro, meso, micro                  */
/************************************************/

proc mixed data=sip.fidel method=ML;
class intervention (ref='Ctrl') time (ref='BL') site;
model total_fidelity = popd IMD compliance intervention time intervention*time/solution cl;
repeated / subject=site type=ar(1);
weight g1w;
estimate 'DiD' intervention*time 1 -1 -1 1;
run;

proc mixed data=sip.fidel method=ML;
class intervention (ref='Ctrl') time (ref='BL') site;
model macro = popd IMD compliance intervention time intervention*time/solution cl;
repeated / subject=site type=ar(1);
weight g1w;
estimate 'DiD' intervention*time 1 -1 -1 1;
run;

proc mixed data=sip.fidel method=ML;
class intervention (ref='Ctrl') time (ref='BL') site;
model meso = popd IMD compliance intervention time intervention*time/solution cl;
repeated / subject=site type=ar(1);
weight g1w;
estimate 'DiD' intervention*time 1 -1 -1 1;
run;

proc mixed data=sip.fidel method=ML;
class intervention (ref='Ctrl') time (ref='BL') site;
model micro = popd IMD compliance intervention time intervention*time/solution cl;
repeated / subject=site type=ar(1);
weight g1w;
estimate 'DiD' intervention*time 1 -1 -1 1;
run;

/************************************************/
/* 3: Alternative 4-group DiD                   */
/* overall, macro, meso, micro                  */
/* Gaussian GEE, Gamma GEE                      */
/* excluding Norfolk                            */
/************************************************/

/* Gaussian GEE model */
proc genmod data=sip.fidel;
class id intervention (ref='Ctrl') time (ref='BL') site;
model total_fidelity = popd IMD compliance intervention time intervention*time /dist=normal link=identity;
repeated subject=id / corr=ar;
weight g1w;
run;

proc genmod data=sip.fidel;
class id intervention (ref='Ctrl') time (ref='BL') site;
model macro = popd IMD compliance intervention time intervention*time /dist=normal link=identity;
repeated subject=id / corr=ar;
weight g1w;
run;

proc genmod data=sip.fidel;
class id intervention (ref='Ctrl') time (ref='BL') site;
model meso = popd IMD compliance intervention time intervention*time /dist=normal link=identity;
repeated subject=site / corr=ar;
weight g1w;
run;

proc genmod data=sip.fidel;
class id intervention (ref='Ctrl') time (ref='BL') site;
model micro = popd IMD compliance intervention time intervention*time /dist=normal link=identity;
repeated subject=site / corr=ar;
weight g1w;
run;

/* Gamma GEE model */
proc genmod data=sip.fidel;
class id intervention (ref='Ctrl') time (ref='BL') site;
model total_fidelity = popd IMD compliance intervention time intervention*time /dist=Gamma link=identity;
repeated subject=site / corr=ar;
weight g1w;
run;

proc genmod data=sip.fidel;
class id intervention (ref='Ctrl') time (ref='BL') site;
model macro = popd IMD compliance intervention time intervention*time /dist=Gamma link=identity;
repeated subject=site / corr=ar;
weight g1w;
run;

proc genmod data=sip.fidel;
class id intervention (ref='Ctrl') time (ref='BL') site;
model meso = popd IMD compliance intervention time intervention*time /dist=Gamma link=identity;
repeated subject=site / corr=ar;
weight g1w;
run;

proc genmod data=sip.fidel;
class id intervention (ref='Ctrl') time (ref='BL') site;
model micro = popd IMD compliance intervention time intervention*time /dist=Gamma link=identity maxiter=1000;
repeated subject=site / corr=ar;
weight g1w;
run;

/* exclude Norfolk */
data nonor;
set sip.fidel;
if site='Norfolk' then IMD=.;
run;

/* adjusted 4-group weights DiD un=351.2, ar=349.5*/
proc mixed data=nonor method=ML;
class intervention (ref='Ctrl') time (ref='BL') site;
model total_fidelity = popd IMD compliance intervention time intervention*time/solution cl;
repeated / subject=site type=ar(1);
weight g1w;
estimate 'DiD' intervention*time 1 -1 -1 1;
run;

proc mixed data=nonor method=ML;
class intervention (ref='Ctrl') time (ref='BL') site;
model macro = popd IMD compliance intervention time intervention*time/solution cl;
repeated / subject=site type=ar(1);
weight g1w;
estimate 'DiD' intervention*time 1 -1 -1 1;
run;

proc mixed data=nonor method=ML;
class intervention (ref='Ctrl') time (ref='BL') site;
model meso = popd IMD compliance intervention time intervention*time/solution cl;
repeated / subject=site type=ar(1);
weight g1w;
estimate 'DiD' intervention*time 1 -1 -1 1;
run;

proc mixed data=nonor method=ML;
class intervention (ref='Ctrl') time (ref='BL') site;
model micro = popd IMD compliance intervention time intervention*time/solution cl;
repeated / subject=site type=ar(1);
weight g1w;
estimate 'DiD' intervention*time 1 -1 -1 1;
run;