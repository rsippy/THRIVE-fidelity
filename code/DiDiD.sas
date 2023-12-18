/************************************************/
/* Interaction model                            */
/* Rachel Sippy                                 */
/*                                              */
/* 1: DiDiD                                     */
/************************************************/

proc sort data=sip.fidel;
by intervention;
run;

proc means data=sip.fidel;
by intervention;
var effectiveness;
run;

/************************************************/
/* 1: DiDiD                                     */
/* overall, macro, meso, micro                  */
/************************************************/

proc mixed data=sip.fidel method=ML;
class high (ref='0') intervention (ref='Ctrl') time (ref='BL') site;
model total_fidelity = popd IMD compliance high intervention time high*intervention high*time intervention*time high*intervention*time /solution cl;
repeated / subject=site type=ar(1);
weight g1w;
estimate 'DiD at High' intervention*time 1 -1 -1 1 high*intervention*time 1 -1 -1 1 0 0 0 0/cl;
estimate 'DiD at Low' intervention*time 1 -1 -1 1 high*intervention*time 0 0 0 0 1 -1 -1 1/cl;
run;

proc mixed data=sip.fidel method=ML;
class high (ref='0') intervention (ref='Ctrl') time (ref='BL') site;
model macro = popd IMD compliance high intervention time high*intervention high*time intervention*time high*intervention*time /solution cl;
repeated / subject=site type=ar(1);
weight g1w;
estimate 'DiD at High' intervention*time 1 -1 -1 1 high*intervention*time 1 -1 -1 1 0 0 0 0/cl;
estimate 'DiD at Low' intervention*time 1 -1 -1 1 high*intervention*time 0 0 0 0 1 -1 -1 1/cl;
run;

proc mixed data=sip.fidel method=ML;
class high (ref='0') intervention (ref='Ctrl') time (ref='BL') site;
model meso = popd IMD compliance high intervention time high*intervention high*time intervention*time high*intervention*time /solution cl;
repeated / subject=site type=ar(1);
weight g1w;
estimate 'DiD at High' intervention*time 1 -1 -1 1 high*intervention*time 1 -1 -1 1 0 0 0 0/cl;
estimate 'DiD at Low' intervention*time 1 -1 -1 1 high*intervention*time 0 0 0 0 1 -1 -1 1/cl;
run;

proc mixed data=sip.fidel method=ML;
class high (ref='0') intervention (ref='Ctrl') time (ref='BL') site;
model micro = popd IMD compliance high intervention time high*intervention high*time intervention*time high*intervention*time /solution cl;
repeated / subject=site type=ar(1);
weight g1w;
estimate 'DiD at High' intervention*time 1 -1 -1 1 high*intervention*time 1 -1 -1 1 0 0 0 0/cl;
estimate 'DiD at Low' intervention*time 1 -1 -1 1 high*intervention*time 0 0 0 0 1 -1 -1 1/cl;
run;