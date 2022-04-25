#!/bin/bash

# settings from input

energy=${1:-1.0}
size=${2:-10}
configs=${3:-0}
dirs=${4:-0}
keep=${5:-1}

echo "RVL: making for E=" $energy "with M=" $size "," $dirs "directories and" $configs "samples"

# settings for files

binary=LSDdiag.IC

# settings for directories

currdir=`pwd`
jobdir=$currdir

jobdir="RVL-L31-E$energy"
mkdir -p $jobdir

jobname=$jobdir-$configs-$dirs
echo $jobname

jobfile=`printf "$jobname-M$size.sh"`
wlsfile=`printf "$jobname-M$size.wls"`
logfile=`printf "$jobname-M$size.log"`

# settings for parallel submission

cd $jobdir

cat > ${wlsfile} << EOD
#!/usr/bin/env wolframscript 
(* ::Package:: *)

(* ::Section:: *)
Print["(*Preliminaries*)"];

\$LOG=\$DBG=\$RVL=False;

LL="31";

MMlist={"$size"};

maxdirs=$dirs;
maxsamples=$configs;

If[maxdirs==0, maxdirs=Infinity];
If[maxsamples==0, maxsamples=Infinity];

Print[{maxdirs,maxsamples}];

maindir="$currdir";
Print[maindir];


(* ::Section:: *)
Print["(*Analysis*)"];


SetDirectory[maindir];


Do[
MM=MMlist[[iMM]];
Print["--- working on M=",MM];

SetDirectory[maindir];
alldirs=FileNames["L"<>LL<>"-"<>MM<>"-*/L"<>LL<>"_M"<>MM<>"*"];
Print[alldirs];

allavglist={};

starttimeD=AbsoluteTime[];
lendirs=Min[maxdirs,Length[alldirs]];
Do[
dirname=alldirs[[idir]];
Print["--- "<>dirname];
SetDirectory[maindir<>"/"<>dirname];
spos=Union[Flatten[StringPosition[dirname,"/"]]][[1]];
topdirname=StringTake[dirname,{1,spos-1}];
bottomdirname=StringTake[dirname,{spos+1,StringLength[dirname]}];

HubDis=ToExpression[StringTake[bottomdirname,{11,16}]]/100.;
RimDis=ToExpression[StringTake[bottomdirname,{20,25}]]/100.;
TarEng=StringTake[bottomdirname,-7];
If[StringContainsQ[TarEng,"-"],
TarEng=ToExpression[TarEng]/100.,
TarEng=ToExpression[StringDrop[TarEng,1]]/100.
];

Print[{Directory[],TarEng,HubDis,RimDis}];

allfiles=FileNames["EVal*.raw"];
allenglist=allrlist=allstatrlist=allseeds={};
ndone=0;

starttime=AbsoluteTime[];
lensamples=Min[maxsamples,Length[allfiles]];
Do[
filename=allfiles[[ifile]];
If[Length[allfiles]<=0, Continue[]];

If[\$LOG,PrintTemporary["--- --- "<>filename]];
rawdata=ReadList[filename,Number];
If[Length[rawdata]<=3, Continue[], ndone++];

AppendTo[allseeds,ToExpression[StringTake[StringDrop[filename,-4],-5]]];

(* drop the length item at the start of the list *)
len=rawdata[[1]];

eng=Sort[DeleteCases[rawdata, _Integer]];
AppendTo[allenglist,eng];

(* raw spacings, removing all negative/zero energies *)
tiny=10^-10;
engpos0=Sort[Select[eng,#>tiny&]];

(* remove flat band energies beyond E=0 *)
Switch[LL,
"31",engpos=engpos0,
"32",engpos=Sort[Select[engpos0,Abs[Abs[#]-1.0]>tiny&]],
"33",engpos=Sort[Select[engpos0,Abs[Abs[#]-Sqrt[2.0]]>tiny&]],
"34",engpos1=Sort[Select[engpos0,Abs[Abs[#]-(1.0+Sqrt[5.0])/2.0]>tiny&]];
engpos=Sort[Select[engpos1,Abs[Abs[#]-(-1.0+Sqrt[5.0])/2.0]>tiny&]]
];

(* compute spacings *)
eng1=Drop[engpos,1];eng0=Drop[engpos,-1];

delta=eng1-eng0;
If[\$DBG,Print[delta]];
delta=DeleteCases[delta,x_/;x==0.];
If[\$DBG,Print[delta]];

rlist={};
Do[
rn=Min[delta[[ind]],delta[[ind-1]]]/Max[delta[[ind]],delta[[ind-1]]];
If[\$DBG,Print[{ind,delta[[ind]],delta[[ind-1]],Min[delta[[ind]],delta[[ind-1]]],Max[delta[[ind]],delta[[ind-1]]],rn}]];
AppendTo[rlist,rn],
{ind,2,Length[delta]}
];
AppendTo[allrlist,rlist];
AppendTo[allstatrlist,{Mean[rlist],StandardDeviation[rlist]/Sqrt[Length[rlist]]}];

If[
Mod[ndone*100/lensamples,10]==0,
Print[{MM,N[idir/lendirs],ifile,N[ndone/lensamples],(AbsoluteTime[]-starttime),(AbsoluteTime[]-starttime)/N[ndone/lensamples]}]]
,{ifile,1,lensamples}
];

maxbins=200;

MeanEng=Mean[Flatten[allenglist]];
MedianEng=Median[Flatten[allenglist]];

(* r value *)
If[Length[Flatten[allrlist]]<=2,Continue[]];
 
(*binwidth=N[Min[0.1,Max[1/maxbins,maxbins/Length[Flatten[allrlist]]]]];
rawhistolist=HistogramList[Flatten[allrlist],{binwidth},"PDF"];bins=Drop[rawhistolist[[1]]-rawhistolist[[1,2]]/2,1];
histo=rawhistolist[[2]];
histolist=Transpose[{bins,histo}];

If[\$LOG,Print[{dirname,
Length[Flatten[allenglist]],
TarEng,MeanEng,MedianEng,HubDis,RimDis,
Length[Flatten[allrlist]],Mean[Flatten[allrlist]]
}]];*)

AppendTo[allavglist,{dirname,
Length[Flatten[allenglist]],Length[allenglist],
Length[Flatten[allrlist]],Length[allrlist],
Chop[TarEng],MeanEng,MedianEng,Chop[HubDis],Chop[RimDis],
Mean[Flatten[allrlist]],
StandardDeviation[Flatten[allrlist]]/Sqrt[Length[Flatten[allrlist]]],
Mean[Transpose[allstatrlist][[1]]],
StandardDeviation[Transpose[allstatrlist][[1]]]/Sqrt[Length[Transpose[allstatrlist][[1]]]]
}];

If[\$RVL,
rvlname=bottomdirname<>"_mE"<>StringDrop[ToString[PaddedForm[MeanEng,{6,3},NumberPadding->{"0","0"},NumberPoint->""]],1]<>"_"<>ToString[Min[allseeds]]<>"-"<>ToString[Max[allseeds]]<>"_"<>ToString[Length[allseeds]]<>"_rvl.txt";
PrintTemporary["     writing "<>rvlname];
Export[rvlname,Flatten[allrlist],"Table"]
];

If[
Mod[idir*100/lendirs,10]==0,
Print[{MM,N[idir/lendirs],(AbsoluteTime[]-starttimeD),(AbsoluteTime[]-starttimeD)/N[idir/lendirs]}]]
,{idir,1,lendirs}
];

(*Print[TableForm[allrmsdlist]];*)
SetDirectory[maindir];

(*Print[allavglist];*)

phasedata=Sort[Transpose[{Transpose[allavglist][[7]],Chop[Transpose[allavglist][[9]]],Transpose[allavglist][[11]],Transpose[allavglist][[12]],Transpose[allavglist][[13]],Transpose[allavglist][[14]]}]];

phasedata=Sort[phasedata,#1[[2]]<#2[[2]] &];

SetDirectory[maindir];
Export["$jobdir/Rstat_E"<>ToString[Floor[TarEng*10]]<>"_M"<>MM<>If[$dirs!=0 || $configs!=0,"-$configs-$dirs",""]<>"_rvl.txt",allavglist,"Table"];
Export["$jobdir/Rstat_E"<>ToString[Floor[TarEng*10]]<>"_M"<>MM<>If[$dirs!=0 || $configs!=0,"-$configs-$dirs",""]<>"_rvl-phase.txt",phasedata,"Table"];

(*Export["Rstat_E"<>ToString[Floor[TarEng*10]]<>"_M"<>MM<>"_rvl-phase.csv",phasedata,"CSV"];*)

,
{iMM,Length[MMlist]}
];

Print[phasedata];
Print["--- FINISHED!"];

EOD

cat > ${jobfile} << EOD
#!/bin/bash
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=2012
#SBATCH --time=48:00:00

module purge
module load Mathematica
module list

pwd
ls -al $jobdir/$wlsfile
$jobdir/$wlsfile

EOD

cd ..
chmod 755 ${jobdir}/${jobfile}
chmod 755 ${jobdir}/${wlsfile}
##(msub -q devel $jobdir/${jobfile}) # for queueing system
##(sbatch -q devel $jobdir/${jobfile}) # for queueing system
#sbatch ${jobdir}/${jobfile} # for queueing system
(source ${jobdir}/${jobfile} ) >& ${jobdir}/${logfile} & # for parallel shell execution
#source ${jobdir}/${jobfile} # for sequential shell execution

#echo "<return>"
sleep 1

cd ..



