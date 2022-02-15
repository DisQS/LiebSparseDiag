#!/bin/bash

# settings from input

size=${1:-10}
seed=${2:-1}
config=${3:-100}
keep=${4:-1}

echo "LSD: making for M=" $size "with starting seed=" $seed "and" $config "samples"

# settings for files

binary=LSDdiag.IC

# settings for directories

currdir=`pwd`
jobdir=$currdir

binarydir=$currdir/../EXE
[ -d $binarydir ] || mkdir $binarydir

[ -e $binarydir/$binary ] || cp $currdir/../src/$binary $binarydir

#for size in 12 14 #16 18
#do

jobdir="LSD-M$size"

[ -d $jobdir ] || mkdir $jobdir
  # mkdir -p $jobdir
    
cd $jobdir


#for disorder in 3.0 2.0 1.0
#do
#    energy=4.0
		
for disorder in 15.0 14.0 13.0 12.0 11.0 10.0 9.0 8.0 7.0 6.0 5.0 4.0 3.0 2.0 1.0 #30.0 29.0 28.0 27.0 26.0 25.0 24.0 23.0 22.0 21.0 20.0 19.0 18.0 17.0 16.0  # 
do

   #energy=`echo "$disorder + 6.0"| bc`
   #energy=`echo "$disorder/2.0 + 3.0"| bc`
    
    if [ $(echo "$disorder < 4.0" | bc) = 1 ] ; then
	energy=`echo "4.0"`
    else
	energy=`echo "$disorder/2.0 + 3.0"| bc`
    fi
    
    echo "--- hDis=" $disorder ", Min_Eng=" $energy

    jobname="LSD-M$size-hD$disorder"
    echo $jobname
    
    jobfile=`printf "$jobname.sh"`
    logfile=`printf "$jobname.log"`

    inpfile=LSDdiag-$disorder.inp

    echo "binarydir=" $binarydir " jobdir=" $jobdir 

    # settings for parallel submission

    #cd $jobdir
	
    cat > ${jobfile} << EOD
#!/bin/bash
#PBS -l nodes=${nodes}:ppn=16
#PBS -l pmem=${memory}
#PBS -l walltime=04:00:00

#       The jobname
#PBS -N ${jobname}


##!/bin/bash
##SBATCH --ntasks=1
##SBATCH --cpus-per-task=1
##SBATCH --mem-per-cpu=2012
##SBATCH --time=48:00:00

#module purge
#module load intel
#module load DMTCP/2.5.2

for iseed in {1..$config..1}
do

myseed=\$(( $seed + \$iseed - 1))
echo "--- working on config" \$iseed "with seed" \$myseed

# create the input file
echo "create the input file"

touch $inpfile

echo "ISeed         = \$myseed       ">  $inpfile 
echo "NConfig       = 1        ">>  $inpfile #
echo "Dim           = 3            ">>  $inpfile 
echo "Nx            = 1            ">>  $inpfile 
echo "IBCFlag       = 1             ">>  $inpfile 
echo "IRNGFlag      = 0             ">>  $inpfile 
echo "IKeepFlag     = $keep      ">>  $inpfile 
echo "IWriteFlag    = 2       ">>  $inpfile 
echo "IStateFlag    = 0       ">>  $inpfile #
echo "Width0        = $size       ">>  $inpfile 
echo "Width1        = $size       ">>  $inpfile 
echo "dWidth        = 2          ">>  $inpfile 
echo "HubDis0       = $disorder      ">>  $inpfile 
echo "HubDis1       = $disorder           ">>  $inpfile 
echo "dHubDis       = 1.0           ">>  $inpfile 
echo "RimDis0       = 0.0          ">>  $inpfile 
echo "Energy0       = 1.0         ">>  $inpfile 
echo "Energy1       = $energy       ">>  $inpfile 
echo "dEnergy       = 1.0        ">>  $inpfile 
echo "NEvals        = 100           ">>  $inpfile 
echo "Memory        = 100          ">>  $inpfile 

cat $inpfile

$binarydir/$binary <$inpfile >& ${logfile}

done

wait
#exit 0

EOD

chmod 755 ${jobfile}
#(msub -q devel $jobdir/${jobfile}) # for queueing system
#(sbatch -q devel $jobdir/${jobfile}) # for queueing system
#sbatch ${jobfile} # for queueing system
(source ${jobfile} ) &
#(source $jobdir/${jobfile} ) >& $jobdir/${logfile} & # for parallel shell execution
#source ${jobfile} #>& ${logfile} # for sequential shell execution

#echo "<return>"
sleep 1

done

cd ..

#done
