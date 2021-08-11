#!/bin/bash

# settings from input

size=${1:-10}
seed=${2:-1}
config=${3:-2}
keep=${4:-1}

echo "LSD: making for M=" $size "with starting seed=" $seed "and" $config "samples"

# settings for files

binary=LSDdiag.IC

# settings for directories

currdir=`pwd`
jobdir=$currdir

binarydir=$HOME/Projects/LiebSparseDiag/EXE

for disorder in 10.0 8.0 6.0 4.0 2.0
do

energy=$disorder

echo "--- hDis=" $disorder ", Min_Eng=" $energy

jobname="LSD-$size-hD$disorder"
echo $jobname

jobfile=`printf "$jobname.sh"`
logfile=`printf "$jobname.log"`
jobdir="LSD-$size"
mkdir -p $jobdir

inpfile=LSDdiag-$disorder.inp

echo "binarydir=" $binarydir " jobdir=" $jobdir 

# settings for parallel submission

cd $jobdir

cat > ${jobfile} << EOD
#!/bin/bash
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=2012
#SBATCH --time=48:00:00

module purge
module load intel
#module load DMTCP/2.5.2

for iseed in {1..$config..1}
do

myseed=\$(( $seed + \$iseed - 1))
echo "--- working on config" \$iseed "with seed" \$myseed

# create the input file
echo create the input file
touch $inpfile

echo "ISeed         = \$myseed       ">  $inpfile #
echo "NConfig       = 1        ">>  $inpfile #
echo "Dim           = 3            ">>  $inpfile #
echo "Nx            = 1            ">>  $inpfile #
echo "IBCFlag       = 1             ">>  $inpfile #
echo "IRNGFlag      = 0             ">>  $inpfile #
echo "IKeepFlag     = $keep      ">>  $inpfile #
echo "IWriteFlag    = 2       ">>  $inpfile #
echo "Width0        = $size       ">>  $inpfile #
echo "Width1        = $size       ">>  $inpfile #
echo "dWidth        = 2          ">>  $inpfile #
echo "HubDis0       = $disorder      ">>  $inpfile #
echo "HubDis1       = $disorder           ">>  $inpfile #
echo "dHubDis       = 1.0           ">>  $inpfile #
echo "RimDis0       = $disorder      ">>  $inpfile #
echo "Energy0       = -$energy       ">>  $inpfile #
echo "Energy1       = $energy       ">>  $inpfile #
echo "dEnergy       = 1.0       ">>  $inpfile #
echo "NEvals        = 100           ">>  $inpfile #
echo "Memory        = 100          ">>  $inpfile #

cat $inpfile

$binarydir/$binary <$inpfile

done

exit 0

EOD

chmod 755 ${jobfile}
#(msub -q devel $jobdir/${jobfile}) # for queueing system
#(sbatch -q devel $jobdir/${jobfile}) # for queueing system
sbatch ${jobfile} # for queueing system
#(source $jobdir/${jobfile} ) >& $jobdir/${logfile} & # for parallel shell execution
#source ${jobfile} #>& ${logfile} # for sequential shell execution

#echo "<return>"
sleep 1

cd ..

done
