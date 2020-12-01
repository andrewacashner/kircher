$infile=$args[0]
$base=(Get-Item $infile).Basename
$ly=($base + '.ly')
$pdf=($base + '.pdf')
$mid=($base + '.mid')

stack run $infile $ly
lilypond --silent -I ../../../lib/ly $ly
start $pdf
start $mid

