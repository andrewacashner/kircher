stack run .\input\Ps-150.xml .\test\winoutput.ly
lilypond -I ../../../lib/ly .\test\winoutput.ly
start .\winoutput.pdf
start .\winoutput.mid
