<?php
# First working web interface to arca
# Andrew Cashner, 2021/06/01
#
# Run the arca to generate PDF music output, given a choice of input texts
# from a webform.
#
# This requires the arca executable to be in the system path.
# Lilypond must also be on the path, and the custom Lilypond library called
# from within arca (currently $HOME/lib/ly).
#
# Tested using local server: `php -S localhost:8000`
#
# Take the choice of input text from the form in index.html,
# select the correct XML input file from the array of file names,
# construct the input and output filenames, and then pass these as arguments
# to 'arca-exe'.

$inputText = $_POST['inputText'];

$inputFile = array(
    "Ps150"                  => "Ps-150.xml",
    "Boethius"               => "Boethius-Nubibus_atris.xml",
    "Ave_maris_stella"       => "Ave_maris_stella.xml",
    "Veni_creator_Spiritus"  => "Veni_creator_Spiritus.xml"
);

$fileTitle = array(
    "Ps150"                  => "Psalmi CL",
    "Boethius"               => "Boethius, <cite>Nubibus atriis</cite>",
    "Ave_maris_stella"       => "<cite>Ave maris stella</cite>",
    "Veni_creator_Spiritus"  => "<cite>Veni creator Spiritus</cite>"
);

$fileBasename = $inputFile[$inputText];
$infileName   = "input/{$fileBasename}";
$outfileName  = "build/{$fileBasename}.pdf";
$MIDIoutfile  = "build/{$fileBasename}.midi";
$title        = $fileTitle[$inputText];

# Run arca (XML input, Lilypond output);
# arca in turn runs Lilypond (PDF and MIDI output)
exec("arca-exe {$infileName} {$outfileName}");

?>

<!DOCTYPE HTML>
<html>
    <head>
        <title>Arca musarithmica output</title>
        <meta charset="utf-8"/> 
        <script type="text/javascript" 
                src="//www.midijs.net/lib/midi.js"></script>
    </head>
    <body>
        <section>
            <h1><?=$title?></h1>
            <h2>Composed by the Arca musarithmica</h2>

            <p>
                <button onclick="MIDIjs.play('<?=$MIDIoutfile?>');">
                    Listen (MIDI)
                </button>
                <button onclick="MIDIjs.stop()">Stop playback</button>
            </p>

            <object data="<?=$outfileName?>" type="application/pdf"
                    width=720 height=930>
                <a href="<?=$outfileName?>">Download PDF score</a>
            </object>
            </div>
        </section>
    </body>
</html>
