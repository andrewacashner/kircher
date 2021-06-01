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

$inputFiles = array(
    "Ps150"                  => "Ps-150.xml",
    "Boethius"               => "Boethius-Nubibus_atris.xml",
    "Ave_maris_stella"       => "Ave_maris_stella.xml",
    "Veni_creator_Spiritus"  => "Veni_creator_Spiritus.xml"
);

$fileBasename = $inputFiles[$inputText];
$infileName   = "input/{$fileBasename}";
$outfileName  = "build/{$fileBasename}.pdf";

exec("arca-exe {$infileName} {$outfileName}");

?>

<!DOCTYPE HTML>
<html>
    <head>
        <title>Arca musarithmica output</title>
        <meta charset="utf-8"/> 
    </head>
    <body>
        <section>
            <h1><?=$infileName?></h1>
            <h2>Composed by the Arca musarithmica</h2>

            <a href="<?=$outfileName?>">Download PDF score</a> 
        </section>
    </body>
</html>
