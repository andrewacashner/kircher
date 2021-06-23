<?php
# Web interface to arca using MEI output displayed via Verovio
# Andrew Cashner, 2021/06/22
#
# Run the arca to generate MEI music output, given a choice of input texts
# from a webform. Display the MEI using the Verovio web app.
#
# This requires the arca executable to be in the system path.
#
# Tested using local server: `php -S localhost:8000`
#
# Take the choice of input text from the form in index.html,
# select the correct XML input file from the array of file names,
# construct the input and output filenames, and then pass these as arguments
# to 'arca-exe'.

$inputText = $_POST['inputText'];

$inputFile = array(
    "Ps150"                  => "Ps-150",
    "Boethius-simple"        => "Boethius-Nubibus_atris",
    "Boethius-florid"        => "Boethius-Nubibus_atris-florid",
    "Ave_maris_stella"       => "Ave_maris_stella",
    "Veni_creator_Spiritus"  => "Veni_creator_Spiritus"
);

$fileTitle = array(
    "Ps150"                  => "Psalmi CL",
    "Boethius-simple"        => "Boethius, <cite>Nubibus atriis</cite>",
    "Boethius-florid"        => "Boethius, <cite>Nubibus atriis</cite>",
    "Ave_maris_stella"       => "<cite>Ave maris stella</cite>",
    "Veni_creator_Spiritus"  => "<cite>Veni creator Spiritus</cite>"
);

$fileBasename = $inputFile[$inputText];
$infileName   = "input/{$fileBasename}.xml";
$outfileName  = "build/{$fileBasename}.mei";
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
    </head>
    <body>
        <section>
            <h1><?=$title?></h1>
            <h2>Composed by the Arca musarithmica</h2>

            <p><a href="index.html">Return to the ark</a></p>

            <div class="panel-body">
                <div id="app" class="panel" 
                    style="border: 1px solid lightgray; min-height: 800px;">
                </div>
            </div>

            <script type="module">
                import 'https://www.verovio.org/javascript/app/verovio-app.js';

                // Create the app - here with an empty option object
                const app = new Verovio.App(document.getElementById("app"), {});

                // Load the MEI file
                fetch("<?=$outfileName?>")
                    .then(function(response) {
                        return response.text();
                    })
                    .then(function(text) {
                        app.loadData(text);
                    });
            </script>
        </section>
    </body>
</html>
