<?php
# Web interface to arca using MEI output displayed via Verovio
# Andrew Cashner, 2021/06/22--07/08
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

$inputText  = $_POST['inputText'];
$inputType  = $_POST['inputType'];
$inputStyle = $_POST['style'];
$inputMode  = $_POST['mode'];
$inputMeter = $_POST['musicMeter'];

$baseName = array(
      "Abide"            => "Abide_with_Me"
    , "Ave_maris_stella" => "Ave_maris_stella"
    , "Ave_Regina"       => "Ave_Regina_Angelorum"
    , "Boethius"         => "Boethius-Nubibus_atris"
    , "Ps150"            => "Ps-150"
    , "Horace"           => "Horace-Maecenas_atavis_edite_regibus"
    , "Iste_confessor"   => "Iste_confessor_Domini"
    , "Stephanus"        => "Stephanus-O_ter_quaterque_felix_Cicada"
    , "Veni_creator"     => "Veni_creator_Spiritus"
);

$fileTitle = array(
      "Abide_with_Me"    => "Abide with Me (Decasyllabic meter)"
    , "Ave_maris_stella" => "Ave maris stella (Iambic Euripidaeic meter)"
    , "Ave_Regina"       => "Ave Regina Angelorum (Iambic Enneasyllabic meter)"
    , "Boethius"         => "Boethius, Nubibus atriis (Adonic meter)"
    , "Ps150"            => "Psalmi CL (Irregular meter/Prose)"
    , "Horace"           => "Horace, Maecenas atavis edite regibus (Dodecasyllabic meter)"
    , "Iste_confessor"   => "Iste confessor Domini (Sapphic meter)"
    , "Stephanus"        => "Stephanus, O ter quaterque felix Cicada (Anacreontic meter)"
    , "Veni_creator"     => "Veni creator Spiritus (Iambic Archilochic meter)"
);

$style = array(
    "simple" => "Simple"
  , "florid" => "Florid"
);

$fileBasename = "{$baseName[$inputText]}";

if ($inputType == "DIY") {
    $infileName = "input/text/$fileBasename.xml";
} else {
    $infileName = "input/prepared/$inputStyle/$fileBasename.xml";
}

$outfileName  = "build/{$fileBasename}.mei";
$title        = "{$fileTitle[$inputText]}";


# Run arca (XML input, Lilypond output);
# arca in turn runs Lilypond (PDF and MIDI output)

if ($inputType == "DIY") {
    $fileString = file_get_contents("$infileName");
    $fileString = str_replace('{style}', $style[$inputStyle], $fileString);
    $fileString = str_replace('{musicMeter}', $inputMeter, $fileString);
    $fileString = str_replace('{mode}', $inputMode, $fileString);

    exec("echo '{$fileString}' | arca-exe - {$outfileName}");
} else {
    exec("arca-exe {$infileName} {$outfileName}");
}
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
