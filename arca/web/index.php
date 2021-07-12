<?php
# Web interface to arca using MEI output displayed via Verovio
# Andrew Cashner, 2021/07/12
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
# 
# For DIY files, where users set the input parameters instead of them being
# preset in the XML input, we have to subsitute the user's input choices
# instead of the placeholders in the XML input file.

# = GET INPUT
# Input values from HTML form
$inputText  = $_POST['inputText'];
$inputType  = $_POST['inputType'];
$inputStyle = $_POST['style'];
$inputMode  = $_POST['mode'];
$inputMeter = $_POST['musicMeter'];

# File basenames for input and output
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

# File titles to be used in the generated HTML output
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

$title        = "$fileTitle[$inputText]";

# Style selection to be inserted into XML input (maps to Haskell Style data type)
$style = array(
    "simple" => "Simple"
  , "florid" => "Florid"
);


# = SET UP INPUT AND OUTPUT FILES

# For DIY files, we need the input filename plus temp and final output filenames.
# For prepared files, we choose the relevant input directory (prepared/simple
# or prepared/florid), and make an output filename that includes the style
# (better for debugging).
$fileBasename = "$baseName[$inputText]";

if ($inputType == "DIY") {
    $infileName      = "input/text/$fileBasename.xml";
    $outfileBasename = "$fileBasename-diy";
    $tmpfileName     = "build/$outfileBasename.tmp";
    $outfileName     = "build/$outfileBasename.mei";
} else {
    $infileName      = "input/prepared/$inputStyle/$fileBasename.xml";
    $outfileBasename = "$baseName[$inputText]-$inputStyle";
    $outfileName     = "build/$outfileBasename.mei";
}


# = FOR DIY FILES: INSERT USER PARAMETERS INTO INPUT FILE
# For DIY files, users set their own parameters for style, meter, and mode.
# The input files have placeholder strings for these XML attributes, so we
# read in the input file, replace the placeholders with the values taken from
# the HTML form input, and write it back out to a temp file.
# We use the temp file for input instead of the original input file.
if ($inputType == "DIY") {
    
    $fileString = file_get_contents($infileName);
    
    $fileString = str_replace(
        array("{style}", "{musicMeter}", "{mode}"),
        array($style[$inputStyle], $inputMeter, $inputMode),
        $fileString);

    file_put_contents($tmpfileName, $fileString);
    $infileName = $tmpfileName;
    usleep(2000000);
}

# = RUN THE ARK
# Run arca to produce MEI output which will be rendered by the Verovio
# web app (called in the Javascript below).
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
