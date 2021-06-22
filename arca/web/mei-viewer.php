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
    "Boethius"               => "Boethius-Nubibus_atris",
    "Ave_maris_stella"       => "Ave_maris_stella",
    "Veni_creator_Spiritus"  => "Veni_creator_Spiritus"
);

$fileTitle = array(
    "Ps150"                  => "Psalmi CL",
    "Boethius"               => "Boethius, <cite>Nubibus atriis</cite>",
    "Ave_maris_stella"       => "<cite>Ave maris stella</cite>",
    "Veni_creator_Spiritus"  => "<cite>Veni creator Spiritus</cite>"
);

$fileBasename = $inputFile[$inputText];
$infileName   = "input/{$fileBasename}.xml";
$outfileName  = "build/{$fileBasename}.mei";
$title        = "$fileTitle[$inputText] (Arca musarithmica)";

# Run arca (XML input, Lilypond output);
# arca in turn runs Lilypond (PDF and MIDI output)
exec("arca-exe {$infileName} {$outfileName}");

?>


<html lang="en" xml:lang="en" xmlns:svg="http://www.w3.org/2000/svg" xmlns="http://www.w3.org/1999/xhtml">

<head>
    <title><?=$title?></title>
    <link rel="icon" type="image/x-icon" href="https://www.verovio.org/favicon.ico" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <!-- Latest compiled and minified CSS -->
    <link rel="stylesheet" href="https://www.verovio.org/css/bootstrap/css/bootstrap.min.css" type="text/css" />
    <!-- Optional theme -->
    <link rel="stylesheet" href="https://www.verovio.org/css/bootstrap/css/bootstrap-theme.min.css" type="text/css" />

    <!-- syntax highlighting CSS -->
    <link rel="stylesheet" href="https://www.verovio.org/css/syntax.css" />

    <!-- Custom CSS -->
    <link rel="stylesheet" href="https://www.verovio.org/css/verovio.css" />
    <link rel="stylesheet" href="https://www.verovio.org/css/verovio-sidebar.css" />
    <link rel="stylesheet" href="https://www.verovio.org/css/midiplayer.css" /> 

    <!-- jQuery (necessary for Bootstrap's JavaScript plugins) -->
    <script src="https://www.verovio.org/javascript/jquery.min.js" type="text/javascript"></script>
    <script src="https://www.verovio.org/javascript/jquery.touchSwipe.min.js" type="text/javascript"></script>
    <script src="https://www.verovio.org/javascript/bootstrap.min.js" type="text/javascript"></script>
    <script src="https://www.verovio.org/javascript/bootstrap-contextmenu.js" type="text/javascript"></script>
    <script src="https://www.verovio.org/javascript/FileSaver.min.js" type="text/javascript"></script>
    <script src="https://www.verovio.org/javascript/d3.min.js" type="text/javascript"></script>
    <script src="https://www.verovio.org/javascript/jquery.cookie.js" type="text/javascript"></script>
    <script src="https://www.verovio.org/javascript/jsonform.js" type="text/javascript"></script>
    <!-- verovio -->
    <!-- use this to increase the memory for verovio -->
    <!--<script src="https://www.verovio.org/javascript/develop/verovio-memory.js" type="text/javascript"></script>-->
    
    <!--<script src="https://www.verovio.org/javascript/develop/verovio-memory.js" type="text/javascript"></script>-->
    <script src="https://www.verovio.org/javascript/develop/verovio-toolkit.js" type="text/javascript"></script>
    
    <!-- midi-player -->
    
    <script src="https://www.verovio.org/javascript/midi-player/wildwebmidi.js" type="text/javascript"></script>
    <script src="https://www.verovio.org/javascript/midi-player/midiplayer.js" type="text/javascript"></script>
    
    <!-- midi.js for playback in the editor -->
    
    <!-- pdf kit -->
    
    <script src="https://www.verovio.org/javascript/pdfkit/blobstream.js" type="text/javascript"></script>
    <script src="https://www.verovio.org/javascript/pdfkit/pdfkit.js" type="text/javascript"></script>
    <script src="https://www.verovio.org/javascript/pdfkit/source.js" type="text/javascript"></script>
    <script src="https://www.verovio.org/javascript/pdfkit/vrv-ttf.js" type="text/javascript"></script>
    

</head>

<body>

    <div id="overlay">
        Composing ...
    </div>

    <div class="row-offcanvas row-offcanvas-right">

        <script>
//<![CDATA[
    var vrvToolkit = new verovio.toolkit();
    var page = 1;
    var zoom = 40;
    var pageHeight = 2970;
    var pageWidth = 2100;
    var swipe_pages = false;
    var format = 'mei';
    var outputFilename = 'output.mei'
    var ids = [];
    var pdfFormat = "A4";
    var pdfOrientation = "portrait";
    var savedOptions = undefined;
    var customOptions = undefined;
    var target = "";
    var disabledOptions = ["adjustPageHeight", "breaks", "landscape", "pageHeight", "pageWidth", "mmOutput", "footer"];

    // reload cookies
    if ( $.cookie('zoom') ) zoom = $.cookie('zoom');
    if ( $.cookie('pdfFormat') ) pdfFormat = $.cookie('pdfFormat');
    if ( $.cookie('pdfOrientation') ) pdfOrientation = $.cookie('pdfOrientation');
    
    if (localStorage['customOptions']) {
        customOptions = JSON.parse(localStorage['customOptions']);
    }

    function calc_page_height() {
        return ($(document).height() - $( "#navbar" ).height() - 4) * 100 / zoom;
    }

    function calc_page_width() {
        return ($(".row-offcanvas").width()) * 100 / zoom ; // - $( "#sidbar" ).width();
    }

    function set_options( ) {
        pageHeight = calc_page_height();
        pageWidth = calc_page_width();
        options = {
                    adjustPageHeight: "true",
                    breaks: "auto",
                    inputFrom: format,
                    mmOutput: "false",
                    footer: "none",
                    pageHeight: pageHeight,
                    pageWidth: pageWidth,
                    scale: zoom
                };
          
        if (customOptions !== undefined) {
            localStorage['customOptions'] = JSON.stringify(customOptions);
            var mergedOptions = {};
            for(var key in customOptions) mergedOptions[key] = customOptions[key];
            for(var key in options) mergedOptions[key] = options[key];
            options = mergedOptions;
        }
        

        //console.log( options );
        vrvToolkit.setOptions( options );
        //vrvToolkit.setOptions( mergedOptions );
    }

    function upload_file() {

        format = 'mei';
        $("#mei_download").hide();
        $('#submit-btn').popover('hide');
        var f = $("#mei_files").prop('files')[0];
        var reader = new FileReader();

        // Closure to capture the file information.
        reader.onload = (function(theFile) {
            outputFilename = theFile.name;
            return function(e) {
                $('.row-offcanvas').toggleClass('active');
                load_data(e.target.result);
            };
        })(f);

        // Read in the image file as a data URL.
        reader.readAsText(f);
    };

    function load_data(data) {
        set_options();

        console.time("loading");
        try {
            vrvToolkit.loadData(data);

            if (vrvToolkit.getPageCount() == 0) {
                $("#overlay").hide().css("cursor", "auto");
                log = vrvToolkit.getLog();
                $('#err').text(log).html();
                $('#errorDialog').modal();
            }
            else {
                $("#total_text").html(vrvToolkit.getPageCount());
                page = 1;
                var pageTarget = 0;
                if (target != "") {
                    pageTarget = vrvToolkit.getPageWithElement(target.substr(1));
                    if (pageTarget != 0) {
                        page = pageTarget;
                    }
                }
                load_page();
                if (pageTarget != 0) {
                    $(target).attr("fill", "#d00").attr("stroke", "#d00");
                }
                $("#play-button").show();
                $("#pdf-button").show();
                $("#options-button").show();
            }
        }
        catch(err) {
            $("#overlay").hide().css("cursor", "auto");
            $('#err').html(err);
            $('#errorDialog').modal();
        }
        console.timeEnd("loading");
    }

    function load_page() {
        $("#overlay").hide().css("cursor", "auto");
        $("#jump_text").val(page);

        svg = vrvToolkit.renderToSVG(page, {});
        $("#svg_output").html(svg);

        adjust_page_height();
    };

    function next_page() {
        if (page >= vrvToolkit.getPageCount()) {
            return;
        }

        page = page + 1;
        load_page();
    };

    function prev_page() {
        if (page <= 1) {
            return;
        }

        page = page - 1;
        load_page();
    };

    function first_page() {
        page = 1;
        load_page();
    };

    function last_page() {
        page = vrvToolkit.getPageCount();
        load_page();
    };

    function apply_zoom() {
        console.log("apply zoom");
        // make sure we have loaded a file
        if (vrvToolkit.getPageCount() == 0) return;
        
        $.cookie('zoom', zoom, { expires: 30 });
        set_options();
        var measure = 0;
        if (page != 1) {
            measure = $("#svg_output .measure").attr("id");
        }

        vrvToolkit.redoLayout();

        $("#total_text").html(vrvToolkit.getPageCount());
        page = 1;
        if (measure != 0) {
            page = vrvToolkit.getPageWithElement(measure);
        }
        load_page();
    }

    function zoom_out() {
        if (zoom < 20) {
            return;
        }

        zoom = zoom / 2;
        apply_zoom();
    }

    function zoom_in() {
        if (zoom > 80) {
            return;
        }

        zoom = zoom * 2;
        apply_zoom();
    }

    function do_page_enter(e) {
        key = e.keyCode || e.which;
        if (key == 13) {

            text = $("#jump_text").val();

            if (text <= vrvToolkit.getPageCount() && text > 0) {
                page = Number(text);
                load_page();
            } else {
                $("#jump_text").val(page);
            }

        }
    }

    function do_zoom_enter(e) {
        key = e.keyCode || e.which;
        if (key == 13) {
            text = $("#zoom_text").val();
            zoom_val = Number(text.replace("%", ""));
            if (zoom_val < 10) zoom_val = 10;
            else if (zoom_val > 160) zoom_val = 160;
            zoom = zoom_val;
            apply_zoom();
        }
    }

    function adjust_page_height() {
        // adjust the height of the panel
        if ( $('#svg_panel svg') ) {
            zoomed_height = pageHeight * zoom / 100;
            if ( zoomed_height < $('#svg_panel svg').height() ) {
                zoomed_height = $('#svg_panel svg').height();
            }
            $('#svg_output').height( zoomed_height ); // slighly more for making sure we have no scroll bar
            //$('#svg_panel svg').height(pageHeight * zoom / 100 );
            //$('#svg_panel svg').width(pageWidth * zoom / 100 );
        }

        // also update the zoom control
        $("#zoom_text").val(zoom + "%");

        // enable the swipe (or not)
        enable_swipe( ( $('#svg_panel svg') && ( $('#svg_panel svg').width() <= $('#svg_panel').width() ) ) );
    }

    function swipe_prev(event, direction, distance, duration, fingerCount) {
          prev_page();
     }

     function swipe_next(event, direction, distance, duration, fingerCount) {
         next_page();
     }

     function swipe_zoom_in(event, target) {
         zoom_in();
     }

     function swipe_zoom_out(event, target) {
         zoom_out();
     }

    function enable_swipe( pages ) {
        if ( pages && !swipe_pages ) {
            $("#svg_output").swipe( "destroy" );
            $("#svg_output").swipe( { swipeLeft: swipe_next, swipeRight: swipe_prev, tap: swipe_zoom_in, doubleTap: swipe_zoom_out, allowPageScroll:"auto"} );
            swipe_pages = true;
        }
        // zoom only
        else if ( !pages && swipe_pages ) {
            $("#svg_output").swipe( "destroy" );
            $("#svg_output").swipe( { tap: swipe_zoom_in, doubleTap: swipe_zoom_out, allowPageScroll:"auto"} );
            swipe_pages = false;
        }
    }

    function getParameterByName(name) {
        var match = RegExp('[?&]' + name + '=([^&]*)').exec(window.location.search);
        return match && decodeURIComponent(match[1].replace(/\+/g, ' '));
    }

    function play_midi() {
        var base64midi = vrvToolkit.renderToMIDI();
        var song = 'data:audio/midi;base64,' + base64midi;
        $("#player").show();
        $("#play-button").hide();
        $("#player").midiPlayer.play(song);
    }

    var midiUpdate = function(time) {
        var vrvTime = Math.max(0, time - 400);
        var elementsattime = vrvToolkit.getElementsAtTime(vrvTime);
        if (elementsattime.page > 0) {
            if (elementsattime.page != page) {
                page = elementsattime.page;
                load_page();
            }
            if ((elementsattime.notes.length > 0) && (ids != elementsattime.notes)) {
                ids.forEach(function(noteid) {
                    if ($.inArray(noteid, elementsattime.notes) == -1) {
                        $("#" + noteid ).attr("fill", "#000");
                        $("#" + noteid ).attr("stroke", "#000");
                        //$("#" + noteid ).removeClassSVG("highlighted");
                    }
                });
                ids = elementsattime.notes;
                ids.forEach(function(noteid) {
                    if ($.inArray(noteid, elementsattime.notes) != -1) {
                    //console.log(noteid);
                        $("#" + noteid ).attr("fill", "#c00");
                        $("#" + noteid ).attr("stroke", "#c00");;
                        //$("#" + noteid ).addClassSVG("highlighted");
                    }
                });
            }
        }
    }

    var midiStop = function() {
        ids.forEach(function(noteid) {
            $("#" + noteid ).attr("fill", "#000");
            $("#" + noteid ).attr("stroke", "#000");
            //$("#" + noteid ).removeClassSVG("highlighted");
        });
        $("#player").hide();
        $("#play-button").show();
    }

    $.fn.addClassSVG = function(className){
        $(this).attr('class', function(index, existingClassNames) {
            return existingClassNames + ' ' + className;
        });
        return this;
    };

    $.fn.removeClassSVG = function(className){
        $(this).attr('class', function(index, existingClassNames) {
            //var re = new RegExp(className, 'g');
            //return existingClassNames.replace(re, '');
        });
        return this;
    };
    
    function generate_pdf() {
        
        var pdfFormat = $("#pdfFormat").val();
        var pdfSize = [2100, 2970];
        if (pdfFormat == "letter") pdfSize = [2159, 2794];
        else if (pdfFormat == "B4") pdfSize = [2500, 3530];
        
        var pdfOrientation = $("#pdfOrientation").val();
        var pdfLandscape = pdfOrientation == 'landscape';
        var pdfHeight = pdfLandscape ? pdfSize[0] : pdfSize[1];
        var pdfWidth = pdfLandscape ? pdfSize[1] : pdfSize[0];
        
        var fontCallback = function(family, bold, italic, fontOptions) {
            if (family == "VerovioText") {
                return family;
            }
            if (family.match(/(?:^|,)\s*sans-serif\s*$/) || true) {
                if (bold && italic) {return 'Times-BoldItalic';}
                if (bold && !italic) {return 'Times-Bold';}
                if (!bold && italic) {return 'Times-Italic';}
                if (!bold && !italic) {return 'Times-Roman';}
            }
        };
    
        var options = {};
        options.fontCallback = fontCallback;
        
        var doc = new PDFDocument({useCSS: true, compress: true, autoFirstPage: false, layout: pdfOrientation}); 
        var stream = doc.pipe(blobStream());
        
        stream.on('finish', function() {
            var blob = stream.toBlob('application/pdf');
            var pdfFilename = outputFilename.replace(/\.[^\.]+$/, '.pdf');
            saveAs(blob, pdfFilename);
        });
        
        var buffer = Uint8Array.from(atob(vrvTTF), c => c.charCodeAt(0));
        doc.registerFont('VerovioText',buffer);  
        
        pdfOptions = {
                    adjustPageHeight: false,
                    breaks: "auto",
                    mmOutput: true,
                    footer: "auto",
                    pageHeight: pdfHeight,
                    pageWidth: pdfWidth,
                    scale: 100
        }
        
        vrvToolkit.setOptions(pdfOptions);
        vrvToolkit.redoLayout();
        for (i = 0; i < vrvToolkit.getPageCount(); i++) {
            doc.addPage({size: pdfFormat, layout: pdfOrientation});
            SVGtoPDF(doc, vrvToolkit.renderToSVG(i + 1, {}), 0, 0, options);
        }        
        
        // Reset the options
        set_options();
        vrvToolkit.redoLayout();
        
        doc.end();
    }
    
    function non_default_options (options) {
        var defaultOptions = vrvToolkit.getOptions(true);
        var nonDefaultOptions = {};
        for(var key in defaultOptions) {
            if (options[key] && (defaultOptions[key] != options[key])) {
                nonDefaultOptions[key] = options[key];
            }
        }
        return nonDefaultOptions;
    }

//]]>
</script>

<!-- modal -->
<div class="modal fade" id="errorDialog" tabindex="-1" role="dialog">
    <div class="modal-dialog">
        <div class="modal-content">
            <div class="modal-header">
                <button type="button" class="close" data-dismiss="modal" aria-label="Close"><span aria-hidden="true" class="glyphicon glyphicon-remove"> </span></button>
                <h4 class="modal-title" id="myModalLabel">Error</h4>
            </div>
            <div class="modal-body">
                <div class="alert alert-danger" role="alert">Sorry, Verovio failed to process the MEI data</div>
                <div id="errDiv">
                    <pre id="err"></pre>
                </div>
            </div>
        </div>
    </div>
</div>


<!-- sidebar -->
<div class="sidebar-offcanvas sidebar-right" id="sidebar" role="navigation">
    <div class="sidebar-panel">
        <div class="row">
            <div class="col-xs-12">
                <h4>Navigation</h4>
            </div>
        </div>
        <div class="row sidebar-row">
            <div class="col-xs-12">
                <button class="btn btn-default btn-sm pull-left popover-btn" type="button" data-container="body" data-toggle="popover" data-placement="left" data-content="On mobile devices, you can swipe left or right to change page and tap or double tap to zoom-in or zoom-out. On desktops, you can use [ctrl+] left or right arrows and +/- keys.">
                    <span class="glyphicon glyphicon-question-sign"/>
                </button>
            </div>
        </div>
        <div class="row sidebar-row">
            <div class="col-md-12">
                <p>
                    Go to page [1-<span id="total_text">1</span>]:
                </p>
                <div class="input-group input-group-sm" style="width: 20px;">
                    <span class="input-group-btn">
                        <button onclick="first_page()" class="btn btn-default" type="button">
                            <span class="glyphicon glyphicon-fast-backward"/>
                        </button>
                        <button onclick="prev_page()" class="btn btn-default" type="button">
                            <span class="glyphicon glyphicon-backward"/>
                        </button>
                    </span>
                    <input type="text" class="form-control" placeholder="0" id="jump_text" style="width: 45px !important;" onkeypress="do_page_enter(event)"/>
                    <span class="input-group-btn">
                        <button onclick="next_page()" class="btn btn-default" type="button">
                            <span class="glyphicon glyphicon-forward"/>
                        </button>
                        <button onclick="last_page()" class="btn btn-default" type="button">
                            <span class="glyphicon glyphicon-fast-forward"/>
                        </button>
                    </span>
                </div>
                <p></p>
                <p><em>(Swipe or click left- or right-key to change screen)</em></p>
                <!-- /input-group -->
            </div>
            <!-- /.col-lg-6 -->
        </div>
        <div class="row sidebar-row">
            <div class="col-md-6">
                <p>
                    Zoom:
                </p>
                <div class="input-group input-group-sm" style="width: 20px;">
                    <span class="input-group-btn">
                        <button onclick="zoom_out()" class="btn btn-default" type="button">
                            <span class="glyphicon glyphicon-zoom-out"/>
                        </button>
                    </span>
                    <input type="text" class="form-control" style="width:60px !important;" placeholder="100%" id="zoom_text" onkeypress="do_zoom_enter(event)"/>
                    <span class="input-group-btn">
                        <button onclick="zoom_in()" class="btn btn-default" type="button">
                            <span class="glyphicon glyphicon-zoom-in"/>
                        </button>
                    </span>
                </div>
                <!-- /input-group -->
            </div>
            <!-- /.col-md-6 -->
        </div>
    </div>


    <div class="hidden-xs hidden-sm sidebar-panel">
        <div class="row">
            <div class="col-xs-12">
                <h4>Select MEI file</h4>
            </div>
        </div>

        <div id="options_panel_body">
            <form id="upload_form" onsubmit="upload_file( $('#upload_form') ); return false" class="form-inline" role="form">
                <div class="row">
                    <div class="col-md-12">
                        <input type="file" id="mei_files" onchange="$('#submit-btn').popover('show');" name="file" style="margin: 4px 0px 8px 0px;"/>
                    </div>
                    <div class="col-md-12">
                        <button type="submit" id="submit-btn" class="btn btn-default btn-sm popover-btn" data-container="body" data-toggle="popover" data-placement="left" data-content="Click to display the MEI file!" data-trigger="manual">Render</button>
                    </div>
                </div>

                <!--><div id="log_panel" style="display: none">
                    <hr class="panel"/>
                    <p id="log_p"></p>
                </div>
                -->

            </form>
        </div>
    </div>

    <div class="sidebar-panel">
        <p>This viewer uses the latest development version <a href="" id="version">[version]</a> and might be unstable.</p>
        <p>Do you need the old MEI viewer? It is available <a href="https://www.verovio.org/mei-viewer-old.xhtml">here</a>.</p>
    </div>
</div>

<div id="page-content-wrapper">
    <!-- top navbar -->
    <div id="navbar" class="navbar navbar-default navbar-with-sidebar">
        <button type="button" class="navbar-toggle sidebar-toggle" data-toggle="offcanvas" data-target=".sidebar-nav">
             <span class="icon-bar"></span>
             <span class="icon-bar"></span>
             <span class="icon-bar"></span>
        </button>
        <div class="btn-group">
            <button id="pdf-button" class="btn btn-sm btn-default" data-toggle="modal" data-target="#pdfDialog" type="button" data-loading-text="Generating PDF..." style="display: none;">
                <span class="hidden-xs">PDF </span><span class="glyphicon glyphicon-book"/>
            </button>
            <button id="options-button" class="btn btn-sm btn-default hidden-xs" data-toggle="modal" data-target="#optionDialog" type="button" data-loading-text="Style editor..." style="display: none;">
                <span class="hidden-xs">Options </span><span class="glyphicon glyphicon-wrench"/>
            </button>
            <button id="play-button" onclick="play_midi();" class="btn btn-sm btn-default" type="button" style="display: none;">
                <span class="glyphicon glyphicon-volume-up"/>
            </button>
        </div>

        <button type="button" id="mei_download" class="navbar-toggle menu-download" style="display: none;">
            <span class="glyphicon glyphicon-download"></span><span> MEI</span>
        </button>
        <a class="navbar-brand logo-nav" href="https://www.verovio.org/index.xhtml">
            <img alt="Verovio logo" src="https://www.verovio.org/images/verovio-fadded-50.png" />
        </a>
        <div style="clear: both;"></div>
        <hr/>
    </div>

    <div id="svg_panel" style="background-color: #fff;">
        <div id="player" style="z-index: 20; position: absolute; display: none;"></div>
        <div id="svg_output" style="overflow:hidden;" />
    </div>
</div>

<div class="modal fade" id="pdfDialog" role="dialog" style="display: none;">
    <div class="modal-dialog">
        <div class="modal-content">
            <div class="modal-header">
                <button type="button" class="close" data-dismiss="modal" aria-label="Close"><span aria-hidden="true">×</span></button>
                <h4>PDF options</h4>
            </div>
            <div class="modal-body">
                <form role="form">
                    <div class="form-group">
                        <label for="pdfFormat">Format</label>
                        <div>
                            <select class="form-control" id="pdfFormat">
                                <option value="A4">A4</option>
                                <option value="letter">Letter</option>
                                <option value="B4">B4</option>
                            </select>
                        </div>
                    </div>
                    <div class="form-group">
                        <label for="pdfOrientation">Orientation</label>
                        <div>
                            <select class="form-control" id="pdfOrientation">
                                <option value="portrait">Portrait</option>
                                <option value="landscape">Landscape</option>
                            </select>
                        </div>
                    </div>
                </form>
            </div>
            <div class="modal-footer">
                <div class="alert alert-warning" style="text-align:left;">
                    <strong>Warning!</strong> This is an experimental feature and it can take some time.
                </div>
                <button type="button" class="btn btn-default" data-dismiss="modal">Cancel</button>
                <button type="button" id="pdfOK" class="btn btn-primary">OK</button>
            </div>
        </div>
    </div>
</div>

<div class="modal fade" id="optionDialog" role="dialog" style="display: none;">
    <div class="modal-dialog modal-lg">
        <div class="modal-content">
            <div class="modal-header">
                <button type="button" class="close" data-dismiss="modal" aria-label="Close"><span aria-hidden="true">×</span></button>
                <h4>Options</h4>
            </div>
            <div class="modal-body" id="optionDialogContent">
                <div id="option-div">
</div>
<script type="text/javascript">
//<![CDATA[
    options = vrvToolkit.getAvailableOptions();

    var ul = $("<ul class='nav nav-tabs' role='tablist'></ul>");
    $('#option-div').append(ul);
    
    var form = $("<form class='form-horizontal' id='option-form'></form>");
    $('#option-div').append(form);
    var tab_content = $("<div class='tab-content'></div>");
    form.append(tab_content);
    
    var i = 0;
    for (grp in options.groups) {
        var li = $("<li role='presentation'></li>");
        ul.append(li);
        if (i == 0) {
            li.attr("class", "active");
        }

        var a = $("<a></a>").attr("href", "#" + grp).attr("aria-controls", grp).attr("role", "tab").attr("data-toggle", "tab");
        li.append(a);
        a.append(options.groups[grp].name);
        
        var tab_panel = $("<div role='tabpanel'></div>");
        tab_content.append(tab_panel);
        if (i == 0) {
            tab_panel.attr("class", "tab-pane active panel-body");
        }
        else {
            tab_panel.attr("class", "tab-pane panel-body");
        }
        tab_panel.attr("id", grp);
        
        
        for (opt in options.groups[grp].options) {
            
            var option = options.groups[grp].options[opt];
            
            var form_group = $("<div class='form-group'></div>");
            tab_panel.append(form_group);
            
            var label = $("<label class='col-sm-3 control-label'></label>");
            form_group.append(label);
            label.attr("for", opt);
            label.append(option.title);
            
            var input_div = $("<div class='col-sm-3'></div>");
            form_group.append(input_div);
    
            if ((option.type == "double") || (option.type == "int") || (option.type == "std::string") || (option.type == "array")) {

                var input = $("<input class='form-control'></input>");
                input_div.append(input);
                input.attr("name", opt);
                
                if ((option.type == "double") || (option.type == "int")) {
                    input.attr("type", "number");
                    input.attr("min", option.min);
                    input.attr("max", option.max);
                    if (option.type == "double") {
                        input.attr("step", "0.05");
                    }
                    else {
                        input.attr("step", "1");
                    }
                }
                else {
                    input.attr("type", "text");
                }
            }
            else if (option.type == "std::string-list") {
                var select = $("<select class='form-control'></select>");
                input_div.append(select);
                select.attr("name", opt);
                
                for (v in option.values) {
                    var opt_select = $("<option></option>");
                    select.append(opt_select);
                    opt_select.attr("value", option.values[v]);
                    opt_select.append(option.values[v]);
                }
            }
            else if (option.type == "bool") {
                var input = $("<input class='form-control' style='width: auto !important;' type='checkbox'></input>");
                input_div.append(input);
                input.attr("name", opt);
            }
            
            var help_div = $("<div class='col-sm-6'></div>");
            form_group.append(help_div);
            
            var help_p = $("<p class='help-block'></p>");
            help_div.append(help_p);
            help_p.append().text(option.description).html();
            
        }
        
        i = i + 1;
        
    }
//]]>
</script>

            </div>
            <div class="modal-footer">
                <button type="button" id="optionDefault" class="btn btn-default">Reset to default</button>
                <button type="button" class="btn btn-default" data-dismiss="modal">Cancel</button>
                <button type="button" id="optionOK" class="btn btn-primary">OK</button>
            </div>
        </div>
    </div>
</div>

<script>
//<![CDATA[
    $( document ).ready(function() {

        var version = vrvToolkit.getVersion();
        var n = version.lastIndexOf('-');
        var commit = version.substring(n + 1);
        $('#version').text( version );
        $("#version").attr( "href", "http://github.com/rism-digital/verovio/commit/" + commit ) ;

        $(window).keyup(function(event){
            // We need to make sure not to capture event on text fields
            if ( $(event.target).hasClass('form-control') ) {
                return;
            }
            if ( event.ctrlKey && (event.keyCode == 37) ) {
                first_page();
            }
            else if ( event.keyCode == 37 ) {
                prev_page();
            }
            else if ( event.ctrlKey && (event.keyCode == 39) ) {
                last_page();
            }
            else if ( event.keyCode == 39 ) {
                next_page();
            }
            else if ( event.keyCode == 107 ) {
                zoom_in();
            }
            else if ( event.keyCode == 109 ) {
                zoom_out();
            }
        });

        $(window).resize(function(){
            apply_zoom();
        });

        $( "#toggle_log" ).click(function() {
            log = !log;
            $( "#log_panel_body" ).toggle();
            // toggle icon
            $("span", this).toggleClass("glyphicon-chevron-down glyphicon-chevron-left");
        });

        $( "#mei_download" ).click(function() {
            outputFilename = outputFilename.replace(/\.[^\.]+$/, '.mei');
            saveAs(new Blob([vrvToolkit.getMEI(-1, true)], {type: "text/xml;charset=utf-8"}), outputFilename);
        });

        // Set the popover for the btn
        $( ".popover-btn" ).popover( );

        // Adjust the size of the svg_output and the zoom according to the div (screen) size
        width = $('#svg_panel').width();

        zoom = Math.min( Math.floor( 100 * width / 2100 ), zoom );

        // Init the swipe
        enable_swipe( true );

        // Load the default file or the file passed in the URL
        var file = getParameterByName("file");
        var musicxml = getParameterByName("musicxml");
        var local = getParameterByName("local");

        if (musicxml == "true") {
            format = 'musicxml';
            $("#mei_download").show();
        }

        if (local == "true") {
            data = localStorage.getItem("musicxml_file");
            outputFilename = localStorage.getItem("musicxml_filename");
            load_data( data );
        }
        else {
            if (!file || (file.length == 0)) {
                    file = "<?=$outfileName?>";
            }
            else {
                target = window.location.hash;
            }
            $("#overlay").show();
            $.ajax({
                url: file
                , dataType: "text"
                , success: function(data) {
                    outputFilename = file.replace(/^.*[\\\/]/, '');
                    load_data( data );
                }
            });
        }
        $(".row-offcanvas").on('transitionend webkitTransitionEnd oTransitionEnd mozTransitionend MSTransitionEnd', function() { 
            if (pageWidth != calc_page_width()) {
                apply_zoom();   
            }
        });
        
        $("#player").midiPlayer({
            color: "#c00",
            onUpdate: midiUpdate,
            onStop: midiStop,
            width: 250
        });
        
        $("#pdfFormat").val(pdfFormat);
        $("#pdfOrientation").val(pdfOrientation);
        
        $('#pdfOK').click(function () {
            $('#pdfDialog').modal('hide');
            var pdfFormat = $("#pdfFormat").val();
            var pdfOrientation = $("#pdfOrientation").val();
            $.cookie('pdfFormat', pdfFormat, { expires: 30 });
            $.cookie('pdfOrientation', pdfOrientation, { expires: 30 });
            generate_pdf();
        });
        
        for (opt in disabledOptions) {
            $("#option-form :input[name='" + disabledOptions[opt] + "']").prop("disabled", true);
        }

        $("#option-form :input").change(function() {
            // Reset all options to default 
            vrvToolkit.setOptions(vrvToolkit.getOptions(true));
            // console.log("Applying options");
            customOptions = non_default_options(JSON.parse($('#option-form').toJSON()));
            apply_zoom();
        });

        $("#optionDialog").on("show.bs.modal", function () {
            savedOptions = non_default_options(vrvToolkit.getOptions(false));
            var currentOptions = vrvToolkit.getOptions(false)
            var defaultOptions = vrvToolkit.getOptions(true);
            for(var key in defaultOptions) {
                
                if (disabledOptions.includes(key)) {
                    continue;
                }
                
                if (!Array.isArray(defaultOptions[key]) && (defaultOptions[key] != currentOptions[key])) {
                    $("label[for='" + key +"']").css("color", "#B00");
                }
                else {
                    $("label[for='" + key +"']").css("color", "");
                }
            }
            
            $("#option-form").fromJSON(JSON.stringify(currentOptions)); 
        });
        
        $("#optionDialog").on("hidden.bs.modal", function () {
            // This means the dialog was dismissed
            console.log(savedOptions);
            if (savedOptions !== undefined) {
                customOptions = savedOptions;
                savedOptions = undefined;
                //console.debug("Restoring options");
            }
            apply_zoom();
        });
        
        $('#optionOK').click(function () {
            savedOptions = undefined;
            $('#optionDialog').modal('hide');
        });
        
        $('#optionDefault').click(function () {
            savedOptions = undefined;
            customOptions = undefined;
            // Reset to default and clear local storage
            vrvToolkit.setOptions(vrvToolkit.getOptions(true));
            localStorage.clear('customOptions');
            //console.debug("Resetting options");
            $('#optionDialog').modal('hide');
        });
    });
//]]>
</script>


    </div>

    <script type="text/javascript">
        //<![CDATA[
        $(document).ready(function() {
            $('[data-toggle=offcanvas]').click(function() {
                $('.row-offcanvas').toggleClass('active');
            });
        });
        //]]>
    </script>

</body>

</html>
