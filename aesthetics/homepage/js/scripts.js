/*  Copyright 2014 gokoururi
    This work is free. You can redistribute it and/or modify it under the
    terms of the Do What The Fuck You Want To Public License, Version 2,
    as published by Sam Hocevar. See the COPYING file for more details. */

/*  Remove mascot class from element "main" if its width is <= mascotMinWidth */
function controlMascot(mascot, mascotMinWidth) {
    $(window).resize(function(event) {
        if ( $(window).width() <= mascotMinWidth && $("main").hasClass("mascot") ) {
            removeMascot();
        } else if( $(window).width() > mascotMinWidth && ! $("main").hasClass("mascot") ) {
            setMascot(mascot);
        }
    });
}

function setMascot(mascot) {
    $('main').addClass("mascot");
    $('main').css("background-image", "url(" + mascot + ")");
    $('main').removeClass("plain");
}

function removeMascot() {
    $('main').removeClass("mascot");
    $('main').css("background-image", "");
    $('main').addClass("plain");
}

$(document).ready(function(event) {
    var mascotEnable    = true;
    var mascotPath      = "images/mascots/"
    var mascotList      = [ 'asuna.png', 'ayano_tateyama.png', 'azu.png', 'cc.png', 'chitanda.png', 'ene.png', 'flonne.png', 'furano.png', 'inori.png', 'inori2.png', 'karen.png', 'kirino.png', 'kirino2.png', 'kon.png', 'konata.png', 'kougyoku.png', 'kurisu.png', 'kuroneko.png', 'mei.png', 'mikasa.png', 'miku.png', 'morgiana.png', 'nadeko.png', 'noel.png', 'onodera.png', 'ookami.png', 'pleinair.png', 'rachel.png', 'ryuuko.png', 'sayaka.png', 'shinobu.png', 'shinobu2.png', 'sora.png', 'suou.png', 'taiga.png', 'tsukiko.png', 'yin.png', 'yukino.png' ];

    var mascot          = mascotPath + mascotList[Math.floor(Math.random() * mascotList.length)];
    var mascotMinWidth  = '750';

    if ( mascotEnable ) { 
        setMascot(mascot);
        controlMascot(mascot, mascotMinWidth);
    } else { removeMascot(); }
});
