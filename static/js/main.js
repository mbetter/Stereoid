/* Compile markup as named templates */
$.template( "albumTemplate", '<a href="#"><span class="a">{{=albumArtistName}}</span><img src="{{=albumArtThumbUrl}}" /><span class="b">{{=albumTitle}}</span></a>');
$.template( "contentWrapperTemplate", '<div id="content-preview" class="content-preview"><div id="content-wrapper"></div><span class="ib-close" style="display:none;">Close Preview</span></div>');
$.template( "contentTemplate", '<a href="{{=albumM3UUrl}}"><img id="album-art" src="{{=albumArtUrl}}" /></a><div id="content-info"><h2>{{=albumTitle}}<span id="content-artist">{{=albumArtistName}}</span></h2><div id="content-songs" style="display:none;"></div></div>');
$.template( "songTemplate", '<span class="song-track">{{=songTrack}}</span><span class="song-name">{{=songName}}</span>');
$.template( "loginTemplate", '<form id="loginform" action="javascript:true;"><input type="text" name="username" id="unameblock" title="Enter your username" class="u1" /><input type="password" name="password" id="pwblock" title="Enter your password" class="u1" /><br /></br /><input type="submit" id="submitbtn" value="Submit" /></form>');

function loadAlbum(x,y) {
    $.ajax({
        url: "http://core.lan/api/albums?limit=1&offset=" + rose(x,y),
        context: $("#filler_" + x + "_" + y),
        success: function(data){
            var item = $(this);
            item.html( $.render( data, "albumTemplate"));
            item.data("json",data);
/*
            item.bind('click', function( event) {

                if (!kinetic_moving ) {
                    openItem (item );
                }
                return false;
            });
*/
        }
    });
}

function loadSongs(item) {
    
    var jsondata = item.data("json");
    $.ajax({
        url: jsondata[0].albumSongsUrl,
        context: $("#content-songs"),
        success: function(data){
            $(this).html( $.render( data, "songTemplate"));
            return false;
        }
    });
} 
     
function openItem($item) {

    if (isAnimating) return false;
    isAnimating = true;
    current = $item;
    loadContentItem ($item, function () { isAnimating = false; });
}

function showLogin () {
       $("#viewport").empty().append($.render( {}, "loginTemplate"));
       $("#loginform" ).submit( function (e) { 
            e.preventDefault();
            authenticate($("#unameblock").val(),$("#pwblock").val());
            } );
}
function authenticate(username,password) {

    var ts = Math.round((new Date()).getTime() / 1000);
    var auth = Sha1.hash(Sha1.hash(password) + ts);
    console.log(username + " " + password);
    $.ajax({
        type: 'PUT', 
        url: 'http://core.lan/api/sessions',
        data: {
                'username' : username,
                'auth'     : auth,
                'timestamp': ts
               },
        success: function(data){
            console.log('success');
            $.cookie('token',data.sessionToken, {raw: true});
            loadSite();
            }
        });
}

loadContentItem = function ( $item , callback) {
    var hasContentPreview   = ( $('#content-preview').length > 0 );
    var jsondata = $item.data('json'); 
    if (!hasContentPreview) {
       $("#viewport").append($.render( jsondata[0], "contentWrapperTemplate"));
    } 
    $("#content-wrapper").html( $.render( jsondata[0], "contentTemplate"));
    loadSongs($item);
    $("#content-preview").css({
        width   :   $item.width(),
        height  :   $item.height(),
        left    :   $item.offset().left,
        top     :   $item.offset().top
    }).show().animate({
        width   :   $(window).width(),
        left    :   0
    }, 500, 'easeOutExpo', function () {
        $(this).animate({
            height  :   $(window).height() - $("#topbar").outerHeight(true),
            top     :   $("#topbar").outerHeight(true)
        }, 400, function() {
            $(this).find('span.ib-close').show().click( closeImgPreview );
            $('#content-songs').show();
            if (callback) callback.call();
        });
    });
} 

    
closeImgPreview             = function() {

    if( isAnimating ) return false;
    isAnimating = true;

    var $item   = current;

    $('#content-songs').hide(); 
    $('#content-preview').find('span.ib-close')
                    .hide()
                    .end()
                    .animate({
                        height  : $item.height()+1,
                        top     : $item.offset().top+1
                        }, 500, 'easeOutExpo', function() {
                        $('#album-art').css('max-width','100%')
                        $('#content-info').fadeOut();
                        $(this).animate({
                            width   : $item.width(),
                            left    : $item.offset().left
                            }, 400, function() {
                                $(this).fadeOut(function() {
                                    $('#album-art').css('max-width','');
                                    isAnimating = false;});
                        } );
                    });
}

kinetic_moving = false;
isAnimating = false;
minrow = 0;
maxrow = 0;
mincol = -5; 
maxcol = 5;
current = null;

maxrows = 8;
maxcolumns = 12;

pwidth = 202;
pheight = 202;

$(document).ready(function () {
   showLogin(); 
});

function loadSite () {
    setWrapperSize();
    mincol = Math.ceil((maxcolumns - 1) / 2) * -1;
    maxcol = (maxcolumns - 1) + mincol;
    $("#viewport").empty().append('<div id="album-div"><div class="layout-row" id="row_0"></div></div>');
    
    //$("#album-div").append('<div class="layout-row" id="row_0"></div>')
    for (i=mincol;i<=maxcol;i++) {
        createPlaceholder(i,0);
        loadAlbum(i,0);
    }
    pwidth = $(".album").outerWidth(true);
    pheight = $(".album").outerHeight(true);

    cwidth = pwidth * ((maxcol - mincol) + 1);
    $(window).resize(function() {
        setWrapperSize();
        $('#content-preview').css({
            width   :   $(window).width(),
            height  :   $(window).height() - $("#bottombar").outerHeight(true) - $("#topbar").outerHeight(true) 
        }); 
    });
    
    $('#album-div').on('click','.album', function() {

        if (!kinetic_moving ) {
            openItem ($(this) );
        }
        return false;
    });

    $("#album-div").css('width', cwidth);
    loadKinetic();
     
    $("#viewport").scrollTo( { top:"+=606", left:"+=606"}, 0, $.scrollTo.defaults);
    //$("#viewport").scrollTo("20%", 0, $.scrollTo.defaults);
    $("#viewport").scroll(function () {
       var vp = $("#viewport");
       var ad = $("#album-div");
       var curheight = ad.height();
       var curwidth = ad.width();
       var ypos =  vp.scrollTop(); 
       var xpos =  vp.scrollLeft(); 
       var yph = vp.height();
       var xph = vp.width();
       var toscrolly = "+=0px";
       var toscrollx = "+=0px";
       // console.log("curheight: " + curheight + ", ypos: " + ypos + ", yph: " + yph);
       // console.log("curwidth: " + curwidth + ", xpos: " + (xpos + xph));
       if ((ypos + yph + (pheight * .90)) > curheight) {
           if (addBottomRow()) {
                toscrolly = "-=" + pheight + "px";
           }
       } else if (ypos < 390) {
           addTopRow();
           toscrolly = "+=" + pheight + "px";
       } 

       if ((xpos + xph + (pwidth * .75)) > curwidth) {
           if (addRightCol()) {
                toscrollx = "-=" + pwidth + "px";
           }
           cwidth = pwidth * ((maxcol - mincol) + 1)
           ad.css('width', cwidth);
       } else if (xpos < 400) {
           addLeftCol();
           toscrollx = "+=" + pwidth + "px";
           cwidth = pwidth * ((maxcol - mincol) + 1);
           ad.css('width', cwidth);
       } 
       if ((toscrollx != "+=0px") || (toscrolly != "+=0px")) {
            vp.scrollTo( { top:toscrolly, left:toscrollx}, 0, $.scrollTo.defaults);
       }

    });
    for(i=i;i<=maxrows;i++) {
        setWrapperSize();
    }
}

function createPlaceholder (x,y) {

       function make_id (nx,ny) { return "#filler_" + nx + "_" + ny }

       var newid = make_id(x,y);
       var bf = $(make_id(x-1,y));
       var af = $(make_id(x+1,y));
       var thisrow = $("#row_" + y);
       var elemexists = $(newid).length;
       var new_content = '<div class="album" id="filler_' + x + '_' + y + '"><img src="./img/ajax-loading.gif" /></div>' ;
       if (!elemexists) {
            if (thisrow.length) {
                if (bf.length) {
                   bf.after(new_content) ;
                } else if (af.length) {
                   af.before(new_content);
                } else {
                    thisrow.append(new_content);
                } 
            } 
       }
}
function addTopRow () {
    minrow--;
    if ((maxrow - minrow) >= maxrows) {
        $("#row_" + maxrow).remove();
        maxrow--;
    }
    $("#album-div").prepend('<div class="layout-row" id="row_' + minrow + '"></div>');
    var cta = ""
    for (i=mincol;i<=maxcol;i++) {
          cta += '<div class="album" id="filler_' + i + '_' + minrow + '"><img src="./img/ajax-loading.gif" /></div>'
//        createPlaceholder(i,minrow);
    }
    $("#row_" + minrow).append(cta);
    for (i=mincol;i<=maxcol;i++) {
        loadAlbum(i,minrow);
    }
}

function addBottomRow () {
    var ret = false;
    maxrow++;
    if ((maxrow - minrow) >= maxrows) {
        ret = true;
        $("#row_" + minrow).remove();
        minrow++;
    }
    $("#album-div").append('<div class="layout-row" id="row_' + maxrow + '"></div>');
    for (i=mincol;i<=maxcol;i++) {
        createPlaceholder(i,maxrow);
        loadAlbum(i,maxrow);
    }
    return ret;
}

function addLeftCol () {
    mincol--;
    
    if ((maxcol - mincol) >= maxcolumns) {
        for (i=minrow;i<=maxrow;i++) {
            $("#filler_" + maxcol + "_" + i).remove();
        }
        maxcol--;
    }    
    for (i=minrow;i<=maxrow;i++) {
        createPlaceholder(mincol,i);
        loadAlbum(mincol,i);
    }
}

function addRightCol () {
    var ret = false;
    maxcol++;
    if ((maxcol - mincol) >= maxcolumns) {
        ret = true;
        for (i=minrow;i<=maxrow;i++) {
            $("#filler_" + mincol + "_" + i).remove();
        }
        mincol++;
    }
    for (i=minrow;i<=maxrow;i++) {
        createPlaceholder(maxcol,i);
        loadAlbum(maxcol,i);
    }
    return ret;
}

function rose(x,y) {
    
    function umax(strata) {
        return ((strata + 1) * (strata / 2) * 8) + 1;
    }

    var ax = Math.abs(x);
    var ay = Math.abs(y);
    var s;
    var res;

    if (ax > ay) {
        s = ax;
    } else {
        s = ay;
    }

    if (y == (s * -1)) {
        res = umax(s) - (s - x);
    } else if (x == (s * -1)) {
        res = (umax(s) - (2 * s)) + ((s * -1) - y);
    } else if (y == s) {
        res = (umax(s) - (4 * s)) + ((s * -1) - x);
    } else {
        res = (umax(s) - (6 * s)) - (s - y);
    }

    return res;

}                

loadKinetic                 = function() {

    $("#viewport").kinetic({
        moved   : function() {
            kinetic_moving = true;
        },
        stopped : function() {
            kinetic_moving = false;
        }
    });
}
setWrapperSize              = function() {
    $("#viewport").css('height', $(window).height() - $("#bottombar").outerHeight(true) - $("#topbar").outerHeight(true));
    $("#viewport").css('width', $(window).width());    
    maxcolumns = Math.ceil($("#viewport").outerWidth(true) / pwidth) + 4;
    maxrows = Math.ceil($("#viewport").outerHeight(true) / pheight) + 4;
    $("#viewport").scroll();
}

