/* Compile markup as named templates */
$.template( "albumTemplate", '<a href="#"><span class="a">{{=albumArtistName}}</span><img src="{{=albumArtThumbUrl}}" /><span class="b">{{=albumTitle}}</span></a>');
$.template( "contentWrapperTemplate", '<div id="content-preview" class="content-preview"><div id="content-wrapper"></div><div id="content-bg" style="display:none;"></div></div>');
$.template( "infoTemplate", '<h2><a id="album-title" title="Add &ldquo;{{=albumTitle}}&rdquo; to playlist" href="javascript:void(0)">{{=albumTitle}}</a><span id="content-artist">{{=albumArtistName}}</span></h2>');
//$.template( "contentTemplate", '<a id="play-album" title="Play &ldquo;{{=albumTitle}}&rdquo; now" href="javascript:void(0)"><img id="album-art" src="{{=albumArtUrl}}" /></a><div id="content-info"><h2><a id="album-title" title="Add &ldquo;{{=albumTitle}}&rdquo; to playlist" href="javascript:void(0)">{{=albumTitle}}</a><span id="content-artist">{{=albumArtistName}}</span></h2><div id="content-songs" style="display:none;"></div></div>');
$.template( "contentTemplate", '<div id="album-info"><div id="info-top"><a id="album-title" class="album-text" title="Add &ldquo;{{=albumTitle}}&rdquo; to playlist" href="javascript:void(0)" style="display:none;">{{=albumTitle}}</a></div><div id="info-left"></div><div id="info-mid"><a id="play-album" title="Play &ldquo;{{=albumTitle}}&rdquo; now" href="javascript:void(0)"><img id="album-art" src="{{=albumArtUrl}}" /></a></div><div id="info-right"></div><div id="info-bottom"><span id="content-artist" class="album-text" style="display:none;">{{=albumArtistName}}</span></div></div>');
//$.template( "contentTemplate", '<div id="album-info"><h2><a id="album-title" class="album-text" title="Add &ldquo;{{=albumTitle}}&rdquo; to playlist" href="javascript:void(0)" style="display:none;">{{=albumTitle}}</a><span id="content-artist" class="album-text" style="display:none;">{{=albumArtistName}}</span></h2><a id="play-album" title="Play &ldquo;{{=albumTitle}}&rdquo; now" href="javascript:void(0)"><img id="album-art" src="{{=albumArtUrl}}" /></a></div>');
$.template( "songTemplate", '<span class="song-track">{{=songTrack}}</span><a id="playlink_{{=songID}}" href="javascript:void(0)" title="Add &ldquo;{{=songName}}&rdquo; to playlist" class="playsong"><span class="song-name">{{=songName}}</span></a>');
$.template( "sideSongsTemplate", '<div id="content-songs"></div>');
$.template( "nowPlayingTemplate", '<div id="nowplaying"><span id="np_np">now playing</span><div class="sidebar-content"><a id="np_handle"></a><img id="np_albumart" src="{{=songAlbumArtUrl}}" /><div id="np_progress"><div id="np_progress_fill"></div></div><span class ="np_info" id="np_title">{{=songName}}</span><span class ="np_info" id="np_artist">{{=songArtistName}}</span><span class ="np_info" id="np_album">{{=songAlbumTitle}}</span></div></div>');
$.template( "loginTemplate", '<div id="login"><form id="loginform" action="javascript:true;"><input type="text" name="username" id="unameblock" title="Enter your username" class="u1" /><input type="password" name="password" id="pwblock" title="Enter your password" class="u1" /><br /></br /><input type="submit" id="submitbtn" value="Submit" /><span id="rememberme"><input type="checkbox" name="remember" id="remcheck" title="Remember me" />Remember me</span></form></div>');
$.template( "controlTemplate", '<div id="player-controls"><a id="pc-prev" href="javascript:void(0)"></a><a id="pc-play" href="javascript:void(0)"></a><a id="pc-pause" href="javascript:void(0)"></a><a id="pc-stop" href="javascript:void(0)"></a><a id="pc-next" href="javascript:void(0)"></a></div>');

session_timeout = 14 * 60 * 1000;
updating_session = false;
seed = Math.round((new Date()).getTime() / 1000);
var t;

function keepSessionAlive () {
    $.ajax({
        url: "http://core.lan/api/sessions",
        success: function(data){
            t=setTimeout("keepSessionAlive()",session_timeout);
        },
        error: function(data){
            clearTimeout(t);
            showLogin();
        }
    });
}

function loadAlbum(x,y) {
    $.ajax({
        url: "http://core.lan/api/albums?sort=random&seed=" + seed + "&limit=1&offset=" + rose(x,y),
        context: $("#filler_" + x + "_" + y),
        success: function(data){
            var item = $(this);
            item.html( $.render( data, "albumTemplate"));
            item.data("json",data);
        }
        /*
        error: function(data){
            if (!updating_session) {
                updating_session = true;
                if (g_authenticate) {
                    updating_session = false;
                    loadAlbum(x,y);
                } else {
                    updating_session = false;
                    showLogin();
                }
            } else {
                while (updating_session){}
                loadAlbum(x,y);
            }
            
        }*/
    });
}

playlist = new Array();

function updateNowPlaying(song) {
    $('#rightbar').html( $.render( song, "nowPlayingTemplate"));
    setWrapperSize();
}

function loadSongs(item) {
    
    var jsondata = item.data("json");
    $('#rightbar').html( $.render( {}, "sideSongsTemplate"));
    $.ajax({
        url: jsondata[0].albumSongsUrl,
        context: $("#content-songs"),
        success: function(data){
            console.log("songs loaded");
            $(this).empty();
            $('#album-title').data("json",data);
            for(var i = 0; i < data.length; i++) {
                $(this).append( $.render( data[i], "songTemplate"));
                $('#playlink_' + data[i].songID).data("json",data[i]);
            }
            return false;
        }
    });
} 
     
function showLogin () {
       $("#viewport").empty().append($.render( {}, "loginTemplate"));
       $("#loginform" ).submit( function (e) { 
            e.preventDefault();
            l_authenticate($("#unameblock").val(),$("#pwblock").val());
            } );
}

function g_authenticate() {
    var username = $.jStorage.get('username',false);
    var logintoken = $.jStorage.get('logintoken',false);

    if (username && logintoken) {
        $.ajax({
            type: 'PUT', 
            url: 'http://core.lan/api/sessions',
            data: {
                    'username'   : username,
                    'logintoken' : logintoken
                   },
            success: function(data){
                    console.log('login success');
                    $.cookie('token',data.sessionToken, {raw: true});
                    $.jStorage.set('username',username);
                    $.jStorage.set('logintoken',data.loginToken);
                    return true;
                },
             error: function(data){
                    console.log('login error');
                    $.cookie('token', null);
                    $.jStorage.deleteKey('username');
                    $.jStorage.deleteKey('logintoken');
                    return false;
                }
                    
            });
    } else {
        return false;
    }
}
function r_authenticate() {
    var username = $.jStorage.get('username',false);
    var logintoken = $.jStorage.get('logintoken',false);

    if (username && logintoken) {
        $.ajax({
            type: 'PUT', 
            url: 'http://core.lan/api/sessions',
            data: {
                    'username'   : username,
                    'logintoken' : logintoken
                   },
            success: function(data){
                    console.log('login success');
                    $.cookie('token',data.sessionToken, {raw: true});
                    $.jStorage.set('username',username);
                    $.jStorage.set('logintoken',data.loginToken);
                    loadSite();
                },
             error: function(data){
                    console.log('login error');
                    $.cookie('token', null);
                    $.jStorage.deleteKey('username');
                    $.jStorage.deleteKey('logintoken');
                    showLogin();
                }
                    
            });
    } else {
        showLogin();
    }
}
        
function l_authenticate(username,password,remember) {

    var ts = Math.round((new Date()).getTime() / 1000);
    var auth = Sha1.hash(Sha1.hash(password) + ts);
    console.log(username + " " + password);
    var authurl = 'http://core.lan/api/sessions'
    if ($("#remcheck:checked").val()) {
        authurl += '?rememberme=true'
    }
    $.ajax({
        type: 'PUT', 
        url: authurl,
        data: {
                'username' : username,
                'auth'     : auth,
                'timestamp': ts
               },
        success: function(data){
            console.log('success');
            $.cookie('token',data.sessionToken, {raw: true});
            if (data.loginToken) {
                $.jStorage.set('username',username);
                $.jStorage.set('logintoken',data.loginToken);
            }    
            loadSite();
            }
        });
}
function openItem($item) {

    if (isAnimating) return false;
    isAnimating = true;
    current = $item;
    loadContentItem ($item, function () { isAnimating = false; });
}

function sizeContent() {
    var vp = $('#viewport');
    var vpX = vp.outerWidth(true)
    var vpY = vp.outerHeight(true)
    var xBlk = (vpX - (pwidth * 2)) / 2;
    var yBlk = pheight * 2;
    var yGap = (vpY - yBlk) / 2;
    var midX = vpX / 2;
    var midY = vpY / 2;
    $("#content-preview").css({
        width   :   pwidth * 2,
        height  :   pheight * 2, 
        left    :   vp.offset().left,
        top     :   vp.offset().top
    }); 
    var aa = $('#content-preview'); 
    $('#album-info').css({
        'height'    :   aa.outerHeight(true),
        'width'     :   vpX, 
        'left'      :   0,
        'top'       :   vp.offset().top
    });
    $('#content-bg').css({
        'height'    :   vp.outerHeight(true),
        'width'     :   vp.outerWidth(true), 
        'left'      :   vp.offset().left,
        'top'       :   vp.offset().top,
    });
        $('#info-top').css({
            'width'     :   '100%',
            'display'   :   'block',
            'height'    :   yGap
        });
        $('#album-title').css({
            'margin-top'    :   yGap - 10 - $('#album-title').height()
        });
        $('#info-bottom').css({
            'width'     :   '100%',
            'display'   :   'block',
            'height'    :   yGap
        });
        $('#info-left').css({
            'height'    :   yBlk,
            'display'   :   'block',
            'width'     :   xBlk
        });
        $('#info-right').css({
            'height'    :   yBlk,
            'display'   :   'block',
            'width'     :   xBlk
        });
}

loadContentItem = function ( $item , callback) {
    var hasContentPreview   = ( $('#content-preview').length > 0 );
    var jsondata = $item.data('json'); 
    if (!hasContentPreview) {
       $("#viewport").append($.render( jsondata[0], "contentWrapperTemplate"));
    } 
    var oldL = $item.offset().left;
    var oldT = $item.offset().top;
    var newL;
    var newT;
    var vpX = $('#viewport').outerWidth(true)
    var vpY = $('#viewport').outerHeight(true)
    var xBlk = (vpX - (pwidth * 2)) / 2;
    var yBlk = pheight * 2;
    var yGap = (vpY - yBlk) / 2;
    var midX = vpX / 2;
    var midY = vpY / 2;
    
    if ((oldL + (.5 * pwidth)) < midX) {
        newL = oldL;
    } else { newL = oldL - pwidth; }
        
    if ((oldT - $('#topbar').outerHeight(true) + (.5 * pheight)) < midY) {
        newT = oldT;
    } else { newT = oldT - pheight; }

    var vp = $('#viewport');
    vp.unbind().css('cursor','auto');

    $("#content-wrapper").html( $.render( jsondata[0], "contentTemplate"));
    //$("#album-info").html( $.render( jsondata[0], "infoTemplate"));
    loadSongs($item);
    
    $("#content-preview,#album-art").css({
        width   :   $item.width(),
        height  :   $item.height(),
        left    :   oldL,
        top     :   oldT
    }).show().animate({
        width   :   pwidth * 2,
        height  :   pheight * 2, 
        left    :   (vp.outerWidth(true) / 2) - pwidth,
        top     :   vp.offset().top + (vp.outerHeight(true) / 2) - pheight
    }, 300, 'easeOutQuad', function() {
        var aa = $('#content-preview'); 
        $('#album-art').click(playAlbum).css({
            'width'     :   pwidth * 2,
            'height'    :   pheight * 2
         /*   'position'  :   'absolute',
            'left'      :   aa.offset().left*/
        });
        // $('#album-info').css('position','fixed');
        $('.album-text').show();
        $('#content-preview').click(closeImgPreview).css({
            'height'    :   aa.outerHeight(true),
            'width'     :   vp.outerWidth(true), 
            'left'      :   0,
            'top'       :   vp.offset().top
        });
        $('#info-top').css({
            'width'     :   '100%',
            'display'   :   'block',
            'height'    :   yGap
        });
        $('#album-title').css({
            'margin-top'    :   yGap - 10 - $('#album-title').height()
        });
        $('#info-bottom').css({
            'width'     :   '100%',
            'display'   :   'block',
            'height'    :   yGap
        });
        $('#info-left').css({
            'height'    :   yBlk,
            'display'   :   'block',
            'width'     :   xBlk
        });
        $('#info-right').css({
            'height'    :   yBlk,
            'display'   :   'block',
            'width'     :   xBlk
        });

     }).click(false);
        vp = $('#viewport');
        $('#content-bg').css({
            'height'    :   vp.outerHeight(true),
            'width'     :   vp.outerWidth(true), 
            'left'      :   vp.offset().left,
            'top'       :   vp.offset().top,
            'opacity'   :   0
        })

        $('#content-bg').show().click(closeImgPreview).animate({
            opacity: 0.8
        }, 300, 'easeOutQuad', function() {
            //$('span.ib-close').show().click( closeImgPreview );
            $('#content-songs').show();
            if (callback) callback.call();
        });
} 

    
closeImgPreview             = function() {

    if( isAnimating ) return false;
    isAnimating = true;

    var $item   = current;
    var vp = $('#viewport');
    $('#content-songs').hide(); 
    $('.album-text').hide();
    $('#info-top,#info-left,#info-right,#info-bottom').hide();
    $('#content-preview,#album-art').css({
        width   :   pwidth * 2,
        height  :   pheight * 2, 
        left    :   (vp.outerWidth(true) / 2) - pwidth,
        top     :   vp.offset().top + (vp.outerHeight(true) / 2) - pheight
     }).animate({
                        height  : $item.height()+1,
                        width   : $item.width(),
                        left    : $item.offset().left,
                        top     : $item.offset().top+1
                        }, 300, 'easeOutQuad', function() {
                        $('#content-info').fadeOut();
                        $(this).animate({
                            }, 400, function() {
                                $(this).fadeOut(function() {
                                    isAnimating = false;});
                        } );
                    });
    loadKinetic();
    addScrollEvents();
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
    r_authenticate(); 
});

plCursor = 0;

function playPlaylist () {
    if (plCursor < playlist.length) {
        soundManager.createSound('songplaying',playlist[plCursor].songUrl);
        updateNowPlaying(playlist[plCursor]);
        soundManager.play('songplaying',{
            onfinish: function() {
               soundManager.destroySound('songplaying');
               if ((plCursor + 1) < playlist.length) {
                    plCursor ++;
                    playPlaylist();
               }
            },
            whileplaying: function() {
                $('#np_progress_fill').css('width',(this.position / this.durationEstimate) * 197);
            }
        });
    }
}

function stopPlaylist () {
    soundManager.destroySound('songplaying');
}

function prevPlaylist () {
   if (plCursor > 0) {
       soundManager.destroySound('songplaying');
       plCursor--;
       playPlaylist();
   }
}    
function pausePlaylist () {
    soundManager.togglePause('songplaying');
}
function nextPlaylist () {
   if ((plCursor + 1) < playlist.length) {
       soundManager.destroySound('songplaying');
       plCursor++;
       playPlaylist();
   }
}    
function playAlbum (e) {
        e.preventDefault();
        if (playlist.length == 0) {
            $('#topbar').html($.render({},"controlTemplate"));
        }
        stopPlaylist();
        playlist = [];
        plCursor = 0;
        var thisData = $('#album-title').data('json');
        var artUrl = $('#album-art').attr('src');
        for(var i = 0; i < thisData.length; i++) {
            thisData[i].songAlbumArtUrl = artUrl;
            playlist.push(thisData[i]);
        }
        closeImgPreview();
        playPlaylist();
        return false;
}    
function loadSite () {
    keepSessionAlive();
    $('#rightbar').html( $.render( {}, "nowPlayingTemplate"));
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
        sizeContent();
    });
    
    $('#album-div').on('click','.album', function() {

        if (!kinetic_moving ) {
            openItem ($(this) );
        }
        return false;
    });
    $('#viewport').on('click','#album-title', function(e) {
        e.preventDefault();
        if (playlist.length == 0) {
            $('#topbar').html($.render({},"controlTemplate"));
        }
        var thisData = $(this).data('json');
        var artUrl = $('#album-art').attr('src');
        for(var i = 0; i < thisData.length; i++) {
            thisData[i].songAlbumArtUrl = artUrl;
            playlist.push(thisData[i]);
        }
        return false;
    });
    $(window).click( function () {
        console.log("testing");
    });
    $('#rightbar').on('click','.playsong', function(e) {
        e.preventDefault();
        var thisData = $(this).data('json');
        thisData.songAlbumArtUrl = $('#album-art').attr('src');    // dirty hack - need to add album art url to json on server side 
        if (playlist.length == 0) {
            $('#topbar').html($.render({},"controlTemplate"));
        }
        playlist.push(thisData);
        return false;
    });
    $('#topbar').on('click','#pc-pause', function(e) {
        e.preventDefault();
        pausePlaylist();
    });

    $('#topbar').on('click','#pc-stop', function(e) {
        e.preventDefault();
        stopPlaylist();
    });

    $('#topbar').on('click','#pc-prev', function(e) {
        e.preventDefault();
        prevPlaylist();
    });

    $('#topbar').on('click','#pc-next', function(e) {
        e.preventDefault();
        nextPlaylist();
    });

    $('#topbar').on('click','#pc-play', function(e) {
        e.preventDefault();
        playPlaylist();
    });
    $('#rightbar').on('click','#np_progress', function(event) {
        var off = $(this).offset();
        var offX = event.pageX - off.left;
        if ((offX >= 3) && (offX <= 197)) {
            var newPos = (offX / 197) * soundManager.getSoundById('songplaying').durationEstimate;
            soundManager.setPosition('songplaying',newPos);
        }
    });
    $("#album-div").css('width', cwidth);
    loadKinetic();
    addScrollEvents(); 
    $("#viewport").scrollTo( { top:"+=606", left:"+=606"}, 0, $.scrollTo.defaults);
    //$("#viewport").scrollTo("20%", 0, $.scrollTo.defaults);
    for(i=i;i<=maxrows;i++) {
        setWrapperSize();
    }
}
function addScrollEvents(){
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
    $("#viewport").css('width', $(window).width() - $("#rightbar").outerWidth(true));    
    maxcolumns = Math.ceil($("#viewport").outerWidth(true) / pwidth) + 4;
    maxrows = Math.ceil($("#viewport").outerHeight(true) / pheight) + 4;
    $("#viewport").scroll();
}

