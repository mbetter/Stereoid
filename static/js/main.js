var api = { 
    base_url : "http://core.lan/api",
    session_timeout : 15 * 60 * 1000,
};

var debug = false;
var cache_enabled = true;
var cache = new Array();

function debug_log (msg) { if (debug) { console.log(msg); } }

var kinetic_moving = false;
var isAnimating = false;
var minrow = 0;
var maxrow = 0;
var mincol = -5; 
var maxcol = 5;
var current = null;

var maxrows = 8;
var maxcolumns = 12;

var pwidth = 202;
var pheight = 202;

/* Compile markup as named templates */
$.template( "albumTemplate", '<a href="#"><span class="a">{{=albumArtistName}}</span><img src="{{=albumArtThumbUrl}}" /><span class="b">{{=albumTitle}}</span></a>');
$.template( "contentWrapperTemplate", '<div id="content-preview" class="content-preview"><div id="content-wrapper"></div><div id="content-bg" style="display:none;"></div></div>');
$.template( "infoTemplate", '<h2><a id="album-title" title="Add &ldquo;{{=albumTitle}}&rdquo; to playlist" href="javascript:void(0)">{{=albumTitle}}</a><span id="content-artist">{{=albumArtistName}}</span></h2>');
$.template( "contentTemplate", '<div id="album-info"><div id="info-top"><a id="album-title" class="album-text" title="Add &ldquo;{{=albumTitle}}&rdquo; to playlist" href="javascript:void(0)" style="display:none;">{{=albumTitle}}</a></div><div id="info-left"></div><div id="info-mid"><a id="play-album" title="Play &ldquo;{{=albumTitle}}&rdquo; now" href="javascript:void(0)"><img id="album-art" src="{{=albumArtUrl}}" /></a></div><div id="info-right"></div><div id="info-bottom"><a id="content-artist" class="album-text" href="javascript:void(0)" style="display:none;"><span>{{=albumArtistName}}</span></a></div></div>');
$.template( "songTemplate", '<span class="song-track">{{=songTrack}}</span><a id="playlink_{{=songID}}" href="javascript:void(0)" title="Add &ldquo;{{=songName}}&rdquo; to playlist" class="playsong"><span class="song-name">{{=songName}}</span></a>');

$.template( "resultSongTemplate", '<div class="r_song"><a id="playlink_{{=songID}}" href="javascript:void(0)" title="Add &ldquo;{{=songName}}&rdquo; to playlist" class="playsong">+</a><span class="rs_title">{{=songName}}</span><span class="rs_artist rs_info">{{=songArtistName}}</span><span class="rs_album rs_info">{{=songAlbumTitle}}</span></div>');

// $.template( "sideSongsTemplate", '<div id="content-songs" class="sidebarmain"><span id="s_s" class="sidebar-header">songs</span><div id="songs-songs" class="sidebar-content"></div></div>');
$.template( "sideSongsTemplate", '<div id="content-songs" class="sidebarmain"><div id="songs-songs" class="sidebar-content"></div></div>');
$.template( "nowPlayingTemplate", '<div id="nowplaying"><span id="np_np" class="sidebar-header">now playing</span><div id="np_content" class="sidebar-content normal"><a id="np_handle"></a><img id="np_albumart" class="normal" src="{{=songArtUrl}}" /><div id="np_progress" class="normal"><div id="np_progress_fill"></div></div><div id="np-controls" class="normal"><a id="np-prev" class="normal" href="javascript:void(0)"></a><a id="np-play" class="normal paused" href="javascript:void(0)"></a><a id="np-next" class="normal" href="javascript:void(0)"></a></div><div id="np_info_block"><span class ="np_info normal" id="np_title">{{=songName}}</span><span class ="np_info normal" id="np_artist">{{=songArtistName}}</span><span class ="np_info normal" id="np_album">{{=songAlbumTitle}}</span></div></div></div>');
$.template( "playlistTemplate", '<div id="playlist"></div>');
$.template( "playlistPlayingTemplate", '<div id="pl_playing"><span class ="np_info normal" id="pl_title">{{=songName}}</span><span class ="np_info normal" id="pl_artist">{{=songArtistName}}</span><span class ="np_info normal" id="pl_album">{{=songAlbumTitle}}</span></div>');
$.template( "playlistSongTemplate", '<span class="pl_title normal">{{=songName}}</span><span class="pl_artist pl_info normal">{{=songArtistName}}</span><span class="pl_album pl_info normal">{{=songAlbumTitle}}</span></div>');
$.template( "filterTemplate", '<div id="filter"><form id="filterform" action="javascript:void(0)"><input type="text" name="filterstring" id="filterblock" title="Filter string" class="u1" /></form></div>');
$.template( "loginTemplate", '<div id="login"><form id="loginform" action="javascript:true;"><input type="text" name="username" id="unameblock" title="Enter your username" class="u1" /><input type="password" name="password" id="pwblock" title="Enter your password" class="u1" /><br /></br /><input type="submit" id="submitbtn" value="Submit" /><span id="rememberme"><input type="checkbox" name="remember" id="remcheck" title="Remember me" />Remember me</span></form></div>');
$.template( "controlTemplate", '<div id="player-controls"><a id="pc-prev" href="javascript:void(0)"></a><a id="pc-play" href="javascript:void(0)"></a><a id="pc-pause" href="javascript:void(0)"></a><a id="pc-stop" href="javascript:void(0)"></a><a id="pc-next" href="javascript:void(0)"></a></div>');
$.template( "resultAlbumTemplate", '<div class="result-album"><div class="result-albumtop"><span class="result-title">{{=albumTitle}}</span></div><a href="javascript:void(0)" class="result-play" id="ralb_{{=albumID}}" ><img class="result-art" src="{{=albumArtThumbUrl}}" /></a><div class="result-albumbottom"><span class="result-artist">{{=albumArtistName}}</span></div></div>');

updating_session = false;
seed = Math.round((new Date()).getTime() / 1000);
var t;

function keepSessionAlive () {
    var token = $.cookie('token'); 
    if (token) {
        $.ajax({
            type: 'PUT',
            data: {
                    'token' : token
                  },
            url: api.base_url + '/sessions',
            success: function(data){
                t=setTimeout("keepSessionAlive()",api.session_timeout - (1 * 60 * 1000));
            },
            error: function(data){
                clearTimeout(t);
                showLogin();
            }
        });
    }
}

function loadAlbum(x,y) {
    $.ajax({
        url: api.base_url + "/albums?sort=random&seed=" + seed + "&limit=1&offset=" + rose(x,y),
        context: $("#filler_" + x + "_" + y),
        success: function(data){
            var item = $(this);
            item.html( $.render( data, "albumTemplate"));
            item.data("json",data[0]);
        }
    });
}

function loadCacheRange(begin,end) {
    if (cache_enabled) {
        var count = end - begin + 1;
        while (cache[begin] && count) {
            begin++;
            count--;
        }
        while (cache[end] && count) {
            end--;
            count--;
        }
        if (count) {
            $.ajax({
                url: api.base_url + "/albums?sort=random&seed=" + seed + "&limit=" + count + "&offset=" + begin,
                success: function(data){
                    for(var i = 0; i < data.length; i++) {
                       cache[begin + i] = data[i];
                    }
                }
            });
        }    
    }
}

function cachePrime() {
    debug_log('cachePrime()');
    var queued = new Array();
    var toprow = maxrow + 1;
    var bottomrow = minrow - 1;
    var rightcol = maxcol + 1;
    var leftcol = mincol - 1;

    for (var i=leftcol; i <= rightcol; i++) {
        queued.push( rose(i,toprow) );
        queued.push( rose(i,bottomrow) );
    }
    for (var i=minrow; i <= maxrow; i++) {
        queued.push( rose(leftcol,i) );
        queued.push( rose(rightcol,i) );
    }

    queued.sort();
    
    var lr;
    var start, end, startx;
    var pending = false;
    for (var i=0; i < queued.length; i++) {
        var ro = queued[i];
        if (!lr) {
            lr = ro;
            start = lr;
            end = lr;
        } else if (ro == (lr + 1)) {
            lr = ro;
            end = lr;
        } else {
            loadCacheRange(start, end);
            lr = ro;
            start = lr;
            startx = i;
            end = lr;
        }
    }

    loadCacheRange(start,end);
}

function loadAlbumRange(begin,end,divs) {
    if (cache_enabled) {
        while (cache[begin] && divs.length) {
            debug_log('cache hit: ' + begin);
            var id = divs.shift();
            debug_log(id);
            var d = $('#' + id);
            d.html( $.render( cache[begin], "albumTemplate"));
            d.data("json",cache[begin]);
            begin++;
        }
        while (cache[end] && divs.length) {
            debug_log('cache hit: ' + end);
            var id = divs.pop();
            debug_log(id);
            var d = $('#' + id);
            d.html( $.render( cache[end], "albumTemplate"));
            d.data("json",cache[end]);
            end--;
        }
    }
    if (divs.length) {
        var limit = end - begin + 1;
        $.ajax({
            url: api.base_url + "/albums?sort=random&seed=" + seed + "&limit=" + limit + "&offset=" + begin,
            success: function(data){
                if (divs.length > data.length) {
                    for(var i = data.length; i < divs.length; i++) {
                        $('#' + divs[i]).empty();
                    }    
                }
                for(var i = 0; i < data.length; i++) {
                    var d = $('#' + divs[i]);
                    d.html( $.render( data[i], "albumTemplate"));
                    d.data("json",data[i]);
                    if (cache_enabled) { cache[begin + i] = data[i]; }
                }
            }
        });
    }
}
function showPlaylist() {
    $('#rightbar').append( $.render( {}, "playlistTemplate") );
    var pl = $('#playlist');
    $('#nowplaying').hide();
    for(var i =0; i < playlist.length; i++) {
        var s_id = 'pl_song_' + i;
        pl.append('<div id="' + s_id + '" class="pl_song normal"></div>');
        $('#'+s_id).html( $.render( playlist[i], "playlistSongTemplate") );
    }
}

function albumReq(url) {
    $('#content-songs').remove(); 
    $('.sidebar-content').show();
    setWrapperSize();
    sizeContent();
    $.ajax({
        url: url,
        context: $('#content-wrapper'),
        success: function(data){
            var item = $(this);
            $('#album-info').hide();
            $('.result-album').remove(); 
            for(var i = 0; i < data.length; i++) {
                item.append( $.render( data[i], "resultAlbumTemplate"));
                var ra = $('#ralb_' + data[i].albumID);
                ra.data("json",data[i]);
            }
            setWrapperSize();
        }
    });
}

function artistAlbums() {
    //e.preventDefault();
    var artistID = currentdata.albumArtistID;
    var url = api.base_url + "/artists/" + artistID + "/albums";
    albumReq(url);
    return false;
}
function filterArtistAlbums (filter) {
   if (filter) {
       blankContentScreen();
       albumReq(api.base_url + "/albums/?artist=" + filter); 
   } else {
       closeImgPreview();
   }
   return false;
}
playlist = new Array();

function updateNowPlaying(song) {
    if ($('#np_content').hasClass('minimized')) {
        $('#nowplaying').remove();
        $('#rightbar').append( $.render( song, "nowPlayingTemplate"));
        minimizeNowPlaying();
    } else {
        $('#nowplaying').remove();
        $('#rightbar').append( $.render( song, "nowPlayingTemplate"));
    }
    setWrapperSize();
}

function maximizeNowPlaying() {
    
    $('#np_np').show();
    $('#np_content,#np_content>*,#np-controls>*,#np_info_block>*').removeClass('minimized').addClass('normal');
    setWrapperSize();
}

function minimizeNowPlaying() {
    
    $('#np_np').hide();
    $('#np_content,#np_content>*,#np-controls>*,#np_info_block>*').removeClass('normal').addClass('minimized');
    setWrapperSize();
}

function songsAjax(url) {
    
    $('#content-songs').remove();
    $('#rightbar').prepend( $.render( {}, "sideSongsTemplate"));
    minimizeNowPlaying(); 
    setWrapperSize();
    loadKineticSongs();
    $.ajax({
        url: url,
        context: $("#songs-songs"),
        success: function(data){
            $(this).empty();
            var a = $('#album-title');
            if (a.length) { a.data("json",data); };
            for(var i = 0; i < data.length; i++) {
                $(this).append( $.render( data[i], "resultSongTemplate"));
                $('#playlink_' + data[i].songID).data("json",data[i]);
            }
            return false;
            setWrapperSize();
        }
    });
} 
function loadSongs(item) {
    
    var jsondata = item.data("json");
    songsAjax(jsondata.albumSongsUrl);
    return false;
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
            type: 'POST', 
            url: api.base_url + '/sessions',
            data: {
                    'username'   : username,
                    'logintoken' : logintoken
                   },
            success: function(data){
                    $.cookie('token',data.sessionToken, {raw: true});
                    $.jStorage.set('username',username);
                    $.jStorage.set('logintoken',data.loginToken);
                    return true;
                },
             error: function(data){
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
            type: 'POST', 
            url: api.base_url + '/sessions',
            data: {
                    'username'   : username,
                    'logintoken' : logintoken
                   },
            success: function(data){
                    $.cookie('token',data.sessionToken, {raw: true});
                    $.jStorage.set('username',username);
                    $.jStorage.set('logintoken',data.loginToken);
                    loadSite();
                },
             error: function(data){
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
    var authurl = api.base_url + '/sessions'
    if ($("#remcheck:checked").val()) {
        authurl += '?rememberme=true'
    }
    $.ajax({
        type: 'POST', 
        url: authurl,
        data: {
                'username' : username,
                'auth'     : auth,
                'timestamp': ts
               },
        success: function(data){
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
    currentdata = $item.data('json');
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
            'height'    :   vp.outerHeight(true),
            'width'     :   vp.outerWidth(true), 
            'left'      :   0,
            'top'       :   vp.offset().top
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

function setupContent () {
   $("#viewport").append($.render( {}, "contentWrapperTemplate"));
   loadOtherK();
   $('#content-preview').on('click','.result-play',function(e){
       if (!kinetic_moving ) {
           openItem ($(this) );
       }
       return false;
    });
    $('#content-preview').on('click','#content-artist', artistAlbums);
    $('#content-preview').on('click','#album-art', playAlbum);
    $('#content-preview').on('click','#album-title', function(e) {
        e.preventDefault();
        var thisData = $(this).data('json');
        var artUrl = $('#album-art').attr('src');
        for(var i = 0; i < thisData.length; i++) {
            // thisData[i].songArtUrl = artUrl;
            addToPlaylist(thisData[i]);
        }
        minimizeNowPlaying();
        return false;
    });
}
function blankContentScreen () {

        if (!$('#content-preview').length) { setupContent(); } 

        if (!$('#content-bg').hasClass('active')) { showLightBox(); }

        $('#content-preview').show().click(closeImgPreview).css({
            'height'    :   vp.outerHeight(true),
            'width'     :   vp.outerWidth(true), 
            'left'      :   0,
            'top'       :   vp.offset().top
        });
    
}
loadContentItem = function ( $item , callback) {

    if (!$('#content-preview').length) { setupContent(); } 
    
    var jsondata = $item.data('json'); 
        
    loadSongs($item);
    
    var vp = $('#viewport');
    var oldL = $item.offset().left;
    var oldT = $item.offset().top;
    var newL, newT;
    var vpX = vp.outerWidth(true);
    var vpY = vp.outerHeight(true);
    var xBlk = (vpX - (pwidth * 2)) / 2;
    var yBlk = pheight * 2;
    var yGap = (vpY - yBlk) / 2;
    var midX = vpX / 2;
    var midY = vpY / 2;
    
    if ((oldL + (.5 * pwidth)) < midX) { newL = oldL; } else { newL = oldL - pwidth; }
        
    if ((oldT - $('#topbar').outerHeight(true) + (.5 * pheight)) < midY) {
        newT = oldT;
    } else { newT = oldT - pheight; }


    $("#content-wrapper").html( $.render( jsondata, "contentTemplate"));
    showLightBox(300); 
    $("#album-art").css({
        width   :   $item.width(),
        height  :   $item.height(),
        left    :   oldL,
        top     :   oldT
    }).show().animate({
        width   :   pwidth * 2,
        height  :   pheight * 2, 
        left    :   (vp.outerWidth(true) / 2) - pwidth,
        top     :   vp.offset().top + (vp.outerHeight(true) / 2) - pheight
    }, 300, 'easeOutQuad');
    $("#content-preview").css({
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
        $('#info-mid').css({
            'width'     :   pwidth * 2,
            'height'    :   pheight * 2
        });
        $('#content-songs').show();
        $('.album-text').show();
        $('#content-preview').click(closeImgPreview).css({
            'height'    :   vp.outerHeight(true),
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

     });
     if (callback) callback.call();
} 

function showLightBox(time) {

    var vtime = time ? time : 0;
    vp = $('#viewport');
    $('#content-bg').css({
        'height'    :   vp.outerHeight(true),
        'width'     :   vp.outerWidth(true), 
        'left'      :   vp.offset().left,
        'top'       :   vp.offset().top,
        'opacity'   :   0
    }).addClass('active').show().animate({
        opacity: 0.8
    }, vtime, 'easeOutQuad', function() {
    });

}


    
closeImgPreview             = function() {

    if( isAnimating || kinetic_moving) return false;
    isAnimating = true;

    var $item   = current ? current : $('#filler_0_0'); 
        
    var vp = $('#viewport');
    $('#filterblock').blur();
    $('.album-text').hide();
    $('#info-top,#info-left,#info-right,#info-bottom').hide();
    $('.result-album').remove();
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
                                    $('#content-songs').remove(); 
                                    $('#content-bg').removeClass('active');
                                    maximizeNowPlaying();
                                    setWrapperSize();
                                    //vp.kinetic();
                                    addScrollEvents();
                                    isAnimating = false;});
                        } );
                    });
}



$(document).ready(function () {
    r_authenticate(); 
});

plCursor = 0;

function addToPlaylist (item) {
    if (!$('#nowplaying').length) {
        updateNowPlaying(item);
    }
    playlist.push(item);
}
function playPlaylist () {
    if (plCursor < playlist.length) {
        soundManager.destroySound('songplaying');
        soundManager.createSound('songplaying',playlist[plCursor].songUrl);
        updateNowPlaying(playlist[plCursor]);
        
        $('#np-play').removeClass('paused').addClass('playing');
        soundManager.play('songplaying',{
            onfinish: function() {
               $('#np-play').removeClass('playing').addClass('paused');
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

function playPausePlaylist () {
    if (soundManager.soundIDs.length) {
        pausePlayList();
    } else {
        playPlaylist();
    }
}

function stopPlaylist () {
    $('#np-play').removeClass('playing').addClass('paused');
    soundManager.destroySound('songplaying');
}

function prevPlaylist () {
   if (plCursor > 0) {
       plCursor--;
       if (!soundManager.soundIDs.length) {
            updateNowPlaying(playlist[plCursor]);
       } else {
            var sp = soundManager.getSoundById('songplaying');
            if (sp.paused) {
                soundManager.destroySound('songplaying');
                updateNowPlaying(playlist[plCursor]);
            } else {
                soundManager.destroySound('songplaying');
                playPlaylist();
            }
        }
   }
}    
function pausePlaylist () {
    if (soundManager.soundIDs.length) {
        sp = soundManager.togglePause('songplaying');
        if (sp.paused) {
            $('#np-play').removeClass('playing').addClass('paused');
        } else {
            $('#np-play').removeClass('paused').addClass('playing');
        }
    } else {
        playPlaylist();
    }
}
function nextPlaylist () {
   if ((plCursor + 1) < playlist.length) {
       plCursor++;
       if (!soundManager.soundIDs.length) {
            updateNowPlaying(playlist[plCursor]);
       } else {
            var sp = soundManager.getSoundById('songplaying');
            if (sp.paused) {
                soundManager.destroySound('songplaying');
                updateNowPlaying(playlist[plCursor]);
            } else {
                soundManager.destroySound('songplaying');
                playPlaylist();
            }
        }
   }
}    
function playAlbum (e) {
        e.preventDefault();
        stopPlaylist();
        playlist = [];
        plCursor = 0;
        var thisData = $('#album-title').data('json');
        var artUrl = $('#album-art').attr('src');
        for(var i = 0; i < thisData.length; i++) {
            playlist.push(thisData[i]);
        }
        closeImgPreview();
        playPlaylist();
        return false;
}    

var keyT;
function attachFilterEvents() {
    var f = $('#filter');
    f.on('keyup',function(e) {
        keyT = setTimeout(function(){
           var v = $("#filterblock").val();
           filterArtistAlbums(v);
           songsAjax(api.base_url + "/songs?title=" + v); 
        },300);
    });
    f.on('keydown',function(e) {
        clearTimeout(keyT);
    });
    $('#filterblock').focus(function() {
        this.select();
    });
}
function loadSite () {
    keepSessionAlive();
    setWrapperSize();
    mincol = Math.ceil((maxcolumns - 1) / 2) * -1;
    maxcol = (maxcolumns - 1) + mincol;
    $("#topbar").empty().append( $.render({}, 'filterTemplate'));
    attachFilterEvents();
    $("#viewport").empty().append('<div id="album-div"><div class="layout-row" id="row_0"></div></div>');
    
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
    
    $(document).on('keyup',function(event){
        var target = $(event.target);
        if (!target.is("#filterblock") ) {
            if ( event.which == 219) {
                event.preventDefault();
                prevPlaylist();
            } else if ( event.which == 221) {
                event.preventDefault();
                nextPlaylist();
            } else if ( event.which == 80) {
                event.preventDefault();
                pausePlaylist();
            } else if ( event.which == 79) {
                event.preventDefault();
                $('#filterblock').focus();
            } else if ( event.which == 37) {
                event.preventDefault();
                var sl = $('#viewport').scrollLeft();
                $('#viewport').scrollLeft(sl - pwidth);
            } else if ( event.which == 38) {
                event.preventDefault();
                var sl = $('#viewport').scrollTop();
                $('#viewport').scrollTop(sl - pheight);
            } else if ( event.which == 39) {
                event.preventDefault();
                var sl = $('#viewport').scrollLeft();
                $('#viewport').scrollLeft(sl + pwidth);
            } else if ( event.which == 40) {
                event.preventDefault();
                var sl = $('#viewport').scrollTop();
                $('#viewport').scrollTop(sl + pheight);
            }
        }
    });
    $('#album-div').on('click','.album', function() {

        if (!kinetic_moving ) {
            openItem ($(this) );
        }
        return false;
    });
    $('#rightbar').on('click','.playsong', function(e) {
        e.preventDefault();
        if (kinetic_moving) { return false; }
        var thisData = $(this).data('json');
        addToPlaylist(thisData);
        minimizeNowPlaying();
        return false;
    });
    $('#rightbar').on('click','#np-prev', function(e) {
        e.preventDefault();
        prevPlaylist();
    });

    $('#rightbar').on('click','#np-next', function(e) {
        e.preventDefault();
        nextPlaylist();
    });

    $('#rightbar').on('click','#np-play', function(e) {
        e.preventDefault();
        pausePlaylist();
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
    for(i=i;i<=maxrows;i++) {
        setWrapperSize();
    }
    cachePrime();
}
var scrollT;
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
       // clearTimeout(scrollT);
       // scrollT = setTimeout(cachePrime,150);
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
    var divs = new Array();
    minrow--;
    if ((maxrow - minrow) >= maxrows) {
        $("#row_" + maxrow).remove();
        maxrow--;
    }
    $("#album-div").prepend('<div class="layout-row" id="row_' + minrow + '"></div>');
    var cta = "";
    var lr;
    var start, end, startx;
    for (i=mincol;i<=maxcol;i++) {
        var ro = rose(i,minrow);
        if (!lr) {
          lr = ro;
          start = lr;
          startx = i;
          end = lr;
          divs.push('filler_' + i + '_' + minrow);
        } else if (ro == (lr + 1)) {
            lr = ro;
            end = lr;
            divs.push('filler_' + i + '_' + minrow);
        } else {
            insertDivs(divs,minrow,startx);
            loadAlbumRange(start, end, divs);
            divs = [];
            lr = ro;
            start = lr;
            startx = i;
            end = lr;
            divs.push('filler_' + i + '_' + minrow);
        }
    }
    if (divs) {
        insertDivs(divs,minrow,startx);
        loadAlbumRange(start,end,divs);
    }

}
    
function insertDivsDec(ids,row,stx) {
    var cta = ""
    var prev = $('#filler_'+ (stx - 1) + '_' + row);
    var next = $('#filler_'+ (stx + ids.length + 1) + '_' + row);
    for (var i=0; i < ids.length; i++) {
        curr = $('#'+ids[i]);
        if (curr.length) {
            curr.remove();
        }
        cta = '<div class="album i" id="' + ids[i] +  '"><img src="./img/ajax-loading.gif" /></div>' + cta;
    }
    if (prev.length) {
        prev.after(cta);
    } else if (next.length) {
        next.before(cta);
    } else {
        $("#row_" + row).append(cta);
    }
}
function insertDivs(ids,row,stx) {
    var cta = ""
    var prev = $('#filler_'+ (stx - 1) + '_' + row);
    var next = $('#filler_'+ (stx + ids.length + 1) + '_' + row);
    for (var i=0; i < ids.length; i++) {
        curr = $('#'+ids[i]);
        if (curr.length) {
            curr.remove();
        }
        cta += '<div class="album i" id="' + ids[i] +  '"><img src="./img/ajax-loading.gif" /></div>';
    }
    if (prev.length) {
        prev.after(cta);
    } else if (next.length) {
        next.before(cta);
    } else {
        $("#row_" + row).append(cta);
    }
}
function addBottomRow () {
    var ret = false;
    var divs = new Array();
    maxrow++;
    if ((maxrow - minrow) >= maxrows) {
        ret = true;
        $("#row_" + minrow).remove();
        minrow++;
    }
    $("#album-div").append('<div class="layout-row" id="row_' + maxrow + '"></div>');
    var lr;
    var start, end, startx;
    for (i=mincol;i<=maxcol;i++) {
        var ro = rose(i,maxrow);
        if (!lr) {
          lr = ro;
          start = lr;
          startx = i;
          end = lr;
          divs.push('filler_' + i + '_' + maxrow);
        } else if (ro == (lr - 1)) {
            lr = ro;
            start = lr;
            startx = i;
            divs.unshift('filler_' + i + '_' + maxrow);
        } else {
            insertDivsDec(divs,maxrow,startx);
            loadAlbumRange(start, end, divs);
            divs = [];
            lr = ro;
            start = lr;
            startx = i;
            end = lr;
            divs.push('filler_' + i + '_' + maxrow);
        }
    }
    if (divs) {
        insertDivsDec(divs,maxrow,startx);
        loadAlbumRange(start,end,divs);
    }
    return ret;
}

function addLeftCol () {
    mincol--;
    var divs = new Array();
    if ((maxcol - mincol) >= maxcolumns) {
        for (i=minrow;i<=maxrow;i++) {
            $("#filler_" + maxcol + "_" + i).remove();
        }
        maxcol--;
    }    
    var lr;
    var start, end, startx;
    for (i=minrow;i<=maxrow;i++) {
        var ro = rose(mincol,i);
        if (!lr) {
          lr = ro;
          start = lr;
          end = lr;
          createPlaceholder(mincol,i);
          divs.push('filler_' + mincol + '_' + i);
        } else if (ro == (lr - 1)) {
            lr = ro;
            start = lr;
            createPlaceholder(mincol,i);
            divs.push('filler_' + mincol + '_' + i);
        } else {
            loadAlbumRange(start, end, divs);
            divs = [];
            lr = ro;
            start = lr;
            end = lr;
            createPlaceholder(mincol,i);
            divs.push('filler_' + mincol + '_' + i);
        }
    }
    if (divs) {
        loadAlbumRange(start,end,divs);
    }
}

function addRightCol () {
    var ret = false
    maxcol++;
    var divs = new Array();
    if ((maxcol - mincol) >= maxcolumns) {
        ret = true;
        for (i=minrow;i<=maxrow;i++) {
            $("#filler_" + mincol + "_" + i).remove();
        }
        mincol++;
    }    
    var lr;
    var start, end, startx;
    for (i=minrow;i<=maxrow;i++) {
        var ro = rose(maxcol,i);
        if (!lr) {
          lr = ro;
          start = lr;
          end = lr;
          createPlaceholder(maxcol,i);
          divs.push('filler_' + maxcol + '_' + i);
        } else if (ro == (lr + 1)) {
            lr = ro;
            end = lr;
            createPlaceholder(maxcol,i);
            divs.push('filler_' + maxcol + '_' + i);
        } else {
            loadAlbumRange(start, end, divs);
            divs = [];
            lr = ro;
            start = lr;
            end = lr;
            createPlaceholder(maxcol,i);
            divs.push('filler_' + maxcol + '_' + i);
        }
    }
    if (divs) {
        loadAlbumRange(start,end,divs);
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

loadOtherK                 = function() {

    $("#content-wrapper").kinetic({
        moved   : function() {
            kinetic_moving = true;
        },
        stopped : function() {
            kinetic_moving = false;
        }
    });
}
loadKineticSongs                 = function() {

    $("#content-songs").kinetic({
        moved   : function() {
            kinetic_moving = true;
        },
        stopped : function() {
            kinetic_moving = false;
        }
    });
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
    var vp = $("#viewport");
    $("#viewport").css('height', $(window).height() - $("#bottombar").outerHeight(true) - $("#topbar").outerHeight(true));
    $("#viewport").css('width', $(window).width() - $("#rightbar").outerWidth(true));    
    $("#content-songs").css('height', $("#viewport").outerHeight(true) - $("#np_content").outerHeight(true));
    maxcolumns = Math.ceil($("#viewport").outerWidth(true) / pwidth) + 4;
    maxrows = Math.ceil($("#viewport").outerHeight(true) / pheight) + 4;
    var bg = $('#content-bg');
    if (bg.hasClass('active')) { 
        var vp = $("#viewport");
        bg.css({
            'height'    :   vp.outerHeight(true),
            'width'     :   vp.outerWidth(true), 
            'left'      :   vp.offset().left,
            'top'       :   vp.offset().top,
        });
        $('#content-preview').css({
            'height'    :   vp.outerHeight(true),
            'width'     :   vp.outerWidth(true), 
            'left'      :   0,
            'top'       :   vp.offset().top
        });
    }

    $("#viewport").scroll();
}

