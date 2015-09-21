
function newMap(viewWidth, viewHeight) {

  var tileWidth   = 250
  var tileHeight  = tileWidth / 1.16

  var tiles = {}

  var container = $('<div/>')
                  .css('position', 'relative')
                  .css('width',  viewWidth + 'px')
                  .css('height', viewHeight + 'px')
                  .css('background-color', 'black')
                  .css('overflow', 'auto')


  function positionTile(x,y,t) {
    var offX = (viewWidth - tileWidth) / 2
    var offY = (viewHeight - tileHeight) / 2

    var wx  = offX + x * (0.75 * tileWidth)
    var wy  = offY -(y * tileHeight + x * (0.5 * tileHeight))
    return t.css('left', wx + 'px')
            .css('top', wy + 'px')
  }

  function drawHex() {
    var hex = 'polygon(25% 0, 75% 0, 100% 50%, 75% 100%, 25% 100%, 0 50%)'
    return $('<div/>')
           .css('position', 'absolute')
           .css('-webkit-clip-path', hex)
           .css('clip-path', hex)
           .css('width',  tileWidth + 'px')
           .css('height', tileHeight + 'px')
  }

  function drawToken(isSmall) {
    var d = (tileWidth / (isSmall ? 6 : 5)) + 'px'
    return $('<div/>')
           .css('width',  d)
           .css('height', d)
           .css('border-radius', d)
           .css('background-size', d)
           .css('display', 'inline-block')
  }

  return {

    getContainer: function() { return container },
    getTile: function(nm) { return tiles[nm] },
    forEachTile: function(f) { jQuery.each(tiles, f) },

    drawTile: function (x,y,nm) {

      var t = positionTile(x,y, drawHex())
              .css('background-color', "rgba(0,0,0,0)")
              .css('background-image', 'url("img/' + nm + '.png")')
              .css('background-size', tileWidth + 'px')
              .css('overflow','auto')

      var inhabitants = $('<div/>')
                        .css('width', (tileWidth  / 1.5) + 'px')
                        .css('height', (tileHeight / 2) + 'px')
                        .css('position', 'absolute')
                        .css('left', 0.25 * tileWidth + 'px')
                        .css('top',  '0px')

      t.append(inhabitants)
      container.append(t)

      tiles[nm] =
           { dim: function(on) { t.css('opacity', on ? '0.4' : '1') }

           , dom: function() { return t }

           , addAcolyte: function(nm, exhausted) {
               var u = 'img/token/player/' + nm + '/acolyte/'
                            + (exhausted ? 'back' : 'front') + '.png'
               var x = drawToken(true)
                       .css('background-image', 'url("' + u + '")')
               inhabitants.append(x)

               return {
                 dim: function(on) { x.css('opacity', on ? '0.6' : '1') }

               }
             }

           , addElder: function(nm, exhausted) {
               var u = 'img/token/player/' + nm + '/elder/'
                            + (exhausted ? 'back' : 'front') + '.png'
               var x = drawToken(false)
                       .css('background-image', 'url("' + u + '")')
               inhabitants.append(x)

               return {
                 dim: function(on) { x.css('opacity', on ? '0.6' : '1') }

               }
             }
           }
    }
  }
}


