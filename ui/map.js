
function newMap(viewWidth, viewHeight) {

  var tileWidth   = 250
  var tileHeight  = tileWidth / 1.16
  var origX       = 0
  var origY       = 0

  var tiles       = newArray2d()
  var druids      = {}

  var choosers = []
  var scrollStartX = null
  var scrollStartX = null

  var container = $('<div/>')
                  .css('position', 'relative')
                  .css('width',  viewWidth + 'px')
                  .css('height', viewHeight + 'px')
                  .css('background-color', 'black')
                  .css('overflow', 'hidden')
                  .css('user-select', 'none')
  container
  .mousedown(function(ev) { scrollStartX = ev.pageX
                            scrollStartY = ev.pageY
                            container.css('cursor','all-scroll')
                          })
  .mousemove(function(ev) {
              if (scrollStartX === null) return
              if (scrollStartY === null) return
              var dx = ev.pageX - scrollStartX
              var dy = ev.pageY - scrollStartY
              scrollStartX = ev.pageX
              scrollStartY = ev.pageY
              scroll(-dx, -dy)
  })
  .mouseup(function() {
    scrollStartX = null
    scrollStartY = null
    container.css('cursor','auto') })

  function scroll(dx,dy) {
    origX = origX + dx
    origY = origY + dy
    tiles.each(function(tX,tY,t) {
      var p = t.position()
      t.animate({ left: '+=' + dx, top: '+=' + dy }, 0)
    })
    jQuery.each(choosers, function(ix,c) {
      var p = c.position()
      c.animate({ left: '+=' + dx, top: '+=' + dy }, 0)
    })
  }

  function tileCoord(loc) {
    var offX = origX + (viewWidth - tileWidth) / 2
    var offY = origY + (viewHeight - tileHeight) / 2

    return { x: offX + loc.x * (0.75 * tileWidth)
           , y: offY -(loc.y * tileHeight + loc.x * (0.5 * tileHeight))
           }
  }


  function positionTile(loc,t) {
    var loc1 = tileCoord(loc)
    return t.css('left', loc1.x + 'px')
            .css('top', loc1.y + 'px')
  }

  function drawHex() {
    var hex = 'polygon(25% 0, 75% 0, 100% 50%, 75% 100%, 25% 100%, 0 50%)'
    return $('<div/>')
           .css('position', 'absolute')
           .css('-webkit-clip-path', hex)
           .css('clip-path', hex)
           .css('width',  tileWidth + 'px')
           .css('height', tileHeight + 'px')
           .css('background-size', tileWidth + 'px')
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

  function druidUrl(circle, isSmall, isReady) {
    return 'img/token/player/' + circle + '/'
         + (isSmall ? 'acolyte' : 'elder') + '/'
         + (isReady ? 'front'   : 'back')  + '.png'
  }

  return {

    appear: function(loc,nm) {
      var t = positionTile(loc, drawHex())
              .css('background-color', "rgba(0,0,0,0)")
              .css('background-image', 'url("img/' + nm + '.png")')
              .css('overflow','hidden')
              .hide()

      var inhabitants = $('<div/>')
                        .addClass('inhabitants')
                        .css('width', (tileWidth  / 2) + 'px')
                        .css('height', (tileHeight / 2) + 'px')
                        .css('position', 'absolute')
                        .css('left', 0.25 * tileWidth + 'px')
                        .css('top',  '0px')


      t.append(inhabitants)
      var old = tiles.setElem(loc.x,loc.y,t)
      if (old !== null) old.remove()
      container.append(t)
      t.fadeIn('slow')
    },

    disappear: function(loc) {
      var t = tiles.removeElem(loc.x,loc.y)
      if (t === null) return
      t.fadeOut('slow', function() { t.remove() })
    },

    swapTiles: function(loc1, loc2) {
      var t1 = tiles.getElem(loc1.x,loc1.y)
      if (t1 === null) return
      var t2 = tiles.getElem(loc2.x,loc2.y)
      if (t2 === null) return

      var left1 = t1.css('left')
      var top1  = t1.css('top')
      var left2 = t2.css('left')
      var top2  = t2.css('top')

      t1.animate( { left: left2, top: top2 }
                , 'slow'
                , 'swing'
                , function() {
        t2.animate( { left: left1, top: top1 }
                   , 'slow'
                   , 'swing'
                   , function() {
          tiles.setElem(loc2.x,loc2.y)
          tiles.setElem(loc1.x,loc1.y)
        })
      })

    },

    moveTile: function(from,to) {
      var t1 = tiles.removeElem(from.x,from.y)
      if (t1 === null) return

      var t2 = tiles.removeElem(to.x,to.y)
      if (t2 !== null) t2.remove()
      var loc = tileCoord(to)

      t1.css('z-index','100')
      t1.animate ({ left: loc.x, top: loc.y }, 'slow', 'swing',
        function() { t1.css('z-index','0'); tiles.setElem(xTo,yTo,t1) })
    },

    addDruid: function(nm, loc, circle, isSmall, isReady) {
      var t = tiles.getElem(loc.x,loc.y)
      if (t === null) return
      if (druids[nm] !== undefined) return

      var url = druidUrl(circle,isSmall,isReady)
      var d = drawToken(isSmall)
              .css('background-image', 'url("' + url + '")')
              .css('z-index', '1')
              .hide()
      druids[nm] = { dom: d, circle: circle, isSmall: isSmall }

      t.find('.inhabitants').append(d)
      d.fadeIn()
    },

    removeDruid: function(nm) {
      var d = druids[nm]
      if (d === undefined) return
      var dom = d.dom
      dom.fadeOut('slow', function() { dom.remove(); druids[nm] = undefined })
    },

    moveDriud: function(nm, loc) {
      var x = loc.x
      var y = loc.y
      var d = druids[nm]
      if (d === undefined) return
      var t = tiles.getElem(x,y)
      if (t === null) return

      var i = t.find('.inhabitants')

      var dom      = d.dom
      var p1       = dom.position()          // in inhabitants
      var p2       = dom.parent().position() // in tile
      var p3       = dom.parent().parent().position()
      var fromLeft = p3.left + p2.left + p1.left
      var fromTop  = p3.top + p2.top + p1.top

      var p4       = t.position()
      var p5       = i.position()
      var toLeft   = p4.left + p5.left
      var toTop    = p4.top + p5.top

      dom.remove()
      dom.css('position','absolute')
         .css('left', fromLeft + 'px')
         .css('top', fromTop + 'px')
      t.parent().append(dom)

      dom.animate( { left: toLeft, top: toTop }, 'slow', 'swing', function() {
        dom.remove()
        dom.css('position', 'static')
        i.prepend(dom)
      })
    },

    setDruidState: function(nm, isReady) {
      var d = druids[nm]
      if (d === undefined) return
      var dom = d.dom
      var url = druidUrl(d.circle, d.isSmall, isReady)
      dom.fadeOut('slow', function() {
          dom.css('background-image', 'url("' + url + '")')
          dom.fadeIn('slow')
      })
    },

    // [Loc] -> Loc
    chooseTile: function(locs,k) {
      choosers = []

      jQuery.each(locs, function(ix,loc) {
        var h = drawHex()
        positionTile(loc,h)
        h.addClass('selector')
         .click(function() {
            jQuery.each(choosers, function(ix,d) { d.remove() })
            k(loc)
            return false
        })
        container.append(h)
        choosers.push(h)
      })
    },



    // [DruidName] -> DruidName
    chooseDruid: function(names, k) {
      var choosers = []
      jQuery.each(names, function(ix,nm) {
        var d = druids[nm]
        if (d === undefined) return true
        var c = drawToken(d.isSmall)
        d.dom.append(c)
        choosers.push(c)
        c.addClass('selector')
         .click(function() {
            jQuery.each(choosers, function(ix,c) { c.remove() })
            k(nm)
            return false
         })
      })
    },

    chooseNewLocation: function(locs,k) {
      var d = $('<div/>')
              .css('width',  '100%')
              .css('height', 1.2 * tileHeight + 'px')
              .css('position', 'relative')
              .css('background-color', 'rgba(0,0,0,0.7)')
              .css('left', '10%')
              .css('top', '10%')
              .css('overflow', 'auto')

      jQuery.each(locs,function(ix,nm) {
        var l = drawHex()
                .css('background-image', 'url("img/' + nm + '.png")')
                .css('top', '10%')
                .css('left', ix * tileWidth + 'px')
                .css('cursor', 'pointer')

        l.click(function() {
          d.remove()
          k(nm)
        })
        d.append(l)
      })
      container.append(d)

    },

    getContainer: function() { return container },
  }
}


