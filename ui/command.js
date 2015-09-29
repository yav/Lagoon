
function command(m) {

  function again() { console.log('again'); setTimeout(function() { command(m) }, 0) }

  jQuery.post('/step', {}, function(cmd) {

    switch(cmd[0]) {
      case 'appear':        m.appear(cmd[1], cmd[2]); again(); break
      case 'disappear':     m.disappear(cmd[1]); again(); break
      case 'swapTiles':     m.swapTiles(cmd[1], cmd[2]); again(); break
      case 'moveTile':      m.moveTile(cmd[1], cmd[2]); again(); break
      case 'addDruid':      m.addDruid(cmd[1], cmd[2],
                              cmd[3], cmd[4], cmd[5]); again(); break

      case 'removeDruid':   m.removeDruid(cmd[1]); again(); break
      case 'moveDriud':     m.moveDriud(cmd[1], cmd[2]); again(); break
      case 'setDruidState': m.setDruidState(cmd[1], cmd[2]); again(); break
      case 'chooseTile':    m.chooseTile(cmd[1], function(loc) {
                                console.log(loc)
                                jQuery.post('/step', loc)
                                again()
                            })
                            break
      case 'chooseDruid':   m.chooseDruid(cmd[2], function(d) {
                              jQuery.post('/step', { next: d })
                              again()
                            })
                            break
    }

  })

}

