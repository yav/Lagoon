
function newArray2d () {
  var content = []

  return {
    each: function(f) {
      jQuery.each(content, function(x,row) {
        if (row === undefined) return true
        jQuery.each(row, function(y, v) {
          if (v === undefined) return true
          f(x,y,v) })
      })
    },

    getElem: function(x,y) {
      if (content[x] === undefined) return null
      var row = content[x]
      if (row[y] === undefined) return null
      return row[y]
    },

    setElem: function(x,y,el) {
      if (content[x] === undefined) content[x] = []
      var row = content[x]
      var old = row[y]
      if (old === undefined) old = null
      row[y] = el
      return old
    },

    removeElem: function(x,y) {
      if (content[x] === undefined) return null
      var row = content[x]
      var old = row[y]
      row[y] = undefined
      return old === undefined ? null : old
    }

  }
}
