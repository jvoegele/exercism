class Bob

  hey: (s) ->
    if isShouting(s)
      "Whoa, chill out!"
    else if isQuestion(s)
      "Sure."
    else if isSilence(s)
      "Fine. Be that way!"
    else
      "Whatever."

  isShouting = (s) ->
    s.match(/[A-Z]/) && s.toUpperCase() == s

  isQuestion = (s) ->
    s.slice(-1) == "?"

  isSilence = (s) ->
    s.trim().length == 0

module.exports = Bob

