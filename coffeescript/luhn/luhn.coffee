class Luhn
  @create: (number) ->
    new Luhn(number)

  constructor: (@number) ->
    @digits = @number.toString().split("").map (digit) -> parseInt(digit)
    @checkDigit = @number % 10
    @addends = @getAddends()
    @checksum = @getChecksum()
    @valid = @checksum % 10 == 0

  getAddends: ->
    appendDigit = (acc, digit) ->
      d = if acc.length > 0 && acc.length % 2 != 0 then digit * 2 else digit
      if d >= 10
        d = d - 9
      acc.push(d)
      acc

    @digits.reverse().reduce(appendDigit, []).reverse()

  getChecksum: ->
    @addends.reduce (acc, num) ->
      acc += num

module.exports = Luhn
