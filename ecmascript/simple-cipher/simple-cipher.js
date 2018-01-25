function wrapNumber(number, min, max) {
  if (number < min) {
    return (max - ((min - number) % min)) + 1;
  } else if (number > max) {
    return ((number % max) + min) - 1;
  }
  return number;
}

export default class Cipher {
  constructor(key = 'aaaaaaaaaa') {
    if (key === '' || /[^a-z]/.test(key)) {
      throw new Error('Bad key');
    }

    this.key = key;

    this.aCharCode = 'a'.charCodeAt(0);
    this.zCharCode = 'z'.charCodeAt(0);
    this.keyCharCodes = [...key].map(c => c.charCodeAt(0));
  }

  encode(str) {
    return this.shiftChars(str, 1);
  }

  decode(str) {
    return this.shiftChars(str, -1);
  }

  shiftChars(str, direction) {
    return [...str].map((c, i) => this.shiftChar(c, i, direction)).join('');
  }

  shiftChar(character, index, shiftDirection) {
    const shiftDistance = this.keyCharCodes[index % this.key.length] - this.aCharCode;
    const shiftedCharCode = character.charCodeAt(0) + (shiftDirection * shiftDistance);
    const wrappedCharCode = wrapNumber(shiftedCharCode, this.aCharCode, this.zCharCode);
    return String.fromCharCode(wrappedCharCode);
  }
}
