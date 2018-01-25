export default class Pangram {
  constructor(sentence) {
    this.sentence = sentence.toLocaleLowerCase();
    this.alphabet = [...'abcdefghijklmnopqrstuvwxyz'];
  }

  isPangram() {
    const characters = new Set([...this.sentence]);
    return this.alphabet.every(c => characters.has(c));
  }
}
