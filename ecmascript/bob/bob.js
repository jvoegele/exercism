/* eslint-disable class-methods-use-this */

export default class Bob {
  hey(message) {
    if (this.isSilence(message)) {
      return 'Fine. Be that way!';
    } else if (this.isShouting(message)) {
      return 'Whoa, chill out!';
    } else if (this.isQuestion(message)) {
      return 'Sure.';
    }
    return 'Whatever.';
  }

  isShouting(message) {
    return /[A-Z]/.test(message) &&
      message === message.toLocaleUpperCase();
  }

  isQuestion(message) {
    return message.endsWith('?');
  }

  isSilence(message) {
    return message.trim() === '';
  }
}
