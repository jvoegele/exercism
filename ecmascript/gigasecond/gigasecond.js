const gigasecond = 1000000000000;

export default class Gigasecond {
  constructor(birthDate) {
    this.birthDate = birthDate;
  }

  date() {
    return new Date(this.birthDate.getTime() + gigasecond);
  }
}
