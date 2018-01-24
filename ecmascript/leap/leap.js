class Year {
  constructor(year) {
    this.year = year;
  }

  isLeap() {
    const isDivisibleBy = divisor => (this.year % divisor) === 0;
    return isDivisibleBy(400) || (isDivisibleBy(4) && !isDivisibleBy(100));
  }
}

export default Year;
