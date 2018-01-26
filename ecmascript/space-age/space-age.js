function round100(number) {
  return Math.round(number * 100) / 100;
}

export default class SpaceAge {
  constructor(seconds) {
    this.seconds = seconds;
  }

  earthAge() {
    return this.seconds / 31557600;
  }

  onEarth() {
    return round100(this.earthAge());
  }

  onMercury() {
    return round100(this.earthAge() / 0.2408467);
  }

  onVenus() {
    return round100(this.earthAge() / 0.61519726);
  }

  onMars() {
    return round100(this.earthAge() / 1.8808158);
  }

  onJupiter() {
    return round100(this.earthAge() / 11.862615);
  }

  onSaturn() {
    return round100(this.earthAge() / 29.447498);
  }

  onUranus() {
    return round100(this.earthAge() / 84.016846);
  }

  onNeptune() {
    return round100(this.earthAge() / 164.79132);
  }
}
