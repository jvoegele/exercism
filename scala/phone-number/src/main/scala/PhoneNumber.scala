class PhoneNumber(string: String) {
  def number =
    string filter (_.isDigit)
}
