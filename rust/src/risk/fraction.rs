use std::fmt;

#[derive(Debug)]
pub struct Fraction {
    numerator: u128,
    denominator: u128,
}

impl Fraction {
    pub const fn new(num: u128, denom: u128) -> Fraction {
        return Fraction{numerator: num, denominator: denom};
    }

    pub fn percentage(&self) -> f64 {
        return self.numerator as f64 / self.denominator as f64;
    }

    pub fn multiply(&self, frac: Fraction) -> Fraction {
        return Fraction::new(self.numerator * frac.numerator, self.denominator * frac.denominator);
    }
}

impl fmt::Display for Fraction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} / {}", self.numerator, self.denominator)
    }
}