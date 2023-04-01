pub mod dice;
pub mod fraction;

use std::fmt;
use fraction::Fraction;

#[derive(Debug)]
struct Casualties(u8, u8);

#[derive(Debug)]
pub struct Result {
    casualties: Casualties,
    probability: Fraction,
}

impl fmt::Display for Result {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        return write!(f, "{:?}: {}", self.casualties, self.probability);
    }
}

pub const ONE_V_ONE: [Result; 2] = one_v_one();
pub const TWO_V_ONE: [Result; 2] = two_v_one();

const fn one_v_one() -> [Result; 2] {
    let total = 6 * 6;
    let mut a: u8 = 0;
    let mut b: u8 = 0;

    let mut attackers = 1;
    let mut defenders = 1;

    while attackers <= 6 {
        while defenders <= 6 {
            if attackers > defenders {
                b += 1;
            } else {
                a += 1;
            }
            defenders += 1;
        }
        attackers += 1;
        defenders = 1;
    }

    return [
        // (0, -1)
        Result{casualties: Casualties(0, 1), probability: Fraction::new(a as u128, total as u128)},
        // (-1, 0)
        Result{casualties: Casualties(1, 0), probability: Fraction::new(b as u128, total as u128)},
    ];
}

pub const fn two_v_one() -> [Result; 2] {
    let total = 216;
    let mut a: u8 = 0;
    let mut b: u8 = 0;

    return [
        // (2, 0)
        Result{casualties: Casualties(0, 1), probability: Fraction::new(a as u128, total as u128)},
        // (1, 1)
        Result{casualties: Casualties(1, 0), probability: Fraction::new(b as u128, total as u128)},
    ];
}

/*
pub fn battle(attackers: u8, defenders: u8, probability: f32) -> Vec<Result> {
    return [].to_vec();
}
*/