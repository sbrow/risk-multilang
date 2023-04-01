mod risk;
use risk::fraction::Fraction;

fn main() {
    for result in risk::ONE_V_ONE {
        println!("{}", result);
    }

    let x = Fraction::new(1, 2).multiply(Fraction::new(1, 3));
    println!("{}", x);
}