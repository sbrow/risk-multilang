pub const fn one_d6() -> [u8; 6] {
    return [1, 2, 3 , 4, 5, 6];
}

pub const fn two_d6() -> [[u8; 2]; 36] {
    let mut results = [[0; 2]; 36];
    let mut x: u8 = 1;
    let mut y: u8 = 1;

    while x < 7 {
        while y < 7 {
            results[((x-1) * 6 + y - 1) as usize] = [x, y];
            y += 1;
        }
        x += 1;
        y = 1;
    }; 

    return results;
}

pub const fn three_d6() -> [[u8; 3]; 216] {
    let mut results = [[0; 3]; 216];
    let mut x: u8 = 1;
    let mut y: u8 = 1;
    let mut z: u8 = 1;

    while x < 7 {
        while y < 7 {
            while z < 7 {
                results[((x-1) * 36 + (y-1) * 6 + z - 1) as usize] = [x, y, z];
                z += 1
            }
            y += 1;
            z = 1;
        }
        x += 1;
        y = 1;
        z = 1;
    }; 

    return results;
}
