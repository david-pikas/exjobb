fn main () {
    let foo: u32 = loop {
        break 'middle: loop {
            loop {
                break 'middle 12
            }
        }
    };
}
