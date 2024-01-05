use {ids_rust::init, std::collections::HashMap};

fn main() {
    let components: Vec<String> = std::env::args().skip(1).collect();
    dbg!(&components);
    let data = init().unwrap();
    let comp_string = components.join("");
    let args = HashMap::from_iter([("input", &comp_string[..])]);
    let out = data.search(args, true);
    println!("{out}");
}
