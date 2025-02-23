use ids_rust::{SearchArgs, init};

fn main() {
    let components: Vec<String> = std::env::args().skip(1).collect();
    dbg!(&components);
    let data = init().unwrap();
    let comp_string = components.join("");
    let out = data.search(SearchArgs {
        reverse: false,
        simple: true,
        lite: true,
        filter_level: ids_rust::FilterLevel::JoyoPlus,
        input: Some(comp_string),
        lookup: None,
    });
    println!("{out}");
}
