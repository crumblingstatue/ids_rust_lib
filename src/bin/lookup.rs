use ids_rust::{SearchArgs, init};

fn main() {
    let components: Vec<String> = std::env::args().skip(1).collect();
    dbg!(&components);
    let data = init().unwrap();
    let comp_string = components.join("");
    let results = data.search(SearchArgs {
        reverse: false,
        simple: true,
        lite: true,
        filter_level: ids_rust::FilterLevel::JoyoPlus,
        input: Some(comp_string),
    });
    let mut prev_strokes = 0;
    for result in results {
        if result.strokes != prev_strokes {
            println!("\n{} strokes:", result.strokes);
        }
        print!(" {} ", result.kanji);
        prev_strokes = result.strokes;
    }
    println!();
}
