use std::{env, error::Error, fs::File, io::Write, path::Path};

fn main() -> Result<(), Box<dyn Error>> {
    let crate_dir = env::var("CARGO_MANIFEST_DIR")?;
    let out_dir = env::var("OUT_DIR")?;

    let path = format!("{out_dir}/days.rs");
    let mut output = File::create(path)?;

    let mut days = Vec::new();

    for file in Path::new("src").read_dir()? {
        if let Some(module_name) = file?
            .file_name()
            .to_str()
            .and_then(|f| f.strip_suffix(".rs"))
        {
            if module_name.starts_with("day_") {
                writeln!(
                    output,
                    r#"#[path = "{crate_dir}/src/{module_name}.rs"] mod {module_name};"#
                )?;
                days.push(module_name.to_owned());
            }
        }
    }

    writeln!(
        output,
        "fn solve(day: &str, input: std::io::BufReader<std::fs::File>) {{
            match day {{"
    )?;

    for module_name in days {
        writeln!(output, "{module_name:?} => {module_name}::solve(input),")?;
    }

    writeln!(output, r#"_ => panic!("no such day: {{day}}") }} }}"#)?;

    Ok(())
}
