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
            if let Some(day_index) = module_name.strip_prefix("day_") {
                let struct_name = format!("Day{day_index}");
                writeln!(
                    output,
                    r#"#[path = "{crate_dir}/src/{module_name}.rs"] mod {module_name}; use {module_name}::{struct_name};"#
                )?;
                days.push((module_name.to_owned(), struct_name));
            }
        }
    }

    writeln!(output, "fn days() -> std::collections::HashMap<&'static str, Box<dyn Day>> {{
        let mut days = std::collections::HashMap::<_, Box<dyn Day>>::new();")?;

    for (module_name, struct_name) in days {
        writeln!(output, "days.insert({module_name:?}, Box::new({struct_name}));")?;
    }

    writeln!(output, "days }}")?;

    Ok(())
}
