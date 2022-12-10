use std::{
    env,
    error::Error,
    fs::{self, File},
    io,
};

fn main() -> Result<(), Box<dyn Error>> {
    let crate_dir = env::var("CARGO_MANIFEST_DIR")?;
    let out_dir = env::var("OUT_DIR")?;

    let path = format!("{out_dir}/days.rs");
    let mut output = File::create(path)?;

    let solutions = find_solutions("src")?.collect::<Vec<_>>();
    write_module_declarations(&solutions, &mut output, &crate_dir)?;
    write_solve_function(&solutions, &mut output)?;

    Ok(())
}

enum Solution {
    Rust {
        file_name: String,
        module_name: String,
    },
    Prolog {
        file_name: String,
        path: String,
    },
}

fn find_solutions(src_dir: &str) -> io::Result<impl Iterator<Item = Solution>> {
    Ok(fs::read_dir(src_dir)?.filter_map(|file| {
        let os_file_name = file.ok()?.file_name();
        let file_name = os_file_name.to_str()?;

        if !file_name.starts_with("day_") {
            return None;
        }

        if let Some(module_name) = file_name.strip_suffix(".rs") {
            Some(Solution::Rust {
                file_name: file_name.to_owned(),
                module_name: module_name.to_owned(),
            })
        } else if file_name.ends_with(".pl") {
            Some(Solution::Prolog {
                file_name: file_name.to_owned(),
                path: format!("src/{file_name}"),
            })
        } else {
            None
        }
    }))
}

fn write_module_declarations<'a>(
    solutions: impl IntoIterator<Item = &'a Solution>,
    output: &mut dyn io::Write,
    crate_dir: &str,
) -> io::Result<()> {
    for solution in solutions {
        if let Solution::Rust {
            file_name,
            module_name,
        } = solution
        {
            writeln!(
                output,
                r#"#[path = "{crate_dir}/src/{file_name}"] mod {module_name};"#
            )?;
        }
    }

    Ok(())
}

fn write_solve_function<'a>(
    solutions: impl IntoIterator<Item = &'a Solution>,
    output: &mut dyn io::Write,
) -> io::Result<()> {
    writeln!(
        output,
        "fn solve(solution_file_name: &str, input_path: &str) {{
            match solution_file_name {{"
    )?;

    for solution in solutions {
        match solution {
            Solution::Rust {
                file_name,
                module_name,
            } => writeln!(
                output,
                "{file_name:?} => run_rust({module_name}::solve, input_path),"
            )?,
            Solution::Prolog { file_name, path } => writeln!(
                output,
                "{file_name:?} => run_prolog({path:?}, input_path),"
            )?,
        }
    }

    writeln!(
        output,
        r#"_ => panic!("no such solution: {{solution_file_name}}") }} }}"#
    )?;

    Ok(())
}
