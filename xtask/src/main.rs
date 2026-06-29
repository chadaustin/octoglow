use std::env;
use std::fs;
use std::path::PathBuf;
use std::process::{Command, ExitCode};

fn main() -> ExitCode {
    match run() {
        Ok(()) => ExitCode::SUCCESS,
        Err(error) => {
            eprintln!("xtask: {error}");
            ExitCode::FAILURE
        }
    }
}

fn run() -> Result<(), String> {
    let command = env::args().nth(1).unwrap_or_else(|| "release".to_string());
    match command.as_str() {
        "release" | "scr" => build_scr(),
        "help" | "-h" | "--help" => {
            println!("Usage: cargo run -p xtask -- [release|scr]");
            Ok(())
        }
        other => Err(format!("unknown command '{other}'")),
    }
}

fn build_scr() -> Result<(), String> {
    let status = Command::new("cargo")
        .args([
            "build",
            "-p",
            "octoglow",
            "-p",
            "octoglow-config-ui",
            "--release",
        ])
        .status()
        .map_err(|error| format!("failed to run cargo build: {error}"))?;

    if !status.success() {
        return Err(format!("cargo build failed with {status}"));
    }

    let target_dir = env::var_os("CARGO_TARGET_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from("target"));
    let exe = target_dir.join("release").join("octoglow.exe");
    let scr = target_dir.join("release").join("octoglow.scr");
    fs::copy(&exe, &scr).map_err(|error| {
        format!(
            "failed to copy {} to {}: {error}",
            exe.display(),
            scr.display()
        )
    })?;

    println!("Generated {}", scr.display());
    println!(
        "Generated {}",
        target_dir
            .join("release")
            .join("octoglow-config-ui.exe")
            .display()
    );
    Ok(())
}
