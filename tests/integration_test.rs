use assert_cmd::Command;
use std::fs;
use std::io::Write;
use std::process::Output;

// Path constants for the tests directory and test files
const TEST_DIR: &str = "tests/test_cases/";
const CNF_EXT: &str = "cnf";
const GOLDEN_EXT: &str = "golden";
const EXECUTABLE_NAME: &str = "babysub-rust";

fn extract_hash(content: &str) -> Result<String, Box<dyn std::error::Error>> {
    content
        .lines()
        .find(|line| line.starts_with("c hash-signature"))
        .ok_or_else(|| "Hash-signature not found".into())
        .map(|line| line.split_whitespace().last().unwrap_or("").to_string())
}

fn run_test_case(test_name: &str) -> Result<(), Box<dyn std::error::Error>> {
    let current_dir = std::env::current_dir().unwrap();
    let cnf_path = current_dir
        .join(TEST_DIR)
        .join(test_name)
        .with_extension(CNF_EXT);
    let golden_path = current_dir
        .join(TEST_DIR)
        .join(test_name)
        .with_extension(GOLDEN_EXT);
    let output_path = current_dir
        .join(TEST_DIR)
        .join(test_name)
        .with_extension("out");
    let log_path = current_dir
        .join(TEST_DIR)
        .join(test_name)
        .with_extension("log");
    let err_path = current_dir
        .join(TEST_DIR)
        .join(test_name)
        .with_extension("err");

    // Cleanup before running
    let _ = fs::remove_file(&output_path);
    let _ = fs::remove_file(&log_path);
    let _ = fs::remove_file(&err_path);

    let executable_path = current_dir.join("target/debug").join(EXECUTABLE_NAME);
    let mut cmd = Command::new(executable_path);
    cmd.arg("-s").arg(&cnf_path).arg(&output_path);

    let output: Output = cmd.output()?;

    // Write output to log file and error to err file
    fs::File::create(&log_path)?.write_all(&output.stdout)?;
    fs::File::create(&err_path)?.write_all(&output.stderr)?;

    // Compare hash-signature with golden file
    let output_content = fs::read_to_string(&output_path)?;
    let golden_content = fs::read_to_string(&golden_path)?;

    let output_hash = extract_hash(&output_content)?;
    let golden_hash = extract_hash(&golden_content)?;

    if output_hash != golden_hash {
        return Err(format!(
            "Hash signatures do not match. Output hash: '{}', Golden hash: '{}'",
            output_hash, golden_hash
        )
        .into());
    }

    Ok(())
}

#[test]
fn test_empty() -> Result<(), Box<dyn std::error::Error>> {
    run_test_case("empty")
}

#[test]
fn test_binbin1() -> Result<(), Box<dyn std::error::Error>> {
    run_test_case("binbin1")
}

#[test]
fn test_binbin2() -> Result<(), Box<dyn std::error::Error>> {
    run_test_case("binbin2")
}

#[test]
fn test_inconsistent1() -> Result<(), Box<dyn std::error::Error>> {
    run_test_case("inconsistent1")
}

#[test]
fn test_inconsistent2() -> Result<(), Box<dyn std::error::Error>> {
    run_test_case("inconsistent2")
}

#[test]
fn test_trivial1() -> Result<(), Box<dyn std::error::Error>> {
    run_test_case("trivial1")
}

#[test]
fn test_trivial2() -> Result<(), Box<dyn std::error::Error>> {
    run_test_case("trivial2")
}
