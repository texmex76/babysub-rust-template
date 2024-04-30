use bzip2;
use bzip2::read::BzDecoder;
use bzip2::write::BzEncoder;
use clap::{Arg, ArgAction, Command};
use flate2;
use flate2::read::GzDecoder;
use flate2::write::GzEncoder;
use std::fs::File;
use std::io::{self, BufRead, BufReader, Read, Write};
use std::ops::{Index, IndexMut};
use std::path::Path;
use std::process;
use std::time::Instant;
use xz2::read::XzDecoder;
use xz2::write::XzEncoder;

macro_rules! die {
    ($($arg:tt)*) => {{
        eprintln!("babysub: error: {}", format!($($arg)*));
        process::exit(1);
    }}
}

macro_rules! message {
    ($verbosity:expr, $($arg:tt)*) => {{
        use std::io::{self, Write};
        if $verbosity >= 0 {
            let stdout = io::stdout();
            let mut handle = stdout.lock();
            if let Err(e) = writeln!(handle, "{}", format!("c {}", format_args!($($arg)*))) {
                die!("Failed to write message: {}", e);
            }
            if let Err(f) = handle.flush() {
                die!("Failed to flush stdout: {}", f);
            }
        }
    }}
}

macro_rules! verbose {
    ($verbosity:expr, $level:expr, $($arg:tt)*) => {{
        use std::io::{self, Write};
        if $verbosity >= $level {
            let stdout = io::stdout();
            let mut handle = stdout.lock();
            if let Err(e) = writeln!(handle, "{}", format!("c {}", format_args!($($arg)*))) {
                die!("Failed to write message: {}", e);
            }
            if let Err(f) = handle.flush() {
                die!("Failed to flush stdout: {}", f);
            }
        }
    }}
}

macro_rules! parse_error {
    ($ctx:expr, $msg:expr, $line:expr) => {{
        eprintln!(
            "babysub: parse error: at line {} in '{}': {}",
            $line, $ctx.config.input_path, $msg
        );
        process::exit(1);
    }};
}

#[cfg(feature = "logging")]
macro_rules! LOG {
    ($verbosity:expr, $($arg:tt)*) => {{
        use std::io::{self, Write};
        if $verbosity >= 999 {
            let stdout = io::stdout();
            let mut handle = stdout.lock();
            if let Err(e) = writeln!(handle, "{}", format!("c LOG {}", format_args!($($arg)*))) {
                die!("Failed to write message: {}", e);
            }
            if let Err(f) = handle.flush() {
                die!("Failed to flush stdout: {}", f);
            }
        }
    }}
}

#[cfg(not(feature = "logging"))]
macro_rules! LOG {
    ($($arg:tt)*) => {{}};
}

struct Config {
    input_path: String,
    output_path: String,
    verbosity: i32,
    forward_mode: bool,
    sign: bool,
}

fn average(a: usize, b: usize) -> f64 {
    if b != 0 {
        a as f64 / b as f64
    } else {
        0.0
    }
}

fn percent(a: usize, b: usize) -> f64 {
    100.0 * average(a, b)
}

struct Stats {
    checked: usize,
    parsed: usize,
    subsumed: usize,
    start_time: Instant,
}

#[derive(Debug, Clone)]
struct Clause {
    garbage: bool,
    // The clause id is just the index in the formula's clauses vector
    literals: Vec<i32>,
}

struct Matrix {
    matrix: Vec<Vec<usize>>,
}

impl Matrix {
    fn new() -> Self {
        Matrix { matrix: Vec::new() }
    }

    fn map_literal_to_index(&self, literal: i32) -> usize {
        // Optimization for matrix indexing
        // With this, lit and -lit will be next to each other
        if literal < 0 {
            (-literal * 2 - 2) as usize
        } else {
            (literal * 2 - 1) as usize
        }
    }

    fn init(&mut self, variables: usize, _verbosity: i32) {
        LOG!(
            _verbosity,
            "initializing matrix with {} variables",
            variables
        );
        let size = 2 * variables;
        self.matrix = vec![vec![0; size]; size];
    }
}

impl Index<i32> for Matrix {
    type Output = Vec<usize>;

    fn index(&self, literal: i32) -> &Self::Output {
        let computed_index = self.map_literal_to_index(literal);
        assert!(
            computed_index < self.matrix.len(),
            "Matrix index out of bounds"
        );
        &self.matrix[computed_index]
    }
}

impl IndexMut<i32> for Matrix {
    fn index_mut(&mut self, literal: i32) -> &mut Self::Output {
        let computed_index = self.map_literal_to_index(literal);
        assert!(
            computed_index < self.matrix.len(),
            "Matrix index out of bounds"
        );
        &mut self.matrix[computed_index]
    }
}

struct CNFFormula {
    variables: usize,
    added_clauses: usize,
    clauses: Vec<Clause>,
    matrix: Matrix,
    marks: Vec<bool>,
}

impl CNFFormula {
    fn new() -> Self {
        CNFFormula {
            variables: 0,
            added_clauses: 0,
            clauses: Vec::new(),
            matrix: Matrix::new(),
            marks: Vec::new(),
        }
    }

    fn add_clause(&mut self, clause: Vec<i32>, _verbosity: i32) {
        LOG!(_verbosity, "adding clause: {:?}", clause);
        let new_clause = Clause {
            garbage: false,
            literals: clause,
        };
        self.added_clauses += 1;
        self.clauses.push(new_clause);
    }

    fn connect_lit(&mut self, lit: i32, clause_id: usize, _verbosity: i32) {
        LOG!(
            _verbosity,
            "connecting literal {} to clause {}",
            lit,
            clause_id
        );
        self.matrix[lit].push(clause_id);
    }

    fn connect_clause(&mut self, clause_id: usize, _verbosity: i32) {
        LOG!(_verbosity, "connecting clause {}", clause_id);
        let clause = &self.clauses[clause_id].clone();
        for &lit in &clause.literals {
            self.connect_lit(lit, clause_id, _verbosity);
        }
    }

    fn collect_garbage_clauses(&mut self, _verbosity: i32) {
        let mut new_clauses = Vec::new();
        for clause in &self.clauses {
            if !clause.garbage {
                new_clauses.push(clause.clone());
            }
        }
        LOG!(
            _verbosity,
            "collected garbage: {} clauses",
            self.clauses.len() - new_clauses.len()
        );
        self.clauses = new_clauses;
    }
}

struct SATContext {
    config: Config,
    formula: CNFFormula,
    stats: Stats,
}

impl SATContext {
    fn new(config: Config) -> Self {
        SATContext {
            config,
            formula: CNFFormula::new(),
            stats: Stats {
                checked: 0,
                parsed: 0,
                subsumed: 0,
                start_time: Instant::now(),
            },
        }
    }
}

fn report_stats(ctx: &mut SATContext) {
    let elapsed_time = ctx.stats.start_time.elapsed().as_secs_f64();
    message!(
        ctx.config.verbosity,
        "{:<20} {:>10}    clauses {:.2} per subsumed",
        "checked:",
        ctx.stats.checked,
        average(ctx.stats.subsumed, ctx.stats.subsumed)
    );
    message!(
        ctx.config.verbosity,
        "{:<20} {:>10}    clauses {:.0}%",
        "subsumed:",
        ctx.stats.subsumed,
        percent(ctx.stats.subsumed, ctx.stats.parsed)
    );
    message!(
        ctx.config.verbosity,
        "{:<20} {:13.2} seconds",
        "process-time:",
        elapsed_time
    );
}

fn compute_signature(ctx: &mut SATContext) -> u64 {
    verbose!(ctx.config.verbosity, 1, "computing hash-signature");
    let nonces = [
        71876167, 708592741, 1483128881, 907283241, 442951013, 537146759, 1366999021, 1854614941,
        647800535, 53523743, 783815875, 1643643143, 682599717, 291474505, 229233697, 1633529763,
    ];
    let mut hash: u64 = 0;

    for clause in &ctx.formula.clauses {
        let mut d: Vec<u32> = clause.literals.iter().map(|&lit| lit as u32).collect();
        d.sort_unstable();
        let mut tmp = (d.len() as u64 + 1).wrapping_mul(nonces[0]);
        let mut i = 1usize;

        for &ulit in &d {
            tmp = (tmp << 4) | (tmp >> 60);
            tmp = tmp.wrapping_add(ulit as u64);
            tmp = tmp.wrapping_mul(nonces[i]);
            i = (i + 1) % nonces.len();
        }

        hash = hash.wrapping_add(tmp);
    }

    hash
}

fn parse_cnf(input_path: String, ctx: &mut SATContext) -> io::Result<()> {
    let path = Path::new(&input_path);
    let input: Box<dyn Read> = if input_path == "<stdin>" {
        message!(ctx.config.verbosity, "reading from '<stdin>'");
        Box::new(io::stdin())
    } else {
        message!(ctx.config.verbosity, "reading from '{}'", input_path);
        let file = File::open(&input_path)?;
        if path.extension().unwrap() == "bz2" {
            LOG!(ctx.config.verbosity, "reading BZ2 compressed file");
            Box::new(BzDecoder::new(file))
        } else if path.extension().unwrap() == "gz" {
            LOG!(ctx.config.verbosity, "reading GZ compressed file");
            Box::new(GzDecoder::new(file))
        } else if path.extension().unwrap() == "xz" {
            LOG!(ctx.config.verbosity, "reading XZ compressed file");
            Box::new(XzDecoder::new(file))
        } else {
            LOG!(ctx.config.verbosity, "reading uncompressed file");
            Box::new(file)
        }
    };

    let reader = BufReader::new(input);
    let mut header_parsed = false;
    let mut clauses_count = 0;
    let mut line_number = 0;

    for line in reader.lines() {
        line_number += 1;
        let line = line?;
        if line.starts_with('c') {
            continue; // Skip comment lines
        }
        if line.starts_with("p cnf") {
            let parts: Vec<&str> = line.split_whitespace().collect();
            if parts.len() < 4 {
                parse_error!(ctx, "Invalid header format.", line_number);
            }
            ctx.formula.variables = parts[2].parse().unwrap_or_else(|_| {
                parse_error!(ctx, "Could not read number of variables.", line_number);
            });
            clauses_count = parts[3].parse().unwrap_or_else(|_| {
                parse_error!(ctx, "Could not read number of clauses.", line_number);
            });
            header_parsed = true;
            message!(
                ctx.config.verbosity,
                "parsed 'p cnf {} {}' header",
                ctx.formula.variables,
                clauses_count
            );
            ctx.formula
                .matrix
                .init(ctx.formula.variables, ctx.config.verbosity);
        } else if header_parsed {
            let clause: Vec<i32> = line
                .split_whitespace()
                .map(|num| {
                    num.parse().unwrap_or_else(|_| {
                        parse_error!(ctx, "Invalid literal format.", line_number);
                    })
                })
                .filter(|&x| x != 0)
                .collect();
            LOG!(ctx.config.verbosity, "parsed clause: {:?}", clause);
            ctx.formula.add_clause(clause, ctx.config.verbosity);
            ctx.stats.parsed += 1;
        } else {
            parse_error!(ctx, "CNF header not found.", line_number);
        }
    }
    if clauses_count != ctx.stats.parsed {
        parse_error!(
            ctx,
            format!(
                "Mismatch in declared and parsed clauses: expected {}, got {}",
                clauses_count, ctx.stats.parsed
            ),
            line_number
        );
    }
    verbose!(
        ctx.config.verbosity,
        1,
        "parsed {} clauses",
        ctx.stats.parsed
    );
    Ok(())
}

fn print(ctx: &mut SATContext) {
    let mut output: Box<dyn Write> = if ctx.config.output_path == "<stdout>" {
        Box::new(io::stdout())
    } else {
        match ctx.config.output_path.as_str() {
            path if path.ends_with(".bz2") => {
                let file = File::create(path).expect("Failed to create output file");
                Box::new(BzEncoder::new(file, bzip2::Compression::default()))
            }
            path if path.ends_with(".gz") => {
                let file = File::create(path).expect("Failed to create output file");
                Box::new(GzEncoder::new(file, flate2::Compression::default()))
            }
            path if path.ends_with(".xz") => {
                let file = File::create(path).expect("Failed to create output file");
                Box::new(XzEncoder::new(file, 6)) // Compression level set to 6
            }
            path => Box::new(File::create(path).expect("Failed to create output file")),
        }
    };

    writeln!(
        output,
        "p cnf {} {}",
        ctx.formula.variables,
        ctx.formula.clauses.len()
    )
    .expect("Failed to write CNF header");

    if ctx.config.sign {
        let signature = compute_signature(ctx);
        writeln!(output, "c signature: {}", signature).expect("Failed to write signature");
    }

    for clause in &ctx.formula.clauses {
        let literals = clause
            .literals
            .iter()
            .map(|lit| lit.to_string())
            .collect::<Vec<String>>()
            .join(" ");
        writeln!(output, "{} 0", literals).expect("Failed to write clause");
    }

    match output.flush() {
        Ok(_) => (),
        Err(e) => die!("Failed to flush output: {}", e),
    }
}

fn forward_subsumption(ctx: &mut SATContext) {
    verbose!(ctx.config.verbosity, 1, "starting forward subsumption");
}

fn backward_subsumption(ctx: &mut SATContext) {
    verbose!(ctx.config.verbosity, 1, "starting backward subsumption");
}

fn simplify(ctx: &mut SATContext) {
    verbose!(ctx.config.verbosity, 1, "starting to simplify formula");
    if ctx.config.forward_mode {
        forward_subsumption(ctx);
    } else {
        backward_subsumption(ctx);
    }
    verbose!(ctx.config.verbosity, 1, "simplification complete");
    ctx.formula.collect_garbage_clauses(ctx.config.verbosity);
}

fn parse_arguments() -> Config {
    let app = Command::new("BabySub")
        .version("1.0")
        .author("Bernhard Gstrein")
        .about("Processes and simplifies logical formulae in DIMACS CNF format.")
        .arg(
            Arg::new("input")
                .help("Sets the input file to use")
                .index(1),
        )
        .arg(
            Arg::new("output")
                .help("Sets the output file to use")
                .index(2),
        )
        .arg(
            Arg::new("verbosity")
                .short('v')
                .action(ArgAction::Count)
                .help("Increases verbosity level"),
        )
        .arg(Arg::new("quiet").short('q').help("Suppresses all output"))
        .arg(
            Arg::new("forward-mode")
                .short('f')
                .help("Enables forward subsumption"),
        )
        .arg(
            Arg::new("backward-mode")
                .short('b')
                .help("Enables backward subsumption"),
        )
        .arg(
            Arg::new("sign")
                .short('s')
                .help("Computes and adds a hash signature to the output"),
        );

    #[cfg(feature = "logging")]
    let app = app.arg(
        Arg::new("logging")
            .short('l')
            .help("Enables detailed logging for debugging")
            .action(ArgAction::SetTrue),
    );

    let matches = app.get_matches();

    #[cfg(not(feature = "logging"))]
    let verbosity = if matches.is_present("quiet") {
        -1
    } else {
        *matches.get_one::<u8>("verbosity").unwrap_or(&0) as i32
    };

    #[cfg(feature = "logging")]
    let verbosity = if matches.is_present("quiet") {
        -1
    } else if matches.get_flag("logging") {
        999
    } else {
        *matches.get_one::<u8>("verbosity").unwrap_or(&0) as i32
    };

    if matches.is_present("forward-mode") && matches.is_present("backward-mode") {
        die!("Cannot enable both forward and backward subsumption");
    }

    Config {
        input_path: matches.value_of("input").unwrap_or("<stdin>").to_string(),
        output_path: matches.value_of("output").unwrap_or("<stdout>").to_string(),
        verbosity,
        forward_mode: matches.is_present("forward-mode"),
        sign: matches.is_present("sign"),
    }
}

fn setup_context(config: Config) -> SATContext {
    let ctx = SATContext::new(config);
    message!(ctx.config.verbosity, "BabySub Subsumption Preprocessor");
    ctx
}

fn main() {
    let config = parse_arguments();
    let mut ctx = setup_context(config);

    if let Err(e) = parse_cnf(ctx.config.input_path.clone(), &mut ctx) {
        die!("Failed to parse CNF: {}", e);
    }

    simplify(&mut ctx);
    print(&mut ctx);
    report_stats(&mut ctx);
}
