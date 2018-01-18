extern crate ecma335;
#[macro_use]
extern crate lazy_static;

use std::path::{ PathBuf };
use std::env;
use std::fs;
use std::process::Command;


use ecma335::reader::parse_module;
use ecma335::tables::*;

lazy_static! {
	static ref ASSETS_PATH: PathBuf = init_assets();
}

fn init_assets() -> PathBuf {
	let mut path = env::current_dir().unwrap();
	path.push("test-assets");

	if path.is_dir() {
		fs::remove_dir_all(&path).unwrap();
	}

	fs::create_dir(&path).unwrap();

	//compile all C# sources
	Command::new("csc")
		.args(&["tests/sample.cs", "/debug:portable", "/target:library", "/out:test-assets/sample.dll"])
		.status()
		.expect("failed to compile sample.cs");

	path
}

#[test]
fn can_decode_pdb_file() {
    let m = parse_module(&ASSETS_PATH.join("sample.pdb"));
	assert_eq!(true, m.is_ok());
}

#[test]
fn can_decode_main_file() {
    let m = parse_module(&ASSETS_PATH.join("sample.dll"));
	assert_eq!(true, m.is_ok());
}

#[test]
fn decode_document_name() {
	let m = parse_module(&ASSETS_PATH.join("sample.pdb")).unwrap();

	assert_eq! (1, m.row_count::<Document>());

	let doc = m.read_row::<Document>(0).unwrap();
	let name = doc.decode_name(&m).unwrap();
	assert_eq!(true, name.ends_with("tests/sample.cs"));
}

#[test]
fn decode_module_mvid() {
    let module = parse_module(&ASSETS_PATH.join("sample.dll")).unwrap();
	let main_module = module.read_row::<Module>(0).unwrap();
	let mvid = main_module.get_mvid(&module).unwrap();
	assert_eq!(36, mvid.len());
}

#[test]
fn decode_method_names() {
    let module = parse_module(&ASSETS_PATH.join("sample.dll")).unwrap();
	let mut found_method_a = false;
	let mut found_method_b = false;

	for i in 0..module.row_count::<MethodDef>() {
		let m = module.read_row::<MethodDef>(i).unwrap();
		match m.get_name(&module).unwrap() {
			"MethodA" => found_method_a = true,
			"MethodB" => found_method_b = true,
			_ => {}
		};
	}

	assert_eq!(true, found_method_a);
	assert_eq!(true, found_method_b);
}

