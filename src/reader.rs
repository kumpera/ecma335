extern crate byteorder;

use self::byteorder::{LittleEndian, ReadBytesExt};

use result::*;
use tables::*;

use std::path::Path;
use std::fs::File;
use std::io::Read;
use std::io::Cursor;

#[derive(Debug)]
struct RvaAndSize {
	rva: u32,
	size: u32
}

#[derive(Debug)]
struct TranslationEntry {
	virtual_base: u32,
	virtual_limit: u32,
	biased_offset: i32,
}

#[allow(dead_code)]
#[derive(Debug)]
struct PeHeader {
	machine: u16,
	sections: u16,
	timestamp: u32,
	symbol_table: u32,
	symbol_count: u32,
	optional_header_size: u16,
	characteristics: u16
}

#[allow(dead_code)]
#[derive(Debug)]
struct PeOptionalHeaderStandardFields {
	magic: u16,
	lmajor: u8,
	lminor: u8,
	code_size: u32,
	init_data_size: u32,
	uninit_data_size: u32,
	entry_point_rva: u32,
	base_of_code: u32,
	base_of_data: u32
}

#[allow(dead_code)]
#[derive(Debug)]
struct PeOptionalHeaderNtFields {
	image_base: u32,
	section_alignment: u32,
	file_alignment: u32,
	os_major : u16,
	os_minor: u16,
	user_major: u16,
	user_minor: u16,
	sub_sys_major: u16,
	sub_sys_minor: u16,
	reserved: u32,
	image_size: u32,
	header_size: u32,
	file_checksum: u32,
	sub_system: u16,
	dll_flags: u16,
	stack_reserve: u32,
	stack_commit: u32,
	heap_reserve: u32,
	heap_commit: u32,
	loader_flags: u32,
	data_directories_count: u32
}

#[allow(dead_code)]
#[derive(Debug)]
struct PeOptionalHeaderDataDirectory {
	export_table: RvaAndSize,
	import_table: RvaAndSize,
	resource_table: RvaAndSize,
	exception_table: RvaAndSize,
	certificate_table: RvaAndSize,
	base_relocation_table: RvaAndSize,
	debug: RvaAndSize,
	copyright: RvaAndSize,
	global_ptr: RvaAndSize,
	tls_table: RvaAndSize,
	load_config_table: RvaAndSize,
	bound_import_table: RvaAndSize,
	import_address_table: RvaAndSize,
	delay_import_descriptor: RvaAndSize,
	cli_header: RvaAndSize,
	reserved: RvaAndSize,
}

#[allow(dead_code)]
#[derive(Debug)]
struct PeOptionalHeader {
	std_fields: PeOptionalHeaderStandardFields,
	nt_fields: PeOptionalHeaderNtFields,
	data_directory: PeOptionalHeaderDataDirectory
}

#[allow(dead_code)]
#[derive(Debug)]
struct CliSection {
	name: String,
	virtual_size: u32,
	virtual_address: u32,
	raw_data_size: u32,
	raw_data_offset: u32,
	relocations_offset: u32,
	line_numbers_offset: u32,
	relocations_count: u16,
	line_numbers_count: u16,
	characteristics: u32,
}

#[allow(dead_code)]
#[derive(Debug)]
struct CliHeader {
	size: u32,
	major_version: u16,
	minor_version: u16,
	metadata: RvaAndSize,
	flags: u32,
	entry_point: u32,
	resources: RvaAndSize,
	strong_name: RvaAndSize,
	code_manager: RvaAndSize,
	vtable_fixus: RvaAndSize,
	export_address_table: RvaAndSize,
	managed_native_header: RvaAndSize,
}

#[derive(Debug)]
pub struct CliModule {
	data: Vec<u8>,
	mr: MetadataRoot,
	tables: TableData,
}

#[derive(Debug)]
struct Stream {
	offset: usize,
	size: usize,
	name: String
}

#[derive(Debug)]
struct MetadataRoot {
	sig: u32,
	major: u16,
	minor: u16,
	version: String,
	streams: Vec<Stream>
}

type Blob<'a> = Cursor<&'a [u8]>;

fn align_up(val: u64, align: u64) -> u64 {
	(val + (align - 1)) & !(align - 1)
}

impl MetadataRoot {
	fn stream_index (&self, name: &str) -> Result<u64> {
		for s in &self.streams {
			if s.name == name {
				return Ok(s.offset as u64);
			}
		}

		Err(Error::Misc(format!("Image doesn't include a {} section", name)))
	}

	fn stream_slice<'a> (&self, data: &'a[u8], name: &str) -> Result<&'a[u8]> {
		for s in &self.streams {
			if s.name == name {
				return Ok (&data[s.offset .. s.offset + s.size]);
			}
		}

		Err(Error::Misc(format!("Image doesn't include a {} section", name)))
	}

}

fn read_fixed_size_string(data: &mut Blob, size: usize) -> Result<String> {
	let mut vec = Vec::new();
	vec.resize(size, 0);

	data.read_exact(&mut vec[..])?;

	for i in 0..8 {
		if vec [i] == 0 {
			vec.truncate(i);
			break;
		}
	}

	Ok(String::from_utf8(vec)?)
}

fn read_embedded_string(data: &mut Blob) -> Result<String> {
	let len = data.read_u32::<LittleEndian>()?;
	let end_pos = align_up(data.position() + (len as u64), 4);

	let mut vec: Vec<u8> = Vec::new();

	for b in data.bytes() {
		let b = b?;
		if b == 0 {
			break;
		}
		vec.push(b);
	}

	data.set_position(end_pos);

	Ok(String::from_utf8(vec)?)
}

fn read_aligned_string(data: &mut Blob) -> Result<String> {
	let mut vec: Vec<u8> = Vec::new();

	for b in data.bytes() {
		let b = b?;
		if b == 0 {
			break;
		}
		vec.push(b);
	}
	let aligned_pos = align_up(data.position(), 4);
	data.set_position(aligned_pos);

	Ok(String::from_utf8(vec)?)
}


fn read_root(data: &mut Blob) -> Result<MetadataRoot> {
	let cli_offset = data.position() as usize;
	let sig = data.read_u32::<LittleEndian>()?;
	let major = data.read_u16::<LittleEndian>()?;
	let minor = data.read_u16::<LittleEndian>()?;
	let _reserved = data.read_u32::<LittleEndian>()?;
	let version = read_embedded_string(data)?;
	let _flags = data.read_u16::<LittleEndian>()?;

	if sig != 0x424A5342 {
		return Err(Error::Misc(format!("Image has invalid signature 0x{:X}", sig)));
	}

	let stream_count = data.read_u16::<LittleEndian>()?;
	let mut streams = Vec::with_capacity(stream_count as usize);

	for _ in 0..stream_count {
		let offset = cli_offset + data.read_u32::<LittleEndian>()? as usize;
		let size = data.read_u32::<LittleEndian>()? as usize;
		let name = read_aligned_string(data)?;
		streams.push(Stream { offset, size, name });
	}

	Ok(MetadataRoot { sig, major, minor, version, streams })
}

fn read_tables(data: &mut Blob) -> Result<TableData> {
	let _reserved = data.read_u32::<LittleEndian>()?;
	let major = data.read_u8()?;
	let minor = data.read_u8()?;
	let heap_sizes = data.read_u8()?;
	let _reserved2 = data.read_u8()?;
	let valid = data.read_u64::<LittleEndian>()?;
	let sorted = data.read_u64::<LittleEndian>()?;
	
	if major != 2 || minor != 0 {
		return Err(Error::Misc(format!("Invalid #~ version {}.{} (expected 1.0)", major, minor)));
	}

	decode_table_data(data, heap_sizes, sorted, valid)
}

fn parse_pe_header(data: &mut Blob) -> Result<PeHeader> {
	Ok(PeHeader {
		machine: data.read_u16::<LittleEndian>()?,
		sections: data.read_u16::<LittleEndian>()?,
		timestamp: data.read_u32::<LittleEndian>()?,
		symbol_table: data.read_u32::<LittleEndian>()?,
		symbol_count: data.read_u32::<LittleEndian>()?,
		optional_header_size: data.read_u16::<LittleEndian>()?,
		characteristics: data.read_u16::<LittleEndian>()?
	})
}

fn parse_pe_optional_header(data: &mut Blob) -> Result<PeOptionalHeader> {
	Ok(PeOptionalHeader {
		std_fields: parse_pe_optional_header_std_fields(data)?,
		nt_fields: parse_pe_optional_header_nt_fields(data)?,
		data_directory: parse_pe_optional_header_data_directory(data)?,
	})
}

fn parse_pe_optional_header_std_fields(data: &mut Blob) -> Result<PeOptionalHeaderStandardFields> {
	Ok(PeOptionalHeaderStandardFields {
		magic: data.read_u16::<LittleEndian>()?,
		lmajor: data.read_u8()?,
		lminor: data.read_u8()?,
		code_size: data.read_u32::<LittleEndian>()?,
		init_data_size: data.read_u32::<LittleEndian>()?,
		uninit_data_size: data.read_u32::<LittleEndian>()?,
		entry_point_rva: data.read_u32::<LittleEndian>()?,
		base_of_code: data.read_u32::<LittleEndian>()?,
		base_of_data: data.read_u32::<LittleEndian>()?
	})
}

fn parse_pe_optional_header_nt_fields(data: &mut Blob) -> Result<PeOptionalHeaderNtFields> {
	Ok(PeOptionalHeaderNtFields {
		image_base: data.read_u32::<LittleEndian>()?,
		section_alignment: data.read_u32::<LittleEndian>()?,
		file_alignment: data.read_u32::<LittleEndian>()?,
		os_major : data.read_u16::<LittleEndian>()?,
		os_minor: data.read_u16::<LittleEndian>()?,
		user_major: data.read_u16::<LittleEndian>()?,
		user_minor: data.read_u16::<LittleEndian>()?,
		sub_sys_major: data.read_u16::<LittleEndian>()?,
		sub_sys_minor: data.read_u16::<LittleEndian>()?,
		reserved: data.read_u32::<LittleEndian>()?,
		image_size: data.read_u32::<LittleEndian>()?,
		header_size: data.read_u32::<LittleEndian>()?,
		file_checksum: data.read_u32::<LittleEndian>()?,
		sub_system: data.read_u16::<LittleEndian>()?,
		dll_flags: data.read_u16::<LittleEndian>()?,
		stack_reserve: data.read_u32::<LittleEndian>()?,
		stack_commit: data.read_u32::<LittleEndian>()?,
		heap_reserve: data.read_u32::<LittleEndian>()?,
		heap_commit: data.read_u32::<LittleEndian>()?,
		loader_flags: data.read_u32::<LittleEndian>()?,
		data_directories_count: data.read_u32::<LittleEndian>()?
	})
}

fn parse_rva_and_size(data: &mut Blob) -> Result<RvaAndSize> {
	Ok(RvaAndSize{
		rva: data.read_u32::<LittleEndian>()?,
		size: data.read_u32::<LittleEndian>()?,
	})
}

fn parse_pe_optional_header_data_directory(data: &mut Blob) -> Result<PeOptionalHeaderDataDirectory> {
	Ok(PeOptionalHeaderDataDirectory {
		export_table: parse_rva_and_size(data)?,
		import_table: parse_rva_and_size(data)?,
		resource_table: parse_rva_and_size(data)?,
		exception_table: parse_rva_and_size(data)?,
		certificate_table: parse_rva_and_size(data)?,
		base_relocation_table: parse_rva_and_size(data)?,
		debug: parse_rva_and_size(data)?,
		copyright: parse_rva_and_size(data)?,
		global_ptr: parse_rva_and_size(data)?,
		tls_table: parse_rva_and_size(data)?,
		load_config_table: parse_rva_and_size(data)?,
		bound_import_table: parse_rva_and_size(data)?,
		import_address_table: parse_rva_and_size(data)?,
		delay_import_descriptor: parse_rva_and_size(data)?,
		cli_header: parse_rva_and_size(data)?,
		reserved: parse_rva_and_size(data)?,
	})
}

fn parse_cli_sections(data: &mut Blob, count: u32) -> Result<Vec<CliSection>> {
	let mut res: Vec<CliSection> = Vec::new ();

	for _ in 0..count {
		res.push(CliSection {
			name: read_fixed_size_string(data, 8)?, //FIXME, this injects null chars at the end :/
			virtual_size: data.read_u32::<LittleEndian>()?,
			virtual_address: data.read_u32::<LittleEndian>()?,
			raw_data_size: data.read_u32::<LittleEndian>()?,
			raw_data_offset: data.read_u32::<LittleEndian>()?,
			relocations_offset: data.read_u32::<LittleEndian>()?,
			line_numbers_offset: data.read_u32::<LittleEndian>()?,
			relocations_count: data.read_u16::<LittleEndian>()?,
			line_numbers_count: data.read_u16::<LittleEndian>()?,
			characteristics: data.read_u32::<LittleEndian>()?,
		});
	}

	Ok(res)
}

fn build_translation_table(sections: &Vec<CliSection>) -> Vec<TranslationEntry>
{
	let mut tt = Vec::new();
	for s in sections {
		tt.push(TranslationEntry {
			virtual_base: s.virtual_address,
			virtual_limit: s.virtual_address + s.raw_data_size,
			biased_offset: (s.raw_data_offset as i32) - (s.virtual_address as i32),
		});
	}

	tt
}

fn rva_to_offset(tt: &Vec<TranslationEntry>, rva: u32) -> Result<u32> {
	for s in tt {
		if s.virtual_base <= rva && s.virtual_limit > rva {
			return Ok((rva as i32 + s.biased_offset) as u32);
		}
	}

	Err(Error::Misc(format!("Could not translate rva {:X}", rva)))
}

fn parse_cli_header(data: &mut Blob, offset: u32) -> Result<CliHeader> {
	data.set_position(offset as u64);

	Ok(CliHeader {
		size: data.read_u32::<LittleEndian>()?,
		major_version: data.read_u16::<LittleEndian>()?,
		minor_version: data.read_u16::<LittleEndian>()?,
		metadata: parse_rva_and_size(data)?,
		flags: data.read_u32::<LittleEndian>()?,
		entry_point: data.read_u32::<LittleEndian>()?,
		resources: parse_rva_and_size(data)?,
		strong_name: parse_rva_and_size(data)?,
		code_manager: parse_rva_and_size(data)?,
		vtable_fixus: parse_rva_and_size(data)?,
		export_address_table: parse_rva_and_size(data)?,
		managed_native_header: parse_rva_and_size(data)?,
	})
}

fn read_pe_headers(data: &mut Blob) -> Result<u32> {
	//skip msdos header
	data.set_position(128);

	//PE header
	let pe_sig = data.read_u32::<LittleEndian>()?;

	if pe_sig != 0x4550 {
		return Err(Error::Misc(format!("Invalid PE signature : {:X} (expected 0x00004550)", pe_sig)));
	}

	let pe_header = parse_pe_header(data)?;
	let pe_opt_header = parse_pe_optional_header(data)?;
	let cli_sections = parse_cli_sections(data, pe_header.sections as u32)?;

	let translations = build_translation_table(&cli_sections);
	let cli_header_offset = rva_to_offset(&translations, pe_opt_header.data_directory.cli_header.rva)?;
	let cli_header = parse_cli_header(data, cli_header_offset)?;

	rva_to_offset(&translations, cli_header.metadata.rva)
}


pub fn parse_module(path: &Path) -> Result<CliModule>
{
	let mut data: Vec<u8> = Vec::new();
	File::open(path)?.read_to_end(&mut data)?;

	let (mr, tables) = {
		let mut cursor: Blob = Cursor::new(&data[..]);

		if data[0..4] == [ 0x4d, 0x5a, 0x90, 0x00 ] {
			let offset = read_pe_headers(&mut cursor)?;
			cursor.set_position(offset as u64);
		}

		let mr = read_root(&mut cursor)?;

		cursor.set_position(mr.stream_index("#~")?);
		let tables = read_tables(&mut cursor)?;

		(mr, tables)
	};

	Ok(CliModule { data, mr, tables })
}

impl CliModule {
	pub fn row_count<T: TableMeta>(&self) -> u32 {
		self.tables.row_count::<T>()
	}

	pub fn read_row<T: TableMeta>(&self, row: u32) -> Result<T> {
		self.tables.read_row::<T>(&self.data, row)
	}

	pub fn stream_slice(&self, name: &str) -> Result<&[u8]> {
		self.mr.stream_slice(&self.data[..], name)
	}
}
