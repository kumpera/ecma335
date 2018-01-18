extern crate byteorder;

use result::*;
use reader::CliModule;

use std::default::Default;
use std::io::Cursor;
use std::io::Read;
use std::fmt;
use std::str;
use std::marker::Sized;

use self::byteorder::{LittleEndian, ReadBytesExt};

macro_rules! def_table_list(
	($($table_name:ident = $table_code:expr),+) => (

	#[derive(Debug, PartialEq, Copy, Clone)]
	pub enum Table {
		Invalid					= 0xFF,
		$( $table_name = $table_code),+
	}

	fn get_row_size(header: &TableDecodingData, table: u32) -> u8 {
		match table {
			$( $table_code => return $table_name::get_row_size (header)),+
			,
			_ => panic! ("Invalid table id {}", table)
		};
	}

	fn get_decode_cookie(header: &TableDecodingData, table: u32) -> u32 {
		match table {
			$( $table_code => return $table_name::get_decode_cookie(header)),+
			,
			_ => panic! ("Invalid table id {}", table)
		};
	}

	fn table_from_i32(idx: i32) -> Option<Table> {
		match idx {
			0xFF => Some(Table::Invalid),
			$( $table_code => Some(Table::$table_name)),+
			,
			_ => None
		}
	}
));

macro_rules! def_table {
	($name:ident $($f_name:ident : $f_type:ident),+) => (
		#[allow(dead_code)]
		#[derive(Debug)]
		pub struct $name {
			$(
				pub $f_name : u32,
			)+
		}

		#[allow(dead_code)]
		impl Row for $name {
			type underlying_type = RowIndex;

			fn get_col_size(header: &TableDecodingData) -> u8 {
				if header.sizes[Table::$name as usize] >= (1 << 16) {
					4
				} else {
					2
				}
			}
		}

		#[allow(dead_code)]
		impl TableMeta for $name {
			fn table_id() -> Table {
				Table::$name
			}

			fn read_from(cursor: &mut Blob, decode_cookie: u32) -> Result<$name> {
				let mut col_shift: u32 = 0;
		
				Ok($name {
					$(
						$f_name: decode_col (cursor, &mut col_shift, decode_cookie)?,
					)+
				})
			}
		}

		impl $name {
			fn get_row_size(header: &TableDecodingData) -> u8 {
				let mut res = 0;
				$(
					res += $f_type :: get_col_size (header);
				)+
				return res;
			}

			#[allow(unused_assignments)]
			fn get_decode_cookie(header: &TableDecodingData) -> u32 {
				let mut shift: u16 = 0;
				let mut res = 0;
				$(
					res |= size_to_cookie($f_type:: get_col_size(header), shift);
					shift += 2;
				)+;

				res
			}
		}
	);
}

macro_rules! def_coded_token {
	($name:ident $bits:expr => $($table:ident),+) => (
		#[allow(dead_code)]
		struct $name;

		#[allow(dead_code)]
		impl Row for $name {
			type underlying_type = CodedToken;

			fn get_col_size(header: &TableDecodingData) -> u8 {
				if calc_coded_token_size(header, 1 << (16 - $bits),
					&[$(Table::$table as u8),+])
				{ 4 }  else { 2 }
			}
		}
	);
}

struct U32;
struct U16;
struct U8;
struct BlobIndex;
struct GuidIndex;
struct StringIndex;
type CodedToken = u32;
type RowIndex = u32;

/*
TODO
underlying_type
	StringIndex could be lazier or a &str
	Blog/Guid could be something that makes decoding easier
	Do Something about coded tokens and row indexes?

decoding in general:
	perhaps rows should have a Module& field to make their API more decent to use?

*/
trait Row {
	type underlying_type;
	fn get_col_size (header: &TableDecodingData) -> u8;
}

#[allow(unused_variables)]
#[allow(dead_code)]
impl Row for U32 {
	type underlying_type = u32;
	fn get_col_size (header: &TableDecodingData) -> u8 { 4 }
}

#[allow(unused_variables)]
#[allow(dead_code)]
impl Row for U16 {
	type underlying_type = u16;
	fn get_col_size (header: &TableDecodingData) -> u8 { 2 }
}

#[allow(unused_variables)]
#[allow(dead_code)]
impl Row for U8 {
	type underlying_type = u8;
	fn get_col_size(header: &TableDecodingData) -> u8 { 1 }
}

#[allow(unused_variables)]
#[allow(dead_code)]
impl Row for StringIndex {
	type underlying_type = String;

	fn get_col_size(header: &TableDecodingData) -> u8 {
		return if header.heap_sizes & 0x1 == 0x1 { 4 } else { 2 };
	}
}

#[allow(unused_variables)]
#[allow(dead_code)]
impl Row for GuidIndex {
	type underlying_type = u32;

	fn get_col_size(header: &TableDecodingData) -> u8 {
		return if header.heap_sizes & 0x2 == 0x2 { 4 } else { 2 };
	}
}

#[allow(unused_variables)]
#[allow(dead_code)]
impl Row for BlobIndex {
	type underlying_type = u32;

	fn get_col_size(header: &TableDecodingData) -> u8 {
		return if header.heap_sizes & 0x4 == 0x4 { 4 } else { 2 };
	}
}

fn calc_coded_token_size(header: &TableDecodingData, max_table_size: u32, tables: &[u8]) -> bool {
	for t in tables.iter() {
		let tb : Table = table_from_i32(*t as i32).unwrap();
		if tb != Table::Invalid && header.sizes[*t as usize] >= max_table_size {
			return true;
		}
	}
	return false;
}

const TABLE_COUNT : usize = 0x38;

def_table_list! (
	Module					= 0x00,
	TypeRef					= 0x01,
	TypeDef					= 0x02,
	Field					= 0x04,
	MethodDef				= 0x06,
	Param					= 0x08,
	InterfaceImpl			= 0x09,
	MemberRef				= 0x0A,
	Constant				= 0x0B,
	CustomAttribute			= 0x0C,
	FieldMarshal			= 0x0D,
	DeclSecurity			= 0x0E,
	ClassLayout				= 0x0F,

	FieldLayout				= 0x10,
	StandAloneSig			= 0x11,
	EventMap				= 0x12,
	Event					= 0x14,
	PropertyMap				= 0x15,
	Property 				= 0x17,
	MethodSemantics			= 0x18,
	MethodImpl				= 0x19,
	ModuleRef				= 0x1A,
	TypeSpec				= 0x1B,
	ImplMap					= 0x1C,
	FieldRVA				= 0x1D,

	Assembly				= 0x20,
	AssemblyProcessor		= 0x21,
	AssemblyOS				= 0x22,
	AssemblyRef				= 0x23,
	AssemblyRefProcessor	= 0x24,
	AssemblyRefOS			= 0x25,
	File					= 0x26,
	ExportedType			= 0x27,
	ManifestResource		= 0x28,
	NestedClass				= 0x29,
	GenericParam			= 0x2A,
	MethodSpec				= 0x2B,
	GenericParamConstraint	= 0x2C,

	Document				= 0x30,
	MethodBody				= 0x31,
	LocalScope				= 0x32,
	LocalVariable			= 0x33,
	LocalConstant			= 0x34,
	ImportScope				= 0x35,
	StateMachineMethod		= 0x36,
	CustomDebugInfo			= 0x37
);

def_table! (Module
	generation: U16,
	name: StringIndex, 
	mvid: GuidIndex,
	encid: GuidIndex,
	end_baseid: GuidIndex
);

def_table! (TypeRef
	resolution_scope: ResolutionScope,
	type_name: StringIndex,
	type_namespace: StringIndex
);

def_table! (TypeDef
	flags: U32,
	type_name: StringIndex,
	type_namespace: StringIndex,
	extends: TypeDefOrRef,
	field_list: Field,
	method_list: MethodDef
);

def_table! (Field
	flags: U16,
	name: StringIndex,
	signature: BlobIndex
);

def_table! (MethodDef
	rva: U32,
	impl_flags: U16,
	flags: U16,
	name: StringIndex,
	signature: BlobIndex,
	param_list: Param
);

def_table! (Param
	flags: U16,
	sequence: U16,
	name: StringIndex
);

def_table! (InterfaceImpl
	class_def: TypeDef,
	interface_impl: TypeDefOrRef
);

def_table! (MemberRef
	parent: MemberRefParent,
	name: StringIndex,
	signature: BlobIndex
);

def_table! (Constant
	const_type: U8,
	padding: U8,
	parent: HasConstant,
	value: BlobIndex
);

def_table! (CustomAttribute
	parent: HasCustomAttribute,
	cattr_type: CustomAttributeType,
	value: BlobIndex
);

def_table! (FieldMarshal
	parent: HasFieldMarshall,
	native_type: BlobIndex
);

def_table! (DeclSecurity
	action: U16,
	parent: HasDeclSecurity,
	permission_set: BlobIndex
);

def_table! (ClassLayout
	packing_size: U16,
	class_size: U32,
	parent: TypeDef
);

def_table! (FieldLayout
	offset: U32,
	field: Field
);

def_table! (StandAloneSig
	signature: BlobIndex
);

def_table! (EventMap
	parent: TypeDef,
	event_list: Event
);

def_table! (Event
	flags: U16,
	name: StringIndex,
	event_type: TypeDefOrRef
);

def_table! (PropertyMap
	parent: TypeDef,
	property_list: Property
);

def_table! (Property
	flags: U16,
	name: StringIndex,
	signature: BlobIndex
);


def_table! (MethodSemantics
	semantics: U16,
	method: MethodDef,
	association: HasSemantics
);

def_table! (MethodImpl
	class: TypeDef,
	method_body: MethodDefOrRef,
	method_declaration: MethodDefOrRef
);

def_table! (ModuleRef
	name: StringIndex
);

def_table! (TypeSpec
	signature: BlobIndex
);

def_table! (ImplMap
	mapping_flags: U16,
	member_forwarded: MemberForwarded,
	import_name: StringIndex,
	import_scope: ModuleRef
);

def_table! (FieldRVA
	rva: U32,
	field: Field
);

def_table! (Assembly
	hash_algorithm_id: U32,
	major_version: U16,
	minor_version: U16,
	build_number: U16,
	revision_number: U16,
	flags: U32,
	public_key: BlobIndex,
	name: StringIndex,
	culture: StringIndex
);

def_table! (AssemblyProcessor
	processor: U32
);

def_table! (AssemblyOS
	os_platform_id: U32,
	os_major_version: U32,
	os_minor_version: U32
);

def_table! (AssemblyRef
	major_version: U16,
	minor_version: U16,
	build_number: U16,
	revision_number: U16,
	flags: U32,
	public_key: BlobIndex,
	name: StringIndex,
	culture: StringIndex,
	hash_value: BlobIndex
);

def_table! (AssemblyRefProcessor
	processor: U32,
	assembly_ref: AssemblyRef
);

def_table! (AssemblyRefOS
	os_platform_id: U32,
	os_major_version: U32,
	os_minor_version: U32,
	assembly_ref: AssemblyRef
);

def_table! (File
	flags: U32,
	name: StringIndex,
	hash_value: BlobIndex
);

def_table! (ExportedType
	flags: U32,
	typedef_id: U32,
	type_name: StringIndex,
	type_namespace: StringIndex,
	implementation: Implementation
);

def_table! (ManifestResource
	offset: U32,
	flags: U32,
	name: StringIndex,
	implementation: Implementation
);

def_table! (NestedClass
	nested_class: TypeDef,
	enclosing_class: TypeDef
);

def_table! (GenericParam
	number: U16,
	flags: U16,
	owner: TypeOrMethodDef,
	name: StringIndex
);

def_table! (MethodSpec
	method: MethodDefOrRef,
	instantiations: BlobIndex
);

def_table! (GenericParamConstraint
	owner: GenericParam,
	constraint: TypeDefOrRef
);

def_table! (Document
	name: BlobIndex,
	hash_algo: GuidIndex,
	hash: BlobIndex,
	language: GuidIndex
);

def_table! (MethodBody
	document: Document,
	sequence_points: BlobIndex
);

def_table! (LocalScope
	method: MethodDef,
	import_scope: ImportScope,
	variable_list: LocalVariable,
	constant_list: LocalConstant,
	start_offset: U32,
	length: U32
);

def_table! (LocalVariable
	attributes: U16,
	index: U16,
	name: StringIndex
);

def_table! (LocalConstant
	name: StringIndex,
	sig: BlobIndex
);

def_table! (ImportScope
	parent: ImportScope,
	import: BlobIndex
);

def_table! (StateMachineMethod
	move_next_method: MethodDef,
	kickoff_method: MethodDef
);

def_table! (CustomDebugInfo
	parent: HasCustomDebugInformation,
	kind: GuidIndex,
	value: BlobIndex
);

def_coded_token! (TypeDefOrRef 2 =>
	TypeDef,
	TypeRef,
	TypeSpec
);

def_coded_token! (HasConstant 2 =>
	Field,
	Param,
	Property
);

def_coded_token! (HasCustomAttribute 5 =>
	MethodDef,
	Field,
	TypeRef,
	TypeDef,
	Param,
	InterfaceImpl,
	MemberRef,
	Module,
	DeclSecurity,
	Property,
	Event,
	StandAloneSig,
	ModuleRef,
	TypeSpec,
	Assembly,
	AssemblyRef,
	File,
	ExportedType,
	ManifestResource,
	GenericParam,
	GenericParamConstraint,
	MethodSpec
);

def_coded_token! (HasFieldMarshall 1 =>
	Field,
	Param
);

def_coded_token! (HasDeclSecurity 2 =>
	TypeDef,
	MethodDef,
	Assembly
);

def_coded_token! (MemberRefParent 3 =>
	TypeDef,
	TypeRef,
	ModuleRef,
	MethodDef,
	TypeSpec
);

def_coded_token! (HasSemantics 1 =>
	Event,
	Property
);

def_coded_token! (MethodDefOrRef 1 =>
	MethodDef, 
	MemberRef
);

def_coded_token! (MemberForwarded 1 =>
	Field,
	MethodDef
);

def_coded_token! (Implementation 2 =>
	File, 
	AssemblyRef, 
	ExportedType
);

def_coded_token! (CustomAttributeType 3 =>
	Invalid, 
	Invalid, 
	MethodDef, 
	MemberRef,
	Invalid
);

def_coded_token! (ResolutionScope 2 =>
	Module,
	ModuleRef,
	AssemblyRef,
	TypeRef
);

def_coded_token! (TypeOrMethodDef 1 =>
	TypeDef,
	MethodDef
);

def_coded_token! (HasCustomDebugInformation 5 =>
	MethodDef,
	Field,
	TypeRef,
	TypeDef,
	Param,
	InterfaceImpl,
	MemberRef,
	Module,
	DeclSecurity,
	Property,
	Event,
	StandAloneSig,
	ModuleRef,
	TypeSpec,
	Assembly,
	AssemblyRef,
	File,
	ExportedType,
	ManifestResource,
	GenericParam,
	GenericParamConstraint,
	MethodSpec,
	Document,
	LocalScope,
	LocalVariable,
	LocalConstant,
	ImportScope
);

#[derive(Default,Debug,Copy,Clone)]
struct TableInfo {
	base_address: u32,
	rows: u32,
	decode_cookie: u32,
	row_size: u8
}

pub trait TableMeta where Self: Sized {
	fn table_id() -> Table;
	fn read_from(cursor: &mut Blob, decode_cookie: u32) -> Result<Self>;
}

#[allow(dead_code)]
pub struct TableData {
	table_data : [TableInfo; TABLE_COUNT]
}

impl TableData {
	pub fn row_count<T: TableMeta>(&self) -> u32 {
		let id = T::table_id();

		self.table_data[id as usize].rows
	}

	pub fn read_row<T: TableMeta>(&self, data: &[u8], row: u32) -> Result<T> {
		let info = &self.table_data[T::table_id() as usize];
		let base_idx: usize = (info.base_address + (info.row_size as u32 * row)) as usize;
		let row_base = &data[base_idx .. base_idx + info.row_size as usize];
		let mut cursor: Blob = Cursor::new(row_base);

		T::read_from(&mut cursor, info.decode_cookie)
	}
}

fn size_to_cookie(size :u8, pos: u16) -> u32 {
	let cookie = match size {
		1 => 0,
		2 => 1,
		4 => 2,
		_ => panic!("Bad size {}", size)
	};

	cookie << pos
}

fn decode_col(data: &mut Blob, col_shift: &mut u32, decode_cookie: u32) -> Result<u32> {
	let val = match (decode_cookie >> *col_shift) & 0x3 {
		0 => data.read_u8()? as u32,
		1 => data.read_u16::<LittleEndian>()? as u32,
		2 => data.read_u32::<LittleEndian>()?,
		other => panic!("Invalid decoding cookie {}", other)
	};
	*col_shift += 2;

	Ok(val)
}

fn read_compressed_u32_with_len<T: Read>(data: &mut T) -> Result<(u32, u32)> {
	let b0 = data.read_u8()? as u32;
	if b0 & 0x80 == 0 {
		Ok((b0, 1))
	} else if b0 & 0x40 == 0 {
		let b1 = data.read_u8()? as u32;
		let val = ((b0 & 0x3f) << 8) + b1;

		Ok((val, 2))
	} else {
		let b1 = data.read_u8()? as u32;
		let b2 = data.read_u8()? as u32;
		let b3 = data.read_u8()? as u32;
		let val = ((b0 & 0x1f) << 24) + (b1 << 16) + (b2 << 8) + b3;

		Ok((val, 4))
	}
}

fn read_compressed_u32<T: Read>(data: &mut T) -> Result<u32> {
	Ok(read_compressed_u32_with_len(data)?.0)
}

fn read_compressed_i32_with_len<T: Read>(data: &mut T) -> Result<(i32, u32)> {
	let (uval, len) = read_compressed_u32_with_len(data)?;
	let ival = (uval >> 1) as i32;
	if (uval & 1) == 0 {
		return Ok((ival, len));
	}

	if ival < 0x40 {
		return Ok((ival - 0x40, len));
	}

	if ival < 0x2000 {
		return Ok((ival - 0x2000, len));
	}

	if ival < 0x10000000 {
		return Ok((ival - 0x10000000, len));
	}

	if ival >= 0x20000000 {
		return Err(Error::CliEncoding(format!("Invalid encoded signed int {}", ival)));
	}

	Ok((ival - 0x20000000, len))
}

fn read_compressed_i32<T: Read>(data: &mut T) -> Result<i32> {
	Ok(read_compressed_i32_with_len(data)?.0)
}

fn get_blob_item(module: &CliModule, idx: u32) -> Result<&[u8]> {
	let blob_slice = module.stream_slice("#Blob")?;

	let idx_slice = &blob_slice [idx as usize .. ];

	let (size, len_size) = read_compressed_u32_with_len (&mut Cursor::new(idx_slice))?;

	Ok(&idx_slice [len_size as usize .. (len_size + size) as usize])
}

fn get_guid_item(module: &CliModule, idx: u32) -> Result<&[u8]> {
	let slice = module.stream_slice("#GUID")?;

	Ok(&slice [idx as usize .. (idx + 16) as usize])
}

fn get_string_item(module: &CliModule, idx: u32) -> Result<&[u8]> {
	let slice = module.stream_slice("#Strings")?;

	let start = &slice [idx as usize .. ];

	for i in 0..start.len() {
		if start[i] == 0 {
			return Ok(&start[0 .. i]);
		}
	}

	Ok(&start)
}



impl Document {
	pub fn decode_name(&self, module: &CliModule) -> Result<String> {
		let mut res = String::new();

		let item = get_blob_item(module, self.name)?;
		let len = item.len();
		let mut data: Blob = Cursor::new(item);

		//FIXME read 1 utf8 char
		let separator = data.read_u8()? as char;

		let mut segs = 0;

		while (data.position() as usize) < len {
			if segs > 0 {
				res.push(separator);
			}

			let segment = read_compressed_u32 (&mut data)?;
			if segment != 0 {
				res.push_str(str::from_utf8(get_blob_item(module, segment)?)?);
			}

			segs += 1;
		}

		Ok(res)
	}
}

#[derive(Debug)]
pub struct SeqPoint {
	pub hidden: bool,
	pub document: u32,
	pub il_offset: u32,
	pub start_line: u32,
	pub end_line: u32,
	pub start_col: u32,
	pub end_col: u32,
}

#[derive(Debug)]
pub struct SeqPointTable {
	pub local_sig: u32,
	pub seq_points: Vec<SeqPoint>
}

impl MethodBody {
	pub fn decode_sequence_points(&self, module: &CliModule) -> Result<SeqPointTable> {
		if self.sequence_points == 0 {
			return Ok(SeqPointTable{ local_sig: 0, seq_points: Vec::new() });
		}

		let mut sps: Vec<SeqPoint> = Vec::new();

		let item = get_blob_item(module, self.sequence_points)?;
		let len = item.len();
		let mut data: Blob = Cursor::new(item);

		let local_sig = read_compressed_u32(&mut data)?;
		let mut cur_doc = if self.document == 0 {
			read_compressed_u32(&mut data)?
		} else {
			self.document
		};

		let mut first_sp = true;
		let mut first_non_hidden = true;
		let mut il_offset: u32 = 0;
		let mut start_line = 0;
		let mut start_col = 0;

		while (data.position() as usize) < len {
			let delta_il = read_compressed_u32(&mut data)?;
			if !first_sp && delta_il == 0 {
				cur_doc = read_compressed_u32(&mut data)?;
				continue;
			}

			il_offset = if first_sp { delta_il } else { il_offset + delta_il };
			first_sp = false;

			let delta_line = read_compressed_u32(&mut data)? as i32;
			let delta_col = if delta_line == 0 {
				read_compressed_u32(&mut data)? as i32
			} else {
				read_compressed_i32(&mut data)?
			};

			//is it a hidden seq point?
			if delta_line == 0 && delta_col == 0 {
				/* hidden seq point */
				sps.push(SeqPoint{ 
					hidden: true,
					document: cur_doc,
					il_offset: il_offset,
					start_line: 0xfeefee,
					end_line: 0xfeefee,
					start_col: 0,
					end_col: 0
				});
				continue;
			}

			if first_non_hidden {
				start_line = read_compressed_u32(&mut data)? as i32;
				start_col = read_compressed_u32(&mut data)? as i32;
			} else {
				start_line += read_compressed_i32(&mut data)?;
				start_col += read_compressed_i32(&mut data)?;
			}

			first_non_hidden = false;

			sps.push(SeqPoint{ 
				hidden: false,
				document: cur_doc,
				il_offset: il_offset,
				start_line: start_line as u32,
				end_line: (start_line + delta_line) as u32,
				start_col: start_col as u32,
				end_col: (start_col + delta_col) as u32
			});
		}

		Ok(SeqPointTable{ local_sig, seq_points: sps })
	}

}

impl Module {
	pub fn get_mvid(&self, module: &CliModule) -> Result<String> {
		if self.mvid == 0 {
			return Ok(String::new());
		}

		let guid = get_guid_item(module, self.mvid - 1)?;

		Ok(format!("{:02X}{:02X}{:02X}{:02X}-{:02X}{:02X}-{:02X}{:02X}-{:02X}{:02X}-{:02X}{:02X}{:02X}{:02X}{:02X}{:02X}",
			guid[3], guid[2], guid[1], guid[0],
			guid[5], guid[4],
			guid[7], guid[6],
			guid[8], guid[9],
			guid[10], guid[11], guid[12], guid[13], guid[14], guid[15]
		))
	}
}

impl MethodDef {
	pub fn get_name<'a>(&self, module: &'a CliModule) -> Result<&'a str> {
		if self.name == 0 {
			return Ok("");
		}
		let raw = get_string_item(module, self.name)?;

		let res = str::from_utf8(raw)?;

		Ok(res)
	}
}

impl LocalVariable {
	pub fn get_name<'a>(&self, module: &'a CliModule) -> Result<&'a str> {
		if self.name == 0 {
			return Ok("");
		}
		let raw = get_string_item(module, self.name)?;

		let res = str::from_utf8(raw)?;

		Ok(res)
	}
}

impl fmt::Debug for TableData {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "TableData {{")?;
		for i in 0..TABLE_COUNT {
			if self.table_data[i].rows > 0 {
				write!(f, "{:?}, ", &self.table_data[i])?;
			}
		}

		write!(f, "}}") 
    }
}


struct TableDecodingData {
	heap_sizes: u8,
	valid: u64,
	#[allow(dead_code)]
	sorted: u64, //XXX this will be used later
	sizes: Vec<u32>
}

type Blob<'a> = Cursor<&'a [u8]>;

pub fn decode_table_data(data: &mut Blob, heap_sizes: u8, sorted: u64, valid: u64) -> Result<TableData> {

	let mut sizes: Vec<u32> = Vec::with_capacity(TABLE_COUNT);
	for i in 0..TABLE_COUNT {
		if valid & (1 << i) != 0 {
			sizes.push(data.read_u32::<LittleEndian>()?);
		} else {
			sizes.push(0);
		}
	}

	let decoding_data = TableDecodingData { heap_sizes, valid, sorted, sizes };

	let mut current_offset = data.position() as u32;
	let mut table_info: [TableInfo; TABLE_COUNT] = [Default::default(); TABLE_COUNT];

	for bit in 0..TABLE_COUNT {
		if decoding_data.valid & 1 << bit != 0 {
			let info = TableInfo {
				base_address: current_offset,
				rows: decoding_data.sizes[bit],
				row_size: get_row_size (&decoding_data, bit as u32),
				decode_cookie: get_decode_cookie (&decoding_data, bit as u32),
			};
			current_offset += info.rows * (info.row_size as u32);
			table_info [bit] = info;
		}
	}

	Ok(TableData { table_data: table_info })
}
