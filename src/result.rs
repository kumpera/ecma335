use std;

#[derive(Debug)]
pub enum Error {
	IoError(std::io::Error),
	EncodingError(std::string::FromUtf8Error),
	StrError(std::str::Utf8Error),
	CliEncoding(String),
	Misc(String)
}

pub type Result<T> = std::result::Result<T, Error>;

impl From<std::io::Error> for Error {
    fn from(error: std::io::Error) -> Self {
        Error::IoError(error)
    }
}

impl From<std::string::FromUtf8Error> for Error {
    fn from(error: std::string::FromUtf8Error) -> Self {
        Error::EncodingError(error)
    }
}

impl From<std::str::Utf8Error> for Error {
    fn from(error: std::str::Utf8Error) -> Self {
        Error::StrError(error)
    }
}
