use crc32fast;

/// https://github.com/Workiva/eva/blob/master/core/java-src/eva/ByteString.java#L32
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct ByteString {
    bytes: Vec<u8>,
    crc32: u32,
}

impl ByteString {
    pub fn from(bytes: Vec<u8>) -> ByteString {
        let crc32 = crc32fast::hash(&bytes);
        ByteString { bytes, crc32 }
    }

    pub fn to_vec_of_bytes(&self) -> Vec<u8> {
        self.bytes.clone()
    }

    pub fn checksum(&self) -> u32 {
        self.crc32
    }
}
