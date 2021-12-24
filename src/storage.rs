use crate::byte_string::ByteString;

pub struct StorageBlock {
    pub storage_namespace: String,
    pub storage_id: String,
    pub attributes: (),
    pub value: Option<ByteString>,
}
