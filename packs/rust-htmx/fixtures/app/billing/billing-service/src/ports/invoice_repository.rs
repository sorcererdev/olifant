use async_trait::async_trait;
use thiserror::Error;

use crate::models::Invoice;

#[async_trait]
pub trait InvoiceRepository: Send + Sync + 'static {
    async fn get_invoice(&self, id: String) -> Result<Option<Invoice>, RepositoryFailure>;

    async fn save(&self, invoice: Invoice) -> Result<(), RepositoryFailure>;
}

#[derive(Error, Debug, PartialEq)]
pub enum RepositoryFailure {
    #[error("Failed to get connection from pool")]
    FailedToGetConnectionFromPool,
    #[error("Something went wrong")]
    Unknown(String),
}
