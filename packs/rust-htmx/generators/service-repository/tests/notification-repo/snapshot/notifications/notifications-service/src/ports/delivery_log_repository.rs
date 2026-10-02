use async_trait::async_trait;
use thiserror::Error;

use crate::models::DeliveryLog;

#[async_trait]
pub trait DeliveryLogRepository: Send + Sync + 'static {
    async fn get_delivery_log(&self, id: String) -> Result<Option<DeliveryLog>, RepositoryFailure>;

    async fn save(&self, delivery_log: DeliveryLog) -> Result<(), RepositoryFailure>;
}

#[derive(Error, Debug, PartialEq)]
pub enum RepositoryFailure {
    #[error("Failed to get connection from pool")]
    FailedToGetConnectionFromPool,
    #[error("Something went wrong")]
    Unknown(String),
}
