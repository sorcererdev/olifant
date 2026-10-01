use async_trait::async_trait;
use thiserror::Error;

use crate::models::EmailTemplate;

#[async_trait]
pub trait EmailTemplateRepository: Send + Sync + 'static {
    async fn get_email_template(&self, id: String) -> Result<Option<EmailTemplate>, RepositoryFailure>;

    async fn save(&self, email_template: EmailTemplate) -> Result<(), RepositoryFailure>;
}

#[derive(Error, Debug, PartialEq)]
pub enum RepositoryFailure {
    #[error("Failed to get connection from pool")]
    FailedToGetConnectionFromPool,
    #[error("Something went wrong")]
    Unknown(String),
}
