use std::sync::Arc;

use thiserror::Error;
use crate::ports::email_template_repository::EmailTemplateRepository;

// Example repo dependency
// use crate::ports::worksite_repository::WorksiteRepository;

#[derive(Clone)]
pub struct SendEmail {
    // Put infra dependencies in this struct
    // Below is an example of a repo dependency
    // pub worksite_repository: Arc<dyn WorksiteRepository>,
    pub email_template_repository: Arc<dyn EmailTemplateRepository>,
}

#[derive(Clone, Debug)]
pub struct SendEmailInput {
    // Put input fields here
    pub id: String
}

// Change the return type, if needed
pub type SendEmailOutput = Result<(), SendEmailFailure>;

impl SendEmail {
    pub async fn send_email(&self, input: SendEmailInput) -> SendEmailOutput {
        todo!("Implement send_email")
    }
}

#[derive(Error, Debug, PartialEq)]
pub enum SendEmailFailure {
    #[error("Worksite does not exist")]
    NotFound,
    #[error("Something went wrong")]
    Unknown(String),
}
