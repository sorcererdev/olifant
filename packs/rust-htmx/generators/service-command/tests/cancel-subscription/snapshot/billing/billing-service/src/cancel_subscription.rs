use std::sync::Arc;

use thiserror::Error;

// Example repo dependency
// use crate::ports::worksite_repository::WorksiteRepository;

#[derive(Clone)]
pub struct CancelSubscription {
    // Put infra dependencies in this struct
    // Below is an example of a repo dependency
    // pub worksite_repository: Arc<dyn WorksiteRepository>,
}

#[derive(Clone, Debug)]
pub struct CancelSubscriptionInput {
    // Put input fields here
    pub id: String
}

// Change the return type, if needed
pub type CancelSubscriptionOutput = Result<(), CancelSubscriptionFailure>;

impl CancelSubscription {
    pub async fn cancel_subscription(&self, input: CancelSubscriptionInput) -> CancelSubscriptionOutput {
        todo!("Implement cancel_subscription")
    }
}

#[derive(Error, Debug, PartialEq)]
pub enum CancelSubscriptionFailure {
    #[error("Worksite does not exist")]
    NotFound,
    #[error("Something went wrong")]
    Unknown(String),
}
