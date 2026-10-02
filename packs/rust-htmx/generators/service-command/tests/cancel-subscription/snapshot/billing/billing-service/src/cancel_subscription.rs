use std::sync::Arc;

use thiserror::Error;

use crate::ports::invoice_repository::InvoiceRepository;

#[derive(Clone)]
pub struct CancelSubscription {
    pub invoice_repository: Arc<dyn InvoiceRepository>,
}

#[derive(Clone, Debug)]
pub struct CancelSubscriptionInput {}

pub type CancelSubscriptionOutput = Result<(), CancelSubscriptionFailure>;

impl CancelSubscription {
    pub async fn cancel_subscription(&self, _input: CancelSubscriptionInput) -> CancelSubscriptionOutput {
        todo!("cancel_subscription")
    }
}

#[derive(Error, Debug, PartialEq)]
pub enum CancelSubscriptionFailure {
    #[error("Something went wrong")]
    Unknown(String),
}
