use thiserror::Error;

#[derive(Clone)]
pub struct CancelSubscription {}

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
