use std::sync::Arc;

use thiserror::Error;

use crate::ports::invoice_repository::InvoiceRepository;

#[derive(Clone)]
pub struct ScheduleDigest {
    pub invoice_repository: Arc<dyn InvoiceRepository>,
}

#[derive(Clone, Debug)]
pub struct ScheduleDigestInput {}

pub type ScheduleDigestOutput = Result<(), ScheduleDigestFailure>;

impl ScheduleDigest {
    pub async fn schedule_digest(&self, _input: ScheduleDigestInput) -> ScheduleDigestOutput {
        todo!("schedule_digest")
    }
}

#[derive(Error, Debug, PartialEq)]
pub enum ScheduleDigestFailure {
    #[error("Something went wrong")]
    Unknown(String),
}
