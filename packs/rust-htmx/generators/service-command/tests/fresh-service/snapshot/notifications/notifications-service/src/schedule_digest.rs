use std::sync::Arc;

use thiserror::Error;

// Example repo dependency
// use crate::ports::worksite_repository::WorksiteRepository;

#[derive(Clone)]
pub struct ScheduleDigest {
    // Put infra dependencies in this struct
    // Below is an example of a repo dependency
    // pub worksite_repository: Arc<dyn WorksiteRepository>,
}

#[derive(Clone, Debug)]
pub struct ScheduleDigestInput {
    // Put input fields here
    pub id: String
}

// Change the return type, if needed
pub type ScheduleDigestOutput = Result<(), ScheduleDigestFailure>;

impl ScheduleDigest {
    pub async fn schedule_digest(&self, input: ScheduleDigestInput) -> ScheduleDigestOutput {
        todo!("Implement schedule_digest")
    }
}

#[derive(Error, Debug, PartialEq)]
pub enum ScheduleDigestFailure {
    #[error("Worksite does not exist")]
    NotFound,
    #[error("Something went wrong")]
    Unknown(String),
}
