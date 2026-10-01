use thiserror::Error;

#[derive(Clone)]
pub struct SendEmail {}

#[derive(Clone, Debug)]
pub struct SendEmailInput {}

pub type SendEmailOutput = Result<(), SendEmailFailure>;

impl SendEmail {
    pub async fn send_email(&self, _input: SendEmailInput) -> SendEmailOutput {
        todo!("send_email")
    }
}

#[derive(Error, Debug, PartialEq)]
pub enum SendEmailFailure {
    #[error("Something went wrong")]
    Unknown(String),
}
