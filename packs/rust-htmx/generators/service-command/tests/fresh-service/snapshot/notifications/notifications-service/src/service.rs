use std::sync::Arc;

use crate::{
    //##PLOP INSERT COMMAND IMPORTS HOOK##
    send_email::{SendEmail, SendEmailInput, SendEmailOutput},
};

#[derive(Clone)]
pub struct NotificationsService {
    //##PLOP INSERT COMMAND HOOK##
    // Add service infra dependencies here
    pub send_email: SendEmail,
}

impl NotificationsService {
    pub fn new() -> Self {
        Self {
            //##PLOP INSERT COMMAND INSTANTIATION HOOK##
            send_email: SendEmail {},
        }
    }

    pub async fn send_email(&self, input: SendEmailInput) -> SendEmailOutput {
        self.send_email.send_email(input).await
    }
    //##PLOP INSERT DELEGATE HOOK##
}
