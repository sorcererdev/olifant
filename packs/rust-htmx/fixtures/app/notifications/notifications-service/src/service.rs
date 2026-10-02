use std::sync::Arc;

use crate::{
    //##PLOP INSERT COMMAND IMPORTS HOOK##
    send_email::{SendEmail, SendEmailInput, SendEmailOutput},
};

#[derive(Clone)]
pub struct NotificationsService {
    //##PLOP INSERT COMMAND HOOK##
    pub send_email: SendEmail,
    // Add service infra dependencies here
}

impl NotificationsService {
    pub fn new() -> Self {
        Self {
            //##PLOP INSERT COMMAND INSTANTIATION HOOK##
            send_email: SendEmail {
              // Add any dependencies for the command here. They should be passed into this function and supplied by main.rs.
            },
        }
    }
    //##PLOP INSERT DELEGATE HOOK##
    pub async fn send_email(
        &self,
        input: SendEmailInput,
    ) -> SendEmailOutput {
        self.send_email.send_email(input).await
    }

}
