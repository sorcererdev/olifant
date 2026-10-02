use std::sync::Arc;

use thiserror::Error;

use crate::ports::{email_sender::EmailSender, invoice_repository::InvoiceRepository};

#[derive(Clone)]
pub struct CancelInvoice {
    pub email_sender: Arc<dyn EmailSender>,
    pub invoice_repository: Arc<dyn InvoiceRepository + Send + Sync>,
}

#[derive(Clone, Debug)]
pub struct CancelInvoiceInput {}

pub type CancelInvoiceOutput = Result<(), CancelInvoiceFailure>;

impl CancelInvoice {
    pub async fn cancel_invoice(&self, _input: CancelInvoiceInput) -> CancelInvoiceOutput {
        todo!("cancel_invoice")
    }
}

#[derive(Error, Debug, PartialEq)]
pub enum CancelInvoiceFailure {
    #[error("Something went wrong")]
    Unknown(String),
}
