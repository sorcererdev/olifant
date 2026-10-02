use std::sync::Arc;

use thiserror::Error;

use crate::ports::email_sender::EmailSender;
use crate::ports::invoice_repository::InvoiceRepository;

#[derive(Clone)]
pub struct VoidInvoice {
    pub email_sender: Arc<dyn EmailSender>,
    pub invoice_repository: Arc<dyn InvoiceRepository>,
}

#[derive(Clone, Debug)]
pub struct VoidInvoiceInput {}

pub type VoidInvoiceOutput = Result<(), VoidInvoiceFailure>;

impl VoidInvoice {
    pub async fn void_invoice(&self, _input: VoidInvoiceInput) -> VoidInvoiceOutput {
        todo!("void_invoice")
    }
}

#[derive(Error, Debug, PartialEq)]
pub enum VoidInvoiceFailure {
    #[error("Something went wrong")]
    Unknown(String),
}
