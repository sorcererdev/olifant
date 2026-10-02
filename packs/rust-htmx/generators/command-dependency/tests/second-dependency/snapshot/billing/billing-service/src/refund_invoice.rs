use std::sync::Arc;

use thiserror::Error;

use crate::ports::invoice_repository::InvoiceRepository;

use crate::ports::email_sender::EmailSender;

#[derive(Clone)]
pub struct RefundInvoice {
    pub invoice_repository: Arc<dyn InvoiceRepository>,
    pub email_sender: Arc<dyn EmailSender>,
}

#[derive(Clone, Debug)]
pub struct RefundInvoiceInput {}

pub type RefundInvoiceOutput = Result<(), RefundInvoiceFailure>;

impl RefundInvoice {
    pub async fn refund_invoice(&self, _input: RefundInvoiceInput) -> RefundInvoiceOutput {
        todo!("refund_invoice")
    }
}

#[derive(Error, Debug, PartialEq)]
pub enum RefundInvoiceFailure {
    #[error("Something went wrong")]
    Unknown(String),
}
