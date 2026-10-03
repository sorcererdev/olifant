use std::sync::Arc;

use thiserror::Error;

use crate::ports::invoice_repository::InvoiceRepository;

#[derive(Clone)]
pub struct RefundInvoice {
    pub invoice_repository: Arc<dyn InvoiceRepository>,
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
    #[error("Too \"late\" for {{refunds}}")]
    TooLate,
}

impl RefundInvoiceFailure {
    pub fn status(&self) -> u16 {
        match self {
            RefundInvoiceFailure::Unknown(_) => 500,
            RefundInvoiceFailure::TooLate => todo!("olifant: status RefundInvoiceFailure::TooLate"),
        }
    }
}
