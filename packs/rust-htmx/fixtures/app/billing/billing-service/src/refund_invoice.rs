use thiserror::Error;

#[derive(Clone)]
pub struct RefundInvoice {}

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
