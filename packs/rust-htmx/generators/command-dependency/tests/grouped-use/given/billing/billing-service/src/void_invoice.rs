use std::sync::Arc;

use thiserror::Error;

use crate::{
    refund_invoice::RefundInvoice,
};

#[derive(Clone)]
pub struct VoidInvoice {}

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
