use crate::{
    refund_invoice::{RefundInvoice, RefundInvoiceInput, RefundInvoiceOutput},
};

#[derive(Clone)]
pub struct BillingService {
    pub refund_invoice: RefundInvoice,
}

impl BillingService {
    pub fn new() -> Self {
        Self {
            refund_invoice: RefundInvoice {},
        }
    }

    pub async fn refund_invoice(&self, input: RefundInvoiceInput) -> RefundInvoiceOutput {
        self.refund_invoice.refund_invoice(input).await
    }
}
