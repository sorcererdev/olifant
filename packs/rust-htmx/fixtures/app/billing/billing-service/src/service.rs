use crate::{
    refund_invoice::{RefundInvoice, RefundInvoiceInput, RefundInvoiceOutput},
    // olifant:command-imports
};

#[derive(Clone)]
pub struct BillingService {
    pub refund_invoice: RefundInvoice,
    // olifant:command-fields
}

impl BillingService {
    pub fn new() -> Self {
        Self {
            refund_invoice: RefundInvoice {},
            // olifant:command-instantiations
        }
    }

    pub async fn refund_invoice(&self, input: RefundInvoiceInput) -> RefundInvoiceOutput {
        self.refund_invoice.refund_invoice(input).await
    }
    // olifant:command-delegates
}
