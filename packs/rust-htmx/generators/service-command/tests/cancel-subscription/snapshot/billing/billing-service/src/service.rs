use crate::{
    refund_invoice::{RefundInvoice, RefundInvoiceInput, RefundInvoiceOutput},
    cancel_subscription::{CancelSubscription, CancelSubscriptionInput, CancelSubscriptionOutput},
    // olifant:command-imports
};

#[derive(Clone)]
pub struct BillingService {
    pub refund_invoice: RefundInvoice,
    pub cancel_subscription: CancelSubscription,
    // olifant:command-fields
}

impl BillingService {
    pub fn new() -> Self {
        Self {
            refund_invoice: RefundInvoice {},
            cancel_subscription: CancelSubscription {},
            // olifant:command-instantiations
        }
    }

    pub async fn refund_invoice(&self, input: RefundInvoiceInput) -> RefundInvoiceOutput {
        self.refund_invoice.refund_invoice(input).await
    }

    pub async fn cancel_subscription(&self, input: CancelSubscriptionInput) -> CancelSubscriptionOutput {
        self.cancel_subscription.cancel_subscription(input).await
    }
    // olifant:command-delegates
}
