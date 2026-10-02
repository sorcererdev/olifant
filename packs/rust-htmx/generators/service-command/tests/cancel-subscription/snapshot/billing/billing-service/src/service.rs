use std::sync::Arc;
use crate::ports::invoice_repository::InvoiceRepository;

use crate::{
    refund_invoice::{RefundInvoice, RefundInvoiceInput, RefundInvoiceOutput},
    void_invoice::{VoidInvoice, VoidInvoiceInput, VoidInvoiceOutput},
    cancel_invoice::{CancelInvoice, CancelInvoiceInput, CancelInvoiceOutput},
    cancel_subscription::{CancelSubscription, CancelSubscriptionInput, CancelSubscriptionOutput},
};

#[derive(Clone)]
pub struct BillingService {
    pub refund_invoice: RefundInvoice,
    pub void_invoice: VoidInvoice,
    pub cancel_invoice: CancelInvoice,
    pub cancel_subscription: CancelSubscription,
}

impl BillingService {
    pub fn new(
        invoice_repository: Arc<dyn InvoiceRepository>,
    ) -> Self {
        Self {
            refund_invoice: RefundInvoice {
                invoice_repository: invoice_repository.clone(),
            },
            void_invoice: VoidInvoice {},
            cancel_invoice: CancelInvoice {},
            cancel_subscription: CancelSubscription {
                // Add any dependencies for the command here. They should be passed into this function and supplied by main.rs.
            },
        }
    }

    pub async fn refund_invoice(&self, input: RefundInvoiceInput) -> RefundInvoiceOutput {
        self.refund_invoice.refund_invoice(input).await
    }

    pub async fn void_invoice(&self, input: VoidInvoiceInput) -> VoidInvoiceOutput {
        self.void_invoice.void_invoice(input).await
    }

    pub async fn cancel_invoice(&self, input: CancelInvoiceInput) -> CancelInvoiceOutput {
        self.cancel_invoice.cancel_invoice(input).await
    }

    pub async fn cancel_subscription(&self, input: CancelSubscriptionInput) -> CancelSubscriptionOutput {
        self.cancel_subscription.cancel_subscription(input).await
    }
}
