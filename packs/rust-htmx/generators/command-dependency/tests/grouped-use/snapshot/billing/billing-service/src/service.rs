use std::sync::Arc;
use crate::ports::invoice_repository::InvoiceRepository;

use crate::{
    refund_invoice::{RefundInvoice, RefundInvoiceInput, RefundInvoiceOutput},
    void_invoice::{VoidInvoice, VoidInvoiceInput, VoidInvoiceOutput},
    cancel_invoice::{CancelInvoice, CancelInvoiceInput, CancelInvoiceOutput},
};

use crate::ports::email_sender::EmailSender;

#[derive(Clone)]
pub struct BillingService {
    pub refund_invoice: RefundInvoice,
    pub void_invoice: VoidInvoice,
    pub cancel_invoice: CancelInvoice,
}

impl BillingService {
    pub fn new(
        invoice_repository: Arc<dyn InvoiceRepository>,
        email_sender: Arc<dyn EmailSender>,
    ) -> Self {
        Self {
            refund_invoice: RefundInvoice {
                invoice_repository: invoice_repository.clone(),
            },
            void_invoice: VoidInvoice {
                email_sender: email_sender.clone(),
            },
            cancel_invoice: CancelInvoice {},
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
}
