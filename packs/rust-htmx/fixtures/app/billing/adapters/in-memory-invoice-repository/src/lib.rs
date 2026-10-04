use std::sync::Arc;

use async_trait::async_trait;
use tokio::sync::RwLock;
use billing_service::models::Invoice;
use billing_service::ports::invoice_repository::{RepositoryFailure, InvoiceRepository};

#[derive(Clone, Debug)]
pub struct InMemoryInvoiceRepository {
    pub invoices: Arc<RwLock<Vec<Invoice>>>,
}

impl InMemoryInvoiceRepository {
    pub fn empty() -> Self {
        Self {
            invoices: Arc::new(RwLock::new(vec![])),
        }
    }

    pub fn with(invoices: Vec<Invoice>) -> Self {
        Self {
            invoices: Arc::new(RwLock::new(invoices)),
        }
    }
}

#[async_trait]
impl InvoiceRepository for InMemoryInvoiceRepository {
    async fn get_invoice(&self, id: String) -> Result<Option<Invoice>, RepositoryFailure> {
        let invoices = self.invoices.read().await;
        Ok(invoices.iter().find(|w| w.id == id).map(|w| w.to_owned()))
    }

    async fn save(&self, invoice: Invoice) -> Result<(), RepositoryFailure> {
        let mut invoices = self.invoices.write().await;

        invoices.retain(|w| w.id != invoice.id);
        invoices.push(invoice.to_owned());

        Ok(())
    }
}
