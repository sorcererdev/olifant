use async_trait::async_trait;
use db::db::PgPool;
use db::schema::*;
use diesel::prelude::*;
use diesel::OptionalExtension;
use diesel_async::scoped_futures::ScopedFutureExt;
use diesel_async::RunQueryDsl;

use billing_service::ports::invoice_repository::{RepositoryFailure, InvoiceRepository};
use billing_service::models::Invoice;
use crate::records::*;

#[derive(Clone)]
pub struct DieselInvoiceRepository {
    pub pg_pool: PgPool,
}

#[async_trait]
impl InvoiceRepository for DieselInvoiceRepository {
    async fn get_invoice(&self, id: String) -> Result<Option<Invoice>, RepositoryFailure> {
        let conn = &mut self
            .pg_pool
            .get()
            .await
            .map_err(|_| RepositoryFailure::FailedToGetConnectionFromPool)?;

        todo!("Fetch record and map to domain model")

    }

    async fn save(&self, invoice: Invoice) -> Result<(), RepositoryFailure> {
        let conn = &mut self
            .pg_pool
            .get()
            .await
            .map_err(|_| RepositoryFailure::FailedToGetConnectionFromPool)?;
        
        todo!("Save record to database")
    }
}
