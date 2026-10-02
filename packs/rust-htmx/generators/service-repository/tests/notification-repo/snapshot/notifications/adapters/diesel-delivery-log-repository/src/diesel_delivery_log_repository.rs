use async_trait::async_trait;
use db::db::PgPool;
use db::schema::*;
use diesel::prelude::*;
use diesel::OptionalExtension;
use diesel_async::scoped_futures::ScopedFutureExt;
use diesel_async::RunQueryDsl;

use notifications_service::ports::delivery_log_repository::{RepositoryFailure, DeliveryLogRepository};
use notifications_service::models::DeliveryLog;
use crate::records::*;

#[derive(Clone)]
pub struct DieselDeliveryLogRepository {
    pub pg_pool: PgPool,
}

#[async_trait]
impl DeliveryLogRepository for DieselDeliveryLogRepository {
    async fn get_delivery_log(&self, id: String) -> Result<Option<DeliveryLog>, RepositoryFailure> {
        let conn = &mut self
            .pg_pool
            .get()
            .await
            .map_err(|_| RepositoryFailure::FailedToGetConnectionFromPool)?;

        todo!("Fetch record and map to domain model")

    }

    async fn save(&self, delivery_log: DeliveryLog) -> Result<(), RepositoryFailure> {
        let conn = &mut self
            .pg_pool
            .get()
            .await
            .map_err(|_| RepositoryFailure::FailedToGetConnectionFromPool)?;
        
        todo!("Save record to database")
    }
}
