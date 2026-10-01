use async_trait::async_trait;
use db::db::PgPool;
use db::schema::*;
use diesel::prelude::*;
use diesel::OptionalExtension;
use diesel_async::scoped_futures::ScopedFutureExt;
use diesel_async::RunQueryDsl;

use notifications_service::ports::email_template_repository::{RepositoryFailure, EmailTemplateRepository};
use notifications_service::models::EmailTemplate;
use crate::records::*;

#[derive(Clone)]
pub struct DieselEmailTemplateRepository {
    pub pg_pool: PgPool,
}

#[async_trait]
impl EmailTemplateRepository for DieselEmailTemplateRepository {
    async fn get_email_template(&self, id: String) -> Result<Option<EmailTemplate>, RepositoryFailure> {
        let conn = &mut self
            .pg_pool
            .get()
            .await
            .map_err(|_| RepositoryFailure::FailedToGetConnectionFromPool)?;

        todo!("Fetch record and map to domain model")

    }

    async fn save(&self, email_template: EmailTemplate) -> Result<(), RepositoryFailure> {
        let conn = &mut self
            .pg_pool
            .get()
            .await
            .map_err(|_| RepositoryFailure::FailedToGetConnectionFromPool)?;
        
        todo!("Save record to database")
    }
}
