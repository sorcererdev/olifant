use std::sync::Arc;

use async_trait::async_trait;
use tokio::sync::RwLock;
use notifications_service::models::EmailTemplate;
use notifications_service::ports::email_template_repository::{RepositoryFailure, EmailTemplateRepository};

#[derive(Clone, Debug)]
pub struct InMemoryEmailTemplateRepository {
    pub email_templates: Arc<RwLock<Vec<EmailTemplate>>>,
}

impl InMemoryEmailTemplateRepository {
    pub fn empty() -> Self {
        Self {
            email_templates: Arc::new(RwLock::new(vec![])),
        }
    }

    pub fn with(email_templates: Vec<EmailTemplate>) -> Self {
        Self {
            email_templates: Arc::new(RwLock::new(email_templates)),
        }
    }
}

#[async_trait]
impl EmailTemplateRepository for InMemoryEmailTemplateRepository {
    async fn get_email_template(&self, id: String) -> Result<Option<EmailTemplate>, RepositoryFailure> {
        let email_templates = self.email_templates.read().await;
        Ok(email_templates.iter().find(|w| w.id == id).map(|w| w.to_owned()))
    }

    async fn save(&self, email_template: EmailTemplate) -> Result<(), RepositoryFailure> {
        let mut email_templates = self.email_templates.write().await;

        email_templates.retain(|w| w.id != email_template.id);
        email_templates.push(email_template.to_owned());

        Ok(())
    }
}
