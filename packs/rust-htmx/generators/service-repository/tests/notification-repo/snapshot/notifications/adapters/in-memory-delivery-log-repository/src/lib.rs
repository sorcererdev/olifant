use std::sync::Arc;

use async_trait::async_trait;
use tokio::sync::RwLock;
use notifications_service::models::DeliveryLog;
use notifications_service::ports::delivery_log_repository::{RepositoryFailure, DeliveryLogRepository};

#[derive(Clone, Debug)]
pub struct InMemoryDeliveryLogRepository {
    pub delivery_logs: Arc<RwLock<Vec<DeliveryLog>>>,
}

impl InMemoryDeliveryLogRepository {
    pub fn empty() -> Self {
        Self {
            delivery_logs: Arc::new(RwLock::new(vec![])),
        }
    }

    pub fn with(delivery_logs: Vec<DeliveryLog>) -> Self {
        Self {
            delivery_logs: Arc::new(RwLock::new(delivery_logs)),
        }
    }
}

#[async_trait]
impl DeliveryLogRepository for InMemoryDeliveryLogRepository {
    async fn get_delivery_log(&self, id: String) -> Result<Option<DeliveryLog>, RepositoryFailure> {
        let delivery_logs = self.delivery_logs.read().await;
        Ok(delivery_logs.iter().find(|w| w.id == id).map(|w| w.to_owned()))
    }

    async fn save(&self, delivery_log: DeliveryLog) -> Result<(), RepositoryFailure> {
        let mut delivery_logs = self.delivery_logs.write().await;

        delivery_logs.retain(|w| w.id != delivery_log.id);
        delivery_logs.push(delivery_log.to_owned());

        Ok(())
    }
}
