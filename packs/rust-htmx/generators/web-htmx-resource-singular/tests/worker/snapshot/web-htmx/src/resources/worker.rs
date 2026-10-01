use axum::{
    extract::{self, State},
    response::{Html, IntoResponse},
    routing::get,
    Form, Router,
};
use axum_flash::Flash;
use http::StatusCode;
use rscx::html;
use serde::Deserialize;

use web_client::server::{
    headers::SecondaryHeader,
    modal::{Modal, ModalSize},
};

use crate::{components::page::PageLayout, routes, state::WebHtmxState};

pub fn worker_routes(state: WebHtmxState) -> Router {
    Router::new()
        .route(
            routes::WORKER, 
            get(get_worker)
        )
        .route(
            routes::WORKER_EDIT_FORM,
            get(get_edit_form).post(post_edit_form),
        )
        .with_state(state)
}

async fn get_worker(
    extract::Path((id)): extract::Path<(
        String,
    )>,
    State(state): State<WebHtmxState>,
) -> impl IntoResponse {
  todo!()
}

async fn get_edit_form(
    extract::Path(id): extract::Path<String>,
    State(state): State<WebHtmxState>,
) -> impl IntoResponse {
    Html(html! {
        <PageLayout
            header="Edit User"
        >
            <Modal size=ModalSize::MediumScreen>
                <SecondaryHeader
                    title="Edit Worker"
                    subtitle="Make changes to the Worker below."
                />
                <p>Add form here!</p>
            </Modal>
        </PageLayout>
    })
}

#[derive(Deserialize, Debug)]
struct UpdateWorkerFormData {
    foo: String
}

async fn post_edit_form(
    extract::Path(id): extract::Path<String>,
    State(state): State<WebHtmxState>,
    flash: Flash,
    Form(form): Form<UpdateWorkerFormData>,
) -> impl IntoResponse {
    (
        StatusCode::OK,
        flash.success("Updated Worker successfully!"),
        [
            ("hx-redirect", routes::workers()),
            ("hx-retarget", "body".into()),
        ],
    )
}
