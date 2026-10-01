use http::{HeaderMap, StatusCode};
use rscx::{component, html, props};
use axum::{
    extract::State,
    response::{Html, IntoResponse},
    routing::get,
    Form, Router,
};
use axum_flash::Flash;
use serde::Deserialize;
use crate::{
    components::{
        page::PageLayout,
    },
    routes,
    state::WebHtmxState,
};
use web_client::server::{
    attrs::Attrs,
    form::{Button, GridCell, GridLayout, Label, TextInput},
    headers::SecondaryHeader,
    modal::{Modal, ModalSize},
};

pub fn workers_routes(state: WebHtmxState) -> Router {
    Router::new()
        .route(
            routes::WORKERS, 
            get(get_workers)
        )
        .route(
            routes::WORKERS_CREATE_FORM,
            get(get_create_form).post(post_create_form),
        )
        .with_state(state)
}

async fn get_workers(
    State(state): State<WebHtmxState>,
) -> impl IntoResponse {
  todo!()
}

async fn get_create_form(headers: HeaderMap) -> impl IntoResponse {
    Html(html! {
        <PageLayout
            partial=headers.contains_key("Hx-Request")
            header="Add Workers"
        >
            <Modal size=ModalSize::MediumScreen>
                <SecondaryHeader
                    title="Add Worker"
                    subtitle="Enter details below."
                />
                <WorkerForm action=routes::workers_create_form() />
            </Modal>
        </PageLayout>
    })
}

#[props]
pub struct WorkerFormProps {
    #[builder(setter(into))]
    action: String,

    #[builder(setter(into), default)]
    name: String,
}

#[component]
pub fn WorkerForm(props: WorkerFormProps) -> String {
    html! {
        <form hx-post=props.action>
            <div class="pb-12">
                <GridLayout class="mt-10">
                    <GridCell span=3>
                        <Label for_input="name">Name</Label>
                        <TextInput name="name" input_type="name" value=props.name/>
                    </GridCell>
                </GridLayout>
            </div>
            <div class="mt-6 flex items-center justify-end gap-x-6">
                <Button
                    onclick="history.go(-1)"
                    attrs=Attrs::with("data-toggle-action", "close".into())
                >
                    Cancel
                </Button>
                <Button kind="submit">Save</Button>
            </div>
        </form>
    }
}

#[derive(Deserialize, Debug)]
struct AddWorkerFormData {
    name: String,
}

async fn post_create_form(
    State(state): State<WebHtmxState>,
    flash: Flash,
    Form(form): Form<AddWorkerFormData>,
) -> impl IntoResponse {
    (
        StatusCode::OK,
        flash.success("Worker added successfully!"),
        [
            ("hx-redirect", routes::workers()),
            ("hx-retarget", "body".into()),
        ],
    )
}
