use rscx::{component, html, props};

#[props]
pub struct StatusBadgeProps {
    #[builder(default)]
    children: String,
    
    #[builder(setter(into))]
    name: String,
}

#[component]
pub fn StatusBadge(props: StatusBadgeProps) -> String {
    html! {
        <div>
            <h1>Hello {props.name}</h1>
            <div>{props.children}</div>
        </div>
    }
}
