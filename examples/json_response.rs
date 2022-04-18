use std::net::SocketAddr;

use axum::{extract::Path, Extension, Json};
use axum_open_api::{IntoOpenApiResponse, OpenApiRouter};
use okapi::openapi3::OpenApi;
use schemars::JsonSchema;
use serde::Serialize;

#[tokio::main]
async fn main() {
    let oar = OpenApiRouter::new().route("/test/:id/:id2", axum_open_api::get(handler));

    let oapi = oar.to_open_api();

    let r = oar
        .as_router()
        .route("/open_api", axum::routing::get(openapi_hander))
        .layer(Extension(oapi));

    // let r = Router::new().route("test", axum::routing::get(handler));

    let addr = SocketAddr::from(([127, 0, 0, 1], 3000));
    axum::Server::bind(&addr)
        .serve(r.into_make_service())
        .await
        .unwrap();
}

#[derive(JsonSchema, Serialize)]
struct SomeJsonBody {
    foo: i32,
    bar: String,
    baz: bool,
}

async fn handler(Path(id): Path<String>) -> impl IntoOpenApiResponse {
    Json(SomeJsonBody {
        foo: 22,
        bar: format!("Got {}", id),
        baz: false,
    })
}

async fn openapi_hander(Extension(oapi): Extension<OpenApi>) -> Json<OpenApi> {
    Json(oapi)
}
