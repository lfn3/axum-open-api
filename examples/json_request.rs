use std::net::SocketAddr;

use axum::{Extension, Json};
use axum_open_api::{IntoOpenApiResponse, OpenApiRouter};
use okapi::openapi3::OpenApi;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

#[tokio::main]
async fn main() {
    let oar = OpenApiRouter::new().route("/identity", axum_open_api::post(handler));

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

#[derive(JsonSchema, Serialize, Deserialize)]
struct SomeJsonBody {
    foo: i32,
    bar: String,
    baz: bool,
}

async fn handler(Json(body): Json<SomeJsonBody>) -> impl IntoOpenApiResponse {
    Json(body)
}

async fn openapi_hander(Extension(oapi): Extension<OpenApi>) -> Json<OpenApi> {
    Json(oapi)
}
