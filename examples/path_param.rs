use std::net::SocketAddr;

use axum::{extract::Path, Extension, Json};
use axum_open_api::OpenApiRouter;
use okapi::openapi3::OpenApi;

#[tokio::main]
async fn main() {
    let oar = OpenApiRouter::new().route("/test/:id/:id2", axum_open_api::get(handler));

    let oapi = oar.to_open_api();

    let r = oar
        .as_router()
        .route("/open_api", axum::routing::get(openapi_hander))
        .layer(Extension(oapi));

    let addr = SocketAddr::from(([127, 0, 0, 1], 3000));
    axum::Server::bind(&addr)
        .serve(r.into_make_service())
        .await
        .unwrap();
}

async fn handler(Path(id): Path<String>) -> String {
    return format!("Got {}", id);
}

async fn openapi_hander(Extension(oapi): Extension<OpenApi>) -> Json<OpenApi> {
    Json(oapi)
}
