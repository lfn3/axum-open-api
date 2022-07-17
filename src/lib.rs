use std::{
    collections::{BTreeMap, HashMap},
    convert::Infallible,
    future::Future,
};

use axum::{
    body::HttpBody, extract::Path, handler::Handler, response::IntoResponse,
    routing::future::RouteFuture, Json, Router,
};
use axum_core::response::Response;
use http::Request;
use hyper::Body;
use okapi::{
    openapi3::{
        Components, MediaType, OpenApi, Operation, Parameter, ParameterValue, PathItem, RefOr,
        RequestBody, Responses, SchemaObject,
    },
    Map,
};
use schemars::{
    schema::{InstanceType, RootSchema, SingleOrVec},
    JsonSchema,
};
use tower::Service;

#[derive(Debug, Clone, PartialEq, Eq)]
struct PathParam {
    name: String,
}

impl Into<Parameter> for PathParam {
    fn into(self) -> Parameter {
        Parameter {
            name: self.name,
            location: "path".to_string(),
            description: None,
            required: true,
            deprecated: false,
            allow_empty_value: false,
            value: ParameterValue::Schema {
                style: None,
                explode: None,
                allow_reserved: false,
                schema: SchemaObject::default(),
                example: None,
                examples: None,
            },
            extensions: Map::new(),
        }
    }
}

pub enum RequestParticle {
    Body(String, MediaType),
    Path(String, SchemaObject),
}

pub trait IntoRequestParticle {
    fn into_request_particle(components: &mut Components) -> RequestParticle;
}

fn string_schema_object() -> SchemaObject {
    SchemaObject {
        instance_type: Some(SingleOrVec::Single(Box::new(InstanceType::String))),
        ..Default::default()
    }
}

impl<T> IntoRequestParticle for Path<T> {
    fn into_request_particle(components: &mut Components) -> RequestParticle {
        RequestParticle::Path("".to_string(), string_schema_object()) // TODO: something better than this
    }
}

pub trait IntoBodyContent {
    fn into_body_content(components: &mut Components) -> (String, MediaType);
}

impl<T> IntoRequestParticle for T
where
    T: IntoBodyContent,
{
    fn into_request_particle(components: &mut Components) -> RequestParticle {
        let (mime_type, media_type) = Self::into_body_content(components);
        RequestParticle::Body(mime_type, media_type)
    }
}

impl<T> IntoBodyContent for Json<T>
where
    T: JsonSchema,
{
    fn into_body_content(components: &mut Components) -> (String, MediaType) {
        let gen = schemars::gen::SchemaSettings::openapi3().into_generator();

        let RootSchema {
            meta_schema: _, // aka. $schema. Explicitly unsupported by openapi: https://swagger.io/docs/specification/data-models/keywords/
            schema,
            definitions,
        } = gen.into_root_schema_for::<T>();

        let mut media_type = MediaType::default();
        media_type.schema = Some(schema);

        let definitions = definitions
            .into_iter()
            .filter_map(|(k, schema)| match schema {
                schemars::schema::Schema::Bool(_) => None,
                schemars::schema::Schema::Object(obj) => Some((k, obj)),
            })
            .collect::<schemars::Map<_, _>>();
        components.schemas.extend(definitions);

        ("application/json".to_string(), media_type)
    }
}

impl IntoBodyContent for String {
    fn into_body_content(_components: &mut Components) -> (String, MediaType) {
        (
            "text/plain".to_string(),
            MediaType {
                schema: Some(string_schema_object()),
                ..Default::default()
            },
        )
    }
}

#[derive(Debug, Clone)]
struct PathDescription {
    params: Vec<PathParam>,
    operations: HashMap<http::Method, Operation>,
}

impl Into<PathItem> for PathDescription {
    fn into(self) -> PathItem {
        let get = self.operations.get(&http::Method::GET).cloned();
        let post = self.operations.get(&http::Method::POST).cloned();
        // TODO: rest of the http verbs
        PathItem {
            parameters: self
                .params
                .into_iter()
                .map(|p| RefOr::Object(p.into()))
                .collect(),
            get,
            post,
            ..PathItem::default()
        }
    }
}

#[derive(Clone)]
pub struct OpenApiRouter<B = Body> {
    router: axum::Router<B>,
    paths: HashMap<String, PathDescription>,
}

impl<B> OpenApiRouter<B>
where
    B: axum::body::HttpBody + Send + 'static,
{
    pub fn new() -> OpenApiRouter<B> {
        return OpenApiRouter {
            router: Router::new(),
            paths: HashMap::new(),
        };
    }

    pub fn to_open_api(&self) -> OpenApi {
        let mut oa = OpenApi::new();
        let paths = self.paths.clone().into_iter().map(|(p, pd)| (p, pd.into()));
        oa.paths.extend(paths);

        oa
    }

    // Reimplementation of methods on axum::Router

    pub fn route<T>(mut self, path: &str, service: T) -> Self
    where
        T: Service<Request<B>, Response = Response, Error = Infallible>
            + DescribableHandler
            + Clone
            + Send
            + 'static,
        T::Future: Send + 'static,
    {
        self.paths.insert(
            path_params_to_bracket_wrapped(path),
            PathDescription {
                params: extract_path_params(path),
                operations: T::operations(&service),
            },
        );
        self.router = self.router.route(path, service);

        self
    }
    // TODO: nest, merge, layer etc

    pub fn as_router(self) -> Router<B> {
        self.router
    }
}

pub trait DescribableHandler {
    fn operations(&self) -> HashMap<http::Method, Operation>;
}

pub struct MethodRouter<B = Body, E = Infallible> {
    components: Components,
    underlying: axum::routing::MethodRouter<B, E>,
    operations: HashMap<http::Method, Operation>,
}

impl Clone for MethodRouter {
    fn clone(&self) -> Self {
        Self {
            components: self.components.clone(),
            underlying: self.underlying.clone(),
            operations: self.operations.clone(),
        }
    }
}

impl<B, E> Service<Request<B>> for MethodRouter<B, E>
where
    B: HttpBody,
{
    type Response = Response;
    type Error = E;
    type Future = RouteFuture<B, E>;

    fn poll_ready(
        &mut self,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Result<(), Self::Error>> {
        self.underlying.poll_ready(cx)
    }

    fn call(&mut self, req: Request<B>) -> Self::Future {
        self.underlying.call(req)
    }
}

impl<B> DescribableHandler for MethodRouter<B, Infallible> {
    fn operations(&self) -> HashMap<http::Method, Operation> {
        self.operations.clone()
    }
}

pub fn get<H, T, B>(handler: H) -> MethodRouter<B, Infallible>
where
    H: Handler<T, B> + ToOperation<T>,
    B: Send + 'static,
    T: 'static,
{
    let mut components = Components::default();
    let mut operations = HashMap::new();
    operations.insert(
        http::Method::GET,
        H::to_operation(&handler, &mut components),
    );
    MethodRouter {
        components,
        underlying: axum::routing::get(handler),
        operations,
    }
}

pub fn post<H, T, B>(handler: H) -> MethodRouter<B, Infallible>
where
    H: Handler<T, B> + ToOperation<T>,
    B: Send + 'static,
    T: 'static,
{
    let mut components = Components::default();
    let mut operations = HashMap::new();
    operations.insert(
        http::Method::POST,
        H::to_operation(&handler, &mut components),
    );
    MethodRouter {
        components,
        underlying: axum::routing::post(handler),
        operations,
    }
}

pub trait ToOperation<T> {
    fn to_operation(&self, components: &mut Components) -> Operation;
}

pub trait IntoOpenApiResponse: IntoBodyContent + IntoResponse {}

impl<T> IntoOpenApiResponse for T where T: IntoBodyContent + IntoResponse {}

impl<F, Fut, Res, T1> ToOperation<(T1,)> for F
where
    F: FnOnce(T1) -> Fut,
    Fut: Future<Output = Res> + Send,
    Res: IntoBodyContent,
    T1: IntoRequestParticle,
{
    fn to_operation(&self, components: &mut Components) -> Operation {
        let mut op = Operation::default();
        merge_request_particle(&mut op, T1::into_request_particle(components));

        let mut resps = Responses::default();
        let (media_type, resp) = Res::into_body_content(components);
        let mut content = BTreeMap::new();
        content.insert(media_type, resp);
        resps.responses.insert(
            "200".to_string(),
            RefOr::Object(okapi::openapi3::Response {
                content,
                ..Default::default()
            }),
        );

        op.responses = resps;

        op
    }
}

fn merge_request_particle(op: &mut Operation, rp: RequestParticle) {
    match rp {
        RequestParticle::Body(mime_type, media_type) => {
            op.request_body = Some(RefOr::Object(RequestBody {
                content: vec![(mime_type, media_type)].into_iter().collect(),
                required: true,
                ..Default::default()
            }))
        }
        RequestParticle::Path(_, _) =>
            // TODO: somehow need to merge the actual path string into this, otherwise we can't match up the paths part strings with their types
            {}
    }
}

fn extract_path_params(path: &str) -> Vec<PathParam> {
    let mut split = path.split(':');

    //First part will be either an empty string or a part of the path that doesn't need replacement
    if split.next().is_none() {
        return vec![];
    }

    split
        .map(|p| p.split_once('/').map(|s| s.0).unwrap_or(p))
        .map(|p| PathParam {
            name: p.to_string(),
        })
        .collect()
}

fn path_params_to_bracket_wrapped(path: &str) -> String {
    let mut split = path.split(':');

    //First part will be either an empty string or a part of the path that doesn't need replacement
    let prefix = if let Some(p) = split.next() {
        p
    } else {
        return String::from("");
    };

    let wrapped: String = split
        .map(|s| {
            if let Some((before, after)) = s.split_once('/') {
                format!("{{{}}}/{}", before, after)
            } else {
                format!("{{{}}}", s)
            }
        })
        .collect();

    format!("{}{}", prefix, wrapped)
}

#[cfg(test)]
mod tests {
    use axum::{extract::Path, Json};
    use insta::assert_json_snapshot;
    use schemars::JsonSchema;
    use serde::{Deserialize, Serialize};

    use crate::{
        extract_path_params, path_params_to_bracket_wrapped, IntoOpenApiResponse, OpenApiRouter,
        PathParam,
    };

    #[test]
    fn path_params_to_bracket_wrapped_should_handle_empty_strings() {
        assert_eq!(path_params_to_bracket_wrapped(""), "")
    }

    #[test]
    fn path_params_to_bracket_wrapped_should_not_change_unparameterized_path() {
        assert_eq!(
            path_params_to_bracket_wrapped("/test/one/two"),
            "/test/one/two"
        )
    }

    #[test]
    fn path_params_to_bracket_wrapped_should_handle_path_starting_with_param() {
        assert_eq!(
            path_params_to_bracket_wrapped(":test/one/two"),
            "{test}/one/two"
        )
    }

    #[test]
    fn path_params_to_bracket_wrapped_should_handle_path_ending_with_unterminated_param() {
        assert_eq!(
            path_params_to_bracket_wrapped("test/one/:two"),
            "test/one/{two}"
        )
    }

    #[test]
    fn path_params_to_bracket_wrapped_should_handle_path_ending_with_terminated_param() {
        assert_eq!(
            path_params_to_bracket_wrapped("test/one/:two/"),
            "test/one/{two}/"
        )
    }

    #[test]
    fn path_params_to_bracket_wrapped_should_handle_path_with_param() {
        assert_eq!(
            path_params_to_bracket_wrapped("test/:one/two/"),
            "test/{one}/two/"
        )
    }

    #[test]
    fn path_params_to_bracket_wrapped_should_handle_path_with_params() {
        assert_eq!(
            path_params_to_bracket_wrapped(":test/:one/:two/"),
            "{test}/{one}/{two}/"
        )
    }

    #[test]
    fn extract_path_params_should_extract_nothing_from_empty_string() {
        assert!(extract_path_params("").is_empty())
    }

    #[test]
    fn extract_path_params_should_extract_nothing_when_string_contains_no_params() {
        assert!(extract_path_params("/test/one/two").is_empty())
    }

    #[test]
    fn extract_path_params_should_extract_single_path_param() {
        let expected = vec![PathParam {
            name: "one".to_string(),
        }];
        assert_eq!(extract_path_params("/test/:one/two"), expected)
    }

    #[test]
    fn extract_path_params_should_extract_unterminated_path_param() {
        let expected = vec![PathParam {
            name: "two".to_string(),
        }];
        assert_eq!(extract_path_params("/test/one/:two"), expected)
    }

    #[test]
    fn extract_path_params_should_extract_terminated_final_path_param() {
        let expected = vec![PathParam {
            name: "two".to_string(),
        }];
        assert_eq!(extract_path_params("/test/one/:two/"), expected)
    }

    #[test]
    fn extract_path_params_should_extract_inital_param() {
        let expected = vec![PathParam {
            name: "test".to_string(),
        }];
        assert_eq!(extract_path_params(":test/one/two/"), expected)
    }

    #[test]
    fn extract_path_params_should_extract_several_params() {
        let expected = vec![
            PathParam {
                name: "test".to_string(),
            },
            PathParam {
                name: "two".to_string(),
            },
        ];
        assert_eq!(extract_path_params(":test/one/:two/"), expected)
    }

    #[test]
    fn open_api_schema_path_parameters() {
        async fn handler(Path(_id): Path<String>) -> String {
            todo!("Should not be called")
        }

        let oar = OpenApiRouter::new().route("/test/:id/:id2", crate::get(handler));

        let oapi = oar.to_open_api();

        assert_json_snapshot!(oapi)
    }

    #[test]
    fn open_api_schema_request_parameters() {
        #[derive(JsonSchema, Serialize, Deserialize)]
        struct SomeJsonBody {
            foo: i32,
            bar: String,
            baz: bool,
        }

        async fn handler(Json(body): Json<SomeJsonBody>) -> impl IntoOpenApiResponse {
            Json(body)
        }

        let oar = OpenApiRouter::new().route("/identity", crate::post(handler));

        let oapi = oar.to_open_api();

        assert_json_snapshot!(oapi)
    }

    #[test]
    fn open_api_schema_response() {
        #[derive(JsonSchema, Serialize, Deserialize)]
        struct SomeJsonBody {
            foo: i32,
            bar: String,
            baz: bool,
        }

        async fn handler(Json(body): Json<SomeJsonBody>) -> impl IntoOpenApiResponse {
            Json(body)
        }

        let oar = OpenApiRouter::new().route("/test", crate::get(handler));

        let oapi = oar.to_open_api();

        assert_json_snapshot!(oapi)
    }
}
