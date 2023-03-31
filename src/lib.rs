use std::collections::HashSet;

use openapiv3::{
    Callback, Components, Example, Header, Link, OpenAPI, Parameter, PathItem, ReferenceOr,
    ReferenceOr::{Item, Reference},
    RequestBody, Response, Schema, SecurityScheme,
};
use openapiv3_visit::VisitMut;

pub fn expand(openapi: &mut OpenAPI) -> (usize, HashSet<String>) {
    let mut all_not_found = HashSet::new();
    let mut all_expanded = 0;
    loop {
        let (expanded, not_found) = expand_once(openapi);

        all_not_found.extend(not_found);
        all_expanded += expanded;

        if expanded == 0 {
            return (all_expanded, all_not_found);
        }
    }
}

fn expand_once(openapi: &mut OpenAPI) -> (usize, HashSet<String>) {
    let mut expand_once = ExpandOnce {
        components: openapi.components.clone().unwrap_or_default(),
        bad_references: HashSet::new(),
        references_expanded: 0,
    };
    expand_once.visit_openapi_mut(openapi);
    (expand_once.references_expanded, expand_once.bad_references)
}

struct ExpandOnce {
    components: Components,
    bad_references: HashSet<String>,
    references_expanded: usize,
}

macro_rules! getters {
    ($($getter_name:ident -> $returns:ty { $key:literal, $field:ident }),* $(,)?) => {
        impl ExpandOnce {
            $(
                fn $getter_name(&self, reference: &str) -> Option<ReferenceOr<$returns>> {
                    match category_and_key(reference)? {
                        ($key, key) => self.components.$field.get(key).cloned(),
                        _ => None,
                    }
                }
            )*
        }
    };
}

getters!(
    get_schema -> Schema {"schemas", schemas},
    get_callback -> Callback {"callbacks", callbacks},
    get_example -> Example {"examples", examples},
    get_header -> Header{"headers", headers},
    get_link -> Link {"links", links},
    get_parameter -> Parameter {"parameters", parameters},
    get_request_body -> RequestBody {"requestBodies", request_bodies},
    get_response -> Response {"responses", responses},
    get_security_scheme -> SecurityScheme {"securitySchemes", security_schemes},
);

fn category_and_key(s: &str) -> Option<(&str, &str)> {
    match s.split('/').collect::<Vec<_>>().as_slice() {
        ["#", "components", category, key] => Some((category, key)),
        _ => None,
    }
}

macro_rules! visitors {
    ($(
        $fn_name:ident($arg:ty) {
            $getter_name:ident,
            $descender_name:ident $(,)?
        }
    ),* $(,)?) => {
        $(
            fn $fn_name(&mut self, node: &'openapi mut ReferenceOr<$arg>) {
                match node {
                    Reference { reference } => match self.$getter_name(reference) {
                        Some(found) => {
                            self.references_expanded += 1;
                            *node = found;
                        }
                        None => {
                            self.bad_references.insert(reference.clone());
                        }
                    },
                    Item(node) => self.$descender_name(node),
                }
            }
        )*
    };
}

// visit discipline - only visit if there is an item in the first instance
impl<'openapi> VisitMut<'openapi> for ExpandOnce {
    fn visit_reference_or_box_schema_mut(&mut self, node: &'openapi mut ReferenceOr<Box<Schema>>) {
        match node {
            Reference { reference } => match self.get_schema(reference) {
                Some(found) => {
                    self.references_expanded += 1;
                    *node = match found {
                        // we only descend here to box the item - caller should repeat to visit again
                        Reference { reference } => Reference { reference },
                        Item(item) => Item(Box::new(item)),
                    }
                }
                None => {
                    self.bad_references.insert(reference.clone());
                }
            },
            Item(node) => self.visit_schema_mut(node),
        }
    }
    fn visit_reference_or_callback_mut(&mut self, node: &'openapi mut ReferenceOr<Callback>) {
        match node {
            Reference { reference } => match self.get_callback(reference) {
                Some(found) => {
                    self.references_expanded += 1;
                    *node = found;
                }
                None => {
                    self.bad_references.insert(reference.clone());
                }
            },
            Item(node) => {
                for (_, node) in node {
                    self.visit_path_item_mut(node)
                }
            }
        }
    }
    fn visit_reference_or_path_item_mut(&mut self, node: &'openapi mut ReferenceOr<PathItem>) {
        match node {
            Reference { reference } => {
                // can't get PathItems from components
                self.bad_references.insert(reference.clone());
            }
            Item(node) => self.visit_path_item_mut(node),
        }
    }

    visitors!(
        visit_reference_or_example_mut(Example) {
            get_example, visit_example_mut
        },
        visit_reference_or_header_mut(Header) {
            get_header, visit_header_mut
        },
        visit_reference_or_link_mut(Link) {
            get_link, visit_link_mut
        },
        visit_reference_or_parameter_mut(Parameter) {
            get_parameter, visit_parameter_mut
        },
        visit_reference_or_request_body_mut(RequestBody) {
            get_request_body, visit_request_body_mut
        },
        visit_reference_or_response_mut(Response) {
            get_response, visit_response_mut
        },
        visit_reference_or_schema_mut(Schema) {
            get_schema, visit_schema_mut
        },
        visit_reference_or_security_scheme_mut(SecurityScheme) {
            get_security_scheme, visit_security_scheme_mut
        }
    );
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;
    use serde_json::json;

    use super::*;

    fn do_test(
        mut start: serde_json::Value,
        mut expected: serde_json::Value,
        expected_not_found: &[&'static str],
    ) {
        // insert other mandatory fields
        for doc in [&mut start, &mut expected] {
            let doc = doc
                .as_object_mut()
                .expect("openapi top level must be a map");
            doc.insert(String::from("openapi"), json!("3.0.3"));
            doc.insert(
                String::from("info"),
                json!({
                    "title": "example",
                    "version": "0",
                }),
            );
        }
        let mut start = serde_json::from_value::<OpenAPI>(start).expect("invalid starting openapi");
        let expected =
            serde_json::from_value::<OpenAPI>(expected).expect("invalid expected openapi");
        let expected_not_found = expected_not_found
            .iter()
            .map(|s| String::from(*s))
            .collect();

        for doc in [&start, &expected] {
            // catch user error - OpenAPI deserialization silently ignores paths that don't start with '/'
            assert_ne!(0, doc.paths.paths.len(), "no paths to compare!")
        }

        let (_, not_found) = expand(&mut start);
        assert_eq!(start, expected);
        assert_eq!(not_found, expected_not_found);
    }

    #[test]
    fn simple() {
        do_test(
            json!({
                "paths": {
                    "/hello": {
                        "get": {
                            "responses": {
                                "200": {
                                    "$ref": "#/components/responses/HelloResponse",
                                }
                            }
                        }
                    }
                },
                "components": {
                    "responses": {
                        "HelloResponse": {
                            "description": "a description",
                        }
                    }
                }
            }),
            json!({
                "paths": {
                    "/hello": {
                        "get": {
                            "responses": {
                                "200": {
                                    "description": "a description",
                                }
                            }
                        }
                    }
                },
                "components": {
                    "responses": {
                        "HelloResponse": {
                            "description": "a description",
                        }
                    }
                }
            }),
            &[],
        )
    }
}
