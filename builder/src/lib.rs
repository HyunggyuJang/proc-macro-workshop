use proc_macro::TokenStream;
use quote::{quote, format_ident};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let structure = syn::parse::<syn::ItemStruct>(input).unwrap();
    let fields = structure.fields;
    let ident = structure.ident;
    let builder = format_ident!("{}Builder", ident);
    let fields_with_optional = fields
        .iter()
        .map(|field| {
            (field, extract_optioned_ty(&field.ty))
        });
    let fields_init = fields.iter().map(|field| {
        let field_ident = &field.ident;
        let field_ty = &field.ty;
        let field_init =
            if extract_matched_ty(field_ty, "Vec").is_some() {
                quote! { std::option::Option::Some(vec![]) }
            } else {
                quote! { std::option::Option::None }
            };
        quote! {
            #field_ident: #field_init,
        }
    });
    let builder_fields = fields_with_optional.clone().map(|(field, optional)| {
        let field_ident = &field.ident;
        let field_ty_raw = &field.ty;
        let field_ty = if optional.is_none() {quote! {std::option::Option<#field_ty_raw>}} else {quote! {#field_ty_raw}};
        quote! {
            #field_ident: #field_ty,
        }
    });
    let builder_methods = fields_with_optional.clone().map(|(field, optional)| {
        let field_ident = &field.ident;
        let field_ty = if let Some(extracted_ty) = optional {
            extracted_ty
        } else { &field.ty };
        let default_impl = quote! {
            fn #field_ident(&mut self, #field_ident: #field_ty) -> &mut Self {
                self.#field_ident = std::option::Option::Some(#field_ident);
                self
            }
        };
        if let Some(attr) = field.attrs.iter().find(|a| a.path.is_ident("builder")) {
            if let Ok(syn::Meta::List(list)) = attr.parse_meta() {
                if let Some(syn::NestedMeta::Meta(syn::Meta::NameValue(syn::MetaNameValue {
                    path,
                    lit: syn::Lit::Str(lit_str),
                    ..
                }))) = list.nested.last()
                {
                    if path.is_ident("each") && let Some(element_ty) = extract_matched_ty(field_ty, "Vec") {
                        let each_ident = format_ident!("{}", lit_str.value());
                        let mut each_fn = quote! {
                            fn #each_ident(&mut self, #each_ident: #element_ty) -> &mut Self {
                                self.#field_ident.get_or_insert(vec![]).push(#each_ident);
                                self
                            }
                        };
                        if lit_str.value() != field_ident.as_ref().expect("field should be named").to_string() {
                            each_fn = quote! {
                                #default_impl
                                #each_fn
                            };
                        }
                        return each_fn
                    }
                }
                return syn::Error::new_spanned(list, "expected `builder(each = \"...\")`").to_compile_error()
            }
            unreachable!("cannot come here")
        }
        return default_impl
    });
    let build_fields = fields_with_optional.clone().map(|(field, optional)| {
        let field_ident = &field.ident;
        let error_msg = format!("{} field is missing", field.ident.as_ref().expect("fields should be named").to_string());
        let field_access = if optional.is_some() {quote! {}} else {quote! {.ok_or(#error_msg)?}};
        quote! {
            #field_ident: self.#field_ident.clone()#field_access,
        }
    });
    quote! {
        impl #ident {
            pub fn builder() -> #builder {
                #builder {
                    #(#fields_init)*
                }
            }
        }
        #[derive(Eq, PartialEq)]
        pub struct #builder {
            #(#builder_fields)*
        }
        impl #builder {
            #(#builder_methods)*
            pub fn build (&mut self) -> std::result::Result<#ident, std::boxed::Box<dyn std::error::Error>> {
                Ok(#ident {
                    #(#build_fields)*
                })
            }
        }
    }.into()
}


fn extract_optioned_ty(ty: &syn::Type) -> std::option::Option<&syn::Type> {
    extract_matched_ty(ty, "Option")
}

fn extract_matched_ty<'a>(ty: &'a syn::Type, head_ty: &str) -> std::option::Option<&'a syn::Type> {
    if let syn::Type::Path(syn::TypePath { path: syn::Path { segments, .. }, .. }, ..) = ty {
        if let Some(syn::PathSegment { ident, arguments: syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments { args, .. })}) = segments.last() {
            if args.len() == 1 && ident == head_ty {
                if let Some(syn::GenericArgument::Type(ty)) = args.last() {
                    return Some(ty)
                }
            }
        }
    };
    None
}
