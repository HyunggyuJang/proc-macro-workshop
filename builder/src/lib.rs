use proc_macro::TokenStream;
use quote::{quote, format_ident};

#[proc_macro_derive(Builder)]
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
    let builder_fields = fields_with_optional.clone().map(|(field, optional)| {
        let field_ident = &field.ident;
        let field_ty_raw = &field.ty;
        let field_ty = if optional.is_none() {quote! {Option<#field_ty_raw>}} else {quote! {#field_ty_raw}};
        quote! {
            #field_ident: #field_ty,
        }
    });
    let builder_methods = fields_with_optional.clone().map(|(field, optional)| {
        let field_ident = &field.ident;
        let field_ty = if let Some(extracted_ty) = optional {
            extracted_ty
        } else { &field.ty };
        quote! {
            fn #field_ident(&mut self, #field_ident: #field_ty) -> &mut Self {
                self.#field_ident = Some(#field_ident);
                self
            }
        }
    });
    let build_fields = fields_with_optional.clone().map(|(field, optional)| {
        let field_ident = &field.ident;
        let error_msg = format!("{} field is missing", field.ident.as_ref().unwrap().to_string());
        let field_access = if optional.is_some() {quote! {}} else {quote! {.ok_or(#error_msg)?}};
        quote! {
            #field_ident: self.#field_ident.clone()#field_access,
        }
    });
    quote! {
        impl #ident {
            pub fn builder() -> #builder {
                #builder::default()
            }
        }
        #[derive(Eq, PartialEq, Default)]
        pub struct #builder {
            #(#builder_fields)*
        }
        impl #builder {
            #(#builder_methods)*
            pub fn build (&mut self) -> Result<#ident, Box<dyn std::error::Error>> {
                Ok(#ident {
                    #(#build_fields)*
                })
            }
        }
    }.into()
}


fn extract_optioned_ty(ty: &syn::Type) -> Option<&syn::Type> {
    if let syn::Type::Path(syn::TypePath { path: syn::Path { segments, .. }, .. }, ..) = ty {
        if let Some(syn::PathSegment { ident, arguments: syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments { args, .. })}) = segments.last() {
            if args.len() == 1 && ident == "Option" {
                if let Some(syn::GenericArgument::Type(ty)) = args.last() {
                    return Some(ty)
                }
            }
        }
    };
    None
}
