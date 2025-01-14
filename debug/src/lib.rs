use proc_macro::TokenStream;
use quote::quote;
use syn::{parse, Data, parse_quote};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let derive: syn::DeriveInput = parse(input).expect("Expected DeriveInput");

    let struct_ident = derive.ident;
    let fields: Vec<_> = match &derive.data {
        Data::Struct(st) => st.fields.iter().collect(),
        Data::Enum(en) => en.variants.iter().flat_map(|v| v.fields.iter()).collect(),
        Data::Union(un) => un.fields.named.iter().collect(),
    };
    let debug_fields = fields.iter().map(|field| {
        let field_ident = field.ident.clone().expect("named field");
        let field_ident_string = field_ident.to_string();
        if let Some(attr) = field.attrs.iter().find(|a| a.path.is_ident("debug")) {
            match attr.parse_meta() {
                Ok(syn::Meta::NameValue(syn::MetaNameValue {
                    lit: syn::Lit::Str(fmt_string),
                    ..
                })) => {
                    quote! {
                        .field(#field_ident_string, &std::format_args!(#fmt_string, &self.#field_ident))
                    }
                },
                Ok(ast) => syn::Error::new_spanned(ast, "expected `debug = \"...\"`").to_compile_error(),
                Err(err) => err.to_compile_error()
            }
        } else {
            quote! {
                .field(#field_ident_string, &self.#field_ident)
            }
        }
    });
    let struct_string = struct_ident.to_string();
    let mut generics = derive.generics;
    for type_param in generics.type_params_mut() {
        type_param.bounds.push(parse_quote!(std::fmt::Debug));
    }
    let where_clause_inspect: syn::WhereClause = syn::parse("where T: Debug".parse().unwrap()).unwrap();
    println!("where clause: {:#?}", where_clause_inspect);
    let (impl_generics, type_generics, where_clause) = generics.split_for_impl();
    quote!{
        impl #impl_generics std::fmt::Debug for #struct_ident #type_generics #where_clause {
            fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                fmt.debug_struct(#struct_string)
                    #(#debug_fields)*
                   .finish() // We're good to go!
            }
        }
    }.into()
}
