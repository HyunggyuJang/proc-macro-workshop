use proc_macro::TokenStream;
use quote::{quote, format_ident, ToTokens};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let structure = syn::parse::<syn::ItemStruct>(input).unwrap();
    let fields = structure.fields;
    let ty: Vec<_> = fields.iter().map(|field| field.ty.to_token_stream()).collect();
    println!("Ty {:#?}", ty);
    let ident = structure.ident;
    let builder = format_ident!("{}Builder", ident);
    quote! {
        impl #ident {
            pub fn builder() -> #builder {
                #builder::default()
            }
        }
        #[derive(Eq, PartialEq, Default)]
        pub struct #builder {
            executable: Option<String>,
            args: Option<Vec<String>>,
            env: Option<Vec<String>>,
            current_dir: Option<String>,
        }
        impl #builder {
            fn executable(&mut self, executable: String) -> &mut Self {
                self.executable = Some(executable);
                self
            }
            fn args(&mut self, args: Vec<String>) -> &mut Self {
                self.args = Some(args);
                self
            }
            fn env(&mut self, env: Vec<String>) -> &mut Self {
                self.env = Some(env);
                self
            }
            fn current_dir(&mut self, current_dir: String) -> &mut Self {
                self.current_dir = Some(current_dir);
                self
            }
            pub fn build (&mut self) -> Result<#ident, Box<dyn std::error::Error>> {
                Ok(#ident {
                    executable: self.executable.clone().ok_or("executable field is missing")?,
                    args: self.args.clone().ok_or("args field is missing")?,
                    env: self.env.clone().ok_or("env field is missing")?,
                    current_dir: self.current_dir.clone().ok_or("current_dir field is missing")?,
                })
            }
        }
    }.into()
}
