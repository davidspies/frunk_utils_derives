extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, parse_quote, Data, DeriveInput, Fields, Index};

#[proc_macro_derive(ToRef)]
pub fn to_ref_derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let struct_name = &input.ident;
    let ref_struct_name = syn::Ident::new(&format!("{}FieldRefs", struct_name), struct_name.span());
    let struct_vis = &input.vis;

    // Adjust generics to include a new lifetime 'z
    let mut generics = input.generics.clone();
    let lifetime_def: syn::LifetimeParam = parse_quote!('z);
    generics
        .params
        .push(syn::GenericParam::Lifetime(lifetime_def));

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    let (_, ty_generics_no_lifetime, _) = input.generics.split_for_impl();

    let output = match input.data {
        Data::Struct(data) => match data.fields {
            Fields::Named(fields) => {
                let field_defs = fields.named.iter().map(|f| {
                    let name = &f.ident;
                    let ty = &f.ty;
                    let vis = &f.vis;
                    quote! { #vis #name: &'z #ty }
                });
                let to_ref_body = fields.named.iter().map(|f| {
                    let name = &f.ident;
                    quote! { #name: &self.#name }
                });
                quote! {
                    #[derive(frunk::Generic)]
                    #struct_vis struct #ref_struct_name #impl_generics {
                        #( #field_defs, )*
                    }

                    impl #impl_generics frunk::traits::ToRef<'z> for #struct_name #ty_generics_no_lifetime #where_clause {
                        type Output = #ref_struct_name #ty_generics;

                        fn to_ref(&'z self) -> Self::Output {
                            #ref_struct_name {
                                #( #to_ref_body, )*
                            }
                        }
                    }
                }
            }
            Fields::Unnamed(fields) => {
                let field_defs = fields.unnamed.iter().map(|f| {
                    let ty = &f.ty;
                    let vis = &f.vis;
                    quote! { #vis &'z #ty }
                });
                let to_ref_body = (0..fields.unnamed.len()).map(|i| {
                    let idx = Index::from(i);
                    quote! { &self.#idx }
                });
                quote! {
                    #[derive(frunk::Generic)]
                    #struct_vis struct #ref_struct_name #impl_generics (
                        #( #field_defs, )*
                    );

                    impl #impl_generics frunk::traits::ToRef<'z> for #struct_name #ty_generics_no_lifetime #where_clause {
                        type Output = #ref_struct_name #ty_generics;

                        fn to_ref(&'z self) -> Self::Output {
                            #ref_struct_name (
                                #( #to_ref_body, )*
                            )
                        }
                    }
                }
            }
            Fields::Unit => panic!("ToRef derive macro does not support unit structs"),
        },
        _ => panic!("ToRef derive macro only supports structs"),
    };

    output.into()
}

#[proc_macro_derive(ToMut)]
pub fn to_mut_derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let struct_name = &input.ident;
    let ref_struct_name =
        syn::Ident::new(&format!("{}FieldMutRefs", struct_name), struct_name.span());
    let struct_vis = &input.vis;

    // Adjust generics to include a new lifetime 'z
    let mut generics = input.generics.clone();
    let lifetime_def: syn::LifetimeParam = parse_quote!('z);
    generics
        .params
        .push(syn::GenericParam::Lifetime(lifetime_def));

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    let (_, ty_generics_no_lifetime, _) = input.generics.split_for_impl();

    let output = match input.data {
        Data::Struct(data) => match data.fields {
            Fields::Named(fields) => {
                let field_defs = fields.named.iter().map(|f| {
                    let name = &f.ident;
                    let ty = &f.ty;
                    let vis = &f.vis;
                    quote! { #vis #name: &'z mut #ty }
                });
                let to_mut_body = fields.named.iter().map(|f| {
                    let name = &f.ident;
                    quote! { #name: &mut self.#name }
                });
                quote! {
                    #[derive(frunk::Generic)]
                    #struct_vis struct #ref_struct_name #impl_generics {
                        #( #field_defs, )*
                    }

                    impl #impl_generics frunk::traits::ToMut<'z> for #struct_name #ty_generics_no_lifetime #where_clause {
                        type Output = #ref_struct_name #ty_generics;

                        fn to_mut(&'z mut self) -> Self::Output {
                            #ref_struct_name {
                                #( #to_mut_body, )*
                            }
                        }
                    }
                }
            }
            Fields::Unnamed(fields) => {
                let field_defs = fields.unnamed.iter().map(|f| {
                    let ty = &f.ty;
                    let vis = &f.vis;
                    quote! { #vis &'z mut #ty }
                });
                let to_mut_body = (0..fields.unnamed.len()).map(|i| {
                    let idx = Index::from(i);
                    quote! { &mut self.#idx }
                });
                quote! {
                    #[derive(frunk::Generic)]
                    #struct_vis struct #ref_struct_name #impl_generics (
                        #( #field_defs, )*
                    );

                    impl #impl_generics frunk::traits::ToMut<'z> for #struct_name #ty_generics_no_lifetime #where_clause {
                        type Output = #ref_struct_name #ty_generics;

                        fn to_mut(&'z mut self) -> Self::Output {
                            #ref_struct_name (
                                #( #to_mut_body, )*
                            )
                        }
                    }
                }
            }
            Fields::Unit => panic!("ToMut derive macro does not support unit structs"),
        },
        _ => panic!("ToMut derive macro only supports structs"),
    };

    output.into()
}
