extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::{Data, DeriveInput, Fields, Index, parse_macro_input, parse_quote};

#[proc_macro_derive(ToRef)]
pub fn to_ref_derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let struct_name = &input.ident;
    let ref_struct_name = syn::Ident::new(&format!("{}FieldRefs", struct_name), struct_name.span());
    let struct_vis = &input.vis;

    // Original generics (for the struct type in the impl)
    let generics_orig = &input.generics;
    let (_, ty_generics_orig, _) = generics_orig.split_for_impl();

    // Generics with 'z added (for struct definition and impl <...>)
    let mut generics_with_z = generics_orig.clone();
    let lifetime_def: syn::LifetimeParam = parse_quote!('z);
    generics_with_z
        .params
        .push(syn::GenericParam::Lifetime(lifetime_def.clone()));
    let (impl_generics_with_z, ty_generics_with_z, where_clause_with_z_bounds) =
        generics_with_z.split_for_impl();

    // Where clause for the impl block (original bounds + FieldTy: 'z bounds)
    let mut where_clause_for_impl =
        where_clause_with_z_bounds
            .cloned()
            .unwrap_or_else(|| syn::WhereClause {
                where_token: Default::default(),
                predicates: Default::default(),
            });

    // Add field type bounds: FieldTy: 'z
    let z_lifetime_ref = &lifetime_def.lifetime;
    match &input.data {
        Data::Struct(data) => {
            let fields_iter = match &data.fields {
                Fields::Named(fields) => fields.named.iter().map(|f| &f.ty).collect::<Vec<_>>(),
                Fields::Unnamed(fields) => fields.unnamed.iter().map(|f| &f.ty).collect::<Vec<_>>(),
                Fields::Unit => {
                    panic!("ToRef derive macro does not support unit structs")
                }
            };
            for field_ty in fields_iter {
                where_clause_for_impl
                    .predicates
                    .push(parse_quote!(#field_ty: #z_lifetime_ref));
            }
        }
        _ => panic!("ToRef derive macro only supports structs"),
    };

    let output = match input.data {
        Data::Struct(data) => match data.fields {
            Fields::Named(fields) => {
                let field_defs = fields.named.iter().map(|f| {
                    let name = &f.ident;
                    let ty = &f.ty;
                    let vis = &f.vis;
                    quote! { #vis #name: &'z #ty } // Use 'z here
                });
                let to_ref_body = fields.named.iter().map(|f| {
                    let name = &f.ident;
                    quote! { #name: &self.#name }
                });
                quote! {
                    // Struct uses generics with 'z
                    #[derive(frunk::Generic, frunk::LabelledGeneric)]
                    #struct_vis struct #ref_struct_name #impl_generics_with_z #where_clause_with_z_bounds {
                        #( #field_defs, )*
                    }

                    // Impl uses generics with 'z for <...>, original generics for Struct<...>, extended where clause
                    impl #impl_generics_with_z frunk::traits::ToRef<'z> for #struct_name #ty_generics_orig #where_clause_for_impl {
                        type Output = #ref_struct_name #ty_generics_with_z;

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
                    quote! { #vis &'z #ty } // Use 'z here
                });
                let to_ref_body = (0..fields.unnamed.len()).map(|i| {
                    let idx = Index::from(i);
                    quote! { &self.#idx }
                });
                quote! {
                    // Struct uses generics with 'z
                    #[derive(frunk::Generic)]
                    #struct_vis struct #ref_struct_name #impl_generics_with_z #where_clause_with_z_bounds (
                        #( #field_defs, )*
                    );

                    // Impl uses generics with 'z for <...>, original generics for Struct<...>, extended where clause
                    impl #impl_generics_with_z frunk::traits::ToRef<'z> for #struct_name #ty_generics_orig #where_clause_for_impl {
                        type Output = #ref_struct_name #ty_generics_with_z; // Output uses generics with 'z

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

    // Original generics (for the struct type in the impl)
    let generics_orig = &input.generics;
    let (_, ty_generics_orig, _) = generics_orig.split_for_impl();

    // Generics with 'z added (for struct definition and impl <...>)
    let mut generics_with_z = generics_orig.clone();
    let lifetime_def: syn::LifetimeParam = parse_quote!('z);
    generics_with_z
        .params
        .push(syn::GenericParam::Lifetime(lifetime_def.clone()));
    let (impl_generics_with_z, ty_generics_with_z, where_clause_with_z_bounds) =
        generics_with_z.split_for_impl();

    // Where clause for the impl block (original bounds + FieldTy: 'z bounds)
    let mut where_clause_for_impl =
        where_clause_with_z_bounds
            .cloned()
            .unwrap_or_else(|| syn::WhereClause {
                where_token: Default::default(),
                predicates: Default::default(),
            });

    // Add field type bounds: FieldTy: 'z
    let z_lifetime_ref = &lifetime_def.lifetime;
    match &input.data {
        Data::Struct(data) => {
            let fields_iter = match &data.fields {
                Fields::Named(fields) => fields.named.iter().map(|f| &f.ty).collect::<Vec<_>>(),
                Fields::Unnamed(fields) => fields.unnamed.iter().map(|f| &f.ty).collect::<Vec<_>>(),
                Fields::Unit => {
                    panic!("ToMut derive macro does not support unit structs")
                }
            };
            for field_ty in fields_iter {
                where_clause_for_impl
                    .predicates
                    .push(parse_quote!(#field_ty: #z_lifetime_ref));
            }
        }
        _ => panic!("ToMut derive macro only supports structs"),
    };

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
                    #[derive(frunk::Generic, frunk::LabelledGeneric)]
                    #struct_vis struct #ref_struct_name #impl_generics_with_z #where_clause_with_z_bounds {
                        #( #field_defs, )*
                    }

                    impl #impl_generics_with_z frunk::traits::ToMut<'z> for #struct_name #ty_generics_orig #where_clause_for_impl {
                        type Output = #ref_struct_name #ty_generics_with_z;

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
                    #struct_vis struct #ref_struct_name #impl_generics_with_z #where_clause_with_z_bounds (
                        #( #field_defs, )*
                    );

                    impl #impl_generics_with_z frunk::traits::ToMut<'z> for #struct_name #ty_generics_orig #where_clause_for_impl {
                        type Output = #ref_struct_name #ty_generics_with_z; // Output uses generics with 'z

                        fn to_mut(&'z mut self) -> Self::Output {
                            #ref_struct_name (
                                #( #to_mut_body, )*
                            )
                        }
                    }
                }
            }
            Fields::Unit => panic!("ToMut derive macro does not support unit structs"), // Already handled above
        },
        _ => panic!("ToMut derive macro only supports structs"), // Already handled above
    };

    output.into()
}
