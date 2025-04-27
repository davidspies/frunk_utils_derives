extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{Data, DeriveInput, Fields, Ident, Index, parse_macro_input, parse_quote};

#[proc_macro_derive(ToRef)]
pub fn to_ref_derive(input: TokenStream) -> TokenStream {
    derive_to_ref_or_mut(input, RefType::Ref)
}

#[proc_macro_derive(ToMut)]
pub fn to_mut_derive(input: TokenStream) -> TokenStream {
    derive_to_ref_or_mut(input, RefType::Mut)
}

#[derive(Clone, Copy)]
enum RefType {
    Ref,
    Mut,
}

impl RefType {
    fn trait_path(self) -> syn::Path {
        match self {
            RefType::Ref => parse_quote!(frunk::traits::ToRef),
            RefType::Mut => parse_quote!(frunk::traits::ToMut),
        }
    }

    fn ref_struct_suffix(self) -> &'static str {
        match self {
            RefType::Ref => "FieldRefs",
            RefType::Mut => "FieldMutRefs",
        }
    }

    fn field_ref_quote(self) -> proc_macro2::TokenStream {
        match self {
            RefType::Ref => quote!(&'z),
            RefType::Mut => quote!(&'z mut),
        }
    }

    fn method_name(self) -> syn::Ident {
        match self {
            RefType::Ref => Ident::new("to_ref", Span::call_site()),
            RefType::Mut => Ident::new("to_mut", Span::call_site()),
        }
    }

    fn self_param(self) -> proc_macro2::TokenStream {
        match self {
            RefType::Ref => quote!(&'z self),
            RefType::Mut => quote!(&'z mut self),
        }
    }

    fn self_access_quote(self) -> proc_macro2::TokenStream {
        match self {
            RefType::Ref => quote!(&self),
            RefType::Mut => quote!(&mut self),
        }
    }

    fn derive_name(self) -> &'static str {
        match self {
            RefType::Ref => "ToRef",
            RefType::Mut => "ToMut",
        }
    }
}

fn derive_to_ref_or_mut(input: TokenStream, ref_type: RefType) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let struct_name = &input.ident;
    let ref_struct_name = syn::Ident::new(
        &format!("{}{}", struct_name, ref_type.ref_struct_suffix()),
        struct_name.span(),
    );

    let generics_orig = &input.generics;
    let (_, ty_generics_orig, _) = generics_orig.split_for_impl();

    let mut generics_with_z = generics_orig.clone();
    let lifetime_def: syn::LifetimeParam = parse_quote!('z);
    generics_with_z
        .params
        .push(syn::GenericParam::Lifetime(lifetime_def.clone()));
    let (impl_generics_with_z, ty_generics_with_z, where_clause_with_z_bounds) =
        generics_with_z.split_for_impl();

    let mut where_clause_for_impl =
        where_clause_with_z_bounds
            .cloned()
            .unwrap_or_else(|| syn::WhereClause {
                where_token: Default::default(),
                predicates: Default::default(),
            });

    let z_lifetime_ref = &lifetime_def.lifetime;
    match &input.data {
        Data::Struct(data) => {
            let fields_iter = match &data.fields {
                Fields::Named(fields) => fields.named.iter().map(|f| &f.ty).collect::<Vec<_>>(),
                Fields::Unnamed(fields) => fields.unnamed.iter().map(|f| &f.ty).collect::<Vec<_>>(),
                Fields::Unit => {
                    panic!(
                        "{} derive macro does not support unit structs",
                        ref_type.derive_name()
                    )
                }
            };
            for field_ty in fields_iter {
                where_clause_for_impl
                    .predicates
                    .push(parse_quote!(#field_ty: #z_lifetime_ref));
            }
        }
        _ => {
            panic!(
                "{} derive macro only supports structs",
                ref_type.derive_name()
            )
        }
    };

    let struct_vis = &input.vis;

    let trait_path = ref_type.trait_path();
    let field_ref_quote = ref_type.field_ref_quote();
    let method_name = ref_type.method_name();
    let self_param = ref_type.self_param();
    let self_access_quote = ref_type.self_access_quote();

    let output = match input.data {
        Data::Struct(data) => match data.fields {
            Fields::Named(fields) => {
                let field_defs = fields.named.iter().map(|f| {
                    let name = &f.ident;
                    let ty = &f.ty;
                    let vis = &f.vis;
                    quote! { #vis #name: #field_ref_quote #ty }
                });
                let body_assignments = fields.named.iter().map(|f| {
                    let name = &f.ident;
                    quote! { #name: #self_access_quote.#name }
                });
                quote! {
                    #[derive(frunk::Generic, frunk::LabelledGeneric)]
                    #struct_vis struct #ref_struct_name #impl_generics_with_z #where_clause_with_z_bounds {
                        #( #field_defs, )*
                    }

                    impl #impl_generics_with_z #trait_path<'z> for #struct_name #ty_generics_orig #where_clause_for_impl {
                        type Output = #ref_struct_name #ty_generics_with_z;

                        fn #method_name(#self_param) -> Self::Output {
                            #ref_struct_name {
                                #( #body_assignments, )*
                            }
                        }
                    }
                }
            }
            Fields::Unnamed(fields) => {
                let field_defs = fields.unnamed.iter().map(|f| {
                    let ty = &f.ty;
                    let vis = &f.vis;
                    quote! { #vis #field_ref_quote #ty }
                });
                let body_assignments = (0..fields.unnamed.len()).map(|i| {
                    let idx = Index::from(i);
                    quote! { #self_access_quote.#idx }
                });
                quote! {
                    #[derive(frunk::Generic)]
                    #struct_vis struct #ref_struct_name #impl_generics_with_z #where_clause_with_z_bounds (
                        #( #field_defs, )*
                    );

                    impl #impl_generics_with_z #trait_path<'z> for #struct_name #ty_generics_orig #where_clause_for_impl {
                        type Output = #ref_struct_name #ty_generics_with_z;

                        fn #method_name(#self_param) -> Self::Output {
                            #ref_struct_name (
                                #( #body_assignments, )*
                            )
                        }
                    }
                }
            }
            Fields::Unit => unreachable!("Unit struct panic handled previously"),
        },
        _ => unreachable!("Non-struct panic handled previously"),
    };

    output.into()
}
