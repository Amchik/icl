macro_rules! define_lints {
    (@int $cat:ident::$name:ident LintLevel::$lev:ident) => {
        crate::ast_lints::Lint {
            category: stringify!($cat),
            name: stringify!($name),
            level: crate::ast_lints::LintLevel::$lev,
            worker: & $cat::$name,
        }
    };
    (@int $cat:ident::$name:ident) => {
        crate::ast_lints::Lint {
            category: stringify!($cat),
            name: stringify!($name),
            level: crate::ast_lints::LintLevel::Warn,
            worker: & $cat::$name,
        }
    };
    ($($cat:ident::$name:ident $( ( LintLevel::$lev:ident ) )? ),* $(,)?) => {
        &[
            $(
                crate::ast_lints::macros::define_lints!(@int $cat::$name $( LintLevel::$lev )?)
            )*
        ]
    };
}

pub(crate) use define_lints;

