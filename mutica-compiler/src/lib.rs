use lalrpop_util::lalrpop_mod;
lalrpop_mod!(pub grammar, "/parser/grammar.rs");

pub mod parser;
pub mod util;
pub use lalrpop_util;
pub use logos;
pub use mutica_core::ariadne;