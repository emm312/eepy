pub const DUMP_TOKENS : &str = "TAUBE_DUMP_TOKENS";
pub const DUMP_AST    : &str = "TAUBE_DUMP_AST";
pub const DUMP_ASM     : &str = "TAUBE_DUMP_ASM";

pub fn env_flag(value: &'static str) -> bool {
    #[cfg(features = "afl")]
    {
        return false
    };

    #[cfg(not(features = "afl"))]
    {
        std::env::var(value).unwrap_or("0".to_string()) == "1"
    }
}
