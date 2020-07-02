#[macro_export]
macro_rules! log {
    { @error $($rest:tt)* } => {
        let state = crate::terminal::get().save();
        print!("\x1bfc[{:16} | ERROR] ", module_path!());
        println!($($rest)*);
        crate::terminal::get().restore(state);
    };
    { @warn $($rest:tt)* } => {
        let state = crate::terminal::get().save();
        print!("\x1bfe[{:16} |  WARN] ", module_path!());
        println!($($rest)*);
        crate::terminal::get().restore(state);
    };
    { @info $($rest:tt)* } => {
        let state = crate::terminal::get().save();
        print!("\x1bfb[{:16} |  INFO] ", module_path!());
        println!($($rest)*);
        crate::terminal::get().restore(state);
    };
    { $($rest:tt)* } => {
        let state = crate::terminal::get().save();
        print!("\x1bf7[{:16} | DEBUG] ", module_path!());
        println!($($rest)*);
        crate::terminal::get().restore(state);
    }
}