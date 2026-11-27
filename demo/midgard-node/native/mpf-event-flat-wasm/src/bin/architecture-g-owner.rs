fn main() {
    if !std::env::args().any(|argument| argument == "--rpc") {
        eprintln!("architecture-g-owner runs only as the node's child process; pass --rpc");
        std::process::exit(2);
    }
    if let Err(error) = midgard_mpf_event_flat_wasm::run_owner_rpc() {
        eprintln!("{error}");
        std::process::exit(1);
    }
}
