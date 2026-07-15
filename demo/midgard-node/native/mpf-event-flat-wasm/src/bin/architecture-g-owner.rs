fn main() {
    let result = if std::env::args().any(|argument| argument == "--rpc") {
        midgard_mpf_event_flat_wasm::run_owner_rpc()
    } else {
        midgard_mpf_event_flat_wasm::run_owner_cli()
    };
    if let Err(error) = result {
        eprintln!("{error}");
        std::process::exit(1);
    }
}
