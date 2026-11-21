use super::*;

pub(super) fn rss_kib(field: &str) -> u64 {
    fs::read_to_string("/proc/self/status")
        .ok()
        .and_then(|status| {
            status.lines().find_map(|line| {
                line.strip_prefix(field)
                    .and_then(|value| value.split_whitespace().next())
                    .and_then(|value| value.parse().ok())
            })
        })
        .unwrap_or(0)
}

pub(super) fn argument(name: &str) -> Result<PathBuf, String> {
    let prefix = format!("--{name}=");
    env::args()
        .find_map(|argument| argument.strip_prefix(&prefix).map(PathBuf::from))
        .ok_or_else(|| format!("Missing --{name}=..."))
}

pub(super) fn mode() -> String {
    env::args()
        .find_map(|argument| argument.strip_prefix("--mode=").map(str::to_owned))
        .unwrap_or_else(|| "prepare".to_owned())
}

pub fn run_owner_cli() -> Result<(), String> {
    let input_path = argument("input")?;
    let sidecar_path = argument("sidecar")?;
    let events_path = argument("events")?;
    let run_mode = mode();
    if run_mode != "prepare" && run_mode != "recover" {
        return Err("Architecture G owner mode must be prepare or recover".to_owned());
    }
    let input_marker = {
        let mut header = [0u8; INPUT_HEADER_BYTES];
        File::open(&input_path)
            .and_then(|mut file| file.read_exact(&mut header))
            .map_err(|error| error.to_string())?;
        marker_from_payload(&header)?
    };
    let startup_started_at = Instant::now();
    let (payload, source, rebuild_reason) = match load_sidecar(&sidecar_path, input_marker) {
        Ok(payload) => (payload, "sidecar", "none".to_owned()),
        Err(reason) => {
            let payload = fs::read(&input_path).map_err(|error| error.to_string())?;
            if marker_from_payload(&payload)? != input_marker {
                return Err("Architecture G input marker changed while loading".to_owned());
            }
            (payload, "level-export", reason)
        }
    };
    let index = FullIndex::from_payload(&payload, input_marker)?;
    if source == "level-export" {
        write_sidecar(&sidecar_path, input_marker, &payload)?;
    }
    let startup_ms = startup_started_at.elapsed().as_secs_f64() * 1_000.0;
    let rss_after_startup = rss_kib("VmRSS:");
    let peak_after_startup = rss_kib("VmHWM:");
    let diagnostics = index.diagnostics();
    drop(payload);

    let event_bytes = fs::read(&events_path).map_err(|error| error.to_string())?;
    let stream = parse_event_stream(&event_bytes)?;
    let simulation_started_at = Instant::now();
    let base_arena = index.proof_arena(&stream)?;
    let proof_nodes = base_arena.nodes.len();
    let mut simulated_owner = SimulatedOwner::new(stream.base_root);
    let promoted_handle = simulated_owner.fork(&base_arena)?;
    let replay_handle = simulated_owner.fork(&base_arena)?;
    let active_generation_cap_rejected = simulated_owner.fork(&base_arena).is_err();
    if !active_generation_cap_rejected {
        return Err("Architecture G simulated generation cap did not fail closed".to_owned());
    }
    let roots = simulated_owner.apply(promoted_handle, &stream)?;
    let candidate = roots.last().copied().unwrap_or(stream.base_root);
    let generated_nodes = simulated_owner.generations[&promoted_handle]
        .arena
        .nodes
        .len()
        .saturating_sub(
            simulated_owner.generations[&promoted_handle]
                .arena
                .base_count,
        );
    let replay_roots = simulated_owner.apply(replay_handle, &stream)?;
    if replay_roots != roots {
        return Err("Architecture G crash replay roots diverged".to_owned());
    }
    simulated_owner.discard(replay_handle)?;
    let discarded_handle_rejected = simulated_owner.root(replay_handle).is_err();
    let stale_handle = simulated_owner.fork(&base_arena)?;
    if simulated_owner.promote(promoted_handle)? != candidate {
        return Err("Architecture G simulated promotion root diverged".to_owned());
    }
    let stale_generation_rejected = simulated_owner.promote(stale_handle).is_err();
    simulated_owner.discard(stale_handle)?;
    if !discarded_handle_rejected || !stale_generation_rejected {
        return Err("Architecture G simulated stale/discard rejection failed".to_owned());
    }
    let root_bytes: Vec<u8> = roots.iter().flatten().copied().collect();
    let replay_digest = digest(&[
        b"MIDGARD-MPF-ARCH-G-REPLAY-V1",
        &stream.base_root,
        &candidate,
        &root_bytes,
        &event_bytes,
    ]);
    let simulation_ms = simulation_started_at.elapsed().as_secs_f64() * 1_000.0;
    let steady_rss = rss_kib("VmRSS:");
    let peak_rss = rss_kib("VmHWM:");
    if peak_rss > 2 * 1024 * 1024 {
        return Err(format!(
            "Architecture G owner exceeded 2 GiB RSS: peak_kib={peak_rss}"
        ));
    }
    println!(
        concat!(
            "{{\"mode\":\"{}\",\"source\":\"{}\",\"rebuildReason\":\"{}\",",
            "\"marker\":\"{}\",\"candidateRoot\":\"{}\",\"replayDigest\":\"{}\",",
            "\"startupMs\":{},\"simulationMs\":{},\"nodes\":{},\"branches\":{},",
            "\"leaves\":{},\"edges\":{},\"compactBytes\":{},\"proofNodes\":{},",
            "\"eventCount\":{},\"generatedNodes\":{},\"rssAfterStartupKiB\":{},",
            "\"peakAfterStartupKiB\":{},\"steadyRssKiB\":{},\"peakRssKiB\":{},",
            "\"rootsExactOnReplay\":true,\"discardedHandleRejected\":{},",
            "\"staleGenerationRejected\":{},\"activeGenerationCapRejected\":{},",
            "\"fixtureWrites\":0}}"
        ),
        run_mode,
        source,
        rebuild_reason,
        hex(&input_marker),
        hex(&candidate),
        hex(&replay_digest),
        startup_ms,
        simulation_ms,
        diagnostics.nodes,
        diagnostics.branches,
        diagnostics.leaves,
        diagnostics.edges,
        diagnostics.compact_bytes,
        proof_nodes,
        stream.events.len(),
        generated_nodes,
        rss_after_startup,
        peak_after_startup,
        steady_rss,
        peak_rss,
        discarded_handle_rejected,
        stale_generation_rejected,
        active_generation_cap_rejected,
    );
    Ok(())
}
