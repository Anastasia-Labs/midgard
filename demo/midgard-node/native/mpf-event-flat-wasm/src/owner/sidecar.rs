use super::*;

pub(super) fn marker_from_payload(payload: &[u8]) -> Result<Hash, String> {
    if payload.len() < INPUT_HEADER_BYTES || &payload[..4] != INPUT_MAGIC {
        return Err("Architecture G input payload header is invalid".to_owned());
    }
    Ok(payload[40..72].try_into().unwrap())
}

pub(super) fn sidecar_digest(marker: &Hash, payload: &[u8]) -> Hash {
    digest(&[b"MIDGARD-MPF-ARCH-G-SIDECAR-V1", marker, payload])
}

pub(super) fn load_sidecar(path: &Path, marker: Hash) -> Result<Vec<u8>, String> {
    let bytes = fs::read(path).map_err(|error| format!("missing:{error}"))?;
    if bytes.len() < SIDECAR_HEADER_BYTES || &bytes[..4] != SIDECAR_MAGIC {
        return Err("corrupt:header".to_owned());
    }
    if u16::from_le_bytes(bytes[4..6].try_into().unwrap()) != ABI_VERSION
        || u16::from_le_bytes(bytes[6..8].try_into().unwrap()) != 0
    {
        return Err("corrupt:version".to_owned());
    }
    let sidecar_marker: Hash = bytes[8..40].try_into().unwrap();
    if sidecar_marker != marker {
        return Err("stale:marker".to_owned());
    }
    let payload_length = u64::from_le_bytes(bytes[40..48].try_into().unwrap()) as usize;
    if bytes.len() != SIDECAR_HEADER_BYTES + payload_length {
        return Err("corrupt:length".to_owned());
    }
    let expected: Hash = bytes[48..80].try_into().unwrap();
    let payload = &bytes[SIDECAR_HEADER_BYTES..];
    if sidecar_digest(&marker, payload) != expected {
        return Err("corrupt:digest".to_owned());
    }
    Ok(payload.to_vec())
}

pub(super) fn write_sidecar(path: &Path, marker: Hash, payload: &[u8]) -> Result<(), String> {
    let temporary = path.with_extension("tmp");
    let mut file = File::create(&temporary).map_err(|error| error.to_string())?;
    file.write_all(SIDECAR_MAGIC)
        .map_err(|error| error.to_string())?;
    file.write_all(&ABI_VERSION.to_le_bytes())
        .map_err(|error| error.to_string())?;
    file.write_all(&0u16.to_le_bytes())
        .map_err(|error| error.to_string())?;
    file.write_all(&marker).map_err(|error| error.to_string())?;
    file.write_all(&(payload.len() as u64).to_le_bytes())
        .map_err(|error| error.to_string())?;
    file.write_all(&sidecar_digest(&marker, payload))
        .map_err(|error| error.to_string())?;
    file.write_all(payload).map_err(|error| error.to_string())?;
    file.sync_all().map_err(|error| error.to_string())?;
    fs::rename(&temporary, path).map_err(|error| error.to_string())?;
    Ok(())
}
