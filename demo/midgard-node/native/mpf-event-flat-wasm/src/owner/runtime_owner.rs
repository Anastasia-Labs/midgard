use super::*;

pub(super) struct RuntimeGeneration {
    pub(super) base_root: Hash,
    pub(super) arena: Option<Arena>,
    pub(super) candidate_root: Option<Hash>,
    pub(super) prepared: bool,
}

pub(super) struct RuntimeOwner {
    pub(super) epoch: [u8; 16],
    pub(super) marker: Option<Hash>,
    pub(super) index: Option<FullIndex>,
    pub(super) load_payload: Vec<u8>,
    pub(super) generations: HashMap<[u8; 16], RuntimeGeneration>,
    pub(super) next_generation: u64,
}

impl RuntimeOwner {
    pub(super) fn new(epoch: [u8; 16]) -> Self {
        Self {
            epoch,
            marker: None,
            index: None,
            load_payload: Vec::new(),
            generations: HashMap::new(),
            next_generation: 1,
        }
    }

    pub(super) fn generation_id(&mut self) -> Result<[u8; 16], String> {
        let counter = self.next_generation;
        self.next_generation = counter
            .checked_add(1)
            .ok_or_else(|| "Architecture G generation id overflow".to_owned())?;
        let hash = digest(&[
            b"MIDGARD-MPF-OWNER-GENERATION-V1",
            &self.epoch,
            &counter.to_le_bytes(),
        ]);
        Ok(hash[..16].try_into().unwrap())
    }

    pub(super) fn fork(&mut self, base_root: Hash) -> Result<[u8; 16], String> {
        if self.generations.len() >= 2 {
            return Err("Architecture G active-generation cap exceeded".to_owned());
        }
        if self.marker != Some(base_root) {
            return Err("Architecture G fork base root is stale".to_owned());
        }
        let id = self.generation_id()?;
        self.generations.insert(
            id,
            RuntimeGeneration {
                base_root,
                arena: None,
                candidate_root: None,
                prepared: false,
            },
        );
        Ok(id)
    }

    pub(super) fn apply(
        &mut self,
        id: [u8; 16],
        event_bytes: &[u8],
    ) -> Result<(Hash, Vec<Hash>, u64, u64), String> {
        let stream = parse_event_stream(event_bytes)?;
        let generation = self
            .generations
            .get_mut(&id)
            .ok_or_else(|| "Architecture G generation handle is stale".to_owned())?;
        if stream.base_root != generation.base_root || self.marker != Some(stream.base_root) {
            return Err("Architecture G replay log base root is stale".to_owned());
        }
        if generation.arena.is_some() {
            return Err("Architecture G generation has already been applied".to_owned());
        }
        let index = self
            .index
            .as_ref()
            .ok_or_else(|| "Architecture G owner is not ready".to_owned())?;
        let proof_started_at = Instant::now();
        let mut arena = index.proof_arena(&stream)?;
        let proof_duration_ns = u64::try_from(proof_started_at.elapsed().as_nanos())
            .map_err(|_| "Architecture G proof timing overflow".to_owned())?;
        let mutation_started_at = Instant::now();
        let mut roots = Vec::with_capacity(stream.events.len());
        for event in &stream.events {
            roots.push(arena.apply_event(event)?);
        }
        let mutation_duration_ns = u64::try_from(mutation_started_at.elapsed().as_nanos())
            .map_err(|_| "Architecture G mutation timing overflow".to_owned())?;
        let candidate = roots.last().copied().unwrap_or(stream.base_root);
        let observed_rss_bytes = (rss_kib("VmRSS:") as usize).saturating_mul(1024);
        if observed_rss_bytes > MAX_RESIDENT_BYTES {
            return Err(format!(
                "Architecture G apply exceeds observed RSS cap: rss_bytes={observed_rss_bytes},max_bytes={MAX_RESIDENT_BYTES}"
            ));
        }
        generation.arena = Some(arena);
        generation.candidate_root = Some(candidate);
        Ok((candidate, roots, proof_duration_ns, mutation_duration_ns))
    }

    pub(super) fn generated_records(
        &mut self,
        id: [u8; 16],
    ) -> Result<(Hash, Hash, Vec<Vec<u8>>), String> {
        self.generated_records_with_caps(id, FULL_INDEX_MAX_RECORDS, MAX_RESIDENT_BYTES)
    }

    pub(super) fn generated_records_with_caps(
        &mut self,
        id: [u8; 16],
        max_resident_nodes: usize,
        max_resident_bytes: usize,
    ) -> Result<(Hash, Hash, Vec<Vec<u8>>), String> {
        let (base, candidate, records, new_records) = {
            let generation = self
                .generations
                .get(&id)
                .ok_or_else(|| "Architecture G generation handle is stale".to_owned())?;
            let candidate = generation
                .candidate_root
                .ok_or_else(|| "Architecture G generation has not been applied".to_owned())?;
            let arena = generation
                .arena
                .as_ref()
                .ok_or_else(|| "Architecture G generation arena is missing".to_owned())?;
            let records = generated_reachable_records(arena, candidate)?;
            let index = self
                .index
                .as_ref()
                .ok_or_else(|| "Architecture G owner is not ready".to_owned())?;
            let new_records = generated_reachable_nodes(arena, candidate)?
                .into_iter()
                .filter(|record| !index.ids.contains_key(&record.hash()))
                .collect::<Vec<_>>();
            (generation.base_root, candidate, records, new_records)
        };
        self.index
            .as_mut()
            .ok_or_else(|| "Architecture G owner is not ready".to_owned())?
            .reserve_promotion(&new_records, max_resident_nodes, max_resident_bytes)?;
        let observed_rss_bytes = (rss_kib("VmRSS:") as usize).saturating_mul(1024);
        if observed_rss_bytes > max_resident_bytes {
            return Err(format!(
                "Architecture G promotion exceeds observed RSS cap: rss_bytes={observed_rss_bytes},max_bytes={max_resident_bytes}"
            ));
        }
        self.generations
            .get_mut(&id)
            .ok_or_else(|| "Architecture G generation handle is stale".to_owned())?
            .prepared = true;
        Ok((base, candidate, records))
    }

    pub(super) fn commit(&mut self, id: [u8; 16]) -> Result<Hash, String> {
        let generation = self
            .generations
            .get(&id)
            .ok_or_else(|| "Architecture G generation handle is stale".to_owned())?;
        if !generation.prepared {
            return Err("Architecture G generation was not prepared for promotion".to_owned());
        }
        if self.marker != Some(generation.base_root) {
            return Err("Architecture G promotion marker is stale".to_owned());
        }
        let candidate = generation
            .candidate_root
            .ok_or_else(|| "Architecture G generation candidate is missing".to_owned())?;
        let arena = generation
            .arena
            .as_ref()
            .ok_or_else(|| "Architecture G generation arena is missing".to_owned())?;
        let index = self
            .index
            .as_mut()
            .ok_or_else(|| "Architecture G owner is not ready".to_owned())?;
        let records = generated_reachable_nodes(arena, candidate)?;
        let new_records: Vec<Node> = records
            .into_iter()
            .filter(|record| !index.ids.contains_key(&record.hash()))
            .collect();
        let child_start = index.children.len();
        for record in new_records {
            index.append(record)?;
        }
        index.resolve_child_ids_from(child_start)?;
        index.root = if candidate == EMPTY_ROOT {
            None
        } else {
            Some(*index.ids.get(&candidate).ok_or_else(|| {
                "Architecture G promoted root is absent from resident index".to_owned()
            })?)
        };
        self.marker = Some(candidate);
        self.generations.remove(&id);
        self.generations.clear();
        Ok(candidate)
    }
}
