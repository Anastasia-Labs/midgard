use super::*;

pub(super) struct RuntimeGeneration {
    pub(super) base_root: Hash,
    pub(super) arena: Option<Arena>,
    pub(super) candidate_root: Option<Hash>,
    pub(super) prepared: bool,
}

pub(super) enum ApplyOutcome {
    NeedNodes(Vec<Hash>),
    Applied {
        candidate: Hash,
        roots: Vec<Hash>,
        proof_duration_ns: u64,
        mutation_duration_ns: u64,
    },
}

pub(super) struct OwnerDiagnostics {
    pub(super) marker: Hash,
    pub(super) cached_nodes: u64,
    pub(super) cached_bytes: u64,
    pub(super) generations: u64,
    pub(super) generated_nodes: u64,
    pub(super) generated_bytes: u64,
}

pub(super) struct RuntimeOwner {
    pub(super) epoch: [u8; 16],
    pub(super) marker: Option<Hash>,
    pub(super) cache: NodeCache,
    pub(super) generations: HashMap<[u8; 16], RuntimeGeneration>,
    pub(super) next_generation: u64,
}

fn elapsed_ns(started_at: Instant, what: &str) -> Result<u64, String> {
    u64::try_from(started_at.elapsed().as_nanos())
        .map_err(|_| format!("Architecture G {what} timing overflow"))
}

impl RuntimeOwner {
    pub(super) fn new(epoch: [u8; 16]) -> Self {
        Self {
            epoch,
            marker: None,
            cache: NodeCache::new(0),
            generations: HashMap::new(),
            next_generation: 1,
        }
    }

    pub(super) fn ready_marker(&self) -> Result<Hash, String> {
        self.marker
            .ok_or_else(|| "Architecture G owner is not ready".to_owned())
    }

    pub(super) fn set_root(&mut self, root: Hash) -> Result<(), String> {
        if !self.generations.is_empty() {
            return Err(
                "Architecture G cannot set the root while a generation is active".to_owned(),
            );
        }
        self.marker = Some(root);
        self.cache.trim();
        Ok(())
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
        let marker = self.ready_marker()?;
        if self.generations.len() >= 2 {
            return Err("Architecture G active-generation cap exceeded".to_owned());
        }
        if marker != base_root {
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
    ) -> Result<ApplyOutcome, String> {
        let marker = self.ready_marker()?;
        let stream = parse_event_stream(event_bytes)?;
        let generation = self
            .generations
            .get_mut(&id)
            .ok_or_else(|| "Architecture G generation handle is stale".to_owned())?;
        if stream.base_root != generation.base_root || marker != stream.base_root {
            return Err("Architecture G replay log base root is stale".to_owned());
        }
        if generation.arena.is_some() {
            return Err("Architecture G generation has already been applied".to_owned());
        }
        let proof_started_at = Instant::now();
        let mut arena = match self.cache.proof_arena(&stream)? {
            Proof::Missing(hashes) => return Ok(ApplyOutcome::NeedNodes(hashes)),
            Proof::Ready(arena) => arena,
        };
        let proof_duration_ns = elapsed_ns(proof_started_at, "proof")?;
        let mutation_started_at = Instant::now();
        let roots = stream
            .events
            .iter()
            .map(|event| arena.apply_event(event))
            .collect::<Result<Vec<_>, _>>();
        // The arena holds its own copies, so the cache may shrink now.
        self.cache.trim();
        let roots = roots?;
        let mutation_duration_ns = elapsed_ns(mutation_started_at, "mutation")?;
        let candidate = roots.last().copied().unwrap_or(stream.base_root);
        generation.arena = Some(arena);
        generation.candidate_root = Some(candidate);
        Ok(ApplyOutcome::Applied {
            candidate,
            roots,
            proof_duration_ns,
            mutation_duration_ns,
        })
    }

    pub(super) fn generated_records(
        &mut self,
        id: [u8; 16],
    ) -> Result<(Hash, Hash, Vec<Vec<u8>>), String> {
        self.ready_marker()?;
        let generation = self
            .generations
            .get_mut(&id)
            .ok_or_else(|| "Architecture G generation handle is stale".to_owned())?;
        let candidate = generation
            .candidate_root
            .ok_or_else(|| "Architecture G generation has not been applied".to_owned())?;
        let arena = generation
            .arena
            .as_ref()
            .ok_or_else(|| "Architecture G generation arena is missing".to_owned())?;
        let records = generated_reachable_records(arena, candidate)?;
        generation.prepared = true;
        Ok((generation.base_root, candidate, records))
    }

    pub(super) fn commit(&mut self, id: [u8; 16]) -> Result<Hash, String> {
        let marker = self.ready_marker()?;
        let generation = self
            .generations
            .get(&id)
            .ok_or_else(|| "Architecture G generation handle is stale".to_owned())?;
        if !generation.prepared {
            return Err("Architecture G generation was not prepared for promotion".to_owned());
        }
        if marker != generation.base_root {
            return Err("Architecture G promotion marker is stale".to_owned());
        }
        let candidate = generation
            .candidate_root
            .ok_or_else(|| "Architecture G generation candidate is missing".to_owned())?;
        let arena = generation
            .arena
            .as_ref()
            .ok_or_else(|| "Architecture G generation arena is missing".to_owned())?;
        for node in generated_reachable_nodes(arena, candidate)? {
            self.cache.insert(node);
        }
        self.marker = Some(candidate);
        self.generations.clear();
        self.cache.trim();
        Ok(candidate)
    }

    pub(super) fn diagnostics(&self) -> Result<OwnerDiagnostics, String> {
        let marker = self.ready_marker()?;
        let generated_nodes = self
            .generations
            .values()
            .filter_map(|generation| generation.arena.as_ref())
            .map(|arena| arena.nodes.len().saturating_sub(arena.base_count))
            .sum::<usize>();
        let generated_bytes = self
            .generations
            .values()
            .filter_map(|generation| Some((generation.arena.as_ref()?, generation.candidate_root?)))
            .map(|(arena, candidate)| {
                generated_reachable_records(arena, candidate)
                    .map(|records| records.iter().map(Vec::len).sum::<usize>())
            })
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .sum::<usize>();
        Ok(OwnerDiagnostics {
            marker,
            cached_nodes: self.cache.len() as u64,
            cached_bytes: self.cache.bytes() as u64,
            generations: self.generations.len() as u64,
            generated_nodes: generated_nodes as u64,
            generated_bytes: generated_bytes as u64,
        })
    }
}
