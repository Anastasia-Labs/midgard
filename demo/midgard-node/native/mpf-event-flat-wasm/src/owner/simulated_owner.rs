use super::*;

pub(super) struct SimulatedGeneration {
    pub(super) base_root: Hash,
    pub(super) arena: Arena,
}

pub(super) struct SimulatedOwner {
    pub(super) marker: Hash,
    pub(super) next_handle: u64,
    pub(super) generations: HashMap<u64, SimulatedGeneration>,
}

impl SimulatedOwner {
    pub(super) fn new(marker: Hash) -> Self {
        Self {
            marker,
            next_handle: 1,
            generations: HashMap::new(),
        }
    }

    pub(super) fn fork(&mut self, arena: &Arena) -> Result<u64, String> {
        if self.generations.len() >= 2 {
            return Err("Architecture G simulated active-generation cap exceeded".to_owned());
        }
        let handle = self.next_handle;
        self.next_handle = self
            .next_handle
            .checked_add(1)
            .ok_or_else(|| "Architecture G simulated handle overflow".to_owned())?;
        self.generations.insert(
            handle,
            SimulatedGeneration {
                base_root: self.marker,
                arena: arena.clone(),
            },
        );
        Ok(handle)
    }

    pub(super) fn apply(&mut self, handle: u64, stream: &EventStream) -> Result<Vec<Hash>, String> {
        let generation = self
            .generations
            .get_mut(&handle)
            .ok_or_else(|| "Architecture G simulated generation handle is stale".to_owned())?;
        if generation.base_root != stream.base_root {
            return Err("Architecture G simulated event stream base is stale".to_owned());
        }
        let mut roots = Vec::with_capacity(stream.events.len());
        for event in &stream.events {
            roots.push(generation.arena.apply_event(event)?);
        }
        Ok(roots)
    }

    pub(super) fn discard(&mut self, handle: u64) -> Result<(), String> {
        self.generations
            .remove(&handle)
            .map(|_| ())
            .ok_or_else(|| "Architecture G simulated generation handle is stale".to_owned())
    }

    pub(super) fn root(&self, handle: u64) -> Result<Hash, String> {
        let generation = self
            .generations
            .get(&handle)
            .ok_or_else(|| "Architecture G simulated generation handle is stale".to_owned())?;
        Ok(generation
            .arena
            .root
            .map(|id| generation.arena.nodes[id].hash())
            .unwrap_or(EMPTY_ROOT))
    }

    pub(super) fn promote(&mut self, handle: u64) -> Result<Hash, String> {
        let generation = self
            .generations
            .get(&handle)
            .ok_or_else(|| "Architecture G simulated generation handle is stale".to_owned())?;
        if generation.base_root != self.marker {
            return Err("Architecture G simulated promotion marker is stale".to_owned());
        }
        let candidate = generation
            .arena
            .root
            .map(|id| generation.arena.nodes[id].hash())
            .unwrap_or(EMPTY_ROOT);
        self.generations.remove(&handle);
        self.marker = candidate;
        Ok(candidate)
    }
}
