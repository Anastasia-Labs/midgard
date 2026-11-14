-- class: B; retention: unchanged (three columns of the own-block journal row, `pending_block_finalizations`, kept and deleted with it)
-- The commit anchor of an own block journal (plan §8.1): the follower block
-- d below the view the commit was planned at (its write permit's view), d
-- the deployment profile's commit-event depth. The header end is at most
-- the anchor's time + event_wait - 1, so every included event's block is
-- strictly below the anchor on its chain. Signing, S6 and the own-journal
-- disposition keep the journal only while the anchor is canonical. A row
-- prepared before this migration, or by a model fixture without a write
-- permit, has none (all three NULL): an unfinished one is disposed of, and
-- a runtime permit never signs one.
ALTER TABLE pending_block_finalizations
  ADD COLUMN commit_anchor_hash bytea,
  ADD COLUMN commit_anchor_height bigint,
  ADD COLUMN commit_anchor_slot bigint,
  ADD CONSTRAINT pending_block_finalizations_commit_anchor_whole CHECK (
    (commit_anchor_hash IS NULL) = (commit_anchor_height IS NULL)
    AND (commit_anchor_height IS NULL) = (commit_anchor_slot IS NULL)),
  ADD CONSTRAINT pending_block_finalizations_commit_anchor_hash_length CHECK (
    commit_anchor_hash IS NULL OR octet_length(commit_anchor_hash) = 32);
