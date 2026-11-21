-- class: D; retention: dropped (user-side submission records left the node:
-- deposits and withdrawals are built, recorded and confirmed by the user's own
-- `midgard-user` tool against its own L1 indexer; the chain answers by tx hash).
-- Children before parents and no CASCADE, so an unexpected dependent fails the
-- migration instead of being dropped silently. Indexes go with their tables.
DROP TABLE event_history_submission_inputs;
DROP TABLE event_history_submissions;
DROP TABLE deposit_submission_attempts;
