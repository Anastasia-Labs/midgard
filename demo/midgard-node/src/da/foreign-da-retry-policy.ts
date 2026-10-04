/**
 * Network retry budget for foreign DA payload retrieval. Dependency-free so the
 * committee's availability response budget test can read it by path.
 */
export const FOREIGN_DA_RETRY_POLICY = Object.freeze({
  maxHeaders: 64,
  attemptsPerEpisode: 4,
  backoffMs: 30_000,
  backoffMaxMs: 120_000,
  cooldownMs: 300_000,
});
