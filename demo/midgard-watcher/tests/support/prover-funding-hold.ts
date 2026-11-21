/**
 * The watcher's prover-funding pause and its deployment-bound dispute bond,
 * for fault-proof tests that drive a family's funding outside the watcher
 * supervisor: the hold a run's funding failure maps to, and the bond the
 * signed deployment binds.
 */
export {
  holdWatcherFaultProofOnFunding,
  WATCHER_PROVER_FUNDING_UNAVAILABLE,
} from "../../src/fault-proofs/fault-proof-funding-hold.js";
export { watcherDeploymentAvailabilityChallengeAuthority } from "../../src/runtime/deployment-identity.watcher-deployment-availability-challenge-authority.js";
