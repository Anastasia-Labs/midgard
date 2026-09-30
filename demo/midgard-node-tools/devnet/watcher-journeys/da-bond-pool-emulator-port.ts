/**
 * The emulator adapter for the pooled DA bond journey (ticket #692): a
 * `DaBondPoolJourneyPort` over the availability emulator harness, so the
 * journey driver runs its whole six-step chronology as a fast dry run.
 *
 * What runs for real, on the emulator ledger with the compiled validators:
 *
 * - block commits: the SDK's `CommitBlockHeader` builder (the node's), after
 *   the queue's tail, from an empty genesis queue;
 * - attestation: DA attestation init and threshold signatures, then the SDK
 *   Apply builder. A refused Apply is the builder's typed
 *   `DaAttestationBuildError` refusal (`pool-under-backed`,
 *   `pool-withdrawing`), not a check of this adapter's;
 * - pool reads, top-up and the withdrawal quorum steps: the operator
 *   `da-bond` commands (`status`, `top-up`, `withdraw begin|cancel|complete`
 *   with each owner's `witness` and `assemble`) over an emulator
 *   `DaBondContext`;
 * - alerts: the watcher's `deriveWatcherDaBondPoolObservation` over its
 *   authenticated pool read, and the committee's pool check, readiness
 *   reasons and one `createDaBondPoolMonitor`, fed only from `observeAlerts`;
 * - the challenge flow (Open, publications, settlements, Close, and the
 *   Timeout that slashes the pool and removes the head): the harness's
 *   hand-built mirrors of those transactions.
 *
 * Waiting advances the emulator clock; nothing sleeps.
 */

import "node:fs";
import "node:os";
import "node:path";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "da-committee-node/coordinator/pool-monitor";
import "effect";
import "midgard-node/commands/da-bond";
import "midgard-node/commands/da-bond-files";
import "midgard-node/tests/helpers/availability-challenge";
import "midgard-node/tests/helpers/availability-challenge-emulator";
import "midgard-watcher";
import "./da-bond-pool-emulator-port.block.js";
import "./da-bond-pool-emulator-port.create-da-bond-pool-emulator-port.js";
export {
  type DaBondPoolEmulatorPort,
  type DaBondPoolEmulatorPortOptions,
} from "./da-bond-pool-emulator-port.block.js";
export { createDaBondPoolEmulatorPort } from "./da-bond-pool-emulator-port.create-da-bond-pool-emulator-port.js";
