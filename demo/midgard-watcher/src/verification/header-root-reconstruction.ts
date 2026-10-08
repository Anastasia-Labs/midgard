/**
 * Header/root reconstruction (GOAL_SPEC 10.3 W22).
 *
 * The watcher must be able to say, for one state-queue block, whether the
 * public DA payload reconstructs *exactly* the root set and count set the
 * operator committed on L1 - and, when it does not, which fields diverge, with
 * no dependence on the operator's own claim about what the block contains.
 *
 * Two rules govern this module.
 *
 * 1. REUSE, NEVER RE-DERIVE. The reconstruction itself is the canonical one:
 *    `reconstructDaPayloadV1` from `@al-ft/midgard-fault-proofs`, reached
 *    through the Q03 evidence core
 *    `canonicalBlockEvidenceFromVerifiedPayload`. The watcher owns no second
 *    root algorithm, so there is no watcher-vs-producer differential to keep in
 *    agreement: an agreement test here is an identity, not a comparison. The
 *    mismatch vocabulary is likewise the canonical one - the eight
 *    `rootMismatches` names and the seven `countMismatches` names - re-declared
 *    here only as a fixed total order (`WATCHER_HEADER_ROOT_FIELDS`,
 *    `WATCHER_HEADER_COUNT_FIELDS`) so the reported lists are stable
 *    regardless of the order the producer emits them in.
 *
 * 2. NON-CIRCULAR BINDING. The expected root/count set comes from ONE place:
 *    the state-queue header record
 *    (`WatcherStateQueueHeader`, decoded from the L1 state-queue UTxO datum).
 *    `makeWatcherAuthenticatedHeaderObservation` rebuilds the `Header`
 *    struct from that record's fields only, re-encodes it and requires the
 *    bytes to equal the datum's own `headerCborHex`, and then hands it to
 *    `admitAuthenticatedStateQueueHeaderObservation`, which re-derives the
 *    header hash with the canonical SDK hasher. A caller therefore cannot pair
 *    an arbitrary header with a real header hash, and cannot introduce a header
 *    field that L1 did not commit.
 *
 *    The payload's own embedded header is NEVER promoted to "expected". It is
 *    only ever a claim that must equal the L1-observed header byte for byte
 *    (`committedHeader` inside the Q03 core) and hash to the L1-observed header
 *    hash (`expectedHeaderHash`). A payload that is perfectly self-consistent
 *    but describes a different block is rejected before any root is compared.
 *    There is no code path in this module that accepts an operator-supplied
 *    root set, count set, or header.
 *
 * Bytes are arguments, not transports. The envelope bytes are exactly what a
 * public DA peer served; this module never fetches.
 * `daProvenance` must be `public_or_permissionless_da` or the evaluation fails
 * closed.
 *
 * Every evaluation returns a versioned, canonical-JSON digest-bound record,
 * so two runs over the same bytes produce the same `resultDigest`, and a
 * decision can be replayed from the record alone.
 */

import "node:crypto";
import "@al-ft/midgard-core/codec/cbor";
import "@al-ft/midgard-core/da-payload-envelope";
import "@al-ft/midgard-core/da-transport";
import "@al-ft/midgard-fault-proofs";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "../storage/durable-store.js";
import "./header-root-reconstruction.make-watcher-authenticated-header-observation.js";
import "./header-root-reconstruction.classify-failure.js";
import "./header-root-reconstruction.evaluate-watcher-header-root-reconstruction.js";
export { type EvaluateWatcherHeaderRootReconstructionInput } from "./header-root-reconstruction.classify-failure.js";
export {
  evaluateWatcherHeaderRootReconstruction,
  makeWatcherHeaderRootReconstructedState,
} from "./header-root-reconstruction.evaluate-watcher-header-root-reconstruction.js";
export {
  makeWatcherAuthenticatedHeaderObservation,
  WATCHER_HEADER_COUNT_FIELDS,
  WATCHER_HEADER_ROOT_FIELDS,
  WATCHER_HEADER_ROOT_RECONSTRUCTION_REASON_CODES,
  WATCHER_HEADER_ROOT_RECONSTRUCTION_SCHEMA_VERSION,
  type WatcherHeaderCountField,
  type WatcherHeaderCountSet,
  type WatcherHeaderRootField,
  WatcherHeaderRootReconstructionError,
  type WatcherHeaderRootReconstructionErrorCode,
  type WatcherHeaderRootReconstructionReasonCode,
  type WatcherHeaderRootReconstructionResult,
  type WatcherHeaderRootSet,
} from "./header-root-reconstruction.make-watcher-authenticated-header-observation.js";
