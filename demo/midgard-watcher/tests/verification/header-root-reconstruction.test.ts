/**
 * W22 header/root reconstruction tests.
 *
 * PROVENANCE OF EVERY INPUT USED BY THESE TESTS (the non-circularity argument
 * these cases exist to demonstrate):
 *
 * - The expected root/count set: `WatcherStateQueueHeader`, the header record
 *   decoded from the L1 state-queue node datum
 *   (demo/midgard-watcher/src/indexers/state-queue-snapshot.ts). In these tests
 *   the record is derived from the fixture's `Header` by re-encoding it the
 *   way the datum parser does, so no test ever feeds a header field that did not
 *   come from a committed header.
 * - The header hash: never taken from the caller. It is re-derived from the
 *   header struct by `admitAuthenticatedStateQueueHeaderObservation`.
 * - The payload bytes: an argument, standing in for the exact bytes the W21
 *   canonical block store persisted from a public DA peer.
 * - The reconstruction: `reconstructDaPayload` in
 *   `@al-ft/midgard-fault-proofs`, reached through the Q03 evidence core.
 *   Nothing in the watcher recomputes a root.
 *
 * The payload's embedded header is only ever a claim. Cases in the
 * "fail-closed / non-circularity" block show it is never promoted to the
 * expected set.
 */

import "node:crypto";
import "node:fs";
import "node:url";
import "@al-ft/midgard-core/codec";
import "@al-ft/midgard-core/codec/cbor";
import "@al-ft/midgard-core/da-payload-envelope";
import "@al-ft/midgard-core/da-transport";
import "@al-ft/midgard-fault-proofs";
import "@al-ft/midgard-sdk";
import "@al-ft/midgard-test-support/hex";
import "@lucid-evolution/lucid";
import "@noble/hashes/blake2.js";
import "vitest";
import "../../src/verification/header-root-reconstruction.js";
import "./header-root-reconstruction.watcher-header-record.js";
import "./header-root-reconstruction.build-fixture.js";
import "./header-root-reconstruction.w22-adjacent-boundaries.js";
import "./header-root-reconstruction.w22-malformed-payload-bytes.js";
import "./header-root-reconstruction.w22-fail-closed-and-non-circularity.js";
