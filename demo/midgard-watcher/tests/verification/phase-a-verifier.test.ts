/**
 * W24 Phase A verifier tests.
 *
 * WHAT THESE TESTS ARE FOR. The W24 CG3 waiver allows a watcher-side Phase A
 * verifier only if it *reuses* canonical validation semantics. So the
 * load-bearing evidence here is not "the watcher rejects bad transactions" -
 * it is the differential: for a corpus of valid and invalid transactions the
 * watcher's record is byte-identical to what `validatePhaseASingle` returns
 * (same `RejectCode`, same stage, same detail), and in the direction that
 * matters, a transaction the canonical path rejects is never accepted by the
 * watcher. Everything else - the published vocabulary, the per-code evidence,
 * the boundary and fail-closed cases - exists to show the adapter cannot
 * silently loosen that identity.
 *
 * PROVENANCE OF EVERY INPUT.
 * - Transaction bytes: `makeNativeTx` from
 *   demo/midgard-validation/tests/validation-fixtures.ts, the canonical V1
 *   native transaction encoder's own fixture builder. This file authors no
 *   transaction encoding of its own.
 * - Expected verdicts: `validatePhaseASingle` itself, called directly. No
 *   expected `RejectCode` here is a hand-written guess about what the protocol
 *   should do; the per-code cases assert an identity against the canonical
 *   function, and the code names only pin *which* canonical outcome each
 *   fixture reaches.
 * - Block bytes: a `DaPayloadEnvelopeV1` built the way the node builds one
 *   (demo/midgard-node/src/workers/commit-block-header/da-payload.ts), then
 *   put through the real W22 evaluation, so the W24 input is a genuinely
 *   accepted W22 record rather than a literal.
 * - Header and Phase A parameters: the L1-committed `Header`, reached only
 *   through `makeWatcherAuthenticatedHeaderObservation`.
 *
 * CROSS-LANGUAGE VECTORS: N/A, and deliberately so. W24 adds no new TS/Aiken
 * boundary: it introduces no serialization format, no hash preimage, and no
 * on-chain-visible value. Every byte it touches is produced and interpreted by
 * canonical modules that carry their own cross-language vectors - the V1
 * native transaction codec, the consensus profile, the DA payload encoding,
 * and the canonical reject-code vocabulary. The one artifact W24 mints, the
 * `resultDigest`, is a watcher-local canonical-JSON digest with no on-chain
 * counterpart.
 */

import "@al-ft/midgard-core/cek-proof";
import "@al-ft/midgard-core/codec";
import "@al-ft/midgard-core/codec/output";
import "@al-ft/midgard-core/consensus-profile";
import "@al-ft/midgard-core/da-payload-envelope";
import "@al-ft/midgard-fault-proofs";
import "@al-ft/midgard-sdk";
import "@al-ft/midgard-test-support/hex";
import "@al-ft/midgard-validation/phase-a";
import "@al-ft/midgard-validation/tests/validation-fixtures";
import "@al-ft/midgard-validation/types";
import "@lucid-evolution/lucid";
import "@noble/hashes/blake2.js";
import "vitest";
import "../../src/storage/durable-store.js";
import "../../src/verification/header-root-reconstruction.js";
import "../../src/verification/phase-a-verifier.js";
import "../../src/verification/rule-bundle.js";
import "./phase-a-verifier.base-header.js";
import "./phase-a-verifier.evidence-cases.js";
import "./phase-a-verifier.build-block.js";
import "./phase-a-verifier.adjacent-boundary.js";
import "./phase-a-verifier.block-verification.js";
import "./phase-a-verifier.malformed-inputs.js";
import "./phase-a-verifier.fail-closed-bindings.js";
