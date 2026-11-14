/**
 * Aiken/TypeScript byte twins for the `fabricated-withdrawal` family (Goal task
 * `Q40`).
 *
 * Every expectation below is an **absolute** hex constant measured out of the
 * Aiken family modules
 * `onchain/aiken/lib/midgard/fraud-proofs/fabricated-withdrawal/step-0{1,2,3,4}.ak`
 * (via `cbor.serialise` / `utils.serialise_and_hash_32` /
 * `user_events.out_ref_to_nonce` / `transition_trace.commit_counted_root` over
 * that family's own test fixtures). Nothing here compares one TypeScript
 * derivation against another: if either side's encoding moves, the literal stops
 * matching.
 *
 * The family is reached by direct module import rather than through
 * `src/fraud-proof/catalogue.ts`, because the `fabricatedWithdrawal` catalogue
 * category is not registered yet.
 *
 * A withdrawal leaf is the first fraud-proof leaf in this family series whose
 * value embeds a `Value` map, so these twins also pin the definite-versus-
 * indefinite map difference between Lucid's typed encoder and Plutus'
 * `serialiseData`: the raw Lucid bytes are asserted **not** to match the on-chain
 * ones, and the normalised bytes are asserted to match exactly.
 */

import "@al-ft/midgard-core/plutus-data-cbor";
import "@lucid-evolution/lucid";
import "effect";
import "vitest";
import "../src/common.js";
import "../src/fraud-proof/fabricated-withdrawal.js";
import "../src/ledger-state.js";
import "../src/transition-trace.js";
import "../src/user-events/history-proof.js";
import "./fabricated-withdrawal.authentic-withdrawal-info.js";
import "./fabricated-withdrawal.fabricated-withdrawal-v1-byte-twins.js";
