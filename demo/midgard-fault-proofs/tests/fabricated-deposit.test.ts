/**
 * `fabricated-deposit` family (Goal task `Q39`) — evidence builder, L1 witness
 * authentication and submit-side re-derivation.
 *
 * The family is reached by **direct module import**: the `fabricatedDeposit`
 * catalogue category and its CLI wiring are parent-owned integration surfaces
 * that land with catalogue registration (#617), so nothing here goes through
 * `src/index.js`, `fraud-proof/catalogue.ts` or `bin.ts`.
 *
 * Every committed-leaf, commitment, nonce and handoff constant below is the
 * value **measured out of the Aiken family modules**
 * `onchain/aiken/lib/midgard/fraud-proofs/fabricated-deposit/step-0{1,2,3,4}.ak`
 * and pinned in `demo/midgard-sdk/tests/fabricated-deposit.test.ts`. The
 * committed `deposits_root`s and the step-04 handoff bytes asserted here are
 * therefore Aiken-measured absolutes, not one TypeScript derivation compared
 * against another.
 *
 * The two challenged blocks are the Aiken fixtures' own scenarios:
 *
 * - **FI** (`fabricated_identity_block_v1`) commits `(FABRICATED_DEPOSIT_ID ->
 *   AUTHENTIC_DEPOSIT_INFO)`, an identity no deposit event ever had; and
 * - **MM** (`mismatched_content_block_v1`) commits `(AUTHENTIC_DEPOSIT_ID ->
 *   DIVERTED_DEPOSIT_INFO)`, the authentic identity with diverted content.
 *
 * Both are re-committed into a real `DaPayload` here, because
 * `tests/helpers/canonical-block-evidence-fixture.ts` hard-wires an empty
 * deposit source set.
 */

import "node:crypto";
import "node:fs/promises";
import "node:os";
import "node:path";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "effect";
import "vitest";
import "../src/fabricated-history-witness.js";
import "../src/prepare-fabricated-deposit.js";
import "../src/submit-fabricated-deposit-step-01.js";
import "../src/submit-fabricated-deposit-step-03.js";
import "../src/submit-fabricated-deposit-step-04.js";
import "../src/transition-trace/phas.js";
import "../src/workflow/fabricated-deposit-evidence.js";
import "../src/workflow/journal.js";
import "./helpers/canonical-block-evidence-fixture.js";
import "./fabricated-deposit.build-deposits-block-fixture.js";
import "./fabricated-deposit.q39-fabricated-deposit-evidence-admission.js";
import "./fabricated-deposit.q39-fabricated-deposit-proof-plan.js";
import "./fabricated-deposit.q39-fabricated-deposit-production-evidence-authority.js";
import "./fabricated-deposit.q39-fabricated-deposit-submit-side-re-derivation.js";
