/**
 * `fabricated-withdrawal` family (Goal task `Q40`) — evidence builder, L1 witness
 * authentication and submit-side re-derivation.
 *
 * The family is reached by **direct module import**: the `fabricatedWithdrawal`
 * catalogue category and its CLI wiring are parent-owned integration surfaces
 * that land with catalogue registration (#617), so nothing here goes through
 * `src/index.js`, `fraud-proof/catalogue.ts` or `bin.ts`.
 *
 * Every committed-leaf, commitment, nonce and handoff constant below is the
 * value **measured out of the Aiken family modules**
 * `onchain/aiken/lib/midgard/fraud-proofs/fabricated-withdrawal/step-0{1,2,3,4}.ak`
 * and pinned in `demo/midgard-sdk/tests/fabricated-withdrawal.test.ts`. The
 * committed `withdrawals_root`s and the step-04 handoff bytes asserted here are
 * therefore Aiken-measured absolutes, not one TypeScript derivation compared
 * against another.
 *
 * The three challenged blocks are the Aiken fixtures' own scenarios:
 *
 * - **FI** (`fabricated_identity_block_v1`) commits `(FABRICATED_WITHDRAWAL_ID ->
 *   AUTHENTIC_WITHDRAWAL_INFO)`, an identity no withdrawal event ever had;
 * - **MM** (`mismatched_content_block_v1`) commits `(AUTHENTIC_WITHDRAWAL_ID ->
 *   DIVERTED_WITHDRAWAL_INFO)`, the authentic identity with a diverted payout
 *   address; and
 * - **AU** (`authentic_withdrawal_block_v1`) commits the authentic pair, and is the
 *   valid block this family must refuse to convict.
 *
 * All three are re-committed into a real `DaPayload` here, because
 * `tests/helpers/canonical-block-evidence-fixture.ts` hard-wires an empty
 * withdrawal source set (`withdrawals: []`, `withdrawalCount: 0n`).
 *
 * Unlike the deposit twin, a withdrawal leaf value embeds a `Value` map, so the
 * definite-versus-indefinite Plutus map difference between Lucid's encoder and
 * `serialise_data` is load-bearing in this family. Two tests below pin it directly:
 * indefinite leaf bytes are refused as non-canonical, and an indefinite *event
 * datum* opening is accepted because the on-chain step re-serialises whatever wire
 * form it receives.
 */

import "node:fs/promises";
import "node:os";
import "node:path";
import "@al-ft/midgard-core/plutus-data-cbor";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "effect";
import "vitest";
import "../src/fabricated-history-witness.js";
import "../src/prepare-fabricated-withdrawal.js";
import "../src/submit-fabricated-withdrawal-step-01.js";
import "../src/submit-fabricated-withdrawal-step-03.js";
import "../src/submit-fabricated-withdrawal-step-04.js";
import "../src/transition-trace/phas.js";
import "../src/workflow/fabricated-withdrawal-evidence.js";
import "../src/workflow/journal.js";
import "./helpers/canonical-block-evidence-fixture.js";
import "./fabricated-withdrawal.build-withdrawals-block-fixture.js";
import "./fabricated-withdrawal.q40-fabricated-withdrawal-evidence-admission.js";
import "./fabricated-withdrawal.fabricated-withdrawal-production-evidence-authority.js";
import "./fabricated-withdrawal.q40-fabricated-withdrawal-proof-plan.js";
import "./fabricated-withdrawal.q40-fabricated-withdrawal-l1-witness-authentication.js";
import "./fabricated-withdrawal.q40-fabricated-withdrawal-submit-side-re-derivation.js";
