/**
 * `fabricated-withdrawal` DA-first evidence builder (Goal task `Q40`, §9.1
 * output 7).
 *
 * The fault this family proves is a committed `withdrawals_root` leaf that is not
 * the authentic L1 withdrawal event pair: either no withdrawal event with the
 * committed `WithdrawalId` was ever authenticated, or the authentic event exists
 * and was due for the block but its `(body, signature)` content is not the
 * committed one. The committed `validity` verdict is not compared: the operator
 * owns it (decision 0007,
 * `docs/fault-proofs/decisions/0007-operator-owned-event-validity.md`), and a
 * verdict the chain contradicts is `withdrawalMistag`'s fault.
 *
 * Such a block cannot be reconstructed by `reconstructDaPayloadV1` — a whole block
 * whose withdrawal source set disagrees with L1 fails reconstruction long before
 * the leaf in question is reached — so, like `prepare-fabricated-deposit`, this
 * builder decodes the retained-DA envelope itself and performs exactly the
 * authentication the proof needs, against two security-graded inputs:
 *
 * 1. an authenticated L1 observation of the committed state-queue header
 *    (`authenticated_cardano_l1`), and
 * 2. the exact `DaPayloadEnvelopeV1` bytes retrieved over the public retained-DA
 *    protocol (`public_or_permissionless_da`),
 *
 * cross-checked by rebuilding the **raw** `(WithdrawalId, WithdrawalInfo)` MPF
 * from the payload's `withdrawals` entries, committing it under the counted
 * `WithdrawalsRootDomain`, and requiring **both** that the counted root equals the
 * L1-committed `withdrawals_root` **and** that the rebuilt cardinality equals the
 * header's `withdrawal_count`. After that check every committed withdrawal leaf is
 * exactly as trustworthy as the header itself.
 *
 * `assertNativeInclusionRootAuthenticatedV1` is deliberately **not** used: it
 * authenticates the native-compact *transaction* leaf convention against
 * `transactions_root` and has no bearing on `withdrawals_root`, which has a single
 * leaf convention. Requiring it would refuse a legitimate fabricated-withdrawal
 * proof whenever the block's unrelated transaction leaves use the payload-source
 * convention. The same reasoning is recorded at
 * `src/evidence/prepare-from-evidence.ts:145-150`.
 *
 * The L1 side is a hub-authenticated sorted-list gap/filler or Order. Large
 * payloads must be supplied by the actual retained-data reference output. The
 * preparation persists a complete payload/Value opening for the later capture
 * commitment; no operator archive or live nonce establishes list membership.
 */

import "node:fs/promises";
import "node:path";
import "@al-ft/midgard-core/da-payload-envelope";
import "@al-ft/midgard-core/da-transport";
import "@al-ft/midgard-core/plutus-data-cbor";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "effect";
import "./fabricated-history-witness.js";
import "./json-file.js";
import "./prepare-double-spend.js";
import "./transition-trace/fetch.js";
import "./transition-trace/phas.js";
import "./prepare-fabricated-withdrawal.classify-fabricated-withdrawal-fault.js";
import "./prepare-fabricated-withdrawal.prepare-fabricated-withdrawal-from-committed-leaves.js";
import "./prepare-fabricated-withdrawal.prepare-fabricated-withdrawal-from-retained-da.js";
export {
  type ClassifiedFabricatedWithdrawalFault,
  classifyFabricatedWithdrawalFault,
  type CommittedWithdrawalLeaf,
  FABRICATED_WITHDRAWAL_EVIDENCE_SCHEMA_VERSION,
  type FabricatedWithdrawalL1Witness,
  FabricatedWithdrawalRejection,
  type FabricatedWithdrawalRejectionCode,
  type PreparedFabricatedWithdrawalContentJson,
  type PreparedFabricatedWithdrawalInclusionJson,
  type PreparedFabricatedWithdrawalOutput,
  type PreparedFabricatedWithdrawalStateJson,
  type PrepareFabricatedWithdrawalFromCommittedLeavesOptions,
} from "./prepare-fabricated-withdrawal.classify-fabricated-withdrawal-fault.js";
export {
  type FabricatedWithdrawalBlockEvidence,
  fabricatedWithdrawalBlockEvidenceFromVerifiedPayload,
  prepareFabricatedWithdrawalFromCommittedLeaves,
} from "./prepare-fabricated-withdrawal.prepare-fabricated-withdrawal-from-committed-leaves.js";
export { prepareFabricatedWithdrawalFromRetainedDa } from "./prepare-fabricated-withdrawal.prepare-fabricated-withdrawal-from-retained-da.js";
