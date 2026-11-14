/**
 * `fabricated-deposit` DA-first evidence builder (Goal task `Q39`, §9.1 output 7).
 *
 * The fault this family proves is a committed `deposits_root` leaf that is not
 * the authentic L1 deposit event pair: either no deposit event with the
 * committed `DepositId` was ever authenticated, or the authentic event exists
 * and was due for the block but its `DepositInfo` is not the committed one.
 *
 * Such a block cannot be reconstructed by `reconstructDaPayloadV1` — a whole
 * block whose deposit source set disagrees with L1 fails reconstruction long
 * before the leaf in question is reached — so, like `prepare-da-hash-preimage`,
 * this builder decodes the retained-DA envelope itself and performs exactly the
 * authentication the proof needs, against two security-graded inputs:
 *
 * 1. an authenticated L1 observation of the committed state-queue header
 *    (`authenticated_cardano_l1`), and
 * 2. the exact `DaPayloadEnvelopeV1` bytes retrieved over the public retained-DA
 *    protocol (`public_or_permissionless_da`),
 *
 * cross-checked by rebuilding the **raw** `(DepositId, DepositInfo)` MPF from the
 * payload's `deposits` entries, committing it under the counted
 * `DepositsRootDomain`, and requiring **both** that the counted root equals the
 * L1-committed `deposits_root` **and** that the rebuilt cardinality equals the
 * header's `deposit_count`. After that check every committed deposit leaf is
 * exactly as trustworthy as the header itself.
 *
 * `assertNativeInclusionRootAuthenticatedV1` is deliberately **not** used: it
 * authenticates the native-compact *transaction* leaf convention against
 * `transactions_root` and has no bearing on `deposits_root`, which has a single
 * leaf convention. Requiring it would refuse a legitimate fabricated-deposit
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
import "./prepare-fabricated-deposit.classify-fabricated-deposit-fault.js";
import "./prepare-fabricated-deposit.prepare-fabricated-deposit-from-committed-leaves.js";
import "./prepare-fabricated-deposit.prepare-fabricated-deposit-from-retained-da.js";
export {
  type ClassifiedFabricatedDepositFault,
  classifyFabricatedDepositFault,
  type CommittedDepositLeaf,
  FABRICATED_DEPOSIT_EVIDENCE_SCHEMA_VERSION,
  type FabricatedDepositL1Witness,
  FabricatedDepositRejection,
  type FabricatedDepositRejectionCode,
  type PreparedFabricatedDepositContentJson,
  type PreparedFabricatedDepositInclusionJson,
  type PreparedFabricatedDepositOutput,
  type PreparedFabricatedDepositStateJson,
  type PrepareFabricatedDepositFromCommittedLeavesOptions,
} from "./prepare-fabricated-deposit.classify-fabricated-deposit-fault.js";
export {
  type FabricatedDepositBlockEvidence,
  fabricatedDepositBlockEvidenceFromVerifiedPayload,
  prepareFabricatedDepositFromCommittedLeaves,
} from "./prepare-fabricated-deposit.prepare-fabricated-deposit-from-committed-leaves.js";
export { prepareFabricatedDepositFromRetainedDa } from "./prepare-fabricated-deposit.prepare-fabricated-deposit-from-retained-da.js";
