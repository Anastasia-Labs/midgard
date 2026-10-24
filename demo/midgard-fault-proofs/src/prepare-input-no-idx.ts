/**
 * **Re-derived onto the flat field commitments by #604.** The two forwarded step
 * states it emits carry §2.5 anchors — `verified_tx_id` and `producing_tx_id` —
 * where they used to carry field-0 and field-2 collection commitments, and the
 * proof-fit measurement reports §8.4's carriage tier where it used to report a
 * retired direct/fold split. See
 * `docs/fault-proofs/decisions/0001-reference-input-field-evidence.md`.
 *
 * `input-no-idx` (`nonExistentInputNoIndex`) evidence builder (Goal task `Q13`,
 * §9.1 outputs 6-8).
 *
 * **Canonical evidence.** One committed block, authenticated end to end:
 *
 * 1. an authenticated L1 observation of the state-queue header
 *    (`authenticated_cardano_l1`), and
 * 2. the exact `DaPayloadEnvelopeV1` bytes retrieved over the public retained-DA
 *    protocol (`public_or_permissionless_da`),
 *
 * reduced by the Q03 evidence-source API to a `CanonicalBlockEvidence` whose
 * transaction leaves re-commit to the header's counted `transactions_root`
 * under `TransactionsV1RootDomain`. Two committed transactions of that single
 * block are the whole evidence: the **bad** transaction, which spends
 * `(producing_tx_id, output_index)`, and its **producing** transaction, which
 * is committed in the same block and whose canonical outputs list is shorter
 * than or equal to `output_index`.
 *
 * No operator REST/DB/file input can reach the security-grade entry point:
 * {@link prepareInputNoIdxFromCanonicalEvidence} routes provenance through
 * `assertSecurityGradeEvidenceV1` and the native inclusion root through
 * `assertNativeInclusionRootAuthenticatedV1`
 * (`src/evidence/prepare-from-evidence.ts`). The `--midgard-node-url` and
 * `--transactions` entry points below are the operator-diagnostic rehearsal
 * routes and are labelled as such; they can never mint a security-grade claim.
 *
 * **Valid-block negative.** A transaction input that really exists cannot be
 * prepared: {@link InputNoIdxRejection} `input_exists_in_producing_tx` is
 * raised whenever the challenged `output_index` is inside the producer's
 * canonical outputs list, and `no_violating_input` whenever the block contains
 * no such input at all.
 */

import "node:fs/promises";
import "node:path";
import "@al-ft/midgard-core";
import "@al-ft/midgard-core/lucid-data";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "effect";
import "./evidence/index.js";
import "./json-file.js";
import "./prepare-double-spend.js";
import "./prepare-input-no-idx.read-value.js";
import "./prepare-input-no-idx.midgard-tx-output-from-canonical-cbor.js";
import "./prepare-input-no-idx.find-candidate.js";
import "./prepare-input-no-idx.prepare-input-no-idx-from-transactions.js";
import "./prepare-input-no-idx.prepare-input-no-idx-from-canonical-evidence.js";
export { type PrepareInputNoIdxFromTransactionsOptions } from "./prepare-input-no-idx.find-candidate.js";
export {
  detectInputNoIdxViolationsFromTransactions,
  type InputNoIdxDetectedViolation,
  midgardTxOutputFromCanonicalCbor,
  type PreparedInputNoIdxInputsPreimageJson,
  type PreparedInputNoIdxOutput,
  type PreparedInputNoIdxOutputsPreimageJson,
  type PreparedInputNoIdxProofFit,
} from "./prepare-input-no-idx.midgard-tx-output-from-canonical-cbor.js";
export { prepareInputNoIdxFromCanonicalEvidence } from "./prepare-input-no-idx.prepare-input-no-idx-from-canonical-evidence.js";
export {
  type PrepareInputNoIdxCliConfig,
  prepareInputNoIdxFromFile,
  type PrepareInputNoIdxFromFileConfig,
  prepareInputNoIdxFromNode,
  prepareInputNoIdxFromTransactions,
} from "./prepare-input-no-idx.prepare-input-no-idx-from-transactions.js";
export {
  INPUT_NO_IDX_EVIDENCE_SCHEMA_VERSION,
  InputNoIdxRejection,
  type InputNoIdxRejectionCode,
} from "./prepare-input-no-idx.read-value.js";
