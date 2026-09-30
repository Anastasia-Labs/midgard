/**
 * `reference-input-no-idx` evidence builder (Goal task `Q31`).
 *
 * Harvested from PR #473 (`fp/no-ref-input-idx`) and reconciled against the
 * current native-codec boundary. It locates a committed transaction that *reads*
 * a reference input whose producing transaction IS committed in the same block,
 * but whose `output_index` is out of range of that producer's outputs, and emits
 * the four submit-step arguments the on-chain chain consumes.
 *
 * The `--midgard-node-url` and `--transactions` routes below are
 * operator-diagnostic rehearsal routes: they read block material from the node's
 * REST surface or an operator-private file and can never mint a security-grade
 * claim. A Q03 evidence-gated entry point (the analogue of
 * `prepareInputNoIdxFromCanonicalEvidenceV1`) is not landed here yet; the four
 * `submit-reference-input-no-idx-step-0N` builders are, and each re-derives its
 * commitments from the on-chain header and step datum rather than trusting
 * these artifacts.
 */

import "node:fs/promises";
import "node:path";
import "@al-ft/midgard-core";
import "@al-ft/midgard-sdk";
import "effect";
import "./evidence/index.js";
import "./json-file.js";
import "./ne-proofs.js";
import "./prepare-double-spend.js";
import "./spend-input-witness.js";
import "./step-support.js";
import "./prepare-reference-input-no-idx.decode-tx.js";
import "./prepare-reference-input-no-idx.prepare-reference-input-no-idx-from-transactions.js";
import "./prepare-reference-input-no-idx.prepare-reference-input-no-idx-from-node.js";
export {
  detectReferenceInputNoIdxViolationsFromTransactions,
  type PreparedReferenceInputNoIdxOutput,
  type PreparedReferenceInputNoIdxTxInclusionJson,
  type PrepareReferenceInputNoIdxCliConfig,
  type PrepareReferenceInputNoIdxFromFileConfig,
  type ReferenceInputNoIdxDetectedViolation,
  type ReferenceInputNoIdxPreimageEntry,
} from "./prepare-reference-input-no-idx.decode-tx.js";
export {
  prepareReferenceInputNoIdxFromCanonicalEvidence,
  prepareReferenceInputNoIdxFromFile,
  prepareReferenceInputNoIdxFromNode,
} from "./prepare-reference-input-no-idx.prepare-reference-input-no-idx-from-node.js";
export { prepareReferenceInputNoIdxFromTransactions } from "./prepare-reference-input-no-idx.prepare-reference-input-no-idx-from-transactions.js";
