/**
 * `double-spend` evidence builder.
 *
 * **Re-derived-adjacent, and checked to be unaffected by #604.** This module
 * emits *evidence* — canonical item lists, compact structures and inclusion
 * proofs — and constructs no datum or redeemer, so the #575 rebind left its
 * output shape alone. What changed is downstream: the submitters that consume
 * this evidence now also take the disputed transaction's compact CBOR, which
 * this module already emits as part of its inclusion argument. The banner it
 * used to carry is gone because the family is re-derived, not because the
 * module was skipped.
 */

import "node:fs/promises";
import "node:path";
import "@aiken-lang/merkle-patricia-forestry";
import "@al-ft/midgard-core";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "effect";
import "./json-file.js";
import "./step-support.js";
import "./prepare-double-spend.encode-l2-transaction-source-value.js";
import "./prepare-double-spend.resolve-double-spend-pair.js";
import "./prepare-double-spend.prepare-double-spend-from-transactions.js";
export {
  type DecodedTransactionMaterial,
  deriveL2TransactionSourceCbor,
  encodeL2TransactionSourceValue,
  type FetchLike,
  fetchNodeBlockTransactions,
  type NodeTransactionPayload,
  type PreparedDoubleSpendOutput,
  type PreparedDoubleSpendTx,
  type PrepareDoubleSpendCliConfig,
  type PrepareDoubleSpendFromFileConfig,
  type PreparedTxInclusionJson,
  type PrepareSampleDoubleSpendConfig,
  readNodeTransactionPayloadsFile,
  type TrieView,
} from "./prepare-double-spend.encode-l2-transaction-source-value.js";
export {
  prepareDoubleSpendFromFile,
  prepareDoubleSpendFromNode,
  prepareDoubleSpendFromTransactions,
  prepareSampleDoubleSpend,
  requireTransactionsRootMatch,
} from "./prepare-double-spend.prepare-double-spend-from-transactions.js";
export {
  buildTrieView,
  decodeTransactionMaterial,
  makeSampleDoubleSpendTransactions,
  requireProof,
  transactionSourceTrieItem,
} from "./prepare-double-spend.resolve-double-spend-pair.js";
