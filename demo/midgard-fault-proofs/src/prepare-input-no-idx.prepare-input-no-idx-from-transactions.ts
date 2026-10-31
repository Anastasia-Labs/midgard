import { mkdir, writeFile } from "node:fs/promises";
import { join } from "node:path";

import {
  encodeMidgardFieldPreimage,
  selectMidgardFieldCarriageTier,
} from "@al-ft/midgard-core";
import { asLucidSchema } from "@al-ft/midgard-core/lucid-data";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { parseHex, stringifyJson } from "./json-file.js";
import {
  buildTrieView,
  decodeTransactionMaterial,
  type FetchLike,
  fetchNodeBlockTransactions,
  readNodeTransactionPayloadsFile,
  requireProof,
  transactionSourceTrieItem,
} from "./prepare-double-spend.js";
import {
  findCandidate,
  parseIndex,
  type PrepareInputNoIdxFromTransactionsOptions,
  txInclusionOf,
} from "./prepare-input-no-idx.find-candidate.js";
import {
  midgardTxOutputFromCanonicalCbor,
  type PreparedInputNoIdxOutput,
  spendInputsOf,
} from "./prepare-input-no-idx.midgard-tx-output-from-canonical-cbor.js";
import {
  INPUT_NO_IDX_EVIDENCE_SCHEMA_VERSION,
  reject,
} from "./prepare-input-no-idx.read-value.js";

/**
 * Core builder over the already-authenticated transaction material of one
 * committed block. Pure with respect to the network: it never fetches.
 */
export const prepareInputNoIdxFromTransactions = async ({
  headerHash,
  transactions,
  expectedTransactionsRoot,
  badTxId,
  badInputsIndex,
  outputDir,
}: PrepareInputNoIdxFromTransactionsOptions): Promise<PreparedInputNoIdxOutput> => {
  const normalizedHeaderHash = parseHex(headerHash, "--header-hash", 28);
  const normalizedExpectedRoot = parseHex(
    expectedTransactionsRoot,
    "--expected-transactions-root",
    32,
  );
  const normalizedBadTxId =
    badTxId === undefined ? undefined : parseHex(badTxId, "--bad-tx-id", 32);
  const normalizedBadInputsIndex = parseIndex(badInputsIndex);

  const decoded = await Promise.all(
    transactions.map(decodeTransactionMaterial),
  );
  if (decoded.length === 0) {
    reject("block_has_no_transactions", `header_hash=${normalizedHeaderHash}`);
  }
  const byTxId = new Map(decoded.map((tx) => [tx.nodeTxId, tx] as const));

  const candidate = findCandidate({
    decoded,
    byTxId,
    ...(normalizedBadTxId === undefined ? {} : { badTxId: normalizedBadTxId }),
    ...(normalizedBadInputsIndex === undefined
      ? {}
      : { badInputsIndex: normalizedBadInputsIndex }),
    headerHash: normalizedHeaderHash,
  });

  const trie = await buildTrieView(decoded.map(transactionSourceTrieItem));
  const committedTransactionsRoot = await Effect.runPromise(
    SDK.commitCountedRootProgram({
      domain: SDK.ROOT_DOMAINS.transactionsV1,
      phasRoot: trie.root,
      count: BigInt(decoded.length),
    }),
  );
  if (committedTransactionsRoot !== normalizedExpectedRoot) {
    reject(
      "transactions_root_mismatch",
      `header_transactions_root=${normalizedExpectedRoot} derived=${committedTransactionsRoot} derived_count=${decoded.length.toString()}; the prepared proof would not verify against this block`,
    );
  }

  const evidence = SDK.inputNoIdxEvidenceFromCommittedTransactions({
    badTxId: candidate.badTx.nodeTxId,
    badInputsIndex: candidate.badInputsIndex,
    badInput: candidate.badInput,
    producingTxOutputCount: candidate.producingOutputs.length,
  });

  const inputsPreimage = spendInputsOf(candidate.badTx);
  const outputsPreimage = candidate.producingOutputs.map(
    midgardTxOutputFromCanonicalCbor,
  );
  // Self-check: the preimages this builder emits must re-derive the exact
  // bounded-collection commitments the two opening steps compare against, so a
  // projection bug can never reach a prover.
  const derivedInputsCommitment =
    SDK.inputNoIdxSpendInputsCommitment(inputsPreimage);
  if (
    derivedInputsCommitment !==
    candidate.badTx.nativeTxCompact.body.spend_inputs_hash
  ) {
    reject(
      "preimage_commitment_mismatch",
      `bad_tx_id=${candidate.badTx.nodeTxId} committed_spend_inputs_hash=${candidate.badTx.nativeTxCompact.body.spend_inputs_hash} derived=${derivedInputsCommitment}`,
    );
  }
  const derivedOutputsCommitment =
    SDK.inputNoIdxOutputsCommitment(outputsPreimage);
  if (
    derivedOutputsCommitment !==
    candidate.producingTx.nativeTxCompact.body.outputs_hash
  ) {
    reject(
      "preimage_commitment_mismatch",
      `producing_tx_id=${candidate.producingTx.nodeTxId} committed_outputs_hash=${candidate.producingTx.nativeTxCompact.body.outputs_hash} derived=${derivedOutputsCommitment}`,
    );
  }

  // #604: thread state carries the §2.5 anchor — the transaction id — not the
  // field-0 commitment. Step-02 re-opens field 0 through the §8.8 door from it.
  const step02State = SDK.inputNoIdxStep02StateFromBadTx(
    candidate.badTx.nodeTxId,
  );
  const step03State = SDK.inputNoIdxStep03StateFromEvidence(evidence);
  const step04State = SDK.inputNoIdxStep04StateFromEvidence({
    evidence,
    producingTxId: candidate.producingTx.nodeTxId,
  });

  // §5.1's envelope over field 0's canonical items — the bytes the door hashes
  // and the length §8.4 partitions the carriage tier on.
  const step02SpendInputsPreimage = encodeMidgardFieldPreimage(
    inputsPreimage.map(SDK.encodeMidgardTxInputCanonical),
  );
  const inputsPreimageDatum = Data.to(
    inputsPreimage,
    asLucidSchema(SDK.MidgardTxInputList),
  );
  const outputsPreimageDatum = Data.to(
    outputsPreimage,
    asLucidSchema(SDK.MidgardTxOutputList),
  );

  const output: PreparedInputNoIdxOutput = {
    schemaVersion: INPUT_NO_IDX_EVIDENCE_SCHEMA_VERSION,
    violationId: SDK.INPUT_NO_IDX_VIOLATION_ID,
    headerHash: normalizedHeaderHash,
    txCount: decoded.length,
    transactionsPhasRoot: trie.root,
    committedTransactionsRoot,
    expectedTransactionsRoot: {
      value: normalizedExpectedRoot,
      matches: true,
    },
    evidence,
    badTxInclusion: txInclusionOf(
      candidate.badTx,
      trie.root,
      requireProof(
        trie,
        transactionSourceTrieItem(candidate.badTx).key,
        "bad tx",
      ),
    ),
    producingTxInclusion: txInclusionOf(
      candidate.producingTx,
      trie.root,
      requireProof(
        trie,
        transactionSourceTrieItem(candidate.producingTx).key,
        "producing tx",
      ),
    ),
    step02: {
      badTxId: candidate.badTx.nodeTxId,
      verifiedTxInputsHash:
        candidate.badTx.nativeTxCompact.body.spend_inputs_hash,
      inputsPreimage,
      badInputsIndex: candidate.badInputsIndex,
    },
    step02State,
    step03State,
    step04: {
      producingTxId: candidate.producingTx.nodeTxId,
      producingTxOutputsHash:
        candidate.producingTx.nativeTxCompact.body.outputs_hash,
      outputsPreimageCbor: candidate.producingOutputs.map((item) =>
        item.toString("hex"),
      ),
      badInputOutputIndex: candidate.badInput.output_index.toString(),
    },
    outputsPreimage,
    step04State,
    proofFit: {
      step02InputsPreimageItemCount: inputsPreimage.length,
      step02InputsPreimageDatumBytes: inputsPreimageDatum.length / 2,
      step04OutputsPreimageItemCount: outputsPreimage.length,
      step04OutputsPreimageDatumBytes: outputsPreimageDatum.length / 2,
      badTxCompactCborBytes: candidate.badTx.nativeCompactCbor.length / 2,
      producingTxCompactCborBytes:
        candidate.producingTx.nativeCompactCbor.length / 2,
      step02SpendInputsPreimageBytes: step02SpendInputsPreimage.length,
      step02CarriageTier: selectMidgardFieldCarriageTier(
        step02SpendInputsPreimage.length,
      ),
    },
  };

  if (outputDir === undefined) {
    return output;
  }
  await mkdir(outputDir, { recursive: true });
  const paths = {
    badTxInclusionPath: join(outputDir, "bad-tx-inclusion.json"),
    producingTxInclusionPath: join(outputDir, "producing-tx-inclusion.json"),
    inputsPreimagePath: join(outputDir, "inputs-preimage.json"),
    outputsPreimagePath: join(outputDir, "outputs-preimage.json"),
    planPath: join(outputDir, "plan.json"),
  };
  await Promise.all([
    writeFile(paths.badTxInclusionPath, stringifyJson(output.badTxInclusion)),
    writeFile(
      paths.producingTxInclusionPath,
      stringifyJson(output.producingTxInclusion),
    ),
    writeFile(paths.inputsPreimagePath, stringifyJson(output.step02)),
    writeFile(paths.outputsPreimagePath, stringifyJson(output.step04)),
    writeFile(
      paths.planPath,
      stringifyJson({
        schemaVersion: output.schemaVersion,
        violationId: output.violationId,
        headerHash: output.headerHash,
        txCount: output.txCount,
        transactionsPhasRoot: output.transactionsPhasRoot,
        committedTransactionsRoot: output.committedTransactionsRoot,
        expectedTransactionsRoot: output.expectedTransactionsRoot,
        evidence: output.evidence,
        step02State: output.step02State,
        step03State: output.step03State,
        step04State: output.step04State,
        proofFit: output.proofFit,
      }),
    ),
  ]);
  return { ...output, files: paths };
};

// ## Layered entry points

export type PrepareInputNoIdxCliConfig = {
  readonly midgardNodeUrl: string;
  readonly headerHash: string;
  readonly expectedTransactionsRoot: string;
  readonly badTxId?: string;
  readonly badInputsIndex?: string | number;
  readonly outputDir?: string;
  readonly fetchImpl?: FetchLike;
};

export type PrepareInputNoIdxFromFileConfig = {
  readonly transactionsPath: string;
  readonly headerHash: string;
  readonly expectedTransactionsRoot: string;
  readonly badTxId?: string;
  readonly badInputsIndex?: string | number;
  readonly outputDir?: string;
};

/**
 * Operator-diagnostic rehearsal route: block material is read from the node's
 * REST surface, an `operator_only_diagnostic_endpoint` under
 * `PROHIBITED_EVIDENCE_TRUST_CLASSES_V1`. Never security grade.
 */
export const prepareInputNoIdxFromNode = async (
  config: PrepareInputNoIdxCliConfig,
): Promise<PreparedInputNoIdxOutput> => {
  const headerHash = parseHex(config.headerHash, "--header-hash", 28);
  const transactions = await fetchNodeBlockTransactions({
    midgardNodeUrl: config.midgardNodeUrl,
    headerHash,
    ...(config.fetchImpl === undefined ? {} : { fetchImpl: config.fetchImpl }),
  });
  return await prepareInputNoIdxFromTransactions({
    headerHash,
    transactions,
    expectedTransactionsRoot: config.expectedTransactionsRoot,
    ...(config.badTxId === undefined ? {} : { badTxId: config.badTxId }),
    ...(config.badInputsIndex === undefined
      ? {}
      : { badInputsIndex: config.badInputsIndex }),
    ...(config.outputDir === undefined ? {} : { outputDir: config.outputDir }),
  });
};

/** Operator-diagnostic rehearsal route over an `operator_private_file`. */
export const prepareInputNoIdxFromFile = async (
  config: PrepareInputNoIdxFromFileConfig,
): Promise<PreparedInputNoIdxOutput> => {
  const transactions = await readNodeTransactionPayloadsFile(
    config.transactionsPath,
  );
  return await prepareInputNoIdxFromTransactions({
    headerHash: config.headerHash,
    transactions,
    expectedTransactionsRoot: config.expectedTransactionsRoot,
    ...(config.badTxId === undefined ? {} : { badTxId: config.badTxId }),
    ...(config.badInputsIndex === undefined
      ? {}
      : { badInputsIndex: config.badInputsIndex }),
    ...(config.outputDir === undefined ? {} : { outputDir: config.outputDir }),
  });
};
