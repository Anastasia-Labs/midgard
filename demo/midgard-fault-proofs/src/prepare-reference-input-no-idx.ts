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
import { mkdir, writeFile } from "node:fs/promises";
import { join } from "node:path";

import {
  computeMidgardNativeTxIdV1,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  encodeMidgardNativeTxCompactV1,
  formatUnknownError,
  type MidgardNativeTxFullV1,
} from "@al-ft/midgard-core";
import {
  commitCountedRootProgram,
  type MidgardTxInput,
  type NativeTxCompact as NativeTxCompactData,
  ROOT_DOMAINS,
} from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import {
  admitCanonicalEvidenceForProofBuildV1,
  type CanonicalBlockEvidenceV1,
} from "./evidence/index.js";
import { parseHex, stringifyJson } from "./json-file.js";
import {
  buildMembershipProof,
  computeTrieRoot,
  type TrieEntry,
} from "./ne-proofs.js";
import {
  deriveL2TransactionSourceCborV1,
  type FetchLike,
  fetchNodeBlockTransactions,
  type NodeTransactionPayload,
  readNodeTransactionPayloadsFile,
} from "./prepare-double-spend.js";
import { spendInputsWitnessFromCbors } from "./spend-input-witness.js";
import { nativeTxFromCoreCompact } from "./submit-step-01.js";

/**
 * One reference input of the bad transaction, as committed by its native
 * `reference_inputs_hash`. Consumed by
 * `submit-reference-input-no-idx-step-02.ts`, which re-commits the whole list
 * against the hash the on-chain step-02 datum carries.
 */
export type ReferenceInputNoIdxPreimageEntry = {
  readonly txId: string;
  readonly index: number | bigint;
};

/** step-01/step-03 material: a committed tx and its membership proof. */
export type PreparedReferenceInputNoIdxTxInclusionJson = {
  readonly nativeTxId: string;
  readonly nativeTx: NativeTxCompactData;
  readonly nativeTxCompactCbor: string;
  readonly l2TransactionSourceCbor: string;
  // Raw transactions MPF root the membership proof opens; authenticated on-chain
  // against the header's counted `transactions_root`.
  readonly transactionsPhasRoot: string;
  readonly txMembershipProofCbor: string;
};

export type PrepareReferenceInputNoIdxCliConfig = {
  readonly midgardNodeUrl: string;
  readonly headerHash: string;
  readonly badTxId?: string;
  readonly badReferenceInputIndex?: string | number;
  readonly expectedTransactionsRoot?: string;
  readonly outputDir?: string;
  readonly fetchImpl?: FetchLike;
};

export type PrepareReferenceInputNoIdxFromFileConfig = {
  readonly transactionsPath: string;
  readonly headerHash: string;
  readonly badTxId?: string;
  readonly badReferenceInputIndex?: string | number;
  readonly expectedTransactionsRoot?: string;
  readonly outputDir?: string;
};

export type PreparedReferenceInputNoIdxOutput = {
  readonly headerHash: string;
  readonly txCount: number;
  /** Raw MPF root used by the transaction membership proofs. */
  readonly transactionsRoot: string;
  /** Counted root committed in the state-queue block header. */
  readonly committedTransactionsRoot: string;
  readonly badTxId: string;
  /** Index of the offending input inside the bad transaction's reference list. */
  readonly badReferenceInputIndex: number;
  /** The offending reference input: its `tx_id` is the producing tx, `output_index` is out of range. */
  readonly badReferenceInput: MidgardTxInput;
  readonly producingTxId: string;
  readonly producingTxOutputCount: number;
  readonly expectedTransactionsRoot?: {
    readonly value: string;
    readonly matches: boolean;
  };
  /** step-01 material: the bad tx and its membership proof. */
  readonly badTxInclusion: PreparedReferenceInputNoIdxTxInclusionJson;
  /** step-02 material: the bad tx's reference-inputs preimage. */
  readonly referenceInputsPreimage: readonly ReferenceInputNoIdxPreimageEntry[];
  /** step-03 material: the producing tx and its membership proof. */
  readonly producingTxInclusion: PreparedReferenceInputNoIdxTxInclusionJson;
  /**
   * step-04 material: the producing tx's canonical outputs preimage, one raw
   * `encode_midgard_tx_output` item per output. The step-04 redeemer carries
   * these as structured `MidgardTxOutput` PlutusData, which JSON cannot hold
   * losslessly (native-asset maps), so the artifact stores the canonical items
   * the on-chain step re-encodes.
   */
  readonly outputsPreimageCbor: readonly string[];
  readonly files?: {
    readonly badTxInclusionPath: string;
    readonly referenceInputsPreimagePath: string;
    readonly producingTxInclusionPath: string;
    readonly outputsPreimagePath: string;
    readonly planPath: string;
  };
};

type DecodedTx = {
  readonly nodeTxId: string;
  readonly nativeTxCompact: NativeTxCompactData;
  readonly nativeCompactCbor: string;
  readonly l2TransactionSourceCbor: string;
  readonly referenceInputs: readonly MidgardTxInput[];
  /** Producing transaction's outputs preimage, one raw CBOR hex string per output. */
  readonly outputsPreimageCbor: readonly string[];
};

export type ReferenceInputNoIdxDetectedViolationV1 = Readonly<{
  badTxIndex: number;
  badTxId: string;
  badReferenceInputIndex: number;
  producingTxId: string;
  badReferenceInputOutputIndex: bigint;
  producingTxOutputCount: number;
}>;

const decodeTx = (payload: NodeTransactionPayload): DecodedTx => {
  const nodeTxId = parseHex(payload.nodeTxId, "nodeTxId", 32);
  const txCbor = parseHex(payload.txCbor, `tx ${nodeTxId} CBOR`);
  let nativeTx: MidgardNativeTxFullV1;
  try {
    nativeTx = decodeMidgardNativeTxFullV1FromCanonicalCbor(
      Buffer.from(txCbor, "hex"),
    );
  } catch (cause) {
    throw new Error(
      `Failed to decode native Midgard tx ${nodeTxId}: ${formatUnknownError(cause)}`,
    );
  }
  const computed = computeMidgardNativeTxIdV1(nativeTx).toString("hex");
  if (computed !== nodeTxId) {
    throw new Error(
      `Node tx id mismatch: listed=${nodeTxId}, computed=${computed}.`,
    );
  }
  const referenceInputCbors = decodeMidgardNativeByteListPreimage(
    nativeTx.body.referenceInputsPreimageCbor,
    `tx ${nodeTxId} reference_inputs`,
  ).map((bytes) => Buffer.from(bytes).toString("hex"));
  const outputsPreimageCbor = decodeMidgardNativeByteListPreimage(
    nativeTx.body.outputsPreimageCbor,
    `tx ${nodeTxId} outputs`,
  ).map((bytes) => Buffer.from(bytes).toString("hex"));
  return {
    nodeTxId,
    nativeTxCompact: nativeTxFromCoreCompact(nativeTx.compact),
    nativeCompactCbor: encodeMidgardNativeTxCompactV1(
      nativeTx.compact,
    ).toString("hex"),
    l2TransactionSourceCbor: deriveL2TransactionSourceCborV1(
      Buffer.from(txCbor, "hex"),
    ),
    referenceInputs: spendInputsWitnessFromCbors(
      referenceInputCbors,
      "reference_inputs",
    ).inputs,
    outputsPreimageCbor,
  };
};

const parseBadReferenceInputIndex = (
  value: string | number | undefined,
): number | undefined => {
  if (value === undefined) {
    return undefined;
  }
  const parsed = typeof value === "number" ? value : Number(value);
  if (!Number.isInteger(parsed) || parsed < 0) {
    throw new Error(
      `--bad-reference-input-index must be a non-negative integer, got "${String(value)}".`,
    );
  }
  return parsed;
};

/**
 * Complete same-block scan used by the sealed production replay bundle. A
 * reference input whose producer is absent belongs to `noReferenceInput`; only
 * an out-of-range index into a producer committed by this same block is
 * emitted here.
 */
export const detectReferenceInputNoIdxViolationsFromTransactionsV1 = (
  transactions: readonly NodeTransactionPayload[],
): readonly ReferenceInputNoIdxDetectedViolationV1[] => {
  const decoded = transactions.map(decodeTx);
  const byId = new Map(decoded.map((tx) => [tx.nodeTxId, tx] as const));
  const detections: ReferenceInputNoIdxDetectedViolationV1[] = [];
  for (const [badTxIndex, badTx] of decoded.entries()) {
    for (const [
      badReferenceInputIndex,
      input,
    ] of badTx.referenceInputs.entries()) {
      const producingTx = byId.get(input.tx_id);
      if (producingTx === undefined) continue;
      if (input.output_index < producingTx.outputsPreimageCbor.length) continue;
      detections.push(
        Object.freeze({
          badTxIndex,
          badTxId: badTx.nodeTxId,
          badReferenceInputIndex,
          producingTxId: producingTx.nodeTxId,
          badReferenceInputOutputIndex: input.output_index,
          producingTxOutputCount: producingTx.outputsPreimageCbor.length,
        }),
      );
    }
  }
  return Object.freeze(detections);
};

/**
 * Locates an out-of-range reference input: an input whose producing transaction
 * IS present in the block, but whose `output_index` is >= that producing
 * transaction's output count. Honours an explicit
 * `--bad-tx-id`/`--bad-reference-input-index` selection when supplied, otherwise
 * scans for the first offending pair.
 */
const selectOffendingReferenceInput = ({
  decoded,
  byId,
  badTxId,
  badReferenceInputIndex,
}: {
  readonly decoded: readonly DecodedTx[];
  readonly byId: ReadonlyMap<string, DecodedTx>;
  readonly badTxId?: string;
  readonly badReferenceInputIndex?: number;
}): {
  readonly badTx: DecodedTx;
  readonly badReferenceInputIndex: number;
  readonly badReferenceInput: MidgardTxInput;
  readonly producingTx: DecodedTx;
} => {
  const isOffending = (
    tx: DecodedTx,
    index: number,
  ):
    | { readonly input: MidgardTxInput; readonly producing: DecodedTx }
    | undefined => {
    const input = tx.referenceInputs[index];
    if (input === undefined) {
      return undefined;
    }
    const producing = byId.get(input.tx_id);
    if (producing === undefined) {
      return undefined;
    }
    if (input.output_index < producing.outputsPreimageCbor.length) {
      return undefined;
    }
    return { input, producing };
  };

  if (badTxId !== undefined) {
    const normalized = parseHex(badTxId, "--bad-tx-id", 32);
    const badTx = decoded.find((tx) => tx.nodeTxId === normalized);
    if (badTx === undefined) {
      throw new Error(`--bad-tx-id ${normalized} not found in the block.`);
    }
    if (badReferenceInputIndex !== undefined) {
      if (badReferenceInputIndex >= badTx.referenceInputs.length) {
        throw new Error(
          `--bad-reference-input-index ${badReferenceInputIndex.toString()} is out of bounds for ${badTx.referenceInputs.length.toString()} reference inputs.`,
        );
      }
      const found = isOffending(badTx, badReferenceInputIndex);
      if (found === undefined) {
        const input = badTx.referenceInputs[badReferenceInputIndex]!;
        const producing = byId.get(input.tx_id);
        throw new Error(
          producing === undefined
            ? `Reference input ${badReferenceInputIndex.toString()} of ${normalized} references producing tx ${input.tx_id}, which is not in the block — that is a no-reference-input fault, not reference-input-no-idx.`
            : `Reference input ${badReferenceInputIndex.toString()} of ${normalized} has output index ${input.output_index.toString()}, which is within range of its producing tx's ${producing.outputsPreimageCbor.length.toString()} outputs — not a fault.`,
        );
      }
      return {
        badTx,
        badReferenceInputIndex,
        badReferenceInput: found.input,
        producingTx: found.producing,
      };
    }
    for (let i = 0; i < badTx.referenceInputs.length; i++) {
      const found = isOffending(badTx, i);
      if (found !== undefined) {
        return {
          badTx,
          badReferenceInputIndex: i,
          badReferenceInput: found.input,
          producingTx: found.producing,
        };
      }
    }
    throw new Error(
      `--bad-tx-id ${normalized} has no out-of-range reference input; specify a different tx.`,
    );
  }

  for (const badTx of decoded) {
    for (let i = 0; i < badTx.referenceInputs.length; i++) {
      const found = isOffending(badTx, i);
      if (found !== undefined) {
        return {
          badTx,
          badReferenceInputIndex: i,
          badReferenceInput: found.input,
          producingTx: found.producing,
        };
      }
    }
  }
  throw new Error(
    "No out-of-range reference input found in the block. Every referenced input " +
      "either resolves within its in-block producing transaction's outputs or " +
      "references a producing tx absent from the block (a no-reference-input fault).",
  );
};

/**
 * Builds the four reference-input-no-idx submit-step artifacts from a block the
 * node actually committed. The transactions trie is reconstructed with the
 * exact canonical `Data(L2TransactionSourceV1)` values keyed by raw tx id, so
 * its root matches the committed `transactions_root` by construction.
 */
export const prepareReferenceInputNoIdxFromTransactions = async ({
  headerHash,
  transactions,
  badTxId,
  badReferenceInputIndex,
  expectedTransactionsRoot,
  outputDir,
}: {
  readonly headerHash: string;
  readonly transactions: readonly NodeTransactionPayload[];
  readonly badTxId?: string;
  readonly badReferenceInputIndex?: string | number;
  readonly expectedTransactionsRoot?: string;
  readonly outputDir?: string;
}): Promise<PreparedReferenceInputNoIdxOutput> => {
  const normalizedHeaderHash = parseHex(headerHash, "--header-hash", 28);
  const decoded = transactions.map(decodeTx);
  if (decoded.length === 0) {
    throw new Error("The selected block contains no transactions.");
  }
  const byId = new Map(decoded.map((tx) => [tx.nodeTxId, tx] as const));

  const {
    badTx,
    badReferenceInputIndex: resolvedBadReferenceInputIndex,
    badReferenceInput,
    producingTx,
  } = selectOffendingReferenceInput({
    decoded,
    byId,
    ...(badTxId === undefined ? {} : { badTxId }),
    ...(badReferenceInputIndex === undefined
      ? {}
      : {
          badReferenceInputIndex: parseBadReferenceInputIndex(
            badReferenceInputIndex,
          ),
        }),
  });

  // --- Transactions trie (native encoding, matches the node) ----------------
  const txsEntries: TrieEntry[] = decoded.map((tx) => ({
    key: Buffer.from(tx.nodeTxId, "hex"),
    value: Buffer.from(tx.l2TransactionSourceCbor, "hex"),
  }));
  const transactionsRoot = await computeTrieRoot(txsEntries);
  const badTxMembershipProofCbor = await buildMembershipProof(
    txsEntries,
    Buffer.from(badTx.nodeTxId, "hex"),
  );
  const producingTxMembershipProofCbor = await buildMembershipProof(
    txsEntries,
    Buffer.from(producingTx.nodeTxId, "hex"),
  );

  const committedTransactionsRoot = await Effect.runPromise(
    commitCountedRootProgram({
      domain: ROOT_DOMAINS.transactionsV1,
      phasRoot: transactionsRoot,
      count: BigInt(decoded.length),
    }),
  );

  const expectedCheck =
    expectedTransactionsRoot === undefined
      ? undefined
      : (() => {
          const value = parseHex(
            expectedTransactionsRoot,
            "--expected-transactions-root",
            32,
          );
          return { value, matches: value === committedTransactionsRoot };
        })();
  if (expectedCheck !== undefined && !expectedCheck.matches) {
    throw new Error(
      `Reconstructed raw transactions root ${transactionsRoot} commits to counted root ${committedTransactionsRoot}, which does not match --expected-transactions-root ${expectedCheck.value}. The prepared proofs would not verify against this block.`,
    );
  }

  const referenceInputsPreimage: readonly ReferenceInputNoIdxPreimageEntry[] =
    badTx.referenceInputs.map((input) => ({
      txId: input.tx_id,
      index: input.output_index,
    }));

  const base: PreparedReferenceInputNoIdxOutput = {
    headerHash: normalizedHeaderHash,
    txCount: decoded.length,
    transactionsRoot,
    committedTransactionsRoot,
    badTxId: badTx.nodeTxId,
    badReferenceInputIndex: resolvedBadReferenceInputIndex,
    badReferenceInput,
    producingTxId: producingTx.nodeTxId,
    producingTxOutputCount: producingTx.outputsPreimageCbor.length,
    ...(expectedCheck === undefined
      ? {}
      : { expectedTransactionsRoot: expectedCheck }),
    badTxInclusion: {
      nativeTxId: badTx.nodeTxId,
      nativeTx: badTx.nativeTxCompact,
      nativeTxCompactCbor: badTx.nativeCompactCbor,
      l2TransactionSourceCbor: badTx.l2TransactionSourceCbor,
      transactionsPhasRoot: transactionsRoot,
      txMembershipProofCbor: badTxMembershipProofCbor,
    },
    referenceInputsPreimage,
    producingTxInclusion: {
      nativeTxId: producingTx.nodeTxId,
      nativeTx: producingTx.nativeTxCompact,
      nativeTxCompactCbor: producingTx.nativeCompactCbor,
      l2TransactionSourceCbor: producingTx.l2TransactionSourceCbor,
      transactionsPhasRoot: transactionsRoot,
      txMembershipProofCbor: producingTxMembershipProofCbor,
    },
    outputsPreimageCbor: producingTx.outputsPreimageCbor,
  };
  if (outputDir === undefined) {
    return base;
  }
  await mkdir(outputDir, { recursive: true });
  const files = {
    badTxInclusionPath: join(
      outputDir,
      "reference-input-no-idx-bad-tx-inclusion.json",
    ),
    referenceInputsPreimagePath: join(
      outputDir,
      "reference-input-no-idx-reference-inputs-preimage.json",
    ),
    producingTxInclusionPath: join(
      outputDir,
      "reference-input-no-idx-producing-tx-inclusion.json",
    ),
    outputsPreimagePath: join(
      outputDir,
      "reference-input-no-idx-outputs-preimage.json",
    ),
    planPath: join(outputDir, "reference-input-no-idx-plan.json"),
  };
  await Promise.all([
    writeFile(files.badTxInclusionPath, stringifyJson(base.badTxInclusion)),
    writeFile(
      files.referenceInputsPreimagePath,
      stringifyJson(base.referenceInputsPreimage),
    ),
    writeFile(
      files.producingTxInclusionPath,
      stringifyJson(base.producingTxInclusion),
    ),
    writeFile(
      files.outputsPreimagePath,
      stringifyJson(base.outputsPreimageCbor),
    ),
    writeFile(
      files.planPath,
      stringifyJson({
        headerHash: base.headerHash,
        badTxId: base.badTxId,
        badReferenceInputIndex: base.badReferenceInputIndex,
        badReferenceInput: base.badReferenceInput,
        producingTxId: base.producingTxId,
        producingTxOutputCount: base.producingTxOutputCount,
        transactionsRoot: base.transactionsRoot,
        committedTransactionsRoot: base.committedTransactionsRoot,
        expectedTransactionsRoot: base.expectedTransactionsRoot,
      }),
    ),
  ]);
  return { ...base, files };
};

/** Security-grade builder over one authenticated header/public-DA block. */
export const prepareReferenceInputNoIdxFromCanonicalEvidenceV1 = async ({
  evidence,
  badTxId,
  badReferenceInputIndex,
  outputDir,
}: {
  readonly evidence: CanonicalBlockEvidenceV1;
  readonly badTxId?: string;
  readonly badReferenceInputIndex?: string | number;
  readonly outputDir?: string;
}): Promise<PreparedReferenceInputNoIdxOutput> => {
  const admitted = admitCanonicalEvidenceForProofBuildV1(evidence);
  return await prepareReferenceInputNoIdxFromTransactions({
    headerHash: admitted.headerHash,
    transactions: admitted.transactions,
    expectedTransactionsRoot: admitted.expectedTransactionsRoot,
    ...(badTxId === undefined ? {} : { badTxId }),
    ...(badReferenceInputIndex === undefined ? {} : { badReferenceInputIndex }),
    ...(outputDir === undefined ? {} : { outputDir }),
  });
};

/**
 * Operator-diagnostic rehearsal route: block material is read from the node's
 * REST surface, an `operator_only_diagnostic_endpoint`. Never security grade.
 */
export const prepareReferenceInputNoIdxFromNode = async (
  config: PrepareReferenceInputNoIdxCliConfig,
): Promise<PreparedReferenceInputNoIdxOutput> => {
  const headerHash = parseHex(config.headerHash, "--header-hash", 28);
  const transactions = await fetchNodeBlockTransactions({
    midgardNodeUrl: config.midgardNodeUrl,
    headerHash,
    ...(config.fetchImpl === undefined ? {} : { fetchImpl: config.fetchImpl }),
  });
  return await prepareReferenceInputNoIdxFromTransactions({
    headerHash,
    transactions,
    ...(config.badTxId === undefined ? {} : { badTxId: config.badTxId }),
    ...(config.badReferenceInputIndex === undefined
      ? {}
      : { badReferenceInputIndex: config.badReferenceInputIndex }),
    ...(config.expectedTransactionsRoot === undefined
      ? {}
      : { expectedTransactionsRoot: config.expectedTransactionsRoot }),
    ...(config.outputDir === undefined ? {} : { outputDir: config.outputDir }),
  });
};

/** Operator-diagnostic rehearsal route over an `operator_private_file`. */
export const prepareReferenceInputNoIdxFromFile = async (
  config: PrepareReferenceInputNoIdxFromFileConfig,
): Promise<PreparedReferenceInputNoIdxOutput> => {
  const transactions = await readNodeTransactionPayloadsFile(
    config.transactionsPath,
  );
  return await prepareReferenceInputNoIdxFromTransactions({
    headerHash: config.headerHash,
    transactions,
    ...(config.badTxId === undefined ? {} : { badTxId: config.badTxId }),
    ...(config.badReferenceInputIndex === undefined
      ? {}
      : { badReferenceInputIndex: config.badReferenceInputIndex }),
    ...(config.expectedTransactionsRoot === undefined
      ? {}
      : { expectedTransactionsRoot: config.expectedTransactionsRoot }),
    ...(config.outputDir === undefined ? {} : { outputDir: config.outputDir }),
  });
};
