/** Security-grade evidence preparation for the spend-side withdrawn-input family. */
import { mkdir, writeFile } from "node:fs/promises";
import { join } from "node:path";

import {
  commitCountedRootProgram,
  committedWithdrawalKeyBytes,
  committedWithdrawalValueBytes,
  isWithdrawnInputViolation,
  type MidgardTxInput,
  type OutputReference,
  Proof,
  ROOT_DOMAINS,
  type WithdrawalInfo,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  blockTransactionsFromCanonicalEvidence,
  type CanonicalBlockEvidence,
} from "./evidence/canonical-block-evidence.js";
import { stringifyJson } from "./json-file.js";
import {
  buildTrieView,
  decodeTransactionMaterial,
  type NodeTransactionPayload,
  requireProof,
  requireTransactionsRootMatch,
  transactionSourceTrieItem,
} from "./prepare-double-spend.js";
import { type SubmitStep01TxInclusion } from "./submit-step-01.js";
import {
  type PreparedWithdrawnInputEvidence,
  WITHDRAWN_INPUT_EVIDENCE_SCHEMA_VERSION,
  withdrawnInputEvidenceReject,
  WithdrawnInputEvidenceRejection,
} from "./withdrawn-input/evidence.js";

export type WithdrawnInputWithdrawalEntry = {
  readonly key: OutputReference;
  readonly value: WithdrawalInfo;
};

export type PrepareWithdrawnInputMaterial = {
  readonly headerHash: string;
  readonly transactions: readonly NodeTransactionPayload[];
  readonly expectedTransactionsRoot: string;
  readonly withdrawals: readonly WithdrawnInputWithdrawalEntry[];
  readonly expectedWithdrawalsRoot: string;
  readonly badTxId?: string;
  readonly badInputIndex?: number;
  readonly outputDir?: string;
};

const toMidgardInput = (input: {
  readonly transactionId: string;
  readonly outputIndex: bigint;
}): MidgardTxInput => ({
  tx_id: input.transactionId,
  output_index: input.outputIndex,
});

export const prepareWithdrawnInputFromMaterial = async ({
  headerHash,
  transactions,
  expectedTransactionsRoot,
  withdrawals,
  expectedWithdrawalsRoot,
  badTxId,
  badInputIndex,
  outputDir,
}: PrepareWithdrawnInputMaterial): Promise<PreparedWithdrawnInputEvidence> => {
  const decoded = await Promise.all(
    transactions.map(decodeTransactionMaterial),
  );
  const selectedTransactions =
    badTxId === undefined
      ? decoded
      : decoded.filter((transaction) => transaction.nodeTxId === badTxId);
  if (badTxId !== undefined && selectedTransactions.length === 0) {
    withdrawnInputEvidenceReject(
      "bad_tx_not_committed",
      `transaction ${badTxId} is not committed by header ${headerHash}`,
    );
  }
  let candidate:
    | {
        readonly transaction: (typeof decoded)[number];
        readonly input: MidgardTxInput;
        readonly inputIndex: number;
        readonly withdrawal: WithdrawnInputWithdrawalEntry;
      }
    | undefined;
  for (const transaction of selectedTransactions) {
    const inputs = transaction.inputs.map(toMidgardInput);
    const indices =
      badInputIndex === undefined
        ? inputs.map((_input, index) => index)
        : [badInputIndex];
    for (const inputIndex of indices) {
      const input = inputs[inputIndex];
      if (input === undefined) {
        if (badInputIndex !== undefined) {
          withdrawnInputEvidenceReject(
            "bad_input_index_out_of_range",
            `index ${badInputIndex.toString()} is outside ${inputs.length.toString()} spend inputs`,
          );
        }
        continue;
      }
      const withdrawal = withdrawals.find((entry) =>
        isWithdrawnInputViolation({ input, withdrawal: entry.value }),
      );
      if (withdrawal !== undefined) {
        candidate = { transaction, input, inputIndex, withdrawal };
        break;
      }
    }
    if (candidate !== undefined) break;
  }
  if (candidate === undefined) {
    throw new WithdrawnInputEvidenceRejection(
      "no_valid_withdrawn_input",
      `header ${headerHash} has no transaction spend matching a valid withdrawal leaf`,
    );
  }
  const selected = candidate;

  const transactionTrie = await buildTrieView(
    decoded.map(transactionSourceTrieItem),
  );
  await requireTransactionsRootMatch({
    sourceRoot: transactionTrie.root,
    expectedTransactionsRoot,
    count: BigInt(decoded.length),
  }).catch((cause: unknown) =>
    withdrawnInputEvidenceReject("transactions_root_mismatch", String(cause)),
  );
  const txProofCbor = requireProof(
    transactionTrie,
    Buffer.from(selected.transaction.nodeTxId, "hex"),
    "withdrawn-input transaction",
  );

  const withdrawalItems = withdrawals.map((entry) => ({
    key: Buffer.from(committedWithdrawalKeyBytes(entry.key), "hex"),
    value: Buffer.from(committedWithdrawalValueBytes(entry.value), "hex"),
  }));
  const withdrawalTrie = await buildTrieView(withdrawalItems);
  const derivedWithdrawalsRoot = await Effect.runPromise(
    commitCountedRootProgram({
      domain: ROOT_DOMAINS.withdrawals,
      phasRoot: withdrawalTrie.root,
      count: BigInt(withdrawals.length),
    }),
  );
  if (derivedWithdrawalsRoot !== expectedWithdrawalsRoot) {
    withdrawnInputEvidenceReject(
      "withdrawals_root_mismatch",
      `derived=${derivedWithdrawalsRoot} expected=${expectedWithdrawalsRoot}`,
    );
  }
  const withdrawalKeyBytes = Buffer.from(
    committedWithdrawalKeyBytes(selected.withdrawal.key),
    "hex",
  );
  const withdrawalProofCbor = requireProof(
    withdrawalTrie,
    withdrawalKeyBytes,
    "withdrawn-input withdrawal",
  );
  const txInclusion: SubmitStep01TxInclusion = {
    nativeTxId: selected.transaction.nodeTxId,
    nativeTx: selected.transaction.nativeTxCompact,
    nativeTxCompactCbor: selected.transaction.nativeCompactCbor,
    l2TransactionSourceCbor: selected.transaction.l2TransactionSourceCbor,
    transactionsPhasRoot: transactionTrie.root,
    txMembershipProof: Data.from(txProofCbor, Proof),
    txMembershipProofCbor: txProofCbor,
  };
  const output: PreparedWithdrawnInputEvidence = {
    schemaVersion: WITHDRAWN_INPUT_EVIDENCE_SCHEMA_VERSION,
    headerHash,
    badTxInclusion: txInclusion,
    spendInputs: selected.transaction.inputs.map(toMidgardInput),
    badInputIndex: selected.inputIndex,
    withdrawnInput: selected.input,
    withdrawalId: selected.withdrawal.key,
    withdrawal: selected.withdrawal.value,
    withdrawalMembership: {
      domain: ROOT_DOMAINS.withdrawals,
      root: expectedWithdrawalsRoot,
      phas_root: withdrawalTrie.root,
      count: BigInt(withdrawals.length),
      key: selected.withdrawal.key,
      value: selected.withdrawal.value,
      proof: Data.from(withdrawalProofCbor, Proof),
    },
  };
  if (outputDir !== undefined) {
    await mkdir(outputDir, { recursive: true });
    await Promise.all([
      writeFile(
        join(outputDir, "tx-inclusion.json"),
        stringifyJson(output.badTxInclusion),
      ),
      writeFile(
        join(outputDir, "spend-inputs.json"),
        stringifyJson({
          inputs: output.spendInputs,
          badInputIndex: output.badInputIndex,
          nativeTxCompactCbor: output.badTxInclusion.nativeTxCompactCbor,
        }),
      ),
      writeFile(
        join(outputDir, "withdrawal-membership.json"),
        stringifyJson(output.withdrawalMembership),
      ),
      writeFile(join(outputDir, "plan.json"), stringifyJson(output)),
    ]);
  }
  return output;
};

/** Canonical L1 + retained-DA entry point. */
export const prepareWithdrawnInputFromCanonicalEvidence = async ({
  evidence,
  badTxId,
  badInputIndex,
  outputDir,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly badTxId?: string;
  readonly badInputIndex?: number;
  readonly outputDir?: string;
}): Promise<PreparedWithdrawnInputEvidence> => {
  const transactions = blockTransactionsFromCanonicalEvidence(evidence);
  if (!evidence.inclusionRootAuthentication.sourceInclusionAuthenticated) {
    withdrawnInputEvidenceReject(
      "transactions_root_mismatch",
      "canonical evidence does not authenticate L2TransactionSourceV1 leaves",
    );
  }
  return await prepareWithdrawnInputFromMaterial({
    headerHash: evidence.headerHash,
    transactions,
    expectedTransactionsRoot: evidence.header.transactionsRoot,
    withdrawals: evidence.reconstruction.withdrawals.map(({ key, value }) => ({
      key,
      value,
    })),
    expectedWithdrawalsRoot: evidence.header.withdrawalsRoot,
    ...(badTxId === undefined ? {} : { badTxId }),
    ...(badInputIndex === undefined ? {} : { badInputIndex }),
    ...(outputDir === undefined ? {} : { outputDir }),
  });
};
