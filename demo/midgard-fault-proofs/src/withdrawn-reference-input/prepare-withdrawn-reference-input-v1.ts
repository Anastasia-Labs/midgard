import { Proof as MpfProof } from "@aiken-lang/merkle-patricia-forestry";
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
  committedWithdrawalKeyBytesV1,
  committedWithdrawalValueBytesV1,
  EMPTY_MERKLE_TREE_ROOT,
  encodeMidgardTxInputCanonicalV1,
  type HeaderV1,
  type MidgardTxInput,
  Proof,
  ROOT_DOMAINS,
  type WithdrawalEvent,
  type WithdrawalSourceMembershipProof,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { NodeTransactionPayload } from "../prepare-double-spend.js";
import { spendInputsWitnessFromCbors } from "../spend-input-witness.js";
import {
  nativeTxFromCoreCompact,
  type SubmitStep01TxInclusion,
} from "../submit-step-01.js";
import {
  buildCountedRoot,
  type KeyValuePhasEntry,
  keyValuePhasProof,
} from "../transition-trace/phas.js";

export type WithdrawnReferenceInputPreparationRefusalV1 =
  | "withdrawal-not-valid"
  | "no-matching-reference-input"
  | "transactions-root-mismatch"
  | "withdrawals-root-mismatch"
  | "membership-proof-mismatch";

export class WithdrawnReferenceInputPreparationErrorV1 extends Error {
  readonly reason: WithdrawnReferenceInputPreparationRefusalV1;

  constructor(
    reason: WithdrawnReferenceInputPreparationRefusalV1,
    message: string,
  ) {
    super(`withdrawn-reference-input prepare: ${reason}: ${message}`);
    this.name = "WithdrawnReferenceInputPreparationErrorV1";
    this.reason = reason;
  }
}

type DecodedBlockTxV1 = {
  readonly txId: string;
  readonly nativeTx: ReturnType<typeof nativeTxFromCoreCompact>;
  readonly nativeTxCompactCbor: string;
  readonly referenceInputs: readonly MidgardTxInput[];
};

export type PreparedWithdrawnReferenceInputV1 = {
  readonly header: HeaderV1;
  readonly txInclusion: SubmitStep01TxInclusion;
  readonly referenceInputs: readonly MidgardTxInput[];
  readonly referenceInputItemCbors: readonly string[];
  readonly badReferenceInputIndex: number;
  readonly missingReferenceInput: MidgardTxInput;
  readonly withdrawal: WithdrawalEvent;
  readonly withdrawalMembership: WithdrawalSourceMembershipProof;
};

const exactHex = (value: string, bytes: number, label: string): string => {
  const normalized = value.toLowerCase();
  if (
    !new RegExp(`^[0-9a-f]{${(bytes * 2).toString()}}$`, "u").test(normalized)
  ) {
    throw new Error(
      `${label} must be ${bytes.toString()} bytes of hexadecimal.`,
    );
  }
  return normalized;
};

const exactHexBytes = (value: string, label: string): Buffer => {
  if (
    value.length % 2 !== 0 ||
    (value.length > 0 && !/^[0-9a-f]+$/u.test(value))
  ) {
    throw new Error(`${label} must be canonical lowercase hexadecimal bytes.`);
  }
  return Buffer.from(value, "hex");
};

const exactProofInteger = (
  value: bigint,
  label: string,
  maximum: number,
): number => {
  if (value < 0n || value > BigInt(maximum)) {
    throw new Error(`${label} is outside the supported MPF proof range.`);
  }
  return Number(value);
};

const decodeBlockTxV1 = (payload: NodeTransactionPayload): DecodedBlockTxV1 => {
  const listedTxId = exactHex(payload.nodeTxId, 32, "nodeTxId");
  let full: MidgardNativeTxFullV1;
  try {
    full = decodeMidgardNativeTxFullV1FromCanonicalCbor(
      exactHexBytes(payload.txCbor, `tx ${listedTxId} txCbor`),
    );
  } catch (cause) {
    throw new Error(
      `Failed to decode native Midgard tx ${listedTxId}: ${formatUnknownError(cause)}`,
    );
  }
  const txId = computeMidgardNativeTxIdV1(full).toString("hex");
  if (txId !== listedTxId) {
    throw new Error(
      `Node tx id mismatch: listed=${listedTxId}, computed=${txId}.`,
    );
  }
  const referenceInputCbors = decodeMidgardNativeByteListPreimage(
    full.body.referenceInputsPreimageCbor,
    `tx ${txId} reference_inputs`,
  ).map((bytes) => Buffer.from(bytes).toString("hex"));
  return {
    txId,
    nativeTx: nativeTxFromCoreCompact(full.compact),
    nativeTxCompactCbor: encodeMidgardNativeTxCompactV1(full.compact).toString(
      "hex",
    ),
    referenceInputs: spendInputsWitnessFromCbors(
      referenceInputCbors,
      "reference_inputs",
    ).inputs,
  };
};

const sameOutRef = (
  left: MidgardTxInput,
  right: WithdrawalEvent["info"]["body"]["l2_outref"],
): boolean =>
  left.tx_id === right.transactionId && left.output_index === right.outputIndex;

const proofStepJson = (step: Proof[number], index: number): object => {
  const label = `proof[${index.toString()}]`;
  if ("Branch" in step) {
    const neighbors = exactHexBytes(
      step.Branch.neighbors,
      `${label}.Branch.neighbors`,
    );
    if (neighbors.length !== 4 * 32) {
      throw new Error(
        `${label}.Branch.neighbors must contain exactly four MPF hashes.`,
      );
    }
    return {
      type: "branch",
      skip: exactProofInteger(step.Branch.skip, `${label}.Branch.skip`, 64),
      neighbors: step.Branch.neighbors,
    };
  }
  if ("Fork" in step) {
    const prefix = exactHexBytes(
      step.Fork.neighbor.prefix,
      `${label}.Fork.neighbor.prefix`,
    );
    const root = exactHexBytes(
      step.Fork.neighbor.root,
      `${label}.Fork.neighbor.root`,
    );
    if (
      prefix.length > 64 ||
      prefix.some((nibble) => nibble > 0x0f) ||
      root.length !== 32
    ) {
      throw new Error(`${label}.Fork.neighbor is not a canonical MPF fork.`);
    }
    return {
      type: "fork",
      skip: exactProofInteger(step.Fork.skip, `${label}.Fork.skip`, 64),
      neighbor: {
        nibble: exactProofInteger(
          step.Fork.neighbor.nibble,
          `${label}.Fork.neighbor.nibble`,
          15,
        ),
        prefix: step.Fork.neighbor.prefix,
        root: step.Fork.neighbor.root,
      },
    };
  }
  const neighborKey = exactHexBytes(step.Leaf.key, `${label}.Leaf.key`);
  const neighborValue = exactHexBytes(step.Leaf.value, `${label}.Leaf.value`);
  if (neighborKey.length !== 32 || neighborValue.length !== 32) {
    throw new Error(`${label}.Leaf is not a canonical MPF leaf.`);
  }
  return {
    type: "leaf",
    skip: exactProofInteger(step.Leaf.skip, `${label}.Leaf.skip`, 64),
    neighbor: { key: step.Leaf.key, value: step.Leaf.value },
  };
};

/** Replays the exact canonical key/value proof that step 03 will verify. */
export const verifyWithdrawnReferenceInputMembershipV1 = (
  membership: WithdrawalSourceMembershipProof,
): void => {
  const key = Buffer.from(committedWithdrawalKeyBytesV1(membership.key), "hex");
  const value = Buffer.from(
    committedWithdrawalValueBytesV1(membership.value),
    "hex",
  );
  let actualRoot: Buffer | null;
  try {
    actualRoot = MpfProof.fromJSON(
      key,
      value,
      membership.proof.map(proofStepJson),
    ).verify(true);
  } catch (cause) {
    throw new WithdrawnReferenceInputPreparationErrorV1(
      "membership-proof-mismatch",
      `withdrawal proof cannot be replayed: ${formatUnknownError(cause)}`,
    );
  }
  const actual =
    actualRoot === null ? EMPTY_MERKLE_TREE_ROOT : actualRoot.toString("hex");
  if (actual !== membership.phas_root) {
    throw new WithdrawnReferenceInputPreparationErrorV1(
      "membership-proof-mismatch",
      `withdrawal proof opens ${actual}, not ${membership.phas_root}.`,
    );
  }
};

/**
 * Classifies and prepares one same-block withdrawn-reference-input fault.
 * Callers provide exactly the transactions and withdrawals committed by
 * `header`; any incomplete or substituted set is refused by counted-root
 * reconstruction.
 */
export const prepareWithdrawnReferenceInputV1 = async ({
  header,
  blockTxs,
  withdrawals,
  accusedTxId,
}: {
  readonly header: HeaderV1;
  readonly blockTxs: readonly NodeTransactionPayload[];
  readonly withdrawals: readonly WithdrawalEvent[];
  readonly accusedTxId?: string;
}): Promise<PreparedWithdrawnReferenceInputV1> => {
  const decoded = blockTxs.map(decodeBlockTxV1);
  const transactionEntries: KeyValuePhasEntry[] = decoded.map((tx) => ({
    key: Buffer.from(tx.txId, "hex"),
    value: Buffer.from(tx.nativeTxCompactCbor, "hex"),
  }));
  const transactionsRoot = await buildCountedRoot(
    ROOT_DOMAINS.transactionsV1,
    transactionEntries,
  );
  if (
    transactionsRoot.root !== header.transactionsRoot ||
    transactionsRoot.count !== header.l2TransactionCount
  ) {
    throw new WithdrawnReferenceInputPreparationErrorV1(
      "transactions-root-mismatch",
      `reconstructed root/count ${transactionsRoot.root}/${transactionsRoot.count.toString()} do not match header ${header.transactionsRoot}/${header.l2TransactionCount.toString()}.`,
    );
  }

  const withdrawalEntries: KeyValuePhasEntry[] = withdrawals.map((event) => ({
    key: Buffer.from(committedWithdrawalKeyBytesV1(event.id), "hex"),
    value: Buffer.from(committedWithdrawalValueBytesV1(event.info), "hex"),
  }));
  const withdrawalsRoot = await buildCountedRoot(
    ROOT_DOMAINS.withdrawals,
    withdrawalEntries,
  );
  if (
    withdrawalsRoot.root !== header.withdrawalsRoot ||
    withdrawalsRoot.count !== header.withdrawalCount
  ) {
    throw new WithdrawnReferenceInputPreparationErrorV1(
      "withdrawals-root-mismatch",
      `reconstructed root/count ${withdrawalsRoot.root}/${withdrawalsRoot.count.toString()} do not match header ${header.withdrawalsRoot}/${header.withdrawalCount.toString()}.`,
    );
  }

  const selectedTxId =
    accusedTxId === undefined
      ? undefined
      : exactHex(accusedTxId, 32, "accusedTxId");
  const candidates =
    selectedTxId === undefined
      ? decoded
      : decoded.filter((tx) => tx.txId === selectedTxId);
  if (candidates.length === 0) {
    throw new WithdrawnReferenceInputPreparationErrorV1(
      "no-matching-reference-input",
      "the accused transaction is not committed by this block.",
    );
  }

  let invalidMatch = false;
  let selected:
    | {
        readonly tx: DecodedBlockTxV1;
        readonly inputIndex: number;
        readonly withdrawal: WithdrawalEvent;
      }
    | undefined;
  for (const tx of candidates) {
    for (const [inputIndex, input] of tx.referenceInputs.entries()) {
      for (const withdrawal of withdrawals) {
        if (!sameOutRef(input, withdrawal.info.body.l2_outref)) {
          continue;
        }
        if (withdrawal.info.validity !== "WithdrawalIsValid") {
          invalidMatch = true;
          continue;
        }
        selected = { tx, inputIndex, withdrawal };
        break;
      }
      if (selected !== undefined) break;
    }
    if (selected !== undefined) break;
  }
  if (selected === undefined) {
    if (invalidMatch) {
      throw new WithdrawnReferenceInputPreparationErrorV1(
        "withdrawal-not-valid",
        "the matching withdrawal is invalid and consumed no L2 input.",
      );
    }
    throw new WithdrawnReferenceInputPreparationErrorV1(
      "no-matching-reference-input",
      "no valid same-block withdrawal spends any committed reference input.",
    );
  }

  const txProof = await keyValuePhasProof(
    { ...transactionsRoot, root: transactionsRoot.phasRoot },
    Buffer.from(selected.tx.txId, "hex"),
    Buffer.from(selected.tx.nativeTxCompactCbor, "hex"),
  );
  const withdrawalKey = Buffer.from(
    committedWithdrawalKeyBytesV1(selected.withdrawal.id),
    "hex",
  );
  const withdrawalValue = Buffer.from(
    committedWithdrawalValueBytesV1(selected.withdrawal.info),
    "hex",
  );
  const withdrawalProof = await keyValuePhasProof(
    { ...withdrawalsRoot, root: withdrawalsRoot.phasRoot },
    withdrawalKey,
    withdrawalValue,
  );
  const withdrawalMembership: WithdrawalSourceMembershipProof = {
    domain: ROOT_DOMAINS.withdrawals,
    root: withdrawalsRoot.root,
    phas_root: withdrawalsRoot.phasRoot,
    count: withdrawalsRoot.count,
    key: selected.withdrawal.id,
    value: selected.withdrawal.info,
    proof: withdrawalProof,
  };
  verifyWithdrawnReferenceInputMembershipV1(withdrawalMembership);

  const countedTransactionsRoot = await Effect.runPromise(
    commitCountedRootProgram({
      domain: ROOT_DOMAINS.transactionsV1,
      phasRoot: transactionsRoot.phasRoot,
      count: transactionsRoot.count,
    }),
  );
  if (countedTransactionsRoot !== header.transactionsRoot) {
    throw new WithdrawnReferenceInputPreparationErrorV1(
      "transactions-root-mismatch",
      "transactions counted-root cross-check failed after proof construction.",
    );
  }

  return {
    header,
    txInclusion: {
      nativeTxId: selected.tx.txId,
      nativeTx: selected.tx.nativeTx,
      nativeTxCompactCbor: selected.tx.nativeTxCompactCbor,
      transactionsPhasRoot: transactionsRoot.phasRoot,
      txMembershipProof: txProof,
      txMembershipProofCbor: Data.to(txProof, Proof),
    },
    referenceInputs: selected.tx.referenceInputs,
    referenceInputItemCbors: selected.tx.referenceInputs.map((input) =>
      Buffer.from(encodeMidgardTxInputCanonicalV1(input)).toString("hex"),
    ),
    badReferenceInputIndex: selected.inputIndex,
    missingReferenceInput: selected.tx.referenceInputs[selected.inputIndex]!,
    withdrawal: selected.withdrawal,
    withdrawalMembership,
  };
};
