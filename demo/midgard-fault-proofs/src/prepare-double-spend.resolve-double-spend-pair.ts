import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeMidgardNativeTxId,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardSpendInputItem,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeMidgardNativeTxCanonical,
  encodeMidgardNativeTxCompact,
  formatUnknownError,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxFull,
  type MidgardTxInput,
} from "@al-ft/midgard-core";
import {
  EMPTY_MERKLE_TREE_ROOT,
  type OutputReference as OutputReferenceData,
} from "@al-ft/midgard-sdk";

import { parseHex } from "./json-file.js";
import {
  bytesHex,
  type DecodedTransactionMaterial,
  deriveL2TransactionSourceCbor,
  type NodeTransactionPayload,
  transactionInputCbor,
  type TrieView,
} from "./prepare-double-spend.encode-l2-transaction-source-value.js";
import { nativeTxFromCoreCompact } from "./step-support.js";

const sampleNativeTx = (
  inputs: readonly Buffer[],
  fee: bigint,
): MidgardNativeTxFull => {
  const body = {
    spendInputsPreimageCbor: encodeCbor(inputs),
    referenceInputsPreimageCbor: Buffer.from(EMPTY_CBOR_LIST),
    outputsPreimageCbor: Buffer.from(EMPTY_CBOR_LIST),
    fee,
    validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
    validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
    requiredObserversPreimageCbor: Buffer.from(EMPTY_CBOR_LIST),
    requiredSignersPreimageCbor: Buffer.from(EMPTY_CBOR_LIST),
    mintPreimageCbor: Buffer.from(EMPTY_CBOR_LIST),
    scriptIntegrityHash: Buffer.from(EMPTY_NULL_ROOT),
    auxiliaryDataHash: Buffer.from(EMPTY_NULL_ROOT),
    networkId: 0n,
  };
  const witnessSet = {
    addrTxWitsPreimageCbor: Buffer.from(EMPTY_CBOR_LIST),
    scriptTxWitsPreimageCbor: Buffer.from(EMPTY_CBOR_LIST),
    redeemerTxWitsPreimageCbor: Buffer.from(EMPTY_CBOR_LIST),
  };
  return materializeMidgardNativeTxFromCanonical({
    version: MIDGARD_NATIVE_TX_VERSION,
    validity: "TxIsValid",
    body,
    witnessSet,
  });
};

const payloadFromNativeTx = (
  tx: MidgardNativeTxFull,
): NodeTransactionPayload => ({
  nodeTxId: computeMidgardNativeTxId(tx).toString("hex"),
  txCbor: encodeMidgardNativeTxCanonical(tx).toString("hex"),
});

export const makeSampleDoubleSpendTransactions =
  (): readonly NodeTransactionPayload[] => {
    const sharedInput = transactionInputCbor("11".repeat(32), 7n);
    const tx1 = sampleNativeTx(
      [sharedInput, transactionInputCbor("22".repeat(32), 0n)],
      1n,
    );
    const tx2 = sampleNativeTx(
      [transactionInputCbor("33".repeat(32), 0n), sharedInput],
      2n,
    );
    const tx3 = sampleNativeTx([transactionInputCbor("44".repeat(32), 0n)], 3n);
    return [
      payloadFromNativeTx(tx1),
      payloadFromNativeTx(tx2),
      payloadFromNativeTx(tx3),
    ];
  };

/**
 * Decodes the §5.3 field-0/1 item form — `82 ‖ 58 20 tx_id(32) ‖ 19
 * index_be16`, a fixed 38 bytes — the same bytes on-chain
 * `decode_midgard_tx_input_cbor` accepts. CML's minimal-index
 * `TransactionInput` CBOR is deliberately rejected.
 */
const outputReferenceFromNativeInput = (
  bytes: Uint8Array,
  label: string,
): OutputReferenceData => {
  let input: MidgardTxInput;
  try {
    input = decodeMidgardSpendInputItem(bytes);
  } catch (cause) {
    throw new Error(
      `${label} is not a valid Midgard §5.3 TxOutRef CBOR: ${formatUnknownError(cause)}`,
    );
  }
  return {
    transactionId: Buffer.from(input.txId).toString("hex"),
    outputIndex: BigInt(input.outputIndex),
  };
};

const decodeNativeInputPreimage = (
  preimageCbor: Uint8Array,
  label: string,
): readonly OutputReferenceData[] =>
  decodeMidgardNativeByteListPreimage(preimageCbor, label).map(
    (bytes: Buffer, index: number) =>
      outputReferenceFromNativeInput(bytes, `${label}[${index.toString()}]`),
  );

const decodeNativeInputCbors = (
  preimageCbor: Uint8Array,
  label: string,
): readonly string[] =>
  decodeMidgardNativeByteListPreimage(preimageCbor, label).map(bytesHex);

export const decodeTransactionMaterial = async (
  payload: NodeTransactionPayload,
): Promise<DecodedTransactionMaterial> => {
  const nodeTxId = parseHex(payload.nodeTxId, "nodeTxId", 32);
  const txCbor = parseHex(payload.txCbor, `tx ${nodeTxId} CBOR`);
  let nativeTx: MidgardNativeTxFull;
  try {
    nativeTx = decodeMidgardNativeTxFullFromCanonicalCbor(
      Buffer.from(txCbor, "hex"),
    );
  } catch (cause) {
    throw new Error(
      `Failed to decode native Midgard tx ${nodeTxId}: ${formatUnknownError(cause)}`,
    );
  }
  const computedNodeTxId = computeMidgardNativeTxId(nativeTx).toString("hex");
  if (computedNodeTxId !== nodeTxId) {
    throw new Error(
      `Node tx id mismatch: listed=${nodeTxId}, computed=${computedNodeTxId}.`,
    );
  }
  const inputs = decodeNativeInputPreimage(
    nativeTx.body.spendInputsPreimageCbor,
    `tx ${nodeTxId} spend_inputs`,
  );
  const referenceInputs = decodeNativeInputPreimage(
    nativeTx.body.referenceInputsPreimageCbor,
    `tx ${nodeTxId} reference_inputs`,
  );
  const spendInputCbors = decodeNativeInputCbors(
    nativeTx.body.spendInputsPreimageCbor,
    `tx ${nodeTxId} spend_inputs`,
  );
  const l2TransactionSourceCbor = deriveL2TransactionSourceCbor(
    Buffer.from(txCbor, "hex"),
  );
  if (
    payload.l2TransactionSourceCbor !== undefined &&
    parseHex(
      payload.l2TransactionSourceCbor,
      `tx ${nodeTxId} transaction source CBOR`,
    ) !== l2TransactionSourceCbor
  ) {
    throw new Error(
      `Transaction source mismatch for ${nodeTxId}: retained DA value does not match the exact native proof-source derivation.`,
    );
  }
  return {
    nodeTxId,
    txCbor,
    nativeTx,
    nativeTxCompact: nativeTxFromCoreCompact(nativeTx.compact),
    inputs,
    referenceInputs,
    spendInputCbors,
    nativeCompactCbor: encodeMidgardNativeTxCompact(nativeTx.compact).toString(
      "hex",
    ),
    l2TransactionSourceCbor,
  };
};

const outputReferenceKey = (outRef: OutputReferenceData): string =>
  `${outRef.transactionId}#${outRef.outputIndex.toString()}`;

export const resolveDoubleSpendPair = ({
  transactions,
  tx1Id,
  tx2Id,
}: {
  readonly transactions: readonly DecodedTransactionMaterial[];
  readonly tx1Id?: string;
  readonly tx2Id?: string;
}): {
  readonly tx1: DecodedTransactionMaterial;
  readonly tx2: DecodedTransactionMaterial;
  readonly doubleSpentInput: OutputReferenceData;
  readonly tx1DoubleSpentInputIndex: number;
  readonly tx2DoubleSpentInputIndex: number;
} => {
  if ((tx1Id === undefined) !== (tx2Id === undefined)) {
    throw new Error("--tx1-id and --tx2-id must be provided together.");
  }

  if (tx1Id !== undefined && tx2Id !== undefined) {
    const normalizedTx1 = parseHex(tx1Id, "--tx1-id", 32);
    const normalizedTx2 = parseHex(tx2Id, "--tx2-id", 32);
    if (normalizedTx1 === normalizedTx2) {
      throw new Error(
        "--tx1-id and --tx2-id must identify distinct transactions.",
      );
    }
    const tx1 = transactions.find((tx) => tx.nodeTxId === normalizedTx1);
    const tx2 = transactions.find((tx) => tx.nodeTxId === normalizedTx2);
    if (tx1 === undefined || tx2 === undefined) {
      throw new Error(
        "Requested --tx1-id/--tx2-id was not found in the block.",
      );
    }
    for (const [tx1InputIndex, input] of tx1.inputs.entries()) {
      const tx2InputIndex = tx2.inputs.findIndex(
        (candidate) =>
          outputReferenceKey(candidate) === outputReferenceKey(input),
      );
      if (tx2InputIndex >= 0) {
        return {
          tx1,
          tx2,
          doubleSpentInput: input,
          tx1DoubleSpentInputIndex: tx1InputIndex,
          tx2DoubleSpentInputIndex: tx2InputIndex,
        };
      }
    }
    throw new Error("Requested transactions do not spend the same input.");
  }

  const firstSpendByInput = new Map<
    string,
    {
      readonly tx: DecodedTransactionMaterial;
      readonly input: OutputReferenceData;
      readonly inputIndex: number;
    }
  >();
  for (const tx of transactions) {
    for (const [inputIndex, input] of tx.inputs.entries()) {
      const key = outputReferenceKey(input);
      const first = firstSpendByInput.get(key);
      if (first === undefined) {
        firstSpendByInput.set(key, { tx, input, inputIndex });
        continue;
      }
      if (first.tx.nodeTxId === tx.nodeTxId) {
        continue;
      }
      return {
        tx1: first.tx,
        tx2: tx,
        doubleSpentInput: first.input,
        tx1DoubleSpentInputIndex: first.inputIndex,
        tx2DoubleSpentInputIndex: inputIndex,
      };
    }
  }
  throw new Error("No double spend found in the selected block.");
};

export const buildTrieView = async (
  items: readonly {
    readonly key: Buffer;
    readonly value: Buffer;
  }[],
): Promise<TrieView> => {
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  for (const item of items) {
    await trie.insert(item.key, item.value);
  }
  const proofEntries = await Promise.all(
    items.map(async (item) => {
      const proof = await trie.prove(item.key);
      return [
        item.key.toString("hex"),
        proof.toCBOR().toString("hex"),
      ] as const;
    }),
  );
  return {
    root:
      trie.hash == null
        ? EMPTY_MERKLE_TREE_ROOT
        : Buffer.from(trie.hash).toString("hex"),
    proofCborByKeyHex: new Map(proofEntries),
  };
};

export const transactionSourceTrieItem = (tx: DecodedTransactionMaterial) => ({
  key: Buffer.from(tx.nodeTxId, "hex"),
  value: Buffer.from(tx.l2TransactionSourceCbor, "hex"),
});

export const requireProof = (
  trie: TrieView,
  key: Buffer,
  label: string,
): string => {
  const proof = trie.proofCborByKeyHex.get(key.toString("hex"));
  if (proof === undefined) {
    throw new Error(`Internal error: missing ${label} membership proof.`);
  }
  return proof;
};
