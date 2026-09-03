/**
 * Direct retained-DA preparer for an accepted transaction whose committed
 * field-length vector disagrees with the retained canonical field preimage.
 *
 * Whole-block reconstruction must reject this payload: that rejection is the
 * defect being proved.  Consequently this module authenticates the raw source
 * leaf against the L1 header's counted transactions root, then binds the
 * retained canonical transaction to the leaf's compact and witness-set
 * identities while deliberately treating the leaf's length vector as the
 * disputed value.
 */
import {
  decodeMidgardNativeTxWitnessSetCompact,
  deriveMidgardNativeTxFaultEvidenceMaterial,
} from "@al-ft/midgard-core";
import {
  type CommittedFieldClaim,
  type FieldCarriage,
  isMidgardWitnessSetField,
  type L2TransactionSource,
  L2TransactionSourceSchema,
  Proof,
  type Proof as SDKProof,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  buildTrieView,
  requireProof,
  requireTransactionsRootMatch,
} from "../prepare-double-spend.js";
import { nativeTxFromCoreCompact } from "../submit-step-01.js";
import {
  type PreparedFieldPreimageLengthWorkflow,
  prepareFieldPreimageLengthWorkflow,
} from "./workflow-v1.js";

export type PreparedAcceptedFieldPreimageLengthMismatch = Readonly<{
  prepared: PreparedFieldPreimageLengthWorkflow;
  claim: CommittedFieldClaim;
  inclusion: Readonly<{
    nativeTxId: string;
    nativeTx: ReturnType<typeof nativeTxFromCoreCompact>;
    nativeTxCompactCbor: string;
    l2TransactionSourceCbor: string;
    transactionsPhasRoot: string;
    txMembershipProof: SDKProof;
    txMembershipProofCbor: string;
  }>;
}>;

export type PreparedAcceptedFieldPreimageLengthMismatchDeferred = Readonly<{
  prepared: PreparedFieldPreimageLengthWorkflow;
  claim: null;
  inclusion: PreparedAcceptedFieldPreimageLengthMismatch["inclusion"];
}>;

export const fieldPreimageLengthCommittedClaim = ({
  fieldIndex,
  witnessSetCompactCbor,
  carriage,
}: {
  readonly fieldIndex: number;
  readonly witnessSetCompactCbor: Uint8Array;
  readonly carriage: FieldCarriage;
}): CommittedFieldClaim => {
  const witness = decodeMidgardNativeTxWitnessSetCompact(witnessSetCompactCbor);
  return isMidgardWitnessSetField(fieldIndex)
    ? {
        WitnessFieldClaim: {
          field_index: BigInt(fieldIndex),
          witness_set: {
            addr_tx_wits_hash: witness.addrTxWitsHash.toString("hex"),
            script_tx_wits_hash: witness.scriptTxWitsHash.toString("hex"),
            redeemer_tx_wits_hash: witness.redeemerTxWitsHash.toString("hex"),
          },
          carriage,
        },
      }
    : { BodyFieldClaim: { field_index: BigInt(fieldIndex), carriage } };
};

const exactHex = (value: string, label: string): string => {
  const normalized = value.toLowerCase();
  if (!/^(?:[0-9a-f]{2})+$/u.test(normalized)) {
    throw new Error(`${label} must be non-empty even-length hexadecimal`);
  }
  return normalized;
};

/**
 * Authenticates one directly adjudicable accepted defect. `entries` are the
 * exact raw retained-DA transaction leaves; `canonicalTransactionCbor` is the
 * matching retained transaction preimage, not a reconstructed-block verdict.
 */
type AcceptedInput = {
  readonly headerHash: string;
  readonly committedTransactionsRoot: string;
  readonly l2TransactionCount: bigint;
  readonly entries: readonly (readonly [string, string])[];
  readonly transactionId: string;
  readonly canonicalTransactionCbor: Uint8Array;
  readonly fieldIndex: number;
  readonly carriage?: FieldCarriage;
  readonly deferNonInlineClaim?: boolean;
};

export function prepareAcceptedFieldPreimageLengthMismatch(
  input: AcceptedInput & { readonly deferNonInlineClaim: true },
): Promise<PreparedAcceptedFieldPreimageLengthMismatchDeferred>;
export function prepareAcceptedFieldPreimageLengthMismatch(
  input: AcceptedInput & { readonly deferNonInlineClaim?: false },
): Promise<PreparedAcceptedFieldPreimageLengthMismatch>;
export async function prepareAcceptedFieldPreimageLengthMismatch({
  headerHash,
  committedTransactionsRoot,
  l2TransactionCount,
  entries,
  transactionId,
  canonicalTransactionCbor,
  fieldIndex,
  carriage: suppliedCarriage,
  deferNonInlineClaim = false,
}: AcceptedInput): Promise<
  | PreparedAcceptedFieldPreimageLengthMismatch
  | PreparedAcceptedFieldPreimageLengthMismatchDeferred
> {
  const normalizedId = transactionId.toLowerCase();
  if (!/^[0-9a-f]{64}$/u.test(normalizedId)) {
    throw new Error(
      "accepted defect transaction id must be 32-byte hexadecimal",
    );
  }
  if (!Number.isInteger(fieldIndex) || fieldIndex < 0 || fieldIndex >= 9) {
    throw new Error("accepted defect field index is outside 0..8");
  }
  if (BigInt(entries.length) !== l2TransactionCount) {
    throw new Error(
      "retained transaction cardinality differs from the L1 header",
    );
  }
  const phasEntries = entries.map(([key, value], index) => ({
    key: Buffer.from(
      exactHex(key, `transactions[${index.toString()}].key`),
      "hex",
    ),
    value: Buffer.from(
      exactHex(value, `transactions[${index.toString()}].value`),
      "hex",
    ),
  }));
  const trie = await buildTrieView(phasEntries);
  await requireTransactionsRootMatch({
    sourceRoot: trie.root,
    expectedTransactionsRoot: committedTransactionsRoot.toLowerCase(),
    count: l2TransactionCount,
  });
  const selected = entries.find(([key]) => key.toLowerCase() === normalizedId);
  if (selected === undefined) {
    throw new Error(
      "accepted defect transaction is not committed by the header",
    );
  }
  const sourceCbor = exactHex(selected[1], "accepted transaction source");
  let source: L2TransactionSource;
  try {
    source = Data.from(
      sourceCbor,
      L2TransactionSourceSchema as never,
    ) as L2TransactionSource;
  } catch (cause) {
    throw new Error(
      `accepted transaction source does not decode: ${String(cause)}`,
    );
  }
  if (
    Data.to(source as never, L2TransactionSourceSchema as never) !== sourceCbor
  ) {
    throw new Error("accepted transaction source is not canonical Data");
  }
  const material = deriveMidgardNativeTxFaultEvidenceMaterial(
    canonicalTransactionCbor,
  );
  if (
    source.tx_id !== normalizedId ||
    material.transactionId.toString("hex") !== normalizedId ||
    source.source.compact_cbor !==
      material.proofSource.compactCbor.toString("hex") ||
    source.source.witness_set_compact_cbor !==
      material.proofSource.witnessSetCompactCbor.toString("hex")
  ) {
    throw new Error(
      "retained canonical transaction differs from the committed compact or witness-set identity",
    );
  }
  const preimage = material.fieldPreimages[fieldIndex];
  if (preimage === undefined)
    throw new Error("retained field preimage is absent");
  const prepared = prepareFieldPreimageLengthWorkflow({
    headerHash,
    transactionId: normalizedId,
    direction: "wrongfulAcceptance",
    fieldIndex,
    fieldPreimageLengthsCbor: Buffer.from(
      source.source.field_preimage_lengths_cbor,
      "hex",
    ),
    fieldPreimage: preimage,
  });
  if (
    prepared.carriage !== "Inline" &&
    suppliedCarriage === undefined &&
    !deferNonInlineClaim
  ) {
    throw new Error(
      "direct accepted preparer requires a separately published RawUtxo/Certified carriage",
    );
  }
  const carriage: FieldCarriage | undefined =
    suppliedCarriage ??
    (prepared.carriage === "Inline"
      ? { Inline: { preimage: preimage.toString("hex") } }
      : undefined);
  if (carriage !== undefined && !(prepared.carriage in carriage)) {
    throw new Error(
      `supplied ${Object.keys(carriage)[0] ?? "unknown"} carriage differs from deterministic ${prepared.carriage} tier`,
    );
  }
  const claim =
    carriage === undefined
      ? null
      : fieldPreimageLengthCommittedClaim({
          fieldIndex,
          witnessSetCompactCbor: material.proofSource.witnessSetCompactCbor,
          carriage,
        });
  const key = Buffer.from(normalizedId, "hex");
  const proof = requireProof(trie, key, "accepted defect transaction");
  return Object.freeze({
    prepared,
    claim,
    inclusion: Object.freeze({
      nativeTxId: normalizedId,
      nativeTx: nativeTxFromCoreCompact(material.compact),
      nativeTxCompactCbor: material.proofSource.compactCbor.toString("hex"),
      l2TransactionSourceCbor: sourceCbor,
      transactionsPhasRoot: trie.root,
      txMembershipProof: Data.from(proof, Proof),
      txMembershipProofCbor: proof,
    }),
  });
}
