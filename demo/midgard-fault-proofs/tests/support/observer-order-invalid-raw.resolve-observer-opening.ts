import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeMidgardNativeTxId,
  decodeMidgardFieldPreimage,
  deriveMidgardNativeTxWitnessSetCompact,
  encodeMidgardFieldPreimage,
  encodeMidgardNativeTxCompact,
  encodeMidgardNativeTxWitnessSetCompact,
  materializeMidgardNativeTxFromCanonical,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core";
import { deriveMidgardForcedTxProofSource } from "@al-ft/midgard-core/codec/forced";
import { materializeMidgardForcedTxFromCanonical } from "@al-ft/midgard-core/codec/forced";
import {
  type FieldOpening,
  ForcedInclusionTxV1Schema,
  OutputReference,
  Proof,
  type RejectionReason,
  ROOT_DOMAINS,
} from "@al-ft/midgard-sdk";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import {
  faultProofFieldOpening,
  planFaultProofFieldOpening,
  resolveFaultProofFieldCarriagePublications,
  resolveFaultProofFieldPreimageCertificate,
} from "../../src/field-opening.js";
import type { ObserverOrderInvalidContracts } from "../../src/observer-order-invalid/contracts.js";
import { type ObserverOrderInvalidEvidence } from "../../src/observer-order-invalid/family.js";
import type { ResolvedProverSigner } from "../../src/runtime.js";
import {
  nativeTxFromCoreCompact,
  type SubmitStep01TxInclusion,
} from "../../src/step-support.js";
import { buildCountedRoot } from "../../src/transition-trace/phas.js";
import { l2TransactionSourceCbor, makeNativeTx } from "./emulator/native-tx.js";

export const FAMILY = "observer-order-invalid";

// ---------------------------------------------------------------------------
// Shapes
// ---------------------------------------------------------------------------

/** A canonical 28-byte observer hash whose order is its big-endian ordinal. */
export const observerAt = (ordinal: number): Buffer => {
  const value = Buffer.alloc(28);
  value.writeUInt32BE(ordinal, 24);
  return value;
};

/** `count` strictly ascending observers, `0..count-1`. */
export const ascendingObservers = (count: number): Buffer[] =>
  Array.from({ length: count }, (_, ordinal) => observerAt(ordinal));

export type ObserverFieldShape = Readonly<{
  label: string;
  observers: readonly Buffer[];
  observerCount: number;
  fee: bigint;
  nativeTx: MidgardNativeTxFull;
  fieldPreimage: Uint8Array;
}>;

/** One native transaction whose field 3 holds exactly `observers`. */
export const observerFieldShape = ({
  label,
  observers,
  fee = 7n,
}: {
  readonly label: string;
  readonly observers: readonly Buffer[];
  readonly fee?: bigint;
}): ObserverFieldShape => {
  const fieldPreimage = encodeMidgardFieldPreimage(observers);
  const base = makeNativeTx({ spendInputCbors: [], fee });
  const nativeTx = materializeMidgardNativeTxFromCanonical({
    version: base.version,
    validity: base.validity,
    body: { ...base.body, requiredObserversPreimageCbor: fieldPreimage },
    witnessSet: base.witnessSet,
  });
  return Object.freeze({
    label,
    observers: Object.freeze([...observers]),
    observerCount: observers.length,
    fee,
    nativeTx,
    fieldPreimage,
  });
};

export const transactionIdOf = (shape: ObserverFieldShape): string =>
  computeMidgardNativeTxId(shape.nativeTx).toString("hex");

export const compactCborHex = (nativeTx: MidgardNativeTxFull): string =>
  encodeMidgardNativeTxCompact(nativeTx.compact).toString("hex");

export const witnessSetCompactCborHex = (
  nativeTx: MidgardNativeTxFull,
): string =>
  encodeMidgardNativeTxWitnessSetCompact(
    deriveMidgardNativeTxWitnessSetCompact(nativeTx.witnessSet),
  ).toString("hex");

// ---------------------------------------------------------------------------
// Accepted block: one transactions root over every committed transaction
// ---------------------------------------------------------------------------

export const buildAcceptedObserverInclusions = async (
  transactions: readonly MidgardNativeTxFull[],
): Promise<{
  readonly transactionsRoot: string;
  readonly l2TransactionCount: bigint;
  readonly inclusions: readonly SubmitStep01TxInclusion[];
}> => {
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  const ids = transactions.map((nativeTx) =>
    computeMidgardNativeTxId(nativeTx).toString("hex"),
  );
  for (const [index, nativeTx] of transactions.entries()) {
    await trie.insert(
      Buffer.from(ids[index]!, "hex"),
      Buffer.from(l2TransactionSourceCbor(nativeTx), "hex"),
    );
  }
  const transactionsRoot = Buffer.from(trie.hash).toString("hex");
  const inclusions: SubmitStep01TxInclusion[] = [];
  for (const [index, nativeTx] of transactions.entries()) {
    const proof = await trie.prove(Buffer.from(ids[index]!, "hex"));
    inclusions.push({
      nativeTxId: ids[index]!,
      nativeTx: nativeTxFromCoreCompact(nativeTx.compact),
      nativeTxCompactCbor: compactCborHex(nativeTx),
      l2TransactionSourceCbor: l2TransactionSourceCbor(nativeTx),
      transactionsPhasRoot: transactionsRoot,
      txMembershipProof: Data.from(proof.toCBOR().toString("hex"), Proof),
      txMembershipProofCbor: proof.toCBOR().toString("hex"),
    });
  }
  return {
    transactionsRoot,
    l2TransactionCount: BigInt(transactions.length),
    inclusions,
  };
};

// ---------------------------------------------------------------------------
// Forced leaf: one rejected forced transaction under a counted root
// ---------------------------------------------------------------------------

export type ForcedObserverLeaf = Awaited<
  ReturnType<typeof buildForcedObserverLeaf>
>;

/**
 * The forced-transactions root carries exactly one rejected leaf typed with
 * `rejectionReason`; the caller binds `root` into the header it submits.
 */
export const buildForcedObserverLeaf = async ({
  shape,
  sourceKey,
  rejectionReason,
}: {
  readonly shape: ObserverFieldShape;
  readonly sourceKey: { transactionId: string; outputIndex: bigint };
  readonly rejectionReason: RejectionReason;
}) => {
  const invalid = materializeMidgardForcedTxFromCanonical(shape.nativeTx);
  const transactionId = computeMidgardNativeTxId(invalid).toString("hex");
  const proofSource = deriveMidgardForcedTxProofSource(invalid);
  const transaction = {
    tx_id: transactionId,
    submitted_source: {
      compact_cbor: proofSource.compactCbor.toString("hex"),
      witness_set_compact_cbor:
        proofSource.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        proofSource.fieldPreimageLengthsCbor.toString("hex"),
    },
    verdict: { ForcedTxInvalid: { reason: rejectionReason } },
  } as const;
  const keyBytes = Buffer.from(Data.to(sourceKey, OutputReference), "hex");
  const valueBytes = Buffer.from(
    Data.to(transaction as never, ForcedInclusionTxV1Schema as never),
    "hex",
  );
  const root = await buildCountedRoot(ROOT_DOMAINS.forcedTransactionsV1, [
    { key: keyBytes, value: valueBytes },
  ]);
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(keyBytes, valueBytes);
  const proof = await trie.prove(keyBytes);
  const membership = {
    domain: root.domain,
    root: root.root,
    phas_root: root.phasRoot,
    count: root.count,
    key: sourceKey,
    value: transaction,
    proof: Data.from(proof.toCBOR().toString("hex"), Proof),
  };
  return {
    transactionId,
    transaction,
    proofSource,
    compactCborHex: proofSource.compactCbor.toString("hex"),
    witnessSetCompactCborHex: proofSource.witnessSetCompactCbor.toString("hex"),
    root,
    membership,
  };
};

// ---------------------------------------------------------------------------
// Field opening shared by the raw step-02/03 submitters
// ---------------------------------------------------------------------------

export type OpeningMutation = (
  opening: FieldOpening,
  referenceInputs: readonly UTxO[],
) => FieldOpening;

export const resolveObserverOpening = async ({
  lucid,
  contracts,
  signer,
  evidence,
  nativeTxCompactCbor,
  stepReference,
  extraReferenceInputs,
  mutateOpening,
  label,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: ObserverOrderInvalidContracts;
  readonly signer: ResolvedProverSigner;
  readonly evidence: ObserverOrderInvalidEvidence;
  readonly nativeTxCompactCbor: string;
  readonly stepReference: UTxO;
  readonly extraReferenceInputs: readonly UTxO[];
  readonly mutateOpening: OpeningMutation;
  readonly label: string;
}) => {
  const planned = planFaultProofFieldOpening({
    anchorSourceKind: evidence.subject.source_kind === 1n ? 1n : 0n,
    fieldIndex: 3,
    anchorTxId: evidence.subject.transaction_id,
    nativeTxCompactCbor,
    itemCbors: decodeMidgardFieldPreimage(
      Buffer.from(evidence.fieldPreimageHex, "hex"),
    ),
    owner: signer.paymentKeyHash,
    publish: true,
    label,
  });
  const carriageUtxos = await resolveFaultProofFieldCarriagePublications({
    lucid,
    publisherAddress: signer.address,
    planned,
  });
  if (carriageUtxos === undefined)
    throw new Error(`${FAMILY} raw: field carriage is not published`);
  const certificateUtxo =
    planned.plan.tier === "Certified"
      ? await resolveFaultProofFieldPreimageCertificate({
          lucid,
          network: lucid.config().network!,
          planned,
          certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
        })
      : undefined;
  if (planned.plan.tier === "Certified" && certificateUtxo === undefined)
    throw new Error(`${FAMILY} raw: field certificate is not published`);
  const referenceInputs = [
    ...carriageUtxos,
    stepReference,
    ...(certificateUtxo === undefined ? [] : [certificateUtxo]),
    ...extraReferenceInputs,
  ];
  const opening = mutateOpening(
    faultProofFieldOpening({
      planned,
      referenceInputs,
      certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
      label,
    }),
    referenceInputs,
  );
  return {
    opening,
    carriageUtxos,
    extraReferenceInputs:
      certificateUtxo === undefined ? [] : [certificateUtxo],
  };
};
