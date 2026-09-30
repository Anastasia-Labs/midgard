import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeMidgardNativeTxId,
  deriveMidgardNativeTxWitnessSetCompact,
  encodeMidgardFieldPreimage,
  encodeMidgardForcedTxCompact,
  encodeMidgardNativeTxCompact,
  encodeMidgardNativeTxWitnessSetCompact,
  materializeMidgardNativeTxFromCanonical,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core";
import { deriveMidgardForcedTxProofSource } from "@al-ft/midgard-core/codec/forced";
import { materializeMidgardForcedTxFromCanonical } from "@al-ft/midgard-core/codec/forced";
import {
  ForcedInclusionTxV1Schema,
  OutputReference,
  Proof,
  type RejectionReason,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
  ROOT_DOMAINS,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  requireLinearFaultReferenceScript,
  requireLinearFaultThreadUtxo,
} from "../../src/linear-fault-family.js";
import { submitLinearFaultContinue } from "../../src/linear-fault-submit.js";
import type { ObserversForbiddenContracts } from "../../src/observers-forbidden-on-untagged-network/contracts.js";
import {
  classifyObserversForbiddenFinding,
  type ObserversForbiddenFinding,
} from "../../src/observers-forbidden-on-untagged-network/family.js";
import {
  ObserversForbiddenStep01RedeemerSchema,
  ObserversForbiddenStep02DatumSchema,
} from "../../src/observers-forbidden-on-untagged-network/schemas.js";
import type { ResolvedProverSigner } from "../../src/runtime.js";
import {
  nativeTxFromCoreCompact,
  requireInitialStepDatum,
  type SubmitStep01TxInclusion,
} from "../../src/step-support.js";
import { buildCountedRoot } from "../../src/transition-trace/phas.js";
import { computationThreadOutputPredicate } from "../../src/tx-layout.js";
import { l2TransactionSourceCbor, makeNativeTx } from "./emulator/native-tx.js";

export const FAMILY = "observers-forbidden-on-untagged-network";

// ---------------------------------------------------------------------------
// Shapes
// ---------------------------------------------------------------------------

/** Deterministic 28-byte observer hashes, strictly ascending by first byte. */
export const observerHashes = (count: number): Buffer[] =>
  Array.from({ length: count }, (_, index) => Buffer.alloc(28, index + 1));

export type ObserverShape = Readonly<{
  label: string;
  observerCount: number;
  networkId: 0 | 1 | 255;
  /** 32 bytes of lowercase hex; the zero hash is the canonical absent value. */
  scriptIntegrityHash: string;
  fee: bigint;
  nativeTx: MidgardNativeTxFull;
  fieldPreimage: Uint8Array;
}>;

export const observerShape = ({
  label,
  observerCount,
  networkId,
  scriptIntegrityHash,
  fee = 7n,
}: {
  readonly label: string;
  readonly observerCount: number;
  readonly networkId: 0 | 1 | 255;
  readonly scriptIntegrityHash: string;
  readonly fee?: bigint;
}): ObserverShape => {
  const fieldPreimage = encodeMidgardFieldPreimage(
    observerHashes(observerCount),
  );
  const base = makeNativeTx({ spendInputCbors: [], fee });
  const nativeTx = materializeMidgardNativeTxFromCanonical({
    version: base.version,
    validity: base.validity,
    body: {
      ...base.body,
      requiredObserversPreimageCbor: fieldPreimage,
      networkId: BigInt(networkId),
      scriptIntegrityHash: Buffer.from(scriptIntegrityHash, "hex"),
    },
    witnessSet: base.witnessSet,
  });
  return Object.freeze({
    label,
    observerCount,
    networkId,
    scriptIntegrityHash,
    fee,
    nativeTx,
    fieldPreimage,
  });
};

export const transactionIdOf = (shape: ObserverShape): string =>
  computeMidgardNativeTxId(shape.nativeTx).toString("hex");

export const compactCborHex = (
  nativeTx: MidgardNativeTxFull,
  sourceKind = 0n,
): string =>
  (sourceKind === 1n
    ? encodeMidgardForcedTxCompact(nativeTx.compact)
    : encodeMidgardNativeTxCompact(nativeTx.compact)
  ).toString("hex");

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
  readonly shape: ObserverShape;
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
// Raw submitters
// ---------------------------------------------------------------------------

/**
 * Forced step 01 with the successor exposed: `nextStepIndex` names a step
 * other than the one the validator was applied with, so the deterministic
 * successor check refuses the continuation on chain.
 */
export const submitObserversForbiddenStep01ForcedRaw = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  finding,
  forcedSource,
  referenceScriptUtxo,
  nextStepIndex = 1,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: ObserversForbiddenContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly finding: ObserversForbiddenFinding;
  readonly forcedSource: Readonly<Record<string, unknown>>;
  readonly referenceScriptUtxo: UTxO;
  readonly nextStepIndex?: 0 | 1;
}) => {
  const exact = classifyObserversForbiddenFinding(finding);
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex: 0,
    threadOutRef,
  });
  requireInitialStepDatum({ threadUtxo, signer });
  signer.selectWallet(lucid);
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[0].spendingScriptHash,
    family: FAMILY,
    stepIndex: 0,
  });
  const datum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: { subject: exact.subject, network_id: BigInt(exact.networkId) },
    } as never,
    ObserversForbiddenStep02DatumSchema as never,
  );
  const nextStep = contracts.steps[nextStepIndex];
  const outputMatches = computationThreadOutputPredicate({
    address: nextStep.spendingScriptAddress,
    datum,
    unit: threadToken.unit,
  });
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, `${FAMILY} raw forced step-01`);
    const outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      `${FAMILY} raw forced output`,
    );
    return Data.to(
      {
        Continue: [
          {
            source: {
              ForcedSource: {
                ...forcedSource,
                input_index: requireInputIndex(ctx, threadUtxo, FAMILY),
                output_index: outputIndex,
              },
            },
          },
        ],
      } as never,
      ObserversForbiddenStep01RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  return await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[0].spendingScript,
    stepRole: `${FAMILY} raw forced step-01`,
    nextAddress: nextStep.spendingScriptAddress,
    nextDatum: datum,
    redeemer,
    awaitConfirmation: true,
  });
};
