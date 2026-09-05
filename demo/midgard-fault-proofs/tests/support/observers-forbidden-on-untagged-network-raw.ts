/**
 * Raw submitters and fixtures for the `observersForbiddenOnUntaggedNetwork`
 * lifecycle. The raw submitters skip the off-chain closure and state guards
 * so an honest verdict or a mutated authentication seam reaches the applied
 * validator and is refused there, never by a builder.
 */
import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  adjudicateMidgardNativeTxFullValidity,
  computeMidgardNativeTxId,
  decodeMidgardFieldPreimage,
  deriveMidgardNativeTxProofSource,
  deriveMidgardNativeTxWitnessSetCompact,
  encodeMidgardFieldPreimage,
  encodeMidgardNativeTxCompact,
  encodeMidgardNativeTxWitnessSetCompact,
  materializeMidgardNativeTxFromCanonical,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core";
import {
  type FieldOpening,
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
  faultProofFieldOpening,
  planFaultProofFieldOpening,
  resolveFaultProofFieldCarriagePublications,
  resolveFaultProofFieldPreimageCertificate,
} from "../../src/field-opening.js";
import {
  requireLinearFaultReferenceScript,
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../../src/linear-fault-family.js";
import { submitLinearFaultFinalize } from "../../src/linear-fault-finalize.js";
import { submitLinearFaultContinue } from "../../src/linear-fault-submit.js";
import type { ObserversForbiddenContracts } from "../../src/observers-forbidden-on-untagged-network/contracts.js";
import {
  classifyObserversForbiddenFinding,
  type ObserversForbiddenEvidence,
  type ObserversForbiddenFinding,
} from "../../src/observers-forbidden-on-untagged-network/family.js";
import {
  ObserversForbiddenStep01RedeemerSchema,
  ObserversForbiddenStep02DatumSchema,
  ObserversForbiddenStep02RedeemerSchema,
} from "../../src/observers-forbidden-on-untagged-network/schemas.js";
import type { ResolvedProverSigner } from "../../src/runtime.js";
import {
  nativeTxFromCoreCompact,
  requireInitialStepDatum,
  type SubmitStep01TxInclusion,
} from "../../src/submit-step-01.js";
import { buildCountedRoot } from "../../src/transition-trace/phas.js";
import { computationThreadOutputPredicate } from "../../src/tx-layout.js";
import type { FaultProofWitnessReferenceScripts } from "../../src/witness-reference-scripts.js";
import { l2TransactionSourceCbor, makeNativeTx } from "./emulator/native-tx.js";

const FAMILY = "observers-forbidden-on-untagged-network";

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
  readonly shape: ObserverShape;
  readonly sourceKey: { transactionId: string; outputIndex: bigint };
  readonly rejectionReason: RejectionReason;
}) => {
  const invalid = adjudicateMidgardNativeTxFullValidity(
    shape.nativeTx,
    "TxIsInvalid",
  );
  const transactionId = computeMidgardNativeTxId(invalid).toString("hex");
  const proofSource = deriveMidgardNativeTxProofSource(invalid);
  const transaction = {
    tx_id: transactionId,
    source: {
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

/**
 * Step 02 without the off-chain `observersForbiddenEvidenceCloses` and bound
 * network-scalar guards, and with the opening exposed: an honest verdict
 * reaches `terminal_contradiction_v1`, and `mutateOpening` rewrites the
 * redeemer's opening after every off-chain check has passed. Carriage must
 * already be published (and certified when the tier requires it) exactly as
 * the production builder expects.
 */
export const submitObserversForbiddenStep02Raw = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  evidence,
  nativeTxCompactCbor,
  referenceScriptUtxo,
  witnessReferenceScripts,
  mutateOpening = (opening) => opening,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: ObserversForbiddenContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: ObserversForbiddenEvidence;
  readonly nativeTxCompactCbor: string;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
  readonly mutateOpening?: (
    opening: FieldOpening,
    referenceInputs: readonly UTxO[],
  ) => FieldOpening;
}) => {
  const stepIndex = 1;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  requireLinearFaultStepState<{ subject: unknown; network_id: bigint }>({
    threadUtxo,
    signer,
    schema: ObserversForbiddenStep02DatumSchema as never,
    family: FAMILY,
    stepIndex,
  });
  const planned = planFaultProofFieldOpening({
    fieldIndex: 3,
    anchorTxId: evidence.subject.transaction_id,
    nativeTxCompactCbor,
    itemCbors: decodeMidgardFieldPreimage(
      Buffer.from(evidence.observerFieldPreimageCbor, "hex"),
    ),
    owner: signer.paymentKeyHash,
    publish: true,
    label: `${FAMILY} raw field 3`,
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
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[1].spendingScriptHash,
    family: FAMILY,
    stepIndex,
  });
  const referenceInputs = [
    ...carriageUtxos,
    stepReference,
    ...(certificateUtxo === undefined ? [] : [certificateUtxo]),
    ...(witnessReferenceScripts.computationThreadMint === undefined
      ? []
      : [witnessReferenceScripts.computationThreadMint]),
    ...(witnessReferenceScripts.fraudProofMint === undefined
      ? []
      : [witnessReferenceScripts.fraudProofMint]),
  ];
  const opening = mutateOpening(
    faultProofFieldOpening({
      planned,
      referenceInputs,
      certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
      label: `${FAMILY} raw field 3`,
    }),
    referenceInputs,
  );
  return await submitLinearFaultFinalize({
    lucid,
    family: FAMILY,
    stepIndex,
    step: contracts.steps[1],
    computationThread: contracts.computationThread,
    fraudProof: contracts.fraudProof,
    signer,
    threadUtxo,
    threadToken,
    spendRedeemerSchema: ObserversForbiddenStep02RedeemerSchema,
    buildFamilyArgs: ({
      inputIndex,
      outputIndex,
      fraudProofMintRedeemerIndex,
    }) => ({
      input_index: inputIndex,
      output_index: outputIndex,
      fraud_proof_mint_redeemer_index: fraudProofMintRedeemerIndex,
      observer_opening: opening,
    }),
    referenceScriptUtxo,
    carriageUtxos,
    extraReferenceInputs:
      certificateUtxo === undefined ? [] : [certificateUtxo],
    witnessReferenceScripts,
    awaitConfirmation: true,
  });
};

/** Patch a certified carriage's reference-input coordinates. */
export const mutateCertifiedCarriage = (
  opening: FieldOpening,
  patch: (carriage: {
    cert_ref_input_index: bigint;
    chunk_ref_input_indices: bigint[];
  }) => {
    cert_ref_input_index: bigint;
    chunk_ref_input_indices: bigint[];
  },
): FieldOpening => {
  if (!("BodyFieldOpening" in opening))
    throw new Error("body opening expected");
  const carriage = opening.BodyFieldOpening.carriage;
  if (!("Certified" in carriage))
    throw new Error("certified carriage expected");
  return {
    BodyFieldOpening: {
      ...opening.BodyFieldOpening,
      carriage: {
        Certified: patch({
          cert_ref_input_index: carriage.Certified.cert_ref_input_index,
          chunk_ref_input_indices: [
            ...carriage.Certified.chunk_ref_input_indices,
          ],
        }),
      },
    },
  };
};

/**
 * Point a published (RawUtxo) carriage at a different reference input. A
 * published plan promotes the inline tier to RawUtxo, so a small field's
 * bytes are always read from the named reference input; naming another one
 * substitutes the bytes the door commits.
 */
export const mutateRawUtxoCarriage = (
  opening: FieldOpening,
  offset: bigint,
): FieldOpening => {
  if (!("BodyFieldOpening" in opening))
    throw new Error("body opening expected");
  const carriage = opening.BodyFieldOpening.carriage;
  if (!("RawUtxo" in carriage)) throw new Error("raw-utxo carriage expected");
  return {
    BodyFieldOpening: {
      ...opening.BodyFieldOpening,
      carriage: {
        RawUtxo: {
          ref_input_index: carriage.RawUtxo.ref_input_index + offset,
        },
      },
    },
  };
};

/** Replace the compact transaction bytes the opening is anchored to. */
export const mutateCompactSource = (
  opening: FieldOpening,
  nativeTxCompactCbor: string,
): FieldOpening => {
  if (!("BodyFieldOpening" in opening))
    throw new Error("body opening expected");
  return {
    BodyFieldOpening: {
      ...opening.BodyFieldOpening,
      native_tx_compact_cbor: nativeTxCompactCbor,
    },
  };
};
