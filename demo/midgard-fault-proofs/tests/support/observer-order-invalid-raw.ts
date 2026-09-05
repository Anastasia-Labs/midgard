/**
 * Shapes, block fixtures and raw submitters for the `observerOrderInvalid`
 * lifecycle. The raw submitters skip the off-chain closure and state guards
 * and expose every prover-supplied value the applied validators authenticate
 * (successor script, field opening, walk checkpoint, successor state, item
 * budget), so an honest verdict or a mutated seam is refused by a validator
 * and never by a builder.
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
import type { ObserverOrderInvalidContracts } from "../../src/observer-order-invalid/contracts.js";
import {
  classifyObserverOrderInvalidFinding,
  type ObserverOrderInvalidEvidence,
  type ObserverOrderInvalidFinding,
} from "../../src/observer-order-invalid/family.js";
import {
  ObserverOrderInvalidStep01RedeemerSchema,
  ObserverOrderInvalidStep02DatumSchema,
  ObserverOrderInvalidStep02RedeemerSchema,
  ObserverOrderInvalidStep03DatumSchema,
  ObserverOrderInvalidStep03RedeemerSchema,
  ObserverOrderInvalidStep04DatumSchema,
  ObserverOrderInvalidStep04RedeemerSchema,
} from "../../src/observer-order-invalid/schemas.js";
import {
  encodeObserverOrderWalkCheckpoint,
  hashObserverOrderWalkCheckpoint,
  type ObserverOrderInvalidStagedPlan,
  observerOrderPrefix,
} from "../../src/observer-order-invalid/staged-plan.js";
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

const FAMILY = "observer-order-invalid";

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
// Field opening shared by the raw step-02/03 submitters
// ---------------------------------------------------------------------------

type OpeningMutation = (
  opening: FieldOpening,
  referenceInputs: readonly UTxO[],
) => FieldOpening;

const resolveObserverOpening = async ({
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

// ---------------------------------------------------------------------------
// Raw submitters
// ---------------------------------------------------------------------------

/**
 * Forced step 01 with the successor exposed: `nextStepIndex` names a step
 * other than the one the validator was applied with, so the deterministic
 * successor check refuses the continuation on chain.
 */
export const submitObserverOrderInvalidStep01ForcedRaw = async ({
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
  readonly contracts: ObserverOrderInvalidContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly finding: ObserverOrderInvalidFinding;
  readonly forcedSource: Readonly<Record<string, unknown>>;
  readonly referenceScriptUtxo: UTxO;
  readonly nextStepIndex?: 0 | 1 | 2 | 3;
}) => {
  const exact = classifyObserverOrderInvalidFinding(finding);
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
      data: {
        Bound: {
          bound: {
            subject: exact.subject,
            observer_index: BigInt(exact.observerIndex),
          },
        },
      },
    } as never,
    ObserverOrderInvalidStep02DatumSchema as never,
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
            observer_index: BigInt(exact.observerIndex),
          },
        ],
      } as never,
      ObserverOrderInvalidStep01RedeemerSchema as never,
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
 * Step 02 with the opening exposed: `mutateOpening` rewrites the redeemer's
 * field opening after every off-chain check has passed. Carriage must
 * already be published (and certified when the tier requires it) exactly as
 * the production builder expects.
 */
export const submitObserverOrderInvalidStep02Raw = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  evidence,
  nativeTxCompactCbor,
  staged,
  referenceScriptUtxo,
  mutateOpening = (opening) => opening,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: ObserverOrderInvalidContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: ObserverOrderInvalidEvidence;
  readonly nativeTxCompactCbor: string;
  readonly staged: ObserverOrderInvalidStagedPlan;
  readonly referenceScriptUtxo: UTxO;
  readonly mutateOpening?: OpeningMutation;
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
  requireLinearFaultStepState<Record<string, unknown>>({
    threadUtxo,
    signer,
    schema: ObserverOrderInvalidStep02DatumSchema as never,
    family: FAMILY,
    stepIndex,
  });
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[1].spendingScriptHash,
    family: FAMILY,
    stepIndex,
  });
  const { opening, carriageUtxos, extraReferenceInputs } =
    await resolveObserverOpening({
      lucid,
      contracts,
      signer,
      evidence,
      nativeTxCompactCbor,
      stepReference,
      extraReferenceInputs: [],
      mutateOpening,
      label: `${FAMILY} raw field 3`,
    });
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        subject: evidence.subject,
        observer_index: BigInt(evidence.observerIndex),
        checkpoint_hash: hashObserverOrderWalkCheckpoint(staged.initialWalk),
        seen: 0n,
        previous_observer: "",
        outcome: 0n,
      },
    } as never,
    ObserverOrderInvalidStep03DatumSchema as never,
  );
  const nextStep = contracts.steps[2];
  const outputMatches = computationThreadOutputPredicate({
    address: nextStep.spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, `${FAMILY} raw step-02`);
    return Data.to(
      {
        Continue: [
          {
            Authenticate: {
              input_index: requireInputIndex(ctx, threadUtxo, FAMILY),
              output_index: requireUniqueOutputIndex(
                ctx.outputs,
                outputMatches,
                `${FAMILY} raw step-02 output`,
              ),
              opening,
            },
          },
        ],
      } as never,
      ObserverOrderInvalidStep02RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  return await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[1].spendingScript,
    stepRole: `${FAMILY} raw step-02`,
    nextAddress: nextStep.spendingScriptAddress,
    nextDatum,
    redeemer,
    carriageUtxos,
    extraReferenceInputs,
    awaitConfirmation: true,
  });
};

export type ObserverOrderScanSuccessor =
  | Readonly<{
      kind: "scan";
      checkpointHash: string;
      seen: bigint;
      previousObserver: string;
    }>
  | Readonly<{ kind: "decision"; violation: boolean }>;

/**
 * Step 03 with every prover-supplied value exposed: the resumed checkpoint
 * bytes, the item budget, the successor state and the successor script.
 * Defaults reproduce the production builder for `walkOrdinal`.
 */
export const submitObserverOrderInvalidStep03Raw = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  evidence,
  nativeTxCompactCbor,
  staged,
  walkOrdinal,
  referenceScriptUtxo,
  checkpointBytesHex,
  itemBudget,
  successor,
  nextStepIndex,
  mutateOpening = (opening) => opening,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: ObserverOrderInvalidContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: ObserverOrderInvalidEvidence;
  readonly nativeTxCompactCbor: string;
  readonly staged: ObserverOrderInvalidStagedPlan;
  readonly walkOrdinal: number;
  readonly referenceScriptUtxo: UTxO;
  readonly checkpointBytesHex?: string;
  readonly itemBudget?: bigint;
  readonly successor?: ObserverOrderScanSuccessor;
  readonly nextStepIndex?: 2 | 3;
  readonly mutateOpening?: OpeningMutation;
}) => {
  const nextCheckpoint = staged.walk[walkOrdinal];
  if (nextCheckpoint === undefined)
    throw new Error(`${FAMILY} raw: walk ordinal is outside plan`);
  const priorCheckpoint =
    walkOrdinal === 0 ? staged.initialWalk : staged.walk[walkOrdinal - 1]!;
  const stepIndex = 2;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  requireLinearFaultStepState<Record<string, unknown>>({
    threadUtxo,
    signer,
    schema: ObserverOrderInvalidStep03DatumSchema as never,
    family: FAMILY,
    stepIndex,
  });
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[2].spendingScriptHash,
    family: FAMILY,
    stepIndex,
  });
  const { opening, carriageUtxos, extraReferenceInputs } =
    await resolveObserverOpening({
      lucid,
      contracts,
      signer,
      evidence,
      nativeTxCompactCbor,
      stepReference,
      extraReferenceInputs: [],
      mutateOpening,
      label: `${FAMILY} raw scan field 3`,
    });
  const terminal = walkOrdinal === staged.walk.length - 1;
  const defaultSuccessor = (): ObserverOrderScanSuccessor => {
    if (terminal) return { kind: "decision", violation: evidence.violation };
    const prefix = observerOrderPrefix({
      items: staged.items,
      nextItemIndex: nextCheckpoint.nextItemIndex,
      observerIndex: evidence.observerIndex,
    });
    return {
      kind: "scan",
      checkpointHash: hashObserverOrderWalkCheckpoint(nextCheckpoint),
      seen: BigInt(prefix.seen),
      previousObserver: prefix.previousObserver,
    };
  };
  const chosen = successor ?? defaultSuccessor();
  const nextData =
    chosen.kind === "decision"
      ? {
          subject: evidence.subject,
          observer_index: BigInt(evidence.observerIndex),
          violation: chosen.violation,
        }
      : {
          subject: evidence.subject,
          observer_index: BigInt(evidence.observerIndex),
          checkpoint_hash: chosen.checkpointHash,
          seen: chosen.seen,
          previous_observer: chosen.previousObserver,
          outcome: 0n,
        };
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: nextData } as never,
    (chosen.kind === "decision"
      ? ObserverOrderInvalidStep04DatumSchema
      : ObserverOrderInvalidStep03DatumSchema) as never,
  );
  const nextStep =
    contracts.steps[nextStepIndex ?? (chosen.kind === "decision" ? 3 : 2)];
  const outputMatches = computationThreadOutputPredicate({
    address: nextStep.spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, `${FAMILY} raw step-03`);
    return Data.to(
      {
        Continue: [
          {
            input_index: requireInputIndex(ctx, threadUtxo, FAMILY),
            output_index: requireUniqueOutputIndex(
              ctx.outputs,
              outputMatches,
              `${FAMILY} raw step-03 output`,
            ),
            opening,
            checkpoint_bytes:
              checkpointBytesHex ??
              encodeObserverOrderWalkCheckpoint(priorCheckpoint).toString(
                "hex",
              ),
            item_budget:
              itemBudget ??
              BigInt(
                Math.max(
                  1,
                  nextCheckpoint.nextItemIndex - priorCheckpoint.nextItemIndex,
                ),
              ),
          },
        ],
      } as never,
      ObserverOrderInvalidStep03RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  return await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[2].spendingScript,
    stepRole: `${FAMILY} raw step-03 walk ${walkOrdinal.toString()}`,
    nextAddress: nextStep.spendingScriptAddress,
    nextDatum,
    redeemer,
    carriageUtxos,
    extraReferenceInputs,
    awaitConfirmation: true,
  });
};

/**
 * Step 04 without the off-chain `observerOrderInvalidEvidenceCloses` and
 * datum/evidence guards: an honest decision reaches
 * `terminal_contradiction_v1` and is refused there.
 */
export const submitObserverOrderInvalidStep04Raw = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  referenceScriptUtxo,
  witnessReferenceScripts,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: ObserverOrderInvalidContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
}) => {
  const stepIndex = 3;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  requireLinearFaultStepState<Record<string, unknown>>({
    threadUtxo,
    signer,
    schema: ObserverOrderInvalidStep04DatumSchema as never,
    family: FAMILY,
    stepIndex,
  });
  return await submitLinearFaultFinalize({
    lucid,
    family: FAMILY,
    stepIndex,
    step: contracts.steps[3],
    computationThread: contracts.computationThread,
    fraudProof: contracts.fraudProof,
    signer,
    threadUtxo,
    threadToken,
    spendRedeemerSchema: ObserverOrderInvalidStep04RedeemerSchema,
    buildFamilyArgs: ({
      inputIndex,
      outputIndex,
      fraudProofMintRedeemerIndex,
    }) => ({
      input_index: inputIndex,
      output_index: outputIndex,
      fraud_proof_mint_redeemer_index: fraudProofMintRedeemerIndex,
    }),
    referenceScriptUtxo,
    witnessReferenceScripts,
    awaitConfirmation: true,
  });
};

// ---------------------------------------------------------------------------
// Opening mutations
// ---------------------------------------------------------------------------

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
