/** Q27 step 02: adjudicate a produced output or authenticate post membership. */
import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core";
import {
  type FieldOpening,
  MIDGARD_FIELD_INDEX,
  MinAdaStep02DatumSchema,
  MinAdaStep02SpendRedeemerSchema,
  MinAdaStep03DatumSchema,
  MinAdaStep05DatumSchema,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
  requireWithdrawalRedeemerIndex,
  scriptRewardAddress,
} from "@al-ft/midgard-sdk";
import {
  buildCanonicalMidgardLedgerOutputMaterial,
  MIDGARD_COINS_PER_UTXO_BYTE,
  outputMeetsMinAda,
} from "@al-ft/midgard-validation";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type Network,
  type Script,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  faultProofFieldOpening,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../field-opening.js";
import {
  linearFaultStepLabel,
  requireLinearFaultReferenceScript,
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import {
  chunkedMembershipClaimRedeemer,
  chunkedVerifyWithdrawalScript,
  derivedChunkReferenceIndices,
  type PublishedProofChunk,
  requireBuiltChunkReferenceIndices,
} from "../proof-chunk-carriage.js";
import {
  encodeRawPhasMembershipProofRedeemer,
  getCompiledScript,
  phasMembershipRewardAddress,
  type ResolvedProverSigner,
} from "../runtime.js";
import { PHAS_MEMBERSHIP_WITHDRAW_TITLE } from "../submit-step-01.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import {
  type FaultProofWitnessReferenceScripts,
  witnessWithdrawalValidatorCarriage,
} from "../witness-reference-scripts.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import {
  MIN_ADA_CATEGORY_LABEL as FAMILY,
  type MinAdaContracts,
} from "./contracts.js";
import type { PreparedMinAdaTx, PreparedMinAdaUtxo } from "./prepare.js";

type State = NonNullable<Data.Static<typeof MinAdaStep02DatumSchema>["data"]>;
type Step02Datum = Data.Static<typeof MinAdaStep02DatumSchema>;
const Step02Datum = MinAdaStep02DatumSchema as unknown as Step02Datum;
type Step03Datum = Data.Static<typeof MinAdaStep03DatumSchema>;
const Step03Datum = MinAdaStep03DatumSchema as unknown as Step03Datum;
type Step05Datum = Data.Static<typeof MinAdaStep05DatumSchema>;
const Step05Datum = MinAdaStep05DatumSchema as unknown as Step05Datum;
type Redeemer = Data.Static<typeof MinAdaStep02SpendRedeemerSchema>;
const Redeemer = MinAdaStep02SpendRedeemerSchema as unknown as Redeemer;

const walletInputsExcludingReferences = ({
  walletUtxos,
  references,
}: {
  readonly walletUtxos: readonly UTxO[];
  readonly references: readonly UTxO[];
}): UTxO[] =>
  walletUtxos.filter(
    (utxo) =>
      !references.some(
        (reference) =>
          reference.txHash === utxo.txHash &&
          reference.outputIndex === utxo.outputIndex,
      ),
  );

const requireStep02 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: MinAdaContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
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
  const state = requireLinearFaultStepState<State>({
    threadUtxo,
    signer,
    schema: Step02Datum,
    family: FAMILY,
    stepIndex,
  });
  return { stepIndex, threadUtxo, threadToken, state };
};

export const submitMinAdaTxStep02 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  prepared,
  publishCarriage = false,
  publishedCarriageUtxos,
  certificateUtxo,
  referenceScriptUtxo,
  yieldReferenceScriptUtxo,
  publicationPreSubmitBoundary,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: MinAdaContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly prepared: PreparedMinAdaTx;
  readonly publishCarriage?: boolean;
  readonly publishedCarriageUtxos?: readonly UTxO[];
  readonly certificateUtxo?: UTxO;
  readonly referenceScriptUtxo: UTxO;
  readonly yieldReferenceScriptUtxo: UTxO;
  readonly publicationPreSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  const { stepIndex, threadUtxo, threadToken, state } = await requireStep02({
    lucid,
    contracts,
    categoryId,
    signer,
    threadOutRef,
  });
  const label = `${linearFaultStepLabel(FAMILY, stepIndex)} transaction`;
  if (
    state.bad_tx_id !== prepared.badTxId ||
    state.post_utxo !== null ||
    state.fault === "MinAdaUtxo" ||
    state.fault.MinAdaTx.output_index !== prepared.badOutputIndex
  ) {
    throw new Error(
      `${label}: prepared transaction does not match thread state`,
    );
  }
  const item = prepared.outputItemCbors[Number(prepared.badOutputIndex)];
  if (item === undefined) {
    throw new Error(`${label}: bad output index is outside field 2`);
  }
  const material = buildCanonicalMidgardLedgerOutputMaterial({
    outputIndex: Number(prepared.badOutputIndex),
    outputCbor: Buffer.from(item, "hex"),
  });
  if (
    material.descriptorCbor.toString("hex") !== prepared.descriptorCbor ||
    outputMeetsMinAda(
      MIDGARD_COINS_PER_UTXO_BYTE,
      BigInt(material.descriptor.totalLength),
      material.descriptor.lovelace,
    )
  ) {
    throw new Error(`${label}: selected output does not violate min-Ada`);
  }
  const planned = planFaultProofFieldOpening({
    fieldIndex: MIDGARD_FIELD_INDEX.outputs,
    anchorTxId: state.bad_tx_id,
    nativeTxCompactCbor: prepared.nativeTxCompactCbor,
    itemCbors: prepared.outputItemCbors.map((cbor) => Buffer.from(cbor, "hex")),
    owner: signer.paymentKeyHash,
    publish: publishCarriage,
    label: `${label} field 2`,
  });
  signer.selectWallet(lucid);
  const carriageUtxos =
    publishedCarriageUtxos ??
    (await publishFaultProofFieldCarriage({
      lucid,
      signer,
      planned,
      publisherAddress: signer.address,
      label: `${label} field 2`,
      preSubmitBoundary: publicationPreSubmitBoundary,
    }));
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[stepIndex].spendingScriptHash,
    family: FAMILY,
    stepIndex,
  });
  const yieldReference = requireLinearFaultReferenceScript({
    utxo: yieldReferenceScriptUtxo,
    expectedScriptHash: contracts.yields.tx.withdrawalScriptHash,
    family: FAMILY,
    stepIndex,
  });
  const opening: FieldOpening = faultProofFieldOpening({
    planned,
    referenceInputs: [
      ...carriageUtxos,
      stepReference,
      ...(certificateUtxo === undefined ? [] : [certificateUtxo]),
      yieldReference,
    ],
    certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
    label: `${label} field 2`,
  });
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        MinAdaTxDescriptor: {
          total_length: BigInt(material.descriptor.totalLength),
          lovelace: material.descriptor.lovelace,
        },
      },
    },
    Step03Datum,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[2].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, label);
    const inputIndex = requireInputIndex(ctx, threadUtxo, label);
    outputIndex = requireUniqueOutputIndex(ctx.outputs, outputMatches, label);
    return Data.to(
      {
        Continue: [
          {
            input_index: inputIndex,
            output_index: outputIndex,
            yield_to_ref_input_index: requireReferenceInputIndex(
              ctx,
              yieldReference,
              label,
            ),
            outputs_opening: opening,
            post_membership: null,
          },
        ],
      },
      Redeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const network = lucid.config().network;
  if (network === undefined) throw new Error(`${label}: Lucid network missing`);
  const { selectFeeInput } = await import("../submit-step-01.js");
  const transactionReferences = [
    stepReference,
    ...carriageUtxos,
    ...(certificateUtxo === undefined ? [] : [certificateUtxo]),
    yieldReference,
  ];
  const feeInput = selectFeeInput(
    walletInputsExcludingReferences({
      walletUtxos: await lucid.wallet().getUtxos(),
      references: transactionReferences,
    }),
  );
  const unsigned = await lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .readFrom(transactionReferences)
    .withdraw(
      scriptRewardAddress(network, contracts.yields.tx.withdrawalScript),
      0n,
      Data.void(),
    )
    .pay.ToContract(
      contracts.steps[2].spendingScriptAddress,
      { kind: "inline", value: nextDatum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [threadToken.unit]: 1n,
      },
    )
    .addSignerKey(signer.paymentKeyHash)
    .complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const {
    reachFraudProofPreSubmitBoundary,
    workflowReferenceScriptsUsedByTransaction,
  } = await import("../workflow/transaction-boundary.js");
  const expectedTxHash = await reachFraudProofPreSubmitBoundary({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransaction({
      signed,
      candidates: [
        {
          role: label,
          utxo: stepReference,
          expectedScript: contracts.steps[stepIndex].spendingScript,
        },
        {
          role: `${label}-yield`,
          utxo: yieldReference,
          expectedScript: contracts.yields.tx.withdrawalScript,
        },
      ],
    }),
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) throw new Error(`${label}: hash mismatch`);
  if (awaitConfirmation) {
    const { DEFAULT_CONFIRMATION_POLL_MS } = await import("../runtime.js");
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  if (outputIndex === undefined) throw new Error(`${label}: unresolved layout`);
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${outputIndex.toString()}`,
    carriageTier: planned.plan.tier,
  };
};

export const submitMinAdaUtxoStep02 = async ({
  lucid,
  blueprint,
  network,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  prepared,
  publishedProofChunks = [],
  referenceScriptUtxo,
  yieldReferenceScriptUtxo,
  witnessReferenceScripts,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly network: Network;
  readonly contracts: MinAdaContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly prepared: PreparedMinAdaUtxo;
  readonly publishedProofChunks?: readonly PublishedProofChunk[];
  readonly referenceScriptUtxo: UTxO;
  readonly yieldReferenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  const { stepIndex, threadUtxo, threadToken, state } = await requireStep02({
    lucid,
    contracts,
    categoryId,
    signer,
    threadOutRef,
  });
  const label = `${linearFaultStepLabel(FAMILY, stepIndex)} post membership`;
  if (
    state.bad_tx_id !== prepared.outRef.transactionId ||
    state.fault !== "MinAdaUtxo" ||
    state.post_utxo === null ||
    state.post_utxo.descriptor_cbor !== prepared.descriptorCbor ||
    state.post_utxo.post_utxos_root !== prepared.postUtxosRoot ||
    state.post_utxo.prev_utxos_root !== prepared.prevUtxosRoot ||
    state.post_utxo.out_ref.transactionId !== prepared.outRef.transactionId ||
    state.post_utxo.out_ref.outputIndex !== prepared.outRef.outputIndex
  ) {
    throw new Error(`${label}: prepared UTxO does not match thread state`);
  }
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[stepIndex].spendingScriptHash,
    family: FAMILY,
    stepIndex,
  });
  const yieldReference = requireLinearFaultReferenceScript({
    utxo: yieldReferenceScriptUtxo,
    expectedScriptHash: contracts.yields.utxo.withdrawalScriptHash,
    family: FAMILY,
    stepIndex,
  });
  const chunks = publishedProofChunks;
  const carriedByChunks = chunks.length > 0;
  const membershipScript: Script = carriedByChunks
    ? chunkedVerifyWithdrawalScript(blueprint)
    : {
        type: "PlutusV3",
        script: getCompiledScript(blueprint, PHAS_MEMBERSHIP_WITHDRAW_TITLE),
      };
  const membershipAddress = phasMembershipRewardAddress(
    network,
    membershipScript,
  );
  const membershipWitness = witnessWithdrawalValidatorCarriage({
    script: membershipScript,
    referenceUtxo: carriedByChunks
      ? witnessReferenceScripts?.chunkedVerifyWithdraw
      : witnessReferenceScripts?.phasMembershipWithdraw,
    label,
  });
  const referenceInputs = [
    ...chunks.map(({ utxo }) => utxo),
    stepReference,
    ...membershipWitness.referenceInputs,
    yieldReference,
  ];
  const chunkIndices = derivedChunkReferenceIndices({
    referenceInputs,
    chunks,
    label,
  });
  const outRefKey = encodeMidgardSpendInputItem({
    txId: Buffer.from(prepared.outRef.transactionId, "hex"),
    outputIndex: Number(prepared.outRef.outputIndex),
  }).toString("hex");
  if (outRefKey !== prepared.outRefKeyCbor) {
    throw new Error(`${label}: out-ref key is noncanonical`);
  }
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        MinAdaUtxoDescriptor: {
          descriptor_cbor: prepared.descriptorCbor,
          out_ref_key: prepared.outRefKeyCbor,
          prev_utxos_root: prepared.prevUtxosRoot,
        },
      },
    },
    Step03Datum,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[2].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, label);
    const inputIndex = requireInputIndex(ctx, threadUtxo, label);
    outputIndex = requireUniqueOutputIndex(ctx.outputs, outputMatches, label);
    requireBuiltChunkReferenceIndices({
      ctx,
      chunks,
      derived: chunkIndices,
      label,
    });
    const postMembership = carriedByChunks
      ? {
          PublishedChunkMembership: [
            { ordered_chunk_reference_input_indices: chunkIndices },
          ],
        }
      : {
          RedeemerCarriedMembership: {
            membership_proof: prepared.postMembershipProof,
            membership_proof_script_redeemer_index:
              requireWithdrawalRedeemerIndex(ctx, membershipAddress, label),
          },
        };
    return Data.to(
      {
        Continue: [
          {
            input_index: inputIndex,
            output_index: outputIndex,
            yield_to_ref_input_index: requireReferenceInputIndex(
              ctx,
              yieldReference,
              label,
            ),
            outputs_opening: null,
            post_membership: postMembership,
          },
        ],
      } as never,
      Redeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const extraReferenceInputs = [
    ...chunks.map(({ utxo }) => utxo),
    ...membershipWitness.referenceInputs,
    yieldReference,
  ];
  const baseArgs = {
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[stepIndex].spendingScript,
    stepRole: label,
    nextAddress: contracts.steps[2].spendingScriptAddress,
    nextDatum,
    preSubmitBoundary,
    awaitConfirmation,
  } as const;
  // This path has a withdrawal, so construct it directly instead of using the
  // continuation helper's withdrawal-free transaction body.
  signer.selectWallet(lucid);
  const { selectFeeInput } = await import("../submit-step-01.js");
  const { walletInputsExcludingChunks } = await import(
    "../proof-chunk-carriage.js"
  );
  const feeInput = selectFeeInput(
    walletInputsExcludingChunks({
      walletUtxos: await lucid.wallet().getUtxos(),
      chunks,
    }),
  );
  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .readFrom([stepReference, ...extraReferenceInputs]);
  const withMembershipWithdrawal = carriedByChunks
    ? base.withdraw(
        membershipAddress,
        0n,
        chunkedMembershipClaimRedeemer({
          merkleRoot: prepared.postUtxosRoot,
          keyBytes: prepared.outRefKeyCbor,
          valueBytes: prepared.descriptorCbor,
          orderedChunkReferenceInputIndices: chunkIndices,
        }),
      )
    : base.withdraw(
        membershipAddress,
        0n,
        encodeRawPhasMembershipProofRedeemer({
          root: prepared.postUtxosRoot,
          keyBytes: prepared.outRefKeyCbor,
          valueBytes: prepared.descriptorCbor,
          membershipProofCbor: prepared.postMembershipProofCbor,
        }),
      );
  const withWithdrawal = withMembershipWithdrawal.withdraw(
    scriptRewardAddress(network, contracts.yields.utxo.withdrawalScript),
    0n,
    Data.void(),
  );
  const unsigned = await membershipWitness
    .attach(
      withWithdrawal.pay
        .ToContract(
          baseArgs.nextAddress,
          { kind: "inline", value: nextDatum },
          {
            lovelace: threadUtxo.assets.lovelace ?? 0n,
            [threadToken.unit]: 1n,
          },
        )
        .addSignerKey(signer.paymentKeyHash),
    )
    .complete({ localUPLCEval: true });
  if (outputIndex === undefined) throw new Error(`${label}: unresolved layout`);
  const signed = await unsigned.sign.withWallet().complete();
  const {
    reachFraudProofPreSubmitBoundary,
    workflowReferenceScriptsUsedByTransaction,
  } = await import("../workflow/transaction-boundary.js");
  const expectedTxHash = await reachFraudProofPreSubmitBoundary({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransaction({
      signed,
      candidates: [
        {
          role: label,
          utxo: stepReference,
          expectedScript: contracts.steps[stepIndex].spendingScript,
        },
        ...(membershipWitness.referenceInputs[0] === undefined
          ? []
          : [
              {
                role: `${label}-proof`,
                utxo: membershipWitness.referenceInputs[0],
                expectedScript: membershipScript,
              },
            ]),
        {
          role: `${label}-yield`,
          utxo: yieldReference,
          expectedScript: contracts.yields.utxo.withdrawalScript,
        },
      ],
    }),
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) throw new Error(`${label}: hash mismatch`);
  if (awaitConfirmation) {
    const { DEFAULT_CONFIRMATION_POLL_MS } = await import("../runtime.js");
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${outputIndex.toString()}`,
    proofCarriage: carriedByChunks ? "published-chunks" : "redeemer",
  } as const;
};
