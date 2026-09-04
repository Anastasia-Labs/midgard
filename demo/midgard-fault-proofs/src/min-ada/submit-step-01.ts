/** Bind Q27's accepted transaction or challenged post-block UTxO roots. */
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import {
  getHeaderFromStateQueueDatum,
  getLinkedListNodeViewFromUTxO,
  HUB_ORACLE_ASSET_NAME,
  MinAdaStep01SpendRedeemerSchema,
  MinAdaStep02DatumSchema,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  credentialToAddress,
  Data,
  type LucidEvolution,
  type Network,
  scriptHashToCredential,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  linearFaultStepLabel,
  requireLinearFaultInitialDatum,
  requireLinearFaultReferenceScript,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import { prepareNativeTxInclusionCarriage } from "../native-inclusion-carriage.js";
import {
  type PublishedProofChunk,
  walletInputsExcludingChunks,
} from "../proof-chunk-carriage.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  fetchUtxoByOutRef,
  parseOutRef,
  requireSingletonUtxo,
  type ResolvedProverSigner,
  resolveFraudulentHeaderHash,
} from "../runtime.js";
import {
  parseSubmitStep01TxInclusion,
  selectFeeInput,
} from "../submit-step-01.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import {
  type FraudProofPreSubmitBoundary,
  reachFraudProofPreSubmitBoundary,
  workflowReferenceScript,
  workflowReferenceScriptsUsedByTransaction,
} from "../workflow/transaction-boundary.js";
import {
  MIN_ADA_CATEGORY_LABEL as FAMILY,
  type MinAdaContracts,
} from "./contracts.js";
import type { PreparedMinAdaTx, PreparedMinAdaUtxo } from "./prepare.js";

type Step02Datum = Data.Static<typeof MinAdaStep02DatumSchema>;
const Step02Datum = asDataType<Step02Datum>(MinAdaStep02DatumSchema);
type Step01Redeemer = Data.Static<typeof MinAdaStep01SpendRedeemerSchema>;
const Step01Redeemer = asDataType<Step01Redeemer>(
  MinAdaStep01SpendRedeemerSchema,
);

const stepState = (prepared: PreparedMinAdaTx | PreparedMinAdaUtxo) => ({
  bad_tx_id:
    prepared.kind === "min-ada-tx"
      ? prepared.badTxId
      : prepared.outRef.transactionId,
  fault: prepared.fault,
  post_utxo:
    prepared.kind === "min-ada-tx"
      ? null
      : {
          out_ref: prepared.outRef,
          descriptor_cbor: prepared.descriptorCbor,
          post_utxos_root: prepared.postUtxosRoot,
          prev_utxos_root: prepared.prevUtxosRoot,
        },
});

const resolveAnchors = async ({
  lucid,
  contracts,
  network,
  stateQueueBlockOutRef,
  expectedHeaderHash,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: MinAdaContracts;
  readonly network: Network;
  readonly stateQueueBlockOutRef: string;
  readonly expectedHeaderHash: string;
}) => {
  const [hubOracleUtxo, stateQueueBlockUtxo] = await Promise.all([
    requireSingletonUtxo({
      lucid,
      address: credentialToAddress(
        network,
        scriptHashToCredential(contracts.hubOraclePolicyId),
      ),
      unit: toUnit(contracts.hubOraclePolicyId, HUB_ORACLE_ASSET_NAME),
      label: `${FAMILY} hub oracle`,
    }),
    fetchUtxoByOutRef({
      lucid,
      outRef: parseOutRef(stateQueueBlockOutRef, "state-queue block out-ref"),
      label: `${FAMILY} state-queue block`,
    }),
  ]);
  const headerHash = resolveFraudulentHeaderHash({
    stateQueuePolicyId: contracts.stateQueuePolicyId,
    fraudulentBlockUtxo: stateQueueBlockUtxo,
  });
  if (headerHash !== expectedHeaderHash) {
    throw new Error(
      `${FAMILY}: state-queue header ${headerHash} does not match prepared header ${expectedHeaderHash}`,
    );
  }
  const header = await Effect.runPromise(
    getHeaderFromStateQueueDatum(
      await Effect.runPromise(
        getLinkedListNodeViewFromUTxO(stateQueueBlockUtxo),
      ),
    ),
  );
  return { hubOracleUtxo, stateQueueBlockUtxo, headerHash, header };
};

type Common = {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly contracts: MinAdaContracts;
  readonly categoryId: string;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly stateQueueBlockOutRef: string;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
};

export const submitMinAdaTxStep01 = async ({
  lucid,
  blueprint,
  contracts,
  categoryId,
  network,
  signer,
  threadOutRef,
  stateQueueBlockOutRef,
  prepared,
  publishedProofChunks = [],
  referenceScriptUtxo,
  witnessReferenceScripts,
  preSubmitBoundary,
  awaitConfirmation = true,
}: Common & {
  readonly prepared: PreparedMinAdaTx;
  readonly publishedProofChunks?: readonly PublishedProofChunk[];
}) => {
  const stepIndex = 0;
  const label = linearFaultStepLabel(FAMILY, stepIndex);
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  requireLinearFaultInitialDatum({ threadUtxo, signer, family: FAMILY });
  if (threadToken.fraudulentHeaderHash !== prepared.headerHash) {
    throw new Error(`${label}: prepared header does not match thread token`);
  }
  const anchors = await resolveAnchors({
    lucid,
    contracts,
    network,
    stateQueueBlockOutRef,
    expectedHeaderHash: prepared.headerHash,
  });
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[stepIndex].spendingScriptHash,
    family: FAMILY,
    stepIndex,
  });
  const txInclusion = parseSubmitStep01TxInclusion(prepared.txInclusion);
  const inclusion = prepareNativeTxInclusionCarriage({
    blueprint,
    network,
    txInclusion,
    publishedProofChunks,
    witnessReferenceScripts,
    label,
    baseReferenceInputs: [
      anchors.hubOracleUtxo,
      anchors.stateQueueBlockUtxo,
      stepReference,
    ],
  });
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: stepState(prepared) },
    Step02Datum,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[1].spendingScriptAddress,
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
            tx_inclusion: inclusion.redeemer(ctx, {
              input_index: inputIndex,
              output_index: outputIndex,
              hub_ref_input_index: requireReferenceInputIndex(
                ctx,
                anchors.hubOracleUtxo,
                `${label} hub oracle`,
              ),
              state_queue_node_ref_input_index: requireReferenceInputIndex(
                ctx,
                anchors.stateQueueBlockUtxo,
                `${label} state-queue block`,
              ),
            }),
            post_utxo_membership: null,
            fault: prepared.fault,
          },
        ],
      },
      Step01Redeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(
    walletInputsExcludingChunks({
      walletUtxos: await lucid.wallet().getUtxos(),
      chunks: publishedProofChunks,
    }),
  );
  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .readFrom(inclusion.referenceInputs)
    .pay.ToContract(
      contracts.steps[1].spendingScriptAddress,
      { kind: "inline", value: nextDatum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [threadToken.unit]: 1n,
      },
    )
    .addSignerKey(signer.paymentKeyHash);
  const unsigned = await inclusion
    .attachWithdrawal(base)
    .complete({ localUPLCEval: true });
  if (outputIndex === undefined) throw new Error(`${label}: unresolved layout`);
  const signed = await unsigned.sign.withWallet().complete();
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
        ...inclusion.referenceScriptCandidates,
      ],
    }),
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) throw new Error(`${label}: hash mismatch`);
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${outputIndex.toString()}`,
    fraudulentHeaderHash: anchors.headerHash,
    computationThreadUnit: threadToken.unit,
    proofCarriage:
      publishedProofChunks.length === 0 ? "redeemer" : "published-chunks",
  } as const;
};

export const submitMinAdaUtxoStep01 = async ({
  lucid,
  contracts,
  categoryId,
  network,
  signer,
  threadOutRef,
  stateQueueBlockOutRef,
  prepared,
  referenceScriptUtxo,
  preSubmitBoundary,
  awaitConfirmation = true,
}: Omit<Common, "blueprint" | "witnessReferenceScripts"> & {
  readonly prepared: PreparedMinAdaUtxo;
}) => {
  const stepIndex = 0;
  const label = `${linearFaultStepLabel(FAMILY, stepIndex)} post-UTxO`;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  requireLinearFaultInitialDatum({ threadUtxo, signer, family: FAMILY });
  if (threadToken.fraudulentHeaderHash !== prepared.headerHash) {
    throw new Error(`${label}: prepared header does not match thread token`);
  }
  const anchors = await resolveAnchors({
    lucid,
    contracts,
    network,
    stateQueueBlockOutRef,
    expectedHeaderHash: prepared.headerHash,
  });
  if (
    anchors.header.utxosRoot !== prepared.postUtxosRoot ||
    anchors.header.prevUtxosRoot !== prepared.prevUtxosRoot
  ) {
    throw new Error(`${label}: prepared roots do not match challenged header`);
  }
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[stepIndex].spendingScriptHash,
    family: FAMILY,
    stepIndex,
  });
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: stepState(prepared) },
    Step02Datum,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[1].spendingScriptAddress,
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
            tx_inclusion: null,
            post_utxo_membership: {
              input_index: inputIndex,
              output_index: outputIndex,
              hub_ref_input_index: requireReferenceInputIndex(
                ctx,
                anchors.hubOracleUtxo,
                `${label} hub oracle`,
              ),
              state_queue_node_ref_input_index: requireReferenceInputIndex(
                ctx,
                anchors.stateQueueBlockUtxo,
                `${label} state-queue block`,
              ),
              out_ref: prepared.outRef,
              descriptor_cbor: prepared.descriptorCbor,
            },
            fault: prepared.fault,
          },
        ],
      },
      Step01Redeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  signer.selectWallet(lucid);
  const unsigned = await lucid
    .newTx()
    .collectFrom([selectFeeInput(await lucid.wallet().getUtxos())])
    .collectFrom([threadUtxo], redeemer)
    .readFrom([
      anchors.hubOracleUtxo,
      anchors.stateQueueBlockUtxo,
      stepReference,
    ])
    .pay.ToContract(
      contracts.steps[1].spendingScriptAddress,
      { kind: "inline", value: nextDatum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [threadToken.unit]: 1n,
      },
    )
    .addSignerKey(signer.paymentKeyHash)
    .complete({ localUPLCEval: true });
  if (outputIndex === undefined) throw new Error(`${label}: unresolved layout`);
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundary({
    signed,
    referenceScripts: [
      workflowReferenceScript({
        role: label,
        utxo: stepReference,
        expectedScript: contracts.steps[stepIndex].spendingScript,
      }),
    ],
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) throw new Error(`${label}: hash mismatch`);
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${outputIndex.toString()}`,
    fraudulentHeaderHash: anchors.headerHash,
    computationThreadUnit: threadToken.unit,
  };
};
