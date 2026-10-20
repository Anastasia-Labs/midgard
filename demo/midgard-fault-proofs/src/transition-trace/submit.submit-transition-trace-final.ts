import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import {
  FraudProofTokenDatum,
  hashBlockHeader,
  HUB_ORACLE_ASSET_NAME,
  HubOracleDatum,
  requireInputIndex,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
  TransitionTraceProofCommitmentDatum,
  TransitionTraceYieldFinalSpendRedeemer,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  credentialToAddress,
  Data,
  scriptHashToCredential,
  toUnit,
  type UTxO,
  validatorToRewardAddress,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  prepareFabricatedCompletedFraud,
  resolveCompletedFraudQueueNode,
} from "../fabricated-completed-fraud.js";
import { fetchCurrentHistory } from "../fabricated-history-witness.js";
import { fabricatedProofValidity } from "../fabricated-proof-validity.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  fetchUtxoByOutRef,
  parseOutRef,
  requireDeploymentReferenceScript,
  requireSingletonUtxo,
  resolveTransitionTraceDeploymentContracts,
} from "../runtime.js";
import {
  requireComputationThreadToken,
  selectFeeInput,
} from "../step-support.js";
import { outputWithDatumAndUnitPredicate } from "../tx-layout.js";
import { witnessMintingPolicyCarriage } from "../witness-reference-scripts.js";
import { createOwnOutputJournal } from "../workflow/own-outputs.js";
import {
  type FraudProofPreSubmitBoundary,
  reachFraudProofPreSubmitBoundary,
  workflowReferenceScriptsUsedByTransaction,
} from "../workflow/transaction-boundary.js";
import { governedUserEventAddress } from "../workflow/user-event-address.js";
import { transitionTraceError } from "./errors.js";
import {
  reopenTransitionDeposit,
  transitionDepositOpening,
} from "./history-opening.js";
import {
  initialTransitionTraceState,
  transitionTracePhaseIsTerminal,
  transitionTracePhaseYield,
} from "./phases.js";
import {
  resolveTransitionTraceByteCarriage,
  transitionTraceByteChunks,
} from "./proof-carriage.js";
import {
  resolveTransitionTraceProofCarriage,
  transitionTraceProofChunks,
} from "./proof-carriage.js";
import { depositSourceView, readTransitionProof } from "./proof-material.js";
import {
  makeComputationThreadSuccessRedeemer,
  makeFraudProofMintRedeemer,
  makeTransitionTraceFinalSpendRedeemer,
  transitionTraceFinalIndex,
  transitionTraceHistoryTimingTarget,
} from "./submit.make-transition-trace-final-spend-redeemer.js";
import {
  type SubmitTransitionTraceFinalResult,
  type SubmitTransitionTraceProofConfig,
  TRANSITION_TRACE_FINAL_REFERENCE_SCRIPT_ENTRIES,
  type TransitionTraceFinalSpendLayout,
} from "./submit.make-transition-trace-route-spend-redeemer.js";
import { prepareTransitionFieldStep } from "./submit.transition-field-step.js";
import { transitionTraceRouteDatum } from "./submit.transition-trace-route-datum.js";
import { transitionTraceYieldData } from "./yield-data.js";
import { TRANSITION_TRACE_YIELD_REFERENCES } from "./yield-references.js";

type SubmitTransitionTraceFinalConfig = SubmitTransitionTraceProofConfig & {
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
};

/**
 * Exact terminal selected by the authenticated router output. A checkpoint
 * final continues the thread at its own output, one transaction per step;
 * the steps run in a loop, so each step's state is released once its
 * transaction confirms rather than held for the rest of the thread.
 */
export const submitTransitionTraceFinal = async (
  config: SubmitTransitionTraceFinalConfig,
): Promise<SubmitTransitionTraceFinalResult> => {
  let step: SubmitTransitionTraceFinalConfig = {
    ...config,
    ownOutputs: config.ownOutputs ?? createOwnOutputJournal(),
  };
  for (;;) {
    const outcome = await submitTransitionTraceFinalStep(step);
    if (outcome.kind === "final") return outcome.result;
    step = { ...step, ...outcome.continuation };
  }
};

const submitTransitionTraceFinalStep = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  proof: proofInput,
  additionalReferenceInputs = [],
  depositOpening,
  witnessReferenceScripts,
  fieldCertificateReferenceScript,
  preSubmitBoundary,
  awaitConfirmation = true,
  ownOutputs,
}: SubmitTransitionTraceFinalConfig): Promise<
  | {
      readonly kind: "checkpoint";
      readonly continuation: Pick<
        SubmitTransitionTraceFinalConfig,
        "threadOutRef" | "depositOpening"
      >;
    }
  | {
      readonly kind: "final";
      readonly result: SubmitTransitionTraceFinalResult;
    }
> => {
  const proof = readTransitionProof(proofInput);
  const {
    deploymentInfo: parsedDeploymentInfo,
    transitionTraceCategory,
    hubOraclePolicyId,
    contracts,
  } = await resolveTransitionTraceDeploymentContracts({
    blueprint,
    deploymentInfo,
    network,
    requireFraudProofSpend: true,
  });
  const computedHeaderHash = await Effect.runPromise(
    hashBlockHeader(proof.header),
  );
  if (computedHeaderHash !== proof.challenged_header_hash) {
    throw transitionTraceError(
      "submissionRejected",
      "Transition-trace final proof changed its challenged header.",
    );
  }
  const finalIndex = transitionTraceFinalIndex(proof);
  const [threadUtxo, hubOracleUtxo, finalReferenceScript] = await Promise.all([
    fetchUtxoByOutRef({
      lucid,
      outRef: parseOutRef(threadOutRef, "transition-trace route out-ref"),
      label: "routed transition-trace computation-thread UTxO",
    }),
    requireSingletonUtxo({
      lucid,
      address: credentialToAddress(
        network,
        scriptHashToCredential(hubOraclePolicyId),
      ),
      unit: toUnit(hubOraclePolicyId, HUB_ORACLE_ASSET_NAME),
      label: "hub oracle",
    }),
    requireDeploymentReferenceScript({
      lucid,
      deploymentInfo: parsedDeploymentInfo,
      name: TRANSITION_TRACE_FINAL_REFERENCE_SCRIPT_ENTRIES[finalIndex]!,
    }),
  ]);
  const finalValidator = contracts.transitionTrace.finals[finalIndex]!;
  if (threadUtxo.address !== finalValidator.spendingScriptAddress) {
    throw transitionTraceError(
      "submissionRejected",
      "Transition-trace routed thread is not at its proof-selected final.",
    );
  }
  if (threadUtxo.datum == null) {
    throw transitionTraceError(
      "submissionRejected",
      "Transition-trace routed thread omitted its inline proof datum.",
    );
  }
  const checkpoint =
    finalIndex === 4 || finalIndex === 5
      ? Data.from(threadUtxo.datum, TransitionTraceProofCommitmentDatum)
      : null;
  if (
    checkpoint === null
      ? aikenSerialisedPlutusDataCborPreservingMapOrder(threadUtxo.datum) !==
        aikenSerialisedPlutusDataCborPreservingMapOrder(
          transitionTraceRouteDatum(proofInput, signer.paymentKeyHash),
        )
      : checkpoint.fraud_prover !== signer.paymentKeyHash ||
        checkpoint.data === null ||
        checkpoint.data.proof_commitment.hash !==
          transitionTraceProofChunks(proofInput).hash ||
        checkpoint.data.kind !== initialTransitionTraceState(proofInput).kind
  ) {
    throw transitionTraceError(
      "submissionRejected",
      "Transition-trace routed thread changed its prover or proof commitment.",
    );
  }
  signer.selectWallet(lucid);
  const proofReferences =
    finalIndex === 4 || finalIndex === 5
      ? await resolveTransitionTraceProofCarriage({
          lucid,
          proof: proofInput,
          publish: false,
        })
      : [];
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: transitionTraceCategory.categoryId,
    categoryLabel: "transition-trace",
  });
  if (threadToken.fraudulentHeaderHash !== proof.challenged_header_hash) {
    throw transitionTraceError(
      "submissionRejected",
      "Transition-trace final proof targets another computation thread.",
    );
  }
  signer.selectWallet(lucid);
  const fraudProofUnit = toUnit(
    contracts.fraudProof.policyId,
    threadToken.assetName,
  );
  const fraudProofDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash },
    FraudProofTokenDatum,
  );
  if (hubOracleUtxo.datum == null)
    throw new Error("transition-trace hub oracle omitted datum");
  const depositWitness =
    "InvalidOneStepTransition" in proof.fault &&
    "ValidDepositTransition" in proof.fault.InvalidOneStepTransition.witness
      ? proof.fault.InvalidOneStepTransition.witness.ValidDepositTransition
      : null;
  const hubDatum = Data.from(hubOracleUtxo.datum, HubOracleDatum);
  const depositPolicyId = hubDatum.deposit;
  let retainedDeposit = depositOpening;
  let depositReference: UTxO | undefined;
  let depositExternalReference: UTxO | undefined;
  if (depositWitness !== null) {
    if (
      checkpoint?.data?.phase === 0n ||
      (retainedDeposit === undefined &&
        checkpoint?.data?.deposit_source_cbor === "")
    ) {
      const history = contracts.transitionTrace.history;
      const current = await fetchCurrentHistory({
        lucid,
        network,
        hubOraclePolicyId,
        history: {
          ...history,
          retentionAddress: history.retentionAddresses.deposit,
        },
        kind: "Deposit",
        id: depositWitness.source_membership.key,
      });
      if (
        current.hubOracleUtxo.txHash !== hubOracleUtxo.txHash ||
        current.hubOracleUtxo.outputIndex !== hubOracleUtxo.outputIndex
      )
        throw new Error(
          "Transition history hub changed during capture; refresh the transaction",
        );
      if (current.witness.kind !== "Present" || current.captured === undefined)
        throw new Error(
          "Transition deposit capture requires its authenticated Order",
        );
      const captured = transitionDepositOpening(current.captured);
      if (retainedDeposit !== undefined)
        reopenTransitionDeposit(
          retainedDeposit,
          depositPolicyId,
          depositSourceView(depositWitness.source_membership),
          captured.commitmentCbor,
        );
      retainedDeposit = captured;
      if (checkpoint?.data?.phase === 0n) {
        depositReference = current.witness.anchor.utxo;
        depositExternalReference = current.witness.retainedDataUtxo;
      }
    }
    if (retainedDeposit === undefined)
      throw new Error(
        "Transition deposit continuation requires the persisted opening from capture",
      );
    reopenTransitionDeposit(
      retainedDeposit,
      depositPolicyId,
      depositSourceView(depositWitness.source_membership),
      checkpoint?.data?.deposit_source_cbor || undefined,
    );
  }
  const timedTarget = transitionTraceHistoryTimingTarget(proof);
  let eventReference:
    | { readonly order: UTxO; readonly external?: UTxO }
    | undefined;
  if (timedTarget !== null) {
    const history = contracts.transitionTrace.history;
    const current = await fetchCurrentHistory({
      lucid,
      network,
      hubOraclePolicyId,
      history: {
        ...history,
        retentionAddress:
          timedTarget.kind === "Deposit"
            ? history.retentionAddresses.deposit
            : history.retentionAddresses.withdrawal,
      },
      kind: timedTarget.kind,
      id: timedTarget.id,
    });
    if (
      current.hubOracleUtxo.txHash !== hubOracleUtxo.txHash ||
      current.hubOracleUtxo.outputIndex !== hubOracleUtxo.outputIndex
    )
      throw new Error(
        "Timed transition hub changed during capture; refresh the transaction",
      );
    if (current.witness.kind !== "Present")
      throw new Error(
        "Timed transition capture requires its current authenticated Order",
      );
    eventReference = {
      order: current.witness.anchor.utxo,
      ...(current.witness.retainedDataUtxo === undefined
        ? {}
        : { external: current.witness.retainedDataUtxo }),
    };
  }
  if (finalIndex === 6 && timedTarget === null) {
    const fault = proof.fault;
    const forced =
      "OmittedDueL1Event" in fault &&
      "OmittedDueForcedTransaction" in fault.OmittedDueL1Event.witness
        ? fault.OmittedDueL1Event.witness.OmittedDueForcedTransaction
        : "OutOfWindowSourceEvent" in fault &&
            "OutOfWindowForcedTransaction" in
              fault.OutOfWindowSourceEvent.witness
          ? fault.OutOfWindowSourceEvent.witness.OutOfWindowForcedTransaction
          : null;
    if (forced === null)
      throw new Error("Timed transition final has no supported event witness");
    eventReference = {
      order: await requireSingletonUtxo({
        lucid,
        address: governedUserEventAddress(network, hubDatum.tx_order_addr),
        unit: toUnit(hubDatum.tx_order, forced.event_asset_name),
        label: "timed transition forced order",
      }),
    };
  }
  const allYieldData = transitionTraceYieldData({
    proof: proofInput,
    network,
    depositPolicyId,
    depositOpening: retainedDeposit,
  });
  const continuationReferences =
    eventReference !== undefined
      ? [
          eventReference.order,
          ...(eventReference.external === undefined
            ? []
            : [eventReference.external]),
        ]
      : depositWitness === null
        ? additionalReferenceInputs
        : depositReference === undefined
          ? []
          : [
              depositReference,
              ...(depositExternalReference === undefined
                ? []
                : [depositExternalReference]),
            ];
  const outputCbors = allYieldData.find(
    (item) => item.outputCbors !== undefined,
  )?.outputCbors;
  // Only the phases that read the selected output's bytes carry its chunks.
  const selectedOutput =
    checkpoint?.data?.output_hash &&
    [2n, 3n, 7n, 8n, 10n].includes(checkpoint.data.phase)
      ? outputCbors?.[Number(checkpoint.data.output_index)]
      : undefined;
  const outputReferences =
    selectedOutput === undefined
      ? []
      : await resolveTransitionTraceByteCarriage({
          lucid,
          chunks: transitionTraceByteChunks(selectedOutput),
          publish: preSubmitBoundary === undefined,
        });
  const selected =
    checkpoint?.data == null
      ? null
      : transitionTracePhaseYield(checkpoint.data);
  const selectedData =
    finalIndex === 6
      ? allYieldData
      : selected === null
        ? []
        : [
            allYieldData.find((item) => item.key === selected) ?? {
              key: selected,
              redeemer: Data.void(),
            },
          ];
  const semanticYields = await Promise.all(
    selectedData.map(async (item) => ({
      ...item,
      reference: await requireDeploymentReferenceScript({
        lucid,
        deploymentInfo: parsedDeploymentInfo,
        name: TRANSITION_TRACE_YIELD_REFERENCES[item.key].entry,
      }),
      validator: contracts.transitionTrace.yields[item.key],
    })),
  );
  if (
    checkpoint?.data != null &&
    !transitionTracePhaseIsTerminal(checkpoint.data)
  ) {
    const semantic = semanticYields[0];
    const fieldStep = await prepareTransitionFieldStep({
      lucid,
      network,
      signer,
      deploymentInfo: parsedDeploymentInfo,
      contracts,
      proof: proofInput,
      state: checkpoint.data,
      yields: allYieldData,
      outputCbors,
      publish: preSubmitBoundary === undefined,
      fieldCertificateReferenceScript,
      ...(ownOutputs === undefined ? {} : { ownOutputs }),
      baseReferences: [
        hubOracleUtxo,
        finalReferenceScript,
        ...(semantic === undefined ? [] : [semantic.reference]),
        ...continuationReferences,
        ...proofReferences,
        ...outputReferences,
      ],
    });
    // The next state and the verifier claims read no opening; the step
    // redeemer is re-planned below against the final reference inputs.
    const planned = fieldStep.plan(fieldStep.referenceInputs);
    const nextDatum = Data.to(
      { fraud_prover: signer.paymentKeyHash, data: planned.state },
      TransitionTraceProofCommitmentDatum,
    );
    let nextIndex: bigint | undefined;
    const redeemer = ((ctx) => {
      nextIndex = requireUniqueOutputIndex(
        ctx.outputs,
        outputWithDatumAndUnitPredicate({
          address: threadUtxo.address,
          datum: nextDatum,
          unit: threadToken.unit,
        }),
        "transition checkpoint output",
      );
      const encoded = Data.to(
        {
          Continue: [
            {
              input_index: requireInputIndex(
                ctx,
                threadUtxo,
                "transition checkpoint",
              ),
              output_index: nextIndex,
              hub_ref_input_index: requireReferenceInputIndex(
                ctx,
                hubOracleUtxo,
                "transition hub",
              ),
              fraud_proof_mint_redeemer_index: 0n,
              deposit_event_ref_index:
                depositReference === undefined ||
                continuationReferences.length === 0
                  ? 0n
                  : requireReferenceInputIndex(
                      ctx,
                      depositReference,
                      "transition deposit event",
                    ),
              deposit_external_ref_index:
                depositExternalReference === undefined
                  ? null
                  : requireReferenceInputIndex(
                      ctx,
                      depositExternalReference,
                      "transition retained deposit data",
                    ),
              completed_fraud_witness: null,
              // The opening's own bytes replace this placeholder below: a
              // Lucid Data value cannot hold every constructor index.
              deposit_opening:
                retainedDeposit !== undefined &&
                [7n, 10n].includes(checkpoint.data!.phase)
                  ? 0n
                  : null,
              output_ref_indices: outputReferences.map((utxo) =>
                requireReferenceInputIndex(
                  ctx,
                  utxo,
                  "transition output chunk",
                ),
              ),
              yield_ref_input_indices:
                semantic === undefined
                  ? []
                  : [
                      requireReferenceInputIndex(
                        ctx,
                        semantic.reference,
                        "transition yield",
                      ),
                    ],
              proof_ref_indices: proofReferences.map((utxo) =>
                requireReferenceInputIndex(ctx, utxo, "transition proof chunk"),
              ),
            },
          ],
        },
        TransitionTraceYieldFinalSpendRedeemer,
      );
      return retainedDeposit !== undefined &&
        [7n, 10n].includes(checkpoint.data!.phase)
        ? replacePlutusConstrFieldCbor(
            encoded,
            [0, 9, 0],
            retainedDeposit.openingCbor,
          )
        : encoded;
    }) satisfies BuildTxWithRedeemer;
    let checkpointBuilder = lucid
      .newTx()
      .collectFrom([selectFeeInput(await lucid.wallet().getUtxos())])
      .collectFrom([threadUtxo], redeemer)
      .readFrom([...fieldStep.referenceInputs]);
    if (depositWitness !== null) {
      const { node } = await resolveCompletedFraudQueueNode({
        lucid,
        hubOraclePolicyId,
        stateQueuePolicyId: hubDatum.state_queue,
        headerHash: proof.challenged_header_hash,
        headerEnd: proof.header.endTime,
      });
      const validity = fabricatedProofValidity({
        nodeBlockTime: node.block_time,
        now: lucid.slotToUnixTime(lucid.currentSlot()),
      });
      checkpointBuilder = checkpointBuilder
        .validFrom(validity.validFrom)
        .validTo(validity.validTo);
    }
    if (semantic !== undefined)
      checkpointBuilder = checkpointBuilder.withdraw(
        validatorToRewardAddress(network, semantic.validator.withdrawalScript),
        0n,
        ((ctx) =>
          fieldStep.plan(ctx.referenceInputs)
            .redeemer) satisfies BuildTxWithRedeemer,
      );
    checkpointBuilder = fieldStep.withdraw(checkpointBuilder, planned);
    const completed = await checkpointBuilder.pay
      .ToContract(
        threadUtxo.address,
        { kind: "inline", value: nextDatum },
        { lovelace: threadUtxo.assets.lovelace ?? 0n, [threadToken.unit]: 1n },
      )
      .addSignerKey(signer.paymentKeyHash)
      .complete({ localUPLCEval: true })
      .catch((cause: unknown) => {
        throw transitionTraceError(
          "submissionRejected",
          `Transition checkpoint ${checkpoint.data?.kind}/${checkpoint.data?.phase} failed: ${formatUnknownError(cause)}`,
          cause,
        );
      });
    const signed = await completed.sign.withWallet().complete();
    await reachFraudProofPreSubmitBoundary({
      signed,
      referenceScripts: workflowReferenceScriptsUsedByTransaction({
        signed,
        candidates: [
          {
            role: `V1 fraud-proof transition-trace final-${finalIndex.toString()}`,
            utxo: finalReferenceScript,
            expectedScript: finalValidator.spendingScript,
          },
          ...(semantic === undefined
            ? []
            : [
                {
                  role: TRANSITION_TRACE_YIELD_REFERENCES[semantic.key].role,
                  utxo: semantic.reference,
                  expectedScript: semantic.validator.withdrawalScript,
                },
              ]),
          ...fieldStep.referenceScripts,
        ],
      }),
      boundary: preSubmitBoundary,
    });
    const txHash = await signed.submit().catch((cause: unknown) => {
      throw transitionTraceError(
        "submissionRejected",
        `Transition checkpoint ${checkpoint.data?.kind}/${checkpoint.data?.phase} submission failed: ${formatUnknownError(cause)}`,
        cause,
      );
    });
    await lucid.awaitTx(txHash);
    if (nextIndex === undefined)
      throw new Error("transition checkpoint layout unresolved");
    return {
      kind: "checkpoint",
      continuation: {
        threadOutRef: `${txHash}#${nextIndex.toString()}`,
        depositOpening: retainedDeposit,
      },
    };
  }

  const completedFraud =
    depositWitness === null && finalIndex !== 6
      ? undefined
      : await prepareFabricatedCompletedFraud({
          lucid,
          hubOraclePolicyId,
          stateQueuePolicyId: hubDatum.state_queue,
          headerHash: proof.challenged_header_hash,
          headerEnd: proof.header.endTime,
          proofAsset: threadToken.assetName,
          queueReferenceScript: await requireDeploymentReferenceScript({
            lucid,
            deploymentInfo: parsedDeploymentInfo,
            name: "stateQueueSpend",
          }),
          now: lucid.slotToUnixTime(lucid.currentSlot()),
        });
  let spendLayout: TransitionTraceFinalSpendLayout | undefined;
  let computationThreadMintRedeemerIndex: bigint | undefined;
  const computationThreadMintCarriage = witnessMintingPolicyCarriage({
    script: contracts.computationThread.mintingScript,
    referenceUtxo: witnessReferenceScripts?.computationThreadMint,
    label: "transition-trace computation-thread mint",
  });
  const fraudProofMintCarriage = witnessMintingPolicyCarriage({
    script: contracts.fraudProof.mintingScript,
    referenceUtxo: witnessReferenceScripts?.fraudProofMint,
    label: "transition-trace fraud-proof mint",
  });
  const finalFeeInput = selectFeeInput(await lucid.wallet().getUtxos());
  let base = lucid
    .newTx()
    .collectFrom([finalFeeInput])
    .collectFrom(
      [threadUtxo],
      makeTransitionTraceFinalSpendRedeemer({
        threadUtxo,
        hubOracleUtxo,
        fraudProofAddress: contracts.fraudProof.spendingScriptAddress,
        fraudProofPolicyId: contracts.fraudProof.policyId,
        fraudProofUnit,
        fraudProofDatum,
        yieldReferences: semanticYields.map((item) => item.reference),
        proofReferences,
        completedFraudWitness: completedFraud?.witness,
        l1Event: finalIndex === 6,
        eventReference,
        onLayout: (resolved) => {
          spendLayout = resolved;
        },
      }),
    )
    .readFrom([
      hubOracleUtxo,
      finalReferenceScript,
      ...continuationReferences,
      ...semanticYields.map((item) => item.reference),
      ...proofReferences,
      ...computationThreadMintCarriage.referenceInputs,
      ...fraudProofMintCarriage.referenceInputs,
    ])
    .mintAssets(
      { [threadToken.unit]: -1n },
      makeComputationThreadSuccessRedeemer({
        computationThreadPolicyId: contracts.computationThread.policyId,
        computationThreadAssetName: threadToken.assetName,
      }),
    )
    .mintAssets(
      { [fraudProofUnit]: 1n },
      makeFraudProofMintRedeemer({
        fraudProofPolicyId: contracts.fraudProof.policyId,
        computationThreadPolicyId: contracts.computationThread.policyId,
        computationThreadAssetName: threadToken.assetName,
        onComputationThreadMintRedeemerIndex: (index) => {
          computationThreadMintRedeemerIndex = index;
        },
      }),
    )
    .pay.ToContract(
      contracts.fraudProof.spendingScriptAddress,
      { kind: "inline", value: fraudProofDatum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [fraudProofUnit]: 1n,
      },
    )
    .addSignerKey(signer.paymentKeyHash);
  if (completedFraud !== undefined) base = completedFraud.apply(base);
  for (const item of semanticYields)
    base.withdraw(
      validatorToRewardAddress(network, item.validator.withdrawalScript),
      0n,
      item.redeemer,
    );
  const unsigned = await fraudProofMintCarriage
    .attach(computationThreadMintCarriage.attach(base))
    .complete({ localUPLCEval: true });
  if (
    spendLayout === undefined ||
    computationThreadMintRedeemerIndex === undefined
  ) {
    throw transitionTraceError(
      "submissionRejected",
      "Transition-trace final layout was not resolved.",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundary({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransaction({
      signed,
      candidates: [
        ...(completedFraud?.queueReferenceScript === undefined
          ? []
          : [
              {
                role: "state queue spending",
                utxo: completedFraud.queueReferenceScript,
                expectedScript: completedFraud.queueReferenceScript.scriptRef!,
              },
            ]),
        ...semanticYields.map((item) => ({
          role: TRANSITION_TRACE_YIELD_REFERENCES[item.key].role,
          utxo: item.reference,
          expectedScript: item.validator.withdrawalScript,
        })),
        {
          role: `V1 fraud-proof transition-trace final-${finalIndex.toString()}`,
          utxo: finalReferenceScript,
          expectedScript: finalValidator.spendingScript,
        },
        {
          role: "V1 fraud-proof computation-thread minting",
          utxo: witnessReferenceScripts?.computationThreadMint,
          expectedScript: contracts.computationThread.mintingScript,
        },
        {
          role: "V1 fraud-proof token minting",
          utxo: witnessReferenceScripts?.fraudProofMint,
          expectedScript: contracts.fraudProof.mintingScript,
        },
      ],
    }),
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw transitionTraceError(
      "submissionRejected",
      "Transition-trace final provider changed the signed transaction hash.",
    );
  }
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  const result = Object.freeze({
    txHash,
    inputIndex: Number(spendLayout.inputIndex),
    outputIndex: Number(spendLayout.outputIndex),
    hubOracleRefInputIndex: Number(spendLayout.hubOracleRefInputIndex),
    computationThreadMintRedeemerIndex: Number(
      computationThreadMintRedeemerIndex,
    ),
    fraudProofMintRedeemerIndex: Number(
      spendLayout.fraudProofMintRedeemerIndex,
    ),
    fraudProofOutRef: `${txHash}#${spendLayout.outputIndex.toString()}`,
    fraudulentHeaderHash: proof.challenged_header_hash,
    computationThreadUnit: threadToken.unit,
    fraudProofUnit,
    finalIndex,
    awaitedConfirmation: awaitConfirmation,
  });
  return { kind: "final", result };
};
