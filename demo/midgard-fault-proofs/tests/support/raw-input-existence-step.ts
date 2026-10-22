/** Raw adversarial producer: current physical ABI and carriage, without local
 * durable admission. Validator refusals must remain observable. */
import * as SDK from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  credentialToAddress,
  Data,
  type LucidEvolution,
  type Network,
  type Script,
  scriptHashToCredential,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";

import { faultProofFieldOpening } from "../../src/field-opening.js";
import type { PreparedInputExistence } from "../../src/input-existence/prepare.js";
import {
  type InputExistenceContracts,
  inputExistenceStates,
  type InputExistenceStepCarriagePlan,
  prepareInputExistenceStepCarriage,
} from "../../src/input-existence/prepare-step-carriage.js";
import {
  requireLinearFaultReferenceScript,
  requireLinearFaultThreadUtxo,
} from "../../src/linear-fault-family.js";
import { prepareNativeTxInclusionCarriage } from "../../src/native-inclusion-carriage.js";
import {
  builtChunkReferenceIndices,
  type PublishedProofChunk,
} from "../../src/proof-chunk-carriage.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  fetchUtxoByOutRef,
  parseOutRef,
  requireSingletonUtxo,
  type ResolvedProverSigner,
} from "../../src/runtime.js";
import { excludeUtxo } from "../../src/spend-input-witness.js";
import {
  requireInitialStepDatum,
  selectFeeInput,
} from "../../src/step-support.js";
import { computationThreadOutputPredicate } from "../../src/tx-layout.js";
import { type FaultProofWitnessReferenceScripts } from "../../src/witness-reference-scripts.js";
import { admitNativeInclusionArtifact } from "../../src/workflow/native-index-artifact.js";
import {
  type FraudProofPreSubmitBoundary,
  reachFraudProofPreSubmitBoundary,
  workflowReferenceScriptsUsedByTransaction,
} from "../../src/workflow/transaction-boundary.js";

export const submitRawInputExistenceStep = async ({
  lucid,
  blueprint,
  network,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  prepared,
  stepIndex,
  stateQueueBlockOutRef,
  referenceScripts,
  witnessReferenceScripts,
  fieldCarriageUtxos = [],
  certificatePolicyId,
  publishedProofChunks = [],
  preSubmitBoundary,
  awaitConfirmation = true,
  mutateArgs,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly network: Network;
  readonly contracts: InputExistenceContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly prepared: PreparedInputExistence;
  readonly stepIndex: 0 | 1 | 2 | 3;
  readonly stateQueueBlockOutRef?: string;
  readonly referenceScripts: {
    readonly steps: readonly UTxO[];
    readonly computationThreadMint: UTxO;
    readonly fraudProofMint: UTxO;
  };
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  readonly fieldCarriageUtxos?: readonly UTxO[];
  readonly certificatePolicyId?: string;
  readonly publishedProofChunks?: readonly PublishedProofChunk[];
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
  readonly mutateArgs?: (args: unknown) => unknown;
}) => {
  const family =
    prepared.field === 0 ? "non-existent-input" : "no-reference-input";
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family,
    stepIndex,
    threadOutRef,
  });
  const states = inputExistenceStates(prepared);
  if (threadToken.fraudulentHeaderHash !== prepared.source.headerHash) {
    throw new Error(`${family}: prepared header differs from the proof thread`);
  }
  const datumSchemas =
    prepared.field === 0
      ? ([
          SDK.NonExistentInputStep02ThreadDatum,
          SDK.NonExistentInputStep03ThreadDatum,
          SDK.NonExistentInputStep04ThreadDatum,
        ] as const)
      : ([
          SDK.NoReferenceInputStep02ThreadDatum,
          SDK.NoReferenceInputStep03ThreadDatum,
          SDK.NoReferenceInputStep04ThreadDatum,
        ] as const);
  const redeemerSchemas =
    prepared.field === 0
      ? ([
          SDK.NonExistentInputStep01SpendRedeemer,
          SDK.NonExistentInputStep02ThreadSpendRedeemer,
          SDK.NonExistentInputStep03ThreadSpendRedeemer,
          SDK.NonExistentInputStep04ThreadSpendRedeemer,
        ] as const)
      : ([
          SDK.NoReferenceInputStep01SpendRedeemer,
          SDK.NoReferenceInputStep02ThreadSpendRedeemer,
          SDK.NoReferenceInputStep03ThreadSpendRedeemer,
          SDK.NoReferenceInputStep04ThreadSpendRedeemer,
        ] as const);
  if (stepIndex === 0) requireInitialStepDatum({ threadUtxo, signer });
  else {
    const expected = Data.to(
      {
        fraud_prover: signer.paymentKeyHash,
        data: states[stepIndex - 1],
      } as never,
      datumSchemas[stepIndex - 1] as never,
    );
    if (
      threadUtxo.datum == null ||
      Data.to(Data.from(threadUtxo.datum)) !== Data.to(Data.from(expected))
    ) {
      throw new Error(`${family}: thread state changed`);
    }
  }
  signer.selectWallet(lucid);
  const step = contracts.steps[stepIndex]!;
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScripts.steps[stepIndex]!,
    expectedScriptHash: step.spendingScriptHash,
    family,
    stepIndex,
  });
  const candidates: {
    role: string;
    utxo: UTxO | undefined;
    expectedScript: Script;
  }[] = [
    {
      role: `${family} step${stepIndex + 1}`,
      utxo: stepReference,
      expectedScript: step.spendingScript,
    },
  ];
  let referenceInputs: readonly UTxO[] = [stepReference];
  if (stepIndex === 3) {
    const ct = requireLinearFaultReferenceScript({
      utxo: referenceScripts.computationThreadMint,
      expectedScriptHash: contracts.computationThread.policyId,
      family,
      stepIndex,
    });
    const fp = requireLinearFaultReferenceScript({
      utxo: referenceScripts.fraudProofMint,
      expectedScriptHash: contracts.fraudProof.policyId,
      family,
      stepIndex,
    });
    referenceInputs = [...referenceInputs, ct, fp];
    candidates.push(
      {
        role: "computation thread mint",
        utxo: ct,
        expectedScript: contracts.computationThread.mintingScript,
      },
      {
        role: "fraud proof mint",
        utxo: fp,
        expectedScript: contracts.fraudProof.mintingScript,
      },
    );
  }
  let attach: (
    tx: ReturnType<LucidEvolution["newTx"]>,
  ) => ReturnType<LucidEvolution["newTx"]> = (tx) => tx;
  let sourceCarriage:
    | ReturnType<typeof prepareNativeTxInclusionCarriage>
    | undefined;
  let hub: UTxO | undefined;
  let queue: UTxO | undefined;
  let eventCarriage: InputExistenceStepCarriagePlan["eventCarriage"];
  let transitionCarriage: InputExistenceStepCarriagePlan["transitionCarriage"];
  let absenceAddress: string | undefined;
  let absencePublished = false;
  if (stepIndex === 0 && prepared.source.kind === "accepted") {
    if (stateQueueBlockOutRef === undefined)
      throw new Error(`${family}: accepted source needs state-queue reference`);
    queue = await fetchUtxoByOutRef({
      lucid,
      outRef: parseOutRef(stateQueueBlockOutRef, family),
      label: family,
    });
    hub = await requireSingletonUtxo({
      lucid,
      address: credentialToAddress(
        network,
        scriptHashToCredential(contracts.hubOraclePolicyId),
      ),
      unit: toUnit(contracts.hubOraclePolicyId, SDK.HUB_ORACLE_ASSET_NAME),
      label: `${family} hub oracle`,
    });
    sourceCarriage = prepareNativeTxInclusionCarriage({
      blueprint,
      network,
      txInclusion: admitNativeInclusionArtifact(
        prepared.source.inclusion,
        family,
      ).inclusion,
      publishedProofChunks,
      witnessReferenceScripts,
      baseReferenceInputs: [queue, hub, stepReference],
      label: family,
    });
    referenceInputs = sourceCarriage.referenceInputs;
    attach = sourceCarriage.attachWithdrawal;
    candidates.push(...sourceCarriage.referenceScriptCandidates);
  } else {
    ({
      referenceInputs,
      attach,
      eventCarriage,
      transitionCarriage,
      absenceAddress,
      absencePublished,
    } = prepareInputExistenceStepCarriage({
      stepIndex,
      prepared,
      blueprint,
      network,
      stepReference,
      fieldCarriageUtxos,
      publishedProofChunks,
      witnessReferenceScripts,
      family,
      referenceInputs,
      candidates,
    }));
  }
  const wallet = await lucid.wallet().getUtxos();
  const usable = referenceInputs.reduce<readonly UTxO[]>(
    (xs, input) => excludeUtxo(xs, input),
    wallet,
  );
  let outputIndex: bigint | undefined;
  const final = stepIndex === 3;
  const proofUnit = toUnit(
    contracts.fraudProof.policyId,
    threadToken.assetName,
  );
  const nextAddress = final
    ? contracts.fraudProof.spendingScriptAddress
    : contracts.steps[stepIndex + 1]!.spendingScriptAddress;
  const datum = final
    ? Data.to({ fraud_prover: signer.paymentKeyHash }, SDK.FraudProofTokenDatum)
    : Data.to(
        {
          fraud_prover: signer.paymentKeyHash,
          data: states[stepIndex],
        } as never,
        datumSchemas[stepIndex] as never,
      );
  const matches = computationThreadOutputPredicate({
    address: nextAddress,
    datum,
    unit: final ? proofUnit : threadToken.unit,
  });
  const spend = ((ctx) => {
    SDK.requireOwnSpendPurpose(ctx, threadUtxo, family);
    const input_index = SDK.requireInputIndex(ctx, threadUtxo, family);
    outputIndex = SDK.requireUniqueOutputIndex(ctx.outputs, matches, family);
    let args: unknown;
    if (stepIndex === 0) {
      args = {
        source:
          prepared.source.kind === "forced"
            ? {
                ForcedSource: {
                  header: prepared.source.header,
                  compact: prepared.compact,
                  membership: prepared.source.membership,
                  input_index,
                  output_index: outputIndex,
                },
              }
            : {
                AcceptedSource: {
                  inclusion: sourceCarriage!.redeemer(ctx, {
                    input_index,
                    output_index: outputIndex,
                    hub_ref_input_index: SDK.requireReferenceInputIndex(
                      ctx,
                      hub!,
                      family,
                    ),
                    state_queue_node_ref_input_index:
                      SDK.requireReferenceInputIndex(ctx, queue!, family),
                  }),
                },
              },
      };
    } else if (stepIndex === 1) {
      args = {
        input_index,
        output_index: outputIndex,
        selected_input_index: prepared.inputIndex,
        inputs_opening: faultProofFieldOpening({
          planned: prepared.fieldPlan,
          referenceInputs: ctx.referenceInputs,
          certificatePolicyId,
          label: family,
        }),
        claimed_inputs_opening:
          prepared.claimedFieldPlan === null
            ? null
            : faultProofFieldOpening({
                planned: prepared.claimedFieldPlan,
                referenceInputs: ctx.referenceInputs,
                certificatePolicyId,
                label: `${family} claimed input`,
              }),
        event_membership: eventCarriage!.redeemer(ctx),
      };
    } else if (stepIndex === 2) {
      args = {
        input_index,
        output_index: outputIndex,
        transition_membership: transitionCarriage!.redeemer(ctx),
      };
    } else {
      const evidence = prepared.ledgerEvidence;
      let ledger_evidence: SDK.InputExistenceLedgerEvidence;
      if (evidence === "OutOfDomain") ledger_evidence = evidence;
      else if ("Present" in evidence)
        ledger_evidence = {
          Present: {
            value_hash: evidence.Present.value_hash,
            proof: evidence.Present.proof,
          },
        };
      else {
        ledger_evidence = {
          Absent: {
            non_membership: absencePublished
              ? {
                  PublishedChunkNonMembership: [
                    {
                      ordered_chunk_reference_input_indices:
                        builtChunkReferenceIndices({
                          ctx,
                          chunks: publishedProofChunks,
                          label: family,
                        }),
                    },
                  ],
                }
              : {
                  RedeemerCarriedNonMembership: {
                    non_membership_proof: evidence.Absent.proof,
                    non_membership_proof_script_redeemer_index:
                      SDK.requireWithdrawalRedeemerIndex(
                        ctx,
                        absenceAddress!,
                        family,
                      ),
                  },
                },
          },
        };
      }
      args = {
        input_index,
        output_index: outputIndex,
        ledger_evidence,
        fraud_proof_mint_redeemer_index: SDK.requireMintRedeemerIndex(
          ctx,
          contracts.fraudProof.policyId,
          family,
        ),
      };
    }
    return Data.to(
      {
        Continue: [mutateArgs === undefined ? args : mutateArgs(args)],
      } as never,
      redeemerSchemas[stepIndex] as never,
    );
  }) satisfies BuildTxWithRedeemer;
  let tx = lucid
    .newTx()
    .collectFrom([selectFeeInput(usable)])
    .collectFrom([threadUtxo], spend);
  if (final) {
    tx = tx
      .mintAssets(
        { [threadToken.unit]: -1n },
        Data.to(
          { Success: { burning_token_asset_name: threadToken.assetName } },
          SDK.FraudProofComputationThreadRedeemer,
        ),
      )
      .mintAssets({ [proofUnit]: 1n }, ((ctx) => {
        SDK.requireOwnMintPurpose(ctx, contracts.fraudProof.policyId, family);
        return Data.to(
          {
            computation_thread_token_asset_name: threadToken.assetName,
            computation_thread_mint_redeemer_index:
              SDK.requireMintRedeemerIndex(
                ctx,
                contracts.computationThread.policyId,
                family,
              ),
          },
          SDK.FraudProofTokenMintRedeemer,
        );
      }) satisfies BuildTxWithRedeemer);
  }
  tx = attach(tx.readFrom([...referenceInputs]))
    .pay.ToContract(
      nextAddress,
      { kind: "inline", value: datum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [final ? proofUnit : threadToken.unit]: 1n,
      },
    )
    .addSignerKey(signer.paymentKeyHash);
  const unsigned = await tx.complete({
    localUPLCEval: true,
    presetWalletInputs: usable as UTxO[],
  });
  const signed = await unsigned.sign.withWallet().complete();
  const expected = await reachFraudProofPreSubmitBoundary({
    signed,
    boundary: preSubmitBoundary,
    referenceScripts: workflowReferenceScriptsUsedByTransaction({
      signed,
      candidates,
    }),
  });
  const txHash = await signed.submit();
  if (txHash !== expected)
    throw new Error(
      `${family}: provider returned a different transaction hash`,
    );
  if (awaitConfirmation)
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  if (outputIndex === undefined)
    throw new Error(`${family}: unresolved output`);
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${outputIndex}`,
    fraudProofUnit: final ? proofUnit : undefined,
    fraudProofOutRef: final ? `${txHash}#${outputIndex}` : undefined,
    sourceEventKey: prepared.source.eventKey,
    eventStepIndex: prepared.eventMembership.value.step_index,
    eventPreUtxosRoot: prepared.transitionMembership.value.pre_utxos_root,
    selectedInput: prepared.selectedInput,
  };
};
