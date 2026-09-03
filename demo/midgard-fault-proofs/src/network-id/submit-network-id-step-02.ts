/** Open Q35's committed output field (when needed) and mint the proof token. */
import {
  FraudProofComputationThreadRedeemer,
  FraudProofTokenDatum,
  FraudProofTokenMintRedeemer,
  NetworkIdStep02Datum,
  NetworkIdStep02SpendRedeemerSchema,
  type NetworkIdStep02State,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
  requireWithdrawalRedeemerIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type Network,
  type Script,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  faultProofFieldOpening,
  type FaultProofFieldOpeningPlan,
  publishFaultProofFieldCarriage,
} from "../field-opening-v1.js";
import { PEXCLUDES_EXCLUSION_WITHDRAW_TITLE } from "../ne-submit-step-03.js";
import {
  chunkedMembershipClaimRedeemer,
  chunkedNonMembershipClaimRedeemer,
  chunkedVerifyWithdrawalScript,
  derivedChunkReferenceIndices,
  type PublishedProofChunk,
  requireBuiltChunkReferenceIndices,
} from "../proof-chunk-carriage.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  encodeRawPexcludesProofRedeemer,
  encodeRawPhasMembershipProofRedeemer,
  getCompiledScript,
  phasMembershipRewardAddress,
  type ResolvedProverSigner,
} from "../runtime.js";
import { excludeUtxo } from "../spend-input-witness.js";
import {
  PHAS_MEMBERSHIP_WITHDRAW_TITLE,
  selectFeeInput,
} from "../submit-step-01.js";
import { outputWithDatumAndUnitPredicate } from "../tx-layout.js";
import {
  type FaultProofWitnessReferenceScripts,
  witnessMintingPolicyCarriage,
  witnessWithdrawalValidatorCarriage,
} from "../witness-reference-scripts-v1.js";
import {
  type FraudProofPreSubmitBoundary,
  reachFraudProofPreSubmitBoundary,
  workflowReferenceScript,
} from "../workflow/transaction-boundary-v1.js";
import type { NetworkIdContracts } from "./contracts-v1.js";
import type {
  PreparedNetworkIdPostUtxoProof,
  PreparedNetworkIdProof,
} from "./prepare-v1.js";
import {
  networkIdStepLabel,
  networkIdSubmitError,
  requireNetworkIdReferenceScript,
  requireNetworkIdStepState,
  requireNetworkIdThreadUtxo,
} from "./submit-common-v1.js";
import {
  networkIdWrongfulRejectionCloses,
  type PreparedNetworkIdWrongfulRejection,
} from "./wrongful-rejection-v1.js";

const STEP_LABEL = networkIdStepLabel(1);

type PreparedNetworkIdFinalProof =
  | PreparedNetworkIdProof
  | PreparedNetworkIdPostUtxoProof
  | PreparedNetworkIdWrongfulRejection;

const isPostUtxoPrepared = (
  prepared: PreparedNetworkIdFinalProof,
): prepared is PreparedNetworkIdPostUtxoProof =>
  prepared.faultClaim.kind === "post-utxo-network";

const isForcedPrepared = (
  prepared: PreparedNetworkIdFinalProof,
): prepared is PreparedNetworkIdWrongfulRejection =>
  prepared.faultClaim.kind === "forced-network-mismatch";

const uniqueUtxos = (utxos: readonly UTxO[]): readonly UTxO[] => {
  const seen = new Set<string>();
  return utxos.filter((utxo) => {
    const key = `${utxo.txHash}#${utxo.outputIndex.toString()}`;
    if (seen.has(key)) return false;
    seen.add(key);
    return true;
  });
};

const sameFault = (
  left: NetworkIdStep02State["fault"],
  right: NetworkIdStep02State["fault"],
): boolean => {
  if (
    left === "TransactionNetwork" ||
    right === "TransactionNetwork" ||
    left === "ForcedNetworkIdMismatch" ||
    right === "ForcedNetworkIdMismatch"
  ) {
    return left === right;
  }
  if ("OutputNetwork" in left || "OutputNetwork" in right) {
    return (
      "OutputNetwork" in left &&
      "OutputNetwork" in right &&
      left.OutputNetwork.output_index === right.OutputNetwork.output_index
    );
  }
  return (
    left.OutputNetworkUtxo.observed_network_id ===
    right.OutputNetworkUtxo.observed_network_id
  );
};

export type SubmitNetworkIdStep02Result = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly fraudProofOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadUnit: string;
  readonly fraudProofUnit: string;
  readonly fraudProofAddress: string;
  readonly state: NetworkIdStep02State;
  readonly outputOpeningTier: string | null;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly computationThreadMintRedeemerIndex: number;
  readonly fraudProofMintRedeemerIndex: number;
  readonly awaitedConfirmation: boolean;
};

export const submitNetworkIdStep02 = async ({
  lucid,
  blueprint,
  contracts,
  categoryId,
  network,
  signer,
  threadOutRef,
  prepared,
  outputsOpeningPlan,
  referenceScriptUtxo,
  witnessReferenceScripts,
  publishedPredecessorProofChunks,
  certificateUtxos = [],
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  /** Required for the post-UTxO predecessor proof route. */
  readonly blueprint?: unknown;
  readonly contracts: NetworkIdContracts;
  readonly categoryId: string;
  /** Required for the post-UTxO predecessor proof route. */
  readonly network?: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly prepared: PreparedNetworkIdFinalProof;
  /** Required exactly for an OutputNetwork claim. */
  readonly outputsOpeningPlan?: FaultProofFieldOpeningPlan;
  /** Mandatory published Q35 step-02 reference script. */
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
  /** Optional published carriage for the predecessor proof. */
  readonly publishedPredecessorProofChunks?: readonly PublishedProofChunk[];
  /** Existing §8.6 certificate UTxOs, required only for tier 3. */
  readonly certificateUtxos?: readonly UTxO[];
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitNetworkIdStep02Result> => {
  const { threadUtxo, threadToken } = await requireNetworkIdThreadUtxo({
    lucid,
    contracts,
    categoryId,
    stepIndex: 1,
    threadOutRef,
  });
  const state = requireNetworkIdStepState({
    threadUtxo,
    signer,
    schema: NetworkIdStep02Datum,
    stepIndex: 1,
  });
  const postUtxo = isPostUtxoPrepared(prepared);
  const forcedPrepared = isForcedPrepared(prepared) ? prepared : null;
  const forced = forcedPrepared !== null;
  const preparedBadTxId = postUtxo
    ? prepared.outRef.transactionId
    : prepared.badTxId;
  const preparedCommittedNetworkId = postUtxo
    ? 255n
    : forced
      ? forcedPrepared!.evidence.committedNetworkId
      : (prepared as PreparedNetworkIdProof).txInclusion.nativeTx.body
          .network_id;
  if (
    state.bad_tx_id !== preparedBadTxId ||
    state.committed_tx_network_id !== preparedCommittedNetworkId ||
    state.expected_network_id !== prepared.expectedNetworkId ||
    !sameFault(state.fault, prepared.fault)
  ) {
    throw networkIdSubmitError(
      "live step-02 state does not match the authenticated prepared artifact",
    );
  }

  let planned: FaultProofFieldOpeningPlan | undefined;
  if (postUtxo) {
    if (outputsOpeningPlan !== undefined) {
      throw networkIdSubmitError(
        "post-UTxO finalization must not carry a transaction outputs opening",
      );
    }
    if (
      state.fault === "TransactionNetwork" ||
      typeof state.fault !== "object" ||
      !("OutputNetworkUtxo" in state.fault) ||
      state.fault.OutputNetworkUtxo.observed_network_id ===
        state.expected_network_id
    ) {
      throw networkIdSubmitError(
        "live step-02 state does not describe an authenticated post-UTxO network mismatch",
      );
    }
    if (
      state.post_utxo === null ||
      state.post_utxo.out_ref.transactionId !== prepared.outRef.transactionId ||
      state.post_utxo.out_ref.outputIndex !== prepared.outRef.outputIndex ||
      state.post_utxo.descriptor_cbor !== prepared.descriptorCbor ||
      state.post_utxo.prev_utxos_root !== prepared.prevUtxosRoot ||
      (state.post_utxo.predecessor === "Introduced"
        ? prepared.predecessor !== "Introduced"
        : prepared.predecessor === "Introduced" ||
          state.post_utxo.predecessor.NetworkChanged
            .previous_descriptor_cbor !==
            prepared.predecessor.NetworkChanged.previous_descriptor_cbor)
    ) {
      throw networkIdSubmitError(
        "live step-02 state does not match the frozen post-UTxO predecessor claim",
      );
    }
    if (blueprint === undefined || network === undefined) {
      throw networkIdSubmitError(
        "post-UTxO finalization requires the Aiken blueprint and Cardano network",
      );
    }
  } else if (forced) {
    if (
      state.fault !== "ForcedNetworkIdMismatch" ||
      state.forced_source_key === null ||
      state.forced_source_key !== forcedPrepared!.subject.source_key ||
      !networkIdWrongfulRejectionCloses(forcedPrepared!.evidence)
    ) {
      throw networkIdSubmitError(
        "forced step state or retained evidence does not contradict NetworkIdMismatch",
      );
    }
    if (outputsOpeningPlan === undefined) {
      throw networkIdSubmitError(
        "forced NetworkIdMismatch contradiction requires the complete field-2 opening",
      );
    }
    if (
      outputsOpeningPlan.fieldIndex !== 2 ||
      outputsOpeningPlan.nativeTxId !== state.bad_tx_id ||
      outputsOpeningPlan.nativeTxCompactCbor !==
        forcedPrepared!.nativeTxCompactCbor ||
      outputsOpeningPlan.itemCount !==
        forcedPrepared!.evidence.outputNetworkIds.length
    ) {
      throw networkIdSubmitError(
        "forced outputs opening changed the authenticated transaction",
      );
    }
    planned = outputsOpeningPlan;
  } else if (prepared.faultClaim.kind === "transaction-network") {
    if (outputsOpeningPlan !== undefined) {
      throw networkIdSubmitError(
        "transaction-network finalization must not carry an outputs opening",
      );
    }
    if (
      state.committed_tx_network_id === 255n ||
      state.committed_tx_network_id === state.expected_network_id
    ) {
      throw networkIdSubmitError(
        "live step-02 state does not describe an explicit transaction network mismatch",
      );
    }
  } else if (prepared.faultClaim.kind === "output-network") {
    if (outputsOpeningPlan === undefined) {
      throw networkIdSubmitError(
        "output-network finalization requires the authenticated field-2 opening",
      );
    }
    if (
      outputsOpeningPlan.fieldIndex !== 2 ||
      outputsOpeningPlan.nativeTxId !== state.bad_tx_id ||
      outputsOpeningPlan.nativeTxCompactCbor !== prepared.nativeTxCompactCbor
    ) {
      throw networkIdSubmitError(
        "outputs opening is not field 2 of the thread's anchored transaction",
      );
    }
    if (
      prepared.faultClaim.outputIndex < 0n ||
      prepared.faultClaim.outputIndex >= BigInt(outputsOpeningPlan.itemCount)
    ) {
      throw networkIdSubmitError(
        "claimed output index is outside the authenticated outputs opening",
      );
    }
    planned = outputsOpeningPlan;
  } else {
    throw networkIdSubmitError(
      "prepared network-id proof has an unknown authenticated fault kind",
    );
  }

  signer.selectWallet(lucid);
  const predecessorChunks = publishedPredecessorProofChunks ?? [];
  if (!postUtxo && predecessorChunks.length > 0) {
    throw networkIdSubmitError(
      "transaction network finalization must not carry predecessor proof chunks",
    );
  }
  const predecessorCarriedByChunks = predecessorChunks.length > 0;
  const predecessorProofScript: Script | undefined = postUtxo
    ? predecessorCarriedByChunks
      ? chunkedVerifyWithdrawalScript(blueprint as unknown)
      : prepared.predecessor === "Introduced"
        ? {
            type: "PlutusV3",
            script: getCompiledScript(
              blueprint as unknown,
              PEXCLUDES_EXCLUSION_WITHDRAW_TITLE,
            ),
          }
        : {
            type: "PlutusV3",
            script: getCompiledScript(
              blueprint as unknown,
              PHAS_MEMBERSHIP_WITHDRAW_TITLE,
            ),
          }
    : undefined;
  const predecessorRewardAddress =
    predecessorProofScript === undefined
      ? undefined
      : phasMembershipRewardAddress(network as Network, predecessorProofScript);
  const predecessorCarriage =
    predecessorProofScript === undefined
      ? undefined
      : witnessWithdrawalValidatorCarriage({
          script: predecessorProofScript,
          referenceUtxo: predecessorCarriedByChunks
            ? witnessReferenceScripts.chunkedVerifyWithdraw
            : postUtxo && prepared.predecessor === "Introduced"
              ? witnessReferenceScripts.pexcludesWithdraw
              : witnessReferenceScripts.phasMembershipWithdraw,
          label: `${STEP_LABEL} predecessor ${
            predecessorCarriedByChunks
              ? "chunked verify"
              : postUtxo && prepared.predecessor === "Introduced"
                ? "pexcludes exclusion"
                : "PHAS membership"
          }`,
        });
  const published =
    planned === undefined
      ? []
      : await publishFaultProofFieldCarriage({
          lucid,
          signer,
          planned,
          publisherAddress: signer.address,
          label: `${STEP_LABEL} outputs`,
          preSubmitBoundary,
        });
  const stepReference = requireNetworkIdReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[1].spendingScriptHash,
    stepIndex: 1,
  });
  const computationThreadMintCarriage = witnessMintingPolicyCarriage({
    script: contracts.computationThread.mintingScript,
    referenceUtxo: witnessReferenceScripts.computationThreadMint,
    label: `${STEP_LABEL} computation-thread mint`,
  });
  const fraudProofMintCarriage = witnessMintingPolicyCarriage({
    script: contracts.fraudProof.mintingScript,
    referenceUtxo: witnessReferenceScripts.fraudProofMint,
    label: `${STEP_LABEL} fraud-proof mint`,
  });
  // Every positional opening index is derived only after the transaction's
  // complete reference-input set has been assembled.
  const referenceInputs = uniqueUtxos([
    ...published,
    ...certificateUtxos,
    ...predecessorChunks.map((chunk) => chunk.utxo),
    stepReference,
    ...(predecessorCarriage?.referenceInputs ?? []),
    ...computationThreadMintCarriage.referenceInputs,
    ...fraudProofMintCarriage.referenceInputs,
  ]);
  const predecessorChunkIndices = derivedChunkReferenceIndices({
    referenceInputs,
    chunks: predecessorChunks,
    label: `${STEP_LABEL} predecessor`,
  });
  const outputsOpening =
    planned === undefined
      ? null
      : faultProofFieldOpening({
          planned,
          referenceInputs,
          certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
          label: `${STEP_LABEL} outputs`,
        });

  const feeInput = selectFeeInput(
    referenceInputs.reduce<readonly UTxO[]>(
      (candidates, utxo) => excludeUtxo(candidates, utxo),
      await lucid.wallet().getUtxos(),
    ),
  );
  const fraudProofUnit = toUnit(
    contracts.fraudProof.policyId,
    threadToken.assetName,
  );
  const fraudProofDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash },
    FraudProofTokenDatum,
  );
  const proofOutputMatches = outputWithDatumAndUnitPredicate({
    address: contracts.fraudProof.spendingScriptAddress,
    datum: fraudProofDatum,
    unit: fraudProofUnit,
  });
  let spendLayout:
    | {
        readonly inputIndex: bigint;
        readonly outputIndex: bigint;
        readonly fraudProofMintRedeemerIndex: bigint;
      }
    | undefined;
  let computationThreadMintRedeemerIndex: bigint | undefined;
  const spendRedeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, STEP_LABEL);
    spendLayout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, STEP_LABEL),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        proofOutputMatches,
        `${STEP_LABEL} proof output`,
      ),
      fraudProofMintRedeemerIndex: requireMintRedeemerIndex(
        ctx,
        contracts.fraudProof.policyId,
        `${STEP_LABEL} proof mint`,
      ),
    };
    requireBuiltChunkReferenceIndices({
      ctx,
      chunks: predecessorChunks,
      derived: predecessorChunkIndices,
      label: `${STEP_LABEL} predecessor`,
    });
    const encodedPredecessorCarriage = !postUtxo
      ? null
      : prepared.predecessor === "Introduced"
        ? {
            IntroducedPredecessor: [
              predecessorCarriedByChunks
                ? {
                    PublishedChunkNonMembership: [
                      {
                        ordered_chunk_reference_input_indices:
                          predecessorChunkIndices,
                      },
                    ],
                  }
                : {
                    RedeemerCarriedNonMembership: {
                      non_membership_proof: prepared.predecessorProof,
                      non_membership_proof_script_redeemer_index:
                        requireWithdrawalRedeemerIndex(
                          ctx,
                          predecessorRewardAddress as string,
                          `${STEP_LABEL} predecessor exclusion`,
                        ),
                    },
                  },
            ],
          }
        : {
            NetworkChangedPredecessor: [
              predecessorCarriedByChunks
                ? {
                    PublishedChunkMembership: [
                      {
                        ordered_chunk_reference_input_indices:
                          predecessorChunkIndices,
                      },
                    ],
                  }
                : {
                    RedeemerCarriedMembership: {
                      membership_proof: prepared.predecessorProof,
                      membership_proof_script_redeemer_index:
                        requireWithdrawalRedeemerIndex(
                          ctx,
                          predecessorRewardAddress as string,
                          `${STEP_LABEL} predecessor membership`,
                        ),
                    },
                  },
            ],
          };
    return Data.to(
      {
        Continue: [
          {
            input_index: spendLayout.inputIndex,
            output_index: spendLayout.outputIndex,
            fraud_proof_mint_redeemer_index:
              spendLayout.fraudProofMintRedeemerIndex,
            outputs_opening: outputsOpening,
            predecessor_carriage: encodedPredecessorCarriage,
          },
        ],
      } as never,
      NetworkIdStep02SpendRedeemerSchema,
    );
  }) satisfies BuildTxWithRedeemer;
  const threadBurnRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.computationThread.policyId,
      `${STEP_LABEL} computation-thread burn`,
    );
    return Data.to(
      { Success: { burning_token_asset_name: threadToken.assetName } },
      FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const fraudProofMintRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.fraudProof.policyId,
      `${STEP_LABEL} fraud-proof mint`,
    );
    computationThreadMintRedeemerIndex = requireMintRedeemerIndex(
      ctx,
      contracts.computationThread.policyId,
      `${STEP_LABEL} computation-thread burn`,
    );
    return Data.to(
      {
        computation_thread_token_asset_name: threadToken.assetName,
        computation_thread_mint_redeemer_index:
          computationThreadMintRedeemerIndex,
      },
      FraudProofTokenMintRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spendRedeemer)
    .readFrom([...referenceInputs]);
  const withPredecessor = !postUtxo
    ? base
    : predecessorCarriedByChunks
      ? base.withdraw(predecessorRewardAddress as string, 0n, ((_ctx) =>
          prepared.predecessor === "Introduced"
            ? chunkedNonMembershipClaimRedeemer({
                merkleRoot: prepared.prevUtxosRoot,
                keyBytes: prepared.outRefKeyCbor,
                orderedChunkReferenceInputIndices: predecessorChunkIndices,
              })
            : chunkedMembershipClaimRedeemer({
                merkleRoot: prepared.prevUtxosRoot,
                keyBytes: prepared.outRefKeyCbor,
                valueBytes:
                  prepared.predecessor.NetworkChanged.previous_descriptor_cbor,
                orderedChunkReferenceInputIndices: predecessorChunkIndices,
              })) satisfies BuildTxWithRedeemer)
      : prepared.predecessor === "Introduced"
        ? base.withdraw(
            predecessorRewardAddress as string,
            0n,
            encodeRawPexcludesProofRedeemer({
              root: prepared.prevUtxosRoot,
              keyBytes: prepared.outRefKeyCbor,
              nonMembershipProofCbor: prepared.predecessorProofCbor,
            }),
          )
        : base.withdraw(
            predecessorRewardAddress as string,
            0n,
            encodeRawPhasMembershipProofRedeemer({
              root: prepared.prevUtxosRoot,
              keyBytes: prepared.outRefKeyCbor,
              valueBytes:
                prepared.predecessor.NetworkChanged.previous_descriptor_cbor,
              membershipProofCbor: prepared.predecessorProofCbor,
            }),
          );
  const minted = withPredecessor
    .mintAssets({ [threadToken.unit]: -1n }, threadBurnRedeemer)
    .mintAssets({ [fraudProofUnit]: 1n }, fraudProofMintRedeemer)
    .pay.ToContract(
      contracts.fraudProof.spendingScriptAddress,
      { kind: "inline", value: fraudProofDatum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [fraudProofUnit]: 1n,
      },
    )
    .addSignerKey(signer.paymentKeyHash);
  const claimBound = minted;
  const unsigned = await fraudProofMintCarriage
    .attach(
      computationThreadMintCarriage.attach(
        predecessorCarriage?.attach(claimBound) ?? claimBound,
      ),
    )
    .complete({ localUPLCEval: true });
  if (
    spendLayout === undefined ||
    computationThreadMintRedeemerIndex === undefined
  ) {
    throw networkIdSubmitError("step-02 transaction layout was not resolved");
  }
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundary({
    signed,
    referenceScripts: [
      workflowReferenceScript({
        role: "V1 fraud-proof network-id step-02",
        utxo: stepReference,
        expectedScript: contracts.steps[1].spendingScript,
      }),
      ...(predecessorProofScript === undefined ||
      predecessorCarriage === undefined
        ? []
        : [
            workflowReferenceScript({
              role: predecessorCarriedByChunks
                ? "network-id-predecessor-chunked-verify"
                : postUtxo && prepared.predecessor === "Introduced"
                  ? "network-id-predecessor-pexcludes"
                  : "network-id-predecessor-phas",
              utxo: predecessorCarriage.referenceInputs[0],
              expectedScript: predecessorProofScript,
            }),
          ]),
      workflowReferenceScript({
        role: "V1 fraud-proof computation-thread minting",
        utxo: witnessReferenceScripts.computationThreadMint,
        expectedScript: contracts.computationThread.mintingScript,
      }),
      workflowReferenceScript({
        role: "V1 fraud-proof token minting",
        utxo: witnessReferenceScripts.fraudProofMint,
        expectedScript: contracts.fraudProof.mintingScript,
      }),
    ],
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw networkIdSubmitError(
      `provider returned transaction hash ${txHash}, expected ${expectedTxHash}`,
    );
  }
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    walletSource: signer.source,
    proverAddress: signer.address,
    fraudProver: signer.paymentKeyHash,
    threadOutRef,
    fraudProofOutRef: `${txHash}#${spendLayout.outputIndex.toString()}`,
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
    computationThreadUnit: threadToken.unit,
    fraudProofUnit,
    fraudProofAddress: contracts.fraudProof.spendingScriptAddress,
    state,
    outputOpeningTier: planned?.plan.tier ?? null,
    inputIndex: Number(spendLayout.inputIndex),
    outputIndex: Number(spendLayout.outputIndex),
    computationThreadMintRedeemerIndex: Number(
      computationThreadMintRedeemerIndex,
    ),
    fraudProofMintRedeemerIndex: Number(
      spendLayout.fraudProofMintRedeemerIndex,
    ),
    awaitedConfirmation: awaitConfirmation,
  };
};
