import {
  FraudProofComputationThreadRedeemer,
  FraudProofTokenDatum,
  FraudProofTokenMintRedeemer,
  NetworkIdStep02Datum,
  NetworkIdStep02SpendRedeemerSchema,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  faultProofFieldOpening,
  type FaultProofFieldOpeningPlan,
  publishFaultProofFieldCarriage,
} from "../field-opening.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  type ResolvedProverSigner,
} from "../runtime.js";
import { excludeUtxo } from "../spend-input-witness.js";
import { selectFeeInput } from "../step-support.js";
import { outputWithDatumAndUnitPredicate } from "../tx-layout.js";
import {
  type FaultProofWitnessReferenceScripts,
  witnessMintingPolicyCarriage,
} from "../witness-reference-scripts.js";
import {
  type FraudProofPreSubmitBoundary,
  reachFraudProofPreSubmitBoundary,
  workflowReferenceScript,
} from "../workflow/transaction-boundary.js";
import { isAcceptedForcedPrepared } from "./accepted-forced.js";
import type { NetworkIdContracts } from "./contracts.js";
import type { PreparedNetworkIdProof } from "./prepare.js";
import {
  networkIdSubmitError,
  requireNetworkIdReferenceScript,
  requireNetworkIdStepState,
  requireNetworkIdThreadUtxo,
} from "./submit-common.js";
import {
  isForcedPrepared,
  type PreparedNetworkIdFinalProof,
  sameFault,
  STEP_LABEL,
  type SubmitNetworkIdStep02Result,
  uniqueUtxos,
} from "./submit-network-id-step-02.same-fault.js";
import { networkIdWrongfulRejectionCloses } from "./wrongful-rejection.js";

export const submitNetworkIdStep02 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  prepared,
  outputsOpeningPlan,
  referenceScriptUtxo,
  witnessReferenceScripts,
  certificateUtxos = [],
  preSubmitBoundary,
  awaitConfirmation = true,
  unsafeSkipLocalViolationCheckForTest = false,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: NetworkIdContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly prepared: PreparedNetworkIdFinalProof;
  /** Required exactly for an OutputNetwork claim. */
  readonly outputsOpeningPlan?: FaultProofFieldOpeningPlan;
  /** Mandatory published Q35 step-02 reference script. */
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
  /** Existing §8.6 certificate UTxOs, required only for tier 3. */
  readonly certificateUtxos?: readonly UTxO[];
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
  /** Test-only: make an honest body-network claim reach the validator. */
  readonly unsafeSkipLocalViolationCheckForTest?: boolean;
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
  const forcedPrepared = isForcedPrepared(prepared) ? prepared : null;
  const forced = forcedPrepared !== null;
  const acceptedForced = isAcceptedForcedPrepared(prepared) ? prepared : null;
  const preparedCommittedNetworkId = forced
    ? forcedPrepared!.evidence.committedNetworkId
    : acceptedForced !== null
      ? acceptedForced.committedNetworkId
      : (prepared as PreparedNetworkIdProof).txInclusion.nativeTx.body
          .network_id;
  if (
    state.bad_tx_id !== prepared.badTxId ||
    state.committed_tx_network_id !== preparedCommittedNetworkId ||
    state.expected_network_id !== prepared.expectedNetworkId ||
    !sameFault(state.fault, prepared.fault)
  ) {
    throw networkIdSubmitError(
      "live step-02 state does not match the authenticated prepared artifact",
    );
  }

  let planned: FaultProofFieldOpeningPlan | undefined;
  if (forced) {
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
    // The whole-field scan moved to `forced_scan`, which folds the outputs in
    // batches and only then writes this state. Re-opening the field here would
    // re-impose the single-transaction budget the scan exists to escape.
    if (outputsOpeningPlan !== undefined) {
      throw networkIdSubmitError(
        "forced finalization must not carry an outputs opening; the forced outputs scan already certified field 2",
      );
    }
  } else if (
    state.forced_source_key !==
    (acceptedForced === null ? null : acceptedForced.subject.source_key)
  ) {
    throw networkIdSubmitError(
      "live step-02 state was bound from another transaction source",
    );
  } else if (prepared.faultClaim.kind === "transaction-network") {
    if (outputsOpeningPlan !== undefined) {
      throw networkIdSubmitError(
        "transaction-network finalization must not carry an outputs opening",
      );
    }
    if (
      !unsafeSkipLocalViolationCheckForTest &&
      (state.committed_tx_network_id === 255n ||
        state.committed_tx_network_id === state.expected_network_id)
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
      outputsOpeningPlan.sourceKind !== (acceptedForced === null ? 0n : 1n) ||
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
  const referenceInputs = uniqueUtxos([
    ...published,
    ...certificateUtxos,
    stepReference,
    ...computationThreadMintCarriage.referenceInputs,
    ...fraudProofMintCarriage.referenceInputs,
  ]);

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
    return Data.to(
      {
        Continue: [
          {
            input_index: spendLayout.inputIndex,
            output_index: spendLayout.outputIndex,
            fraud_proof_mint_redeemer_index:
              spendLayout.fraudProofMintRedeemerIndex,
            // Opening indices name positions in the builder's final
            // reference-input order.
            outputs_opening:
              planned === undefined
                ? null
                : faultProofFieldOpening({
                    planned,
                    referenceInputs: ctx.referenceInputs,
                    certificatePolicyId:
                      contracts.fieldPreimageCertificatePolicyId,
                    label: `${STEP_LABEL} outputs`,
                  }),
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
  const minted = base
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
  const unsigned = await fraudProofMintCarriage
    .attach(computationThreadMintCarriage.attach(minted))
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
