/**
 * `withdrawn-reference-input` step-03 submitter (offchain plan §4.2): the
 * proven verdict finalizes — the computation-thread NFT burns and the
 * permanent fraud-proof token mints to the fraud-proof address under the
 * generic finalization validations.
 *
 * The withdrawal's validity, exact out-ref, counted-root binding and MPF proof
 * are checked locally before the same facts are enforced by the validator.
 */
import {
  commitCountedRootProgram,
  FraudProofComputationThreadRedeemer,
  FraudProofTokenDatum,
  FraudProofTokenMintRedeemer,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
  ROOT_DOMAINS,
  type WithdrawalSourceMembershipProof,
  WithdrawnReferenceInputStep03Datum,
  WithdrawnReferenceInputStep03SpendRedeemer,
  type WithdrawnReferenceInputStep03State,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  type PreparedClaimRegistryMutationV1,
  prepareFamilyClaimRegistryMutationV1,
  requirePreparedClaimRegistryMutationV1,
} from "../claim-registry-transaction-v1.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  type ResolvedProverSigner,
} from "../runtime.js";
import { excludeUtxo } from "../spend-input-witness.js";
import { selectFeeInput } from "../submit-step-01.js";
import { outputWithDatumAndUnitPredicate } from "../tx-layout.js";
import {
  type FaultProofWitnessReferenceScriptsV1,
  witnessMintingPolicyCarriageV1,
} from "../witness-reference-scripts-v1.js";
import {
  type FraudProofPreSubmitBoundaryV1,
  reachFraudProofPreSubmitBoundaryV1,
  workflowReferenceScriptsUsedByTransactionV1,
} from "../workflow/transaction-boundary-v1.js";
import type { WithdrawnReferenceInputContractsV1 } from "./contracts-v1.js";
import { verifyWithdrawnReferenceInputMembershipV1 } from "./prepare-withdrawn-reference-input-v1.js";
import {
  requireWithdrawnReferenceInputReferenceScriptV1,
  requireWithdrawnReferenceInputStepStateV1,
  requireWithdrawnReferenceInputThreadUtxoV1,
  withdrawnReferenceInputStepLabelV1,
  withdrawnReferenceInputSubmitError,
} from "./submit-common-v1.js";

const STEP_LABEL = withdrawnReferenceInputStepLabelV1(2);

export type SubmitWithdrawnReferenceInputStep03Result = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly fraudProofOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadUnit: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofAssetName: string;
  readonly fraudProofUnit: string;
  readonly fraudProofAddress: string;
  readonly state: WithdrawnReferenceInputStep03State;
  readonly withdrawalMembership: WithdrawalSourceMembershipProof;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly computationThreadMintRedeemerIndex: number;
  readonly fraudProofMintRedeemerIndex: number;
  readonly referenceScriptOutRef: string;
  readonly awaitedConfirmation: boolean;
};

type Step03SpendLayout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
  readonly fraudProofMintRedeemerIndex: bigint;
};

export const submitWithdrawnReferenceInputStep03 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  withdrawalMembership,
  referenceScriptUtxo,
  witnessReferenceScripts,
  claimRegistryMutation,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: WithdrawnReferenceInputContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly withdrawalMembership: WithdrawalSourceMembershipProof;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScriptsV1;
  readonly claimRegistryMutation?: PreparedClaimRegistryMutationV1;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitWithdrawnReferenceInputStep03Result> => {
  const { threadUtxo, threadToken } =
    await requireWithdrawnReferenceInputThreadUtxoV1({
      lucid,
      contracts,
      categoryId,
      stepIndex: 2,
      threadOutRef,
    });
  const state: WithdrawnReferenceInputStep03State =
    requireWithdrawnReferenceInputStepStateV1({
      threadUtxo,
      signer,
      schema: WithdrawnReferenceInputStep03Datum,
      stepIndex: 2,
    });
  if (withdrawalMembership.domain !== ROOT_DOMAINS.withdrawals) {
    throw withdrawnReferenceInputSubmitError(
      `withdrawal membership domain is ${withdrawalMembership.domain}, not ${ROOT_DOMAINS.withdrawals}.`,
    );
  }
  if (
    withdrawalMembership.root !== state.blocks_withdrawals_root ||
    withdrawalMembership.count !== state.blocks_withdrawal_count
  ) {
    throw withdrawnReferenceInputSubmitError(
      "withdrawal membership root/count do not match the committed block state.",
    );
  }
  if (withdrawalMembership.value.validity !== "WithdrawalIsValid") {
    throw withdrawnReferenceInputSubmitError("withdrawal-not-valid");
  }
  const withdrawn = withdrawalMembership.value.body.l2_outref;
  if (
    withdrawn.transactionId !== state.missing_reference_input.tx_id ||
    withdrawn.outputIndex !== state.missing_reference_input.output_index
  ) {
    throw withdrawnReferenceInputSubmitError(
      "withdrawal l2_outref does not equal the challenged reference input.",
    );
  }
  const countedRoot = await Effect.runPromise(
    commitCountedRootProgram({
      domain: ROOT_DOMAINS.withdrawals,
      phasRoot: withdrawalMembership.phas_root,
      count: withdrawalMembership.count,
    }),
  );
  if (countedRoot !== withdrawalMembership.root) {
    throw withdrawnReferenceInputSubmitError(
      "withdrawal membership phas_root/count do not derive the committed counted root.",
    );
  }
  verifyWithdrawnReferenceInputMembershipV1(withdrawalMembership);

  const computationThreadCarriage = witnessMintingPolicyCarriageV1({
    script: contracts.computationThread.mintingScript,
    referenceUtxo: witnessReferenceScripts?.computationThreadMint,
    label: `${STEP_LABEL} computation-thread mint`,
  });
  const fraudProofCarriage = witnessMintingPolicyCarriageV1({
    script: contracts.fraudProof.mintingScript,
    referenceUtxo: witnessReferenceScripts?.fraudProofMint,
    label: `${STEP_LABEL} fraud-proof mint`,
  });

  signer.selectWallet(lucid);
  const resolvedClaimRegistryMutation = requirePreparedClaimRegistryMutationV1({
    mutation:
      claimRegistryMutation ??
      (await prepareFamilyClaimRegistryMutationV1({
        lucid,
        claimRegistry: contracts.claimRegistry,
        claimRegistryReferenceUtxo: witnessReferenceScripts.claimRegistrySpend,
        hubOraclePolicyId: contracts.hubOraclePolicyId,
        computationThreadPolicyId: contracts.computationThread.policyId,
        claimId: threadToken.assetName,
        kind: "close",
      })),
    kind: "close",
    claimId: threadToken.assetName,
    label: STEP_LABEL,
  });
  const feeInput = selectFeeInput(
    resolvedClaimRegistryMutation.referenceInputs.reduce<readonly UTxO[]>(
      (utxos, reference) => excludeUtxo(utxos, reference),
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
  const fraudProofOutputMatches = outputWithDatumAndUnitPredicate({
    address: contracts.fraudProof.spendingScriptAddress,
    datum: fraudProofDatum,
    unit: fraudProofUnit,
  });
  let spendLayout: Step03SpendLayout | undefined;
  let computationThreadMintRedeemerIndex: bigint | undefined;

  const spendRedeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, STEP_LABEL);
    const layout: Step03SpendLayout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, STEP_LABEL),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        fraudProofOutputMatches,
        `${STEP_LABEL} fraud-proof`,
      ),
      fraudProofMintRedeemerIndex: requireMintRedeemerIndex(
        ctx,
        contracts.fraudProof.policyId,
        `${STEP_LABEL} fraud-proof`,
      ),
    };
    spendLayout = layout;
    return Data.to(
      {
        Continue: [
          {
            input_index: layout.inputIndex,
            output_index: layout.outputIndex,
            fraud_proof_mint_redeemer_index: layout.fraudProofMintRedeemerIndex,
            withdrawal_membership: withdrawalMembership,
          },
        ],
      },
      WithdrawnReferenceInputStep03SpendRedeemer,
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
  const stepReference = requireWithdrawnReferenceInputReferenceScriptV1({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[2].spendingScriptHash,
    stepIndex: 2,
  });
  const withReferences = base.readFrom([
    stepReference,
    ...computationThreadCarriage.referenceInputs,
    ...fraudProofCarriage.referenceInputs,
  ]);
  const tx = fraudProofCarriage.attach(
    computationThreadCarriage.attach(
      resolvedClaimRegistryMutation.apply(withReferences),
    ),
  );

  const unsigned = await tx.complete({ localUPLCEval: true });
  if (
    spendLayout === undefined ||
    computationThreadMintRedeemerIndex === undefined
  ) {
    throw withdrawnReferenceInputSubmitError(
      "BuildTxWithRedeemer did not resolve the step-03 layout.",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundaryV1({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransactionV1({
      signed,
      candidates: [
        {
          role: "V1 fraud-proof withdrawn-reference-input step-03",
          utxo: referenceScriptUtxo,
          expectedScript: contracts.steps[2].spendingScript,
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
        {
          role: "claim-registry spending",
          utxo: resolvedClaimRegistryMutation.referenceScriptUtxo,
          expectedScript: resolvedClaimRegistryMutation.registryScript,
        },
      ],
    }),
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw withdrawnReferenceInputSubmitError(
      `step-03 provider returned ${txHash}, expected ${expectedTxHash}.`,
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
    fraudProofPolicyId: contracts.fraudProof.policyId,
    fraudProofAssetName: threadToken.assetName,
    fraudProofUnit,
    fraudProofAddress: contracts.fraudProof.spendingScriptAddress,
    state,
    withdrawalMembership,
    inputIndex: Number(spendLayout.inputIndex),
    outputIndex: Number(spendLayout.outputIndex),
    computationThreadMintRedeemerIndex: Number(
      computationThreadMintRedeemerIndex,
    ),
    fraudProofMintRedeemerIndex: Number(
      spendLayout.fraudProofMintRedeemerIndex,
    ),
    referenceScriptOutRef: `${stepReference.txHash}#${stepReference.outputIndex.toString()}`,
    awaitedConfirmation: awaitConfirmation,
  };
};
