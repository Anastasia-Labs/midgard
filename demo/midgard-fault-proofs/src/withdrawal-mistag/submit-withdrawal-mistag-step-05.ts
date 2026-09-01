/** Terminal handoff: burn the thread NFT and mint the permanent fraud token. */
import {
  FraudProofComputationThreadRedeemer,
  FraudProofTokenDatum,
  FraudProofTokenMintRedeemer,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
  type WithdrawalMistagPreparedEvidenceV1,
  WithdrawalMistagStep05Datum,
  WithdrawalMistagStep05SpendRedeemer,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  DEFAULT_CONFIRMATION_POLL_MS,
  type ResolvedProverSigner,
} from "../runtime.js";
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
import type { WithdrawalMistagContractsV1 } from "./contracts-v1.js";
import {
  requireWithdrawalMistagReferenceScriptV1,
  requireWithdrawalMistagThreadUtxoV1,
  withdrawalMistagError,
  withdrawalMistagStepLabelV1,
} from "./submit-common-v1.js";
import { withdrawalMistagStatesV1 } from "./submit-withdrawal-mistag-steps.js";

export const submitWithdrawalMistagStep05 = async ({
  lucid,
  contracts,
  signer,
  prepared,
  threadOutRef,
  referenceScriptUtxo,
  witnessReferenceScripts,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: WithdrawalMistagContractsV1;
  readonly signer: ResolvedProverSigner;
  readonly prepared: WithdrawalMistagPreparedEvidenceV1;
  readonly threadOutRef: string;
  readonly referenceScriptUtxo: UTxO;
  /** Required published witness reference scripts for this transaction. */
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScriptsV1;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}) => {
  const { threadUtxo, threadToken } = await requireWithdrawalMistagThreadUtxoV1(
    {
      lucid,
      contracts,
      stepIndex: 4,
      threadOutRef,
    },
  );
  if (threadUtxo.datum == null)
    throw withdrawalMistagError("step 05 has no datum");
  const datum = Data.from(threadUtxo.datum, WithdrawalMistagStep05Datum);
  const expected = withdrawalMistagStatesV1(prepared)[4];
  if (
    datum.fraud_prover !== signer.paymentKeyHash ||
    datum.data === null ||
    Data.to(datum, WithdrawalMistagStep05Datum) !==
      Data.to(
        { fraud_prover: signer.paymentKeyHash, data: expected },
        WithdrawalMistagStep05Datum,
      )
  ) {
    throw withdrawalMistagError(
      "step 05 datum does not match prepared evidence/prover",
    );
  }
  if (expected.claimed_valid === expected.actual_valid) {
    throw withdrawalMistagError("honestly tagged withdrawal cannot finalize");
  }

  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const fraudProofUnit = toUnit(
    contracts.fraudProof.policyId,
    threadToken.assetName,
  );
  const fraudProofDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash },
    FraudProofTokenDatum,
  );
  const outputMatches = outputWithDatumAndUnitPredicate({
    address: contracts.fraudProof.spendingScriptAddress,
    datum: fraudProofDatum,
    unit: fraudProofUnit,
  });
  let layout:
    | {
        readonly inputIndex: bigint;
        readonly outputIndex: bigint;
        readonly fraudMintIndex: bigint;
      }
    | undefined;
  let threadMintIndex: bigint | undefined;
  const spendRedeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, withdrawalMistagStepLabelV1(4));
    layout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        withdrawalMistagStepLabelV1(4),
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        outputMatches,
        "withdrawal-mistag fraud token output",
      ),
      fraudMintIndex: requireMintRedeemerIndex(
        ctx,
        contracts.fraudProof.policyId,
        "withdrawal-mistag fraud mint",
      ),
    };
    return Data.to(
      {
        Continue: [
          {
            input_index: layout.inputIndex,
            output_index: layout.outputIndex,
            fraud_proof_mint_redeemer_index: layout.fraudMintIndex,
          },
        ],
      },
      WithdrawalMistagStep05SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const threadBurnRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.computationThread.policyId,
      "thread burn",
    );
    return Data.to(
      { Success: { burning_token_asset_name: threadToken.assetName } },
      FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const fraudMintRedeemer = ((ctx) => {
    requireOwnMintPurpose(ctx, contracts.fraudProof.policyId, "fraud mint");
    threadMintIndex = requireMintRedeemerIndex(
      ctx,
      contracts.computationThread.policyId,
      "thread burn",
    );
    return Data.to(
      {
        computation_thread_token_asset_name: threadToken.assetName,
        computation_thread_mint_redeemer_index: threadMintIndex,
      },
      FraudProofTokenMintRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const computationThreadMintCarriage = witnessMintingPolicyCarriageV1({
    script: contracts.computationThread.mintingScript,
    referenceUtxo: witnessReferenceScripts?.computationThreadMint,
    label: "withdrawal-mistag step 05 computation-thread mint",
  });
  const fraudProofMintCarriage = witnessMintingPolicyCarriageV1({
    script: contracts.fraudProof.mintingScript,
    referenceUtxo: witnessReferenceScripts?.fraudProofMint,
    label: "withdrawal-mistag step 05 fraud-proof mint",
  });
  const referenceInputs = [
    requireWithdrawalMistagReferenceScriptV1({
      utxo: referenceScriptUtxo,
      contracts,
      stepIndex: 4,
    }),
    ...computationThreadMintCarriage.referenceInputs,
    ...fraudProofMintCarriage.referenceInputs,
  ];
  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spendRedeemer)
    .mintAssets({ [threadToken.unit]: -1n }, threadBurnRedeemer)
    .mintAssets({ [fraudProofUnit]: 1n }, fraudMintRedeemer)
    .pay.ToContract(
      contracts.fraudProof.spendingScriptAddress,
      { kind: "inline", value: fraudProofDatum },
      { lovelace: threadUtxo.assets.lovelace ?? 0n, [fraudProofUnit]: 1n },
    )
    .addSignerKey(signer.paymentKeyHash);
  const tx = fraudProofMintCarriage.attach(
    computationThreadMintCarriage.attach(base.readFrom(referenceInputs)),
  );
  const unsigned = await tx.complete({ localUPLCEval: true });
  if (layout === undefined || threadMintIndex === undefined) {
    throw withdrawalMistagError(
      "transaction builder did not resolve final layout",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundaryV1({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransactionV1({
      signed,
      candidates: [
        {
          role: "V1 fraud-proof withdrawal-mistag step-05",
          utxo: referenceScriptUtxo,
          expectedScript: contracts.steps[4].spendingScript,
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
    throw withdrawalMistagError(
      `step 05 provider returned ${txHash}, expected ${expectedTxHash}`,
    );
  }
  if (awaitConfirmation)
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  return {
    txHash,
    threadOutRef,
    fraudProofOutRef: `${txHash}#${layout.outputIndex.toString()}`,
    fraudProofUnit,
    fraudProofAssetName: threadToken.assetName,
    inputIndex: Number(layout.inputIndex),
    outputIndex: Number(layout.outputIndex),
    computationThreadMintRedeemerIndex: Number(threadMintIndex),
    fraudProofMintRedeemerIndex: Number(layout.fraudMintIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};
