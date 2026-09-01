import {
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
  WithdrawnInputStep03Datum,
  WithdrawnInputStep03SpendRedeemer,
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
import {
  WITHDRAWN_INPUT_CATEGORY_LABEL,
  type WithdrawnInputContractsV1,
} from "./contracts-v1.js";
import {
  requireWithdrawnInputReferenceScriptV1,
  requireWithdrawnInputStepStateV1,
  requireWithdrawnInputThreadUtxoV1,
  withdrawnInputSubmitError,
} from "./submit-common-v1.js";

export type SubmitWithdrawnInputStep03Result = {
  readonly txHash: string;
  readonly fraudProofOutRef: string;
  readonly fraudProofUnit: string;
  readonly fraudProofAddress: string;
  readonly fraudulentHeaderHash: string;
};

export const submitWithdrawnInputStep03 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  withdrawalMembership,
  referenceScriptUtxo,
  witnessReferenceScripts,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: WithdrawnInputContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly withdrawalMembership: WithdrawalSourceMembershipProof;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScriptsV1;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitWithdrawnInputStep03Result> => {
  const { threadUtxo, threadToken } = await requireWithdrawnInputThreadUtxoV1({
    lucid,
    contracts,
    categoryId,
    stepIndex: 2,
    threadOutRef,
  });
  const state = requireWithdrawnInputStepStateV1({
    threadUtxo,
    signer,
    schema: WithdrawnInputStep03Datum,
    stepIndex: 2,
  });
  const stepReference = requireWithdrawnInputReferenceScriptV1({
    utxo: referenceScriptUtxo,
    contracts,
    stepIndex: 2,
  });
  if (
    withdrawalMembership.domain !== ROOT_DOMAINS.withdrawals ||
    withdrawalMembership.root !== state.blocks_withdrawals_root ||
    withdrawalMembership.count !== state.blocks_withdrawal_count
  ) {
    throw withdrawnInputSubmitError(
      "withdrawal membership does not name the counted withdrawals commitment carried by the thread.",
    );
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
        readonly fraudProofMintRedeemerIndex: bigint;
      }
    | undefined;
  let computationThreadMintRedeemerIndex: bigint | undefined;
  const spendRedeemer = ((ctx) => {
    requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      `${WITHDRAWN_INPUT_CATEGORY_LABEL} step 03`,
    );
    layout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        `${WITHDRAWN_INPUT_CATEGORY_LABEL} step 03`,
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        outputMatches,
        `${WITHDRAWN_INPUT_CATEGORY_LABEL} fraud proof`,
      ),
      fraudProofMintRedeemerIndex: requireMintRedeemerIndex(
        ctx,
        contracts.fraudProof.policyId,
        `${WITHDRAWN_INPUT_CATEGORY_LABEL} fraud-proof mint`,
      ),
    };
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
      WithdrawnInputStep03SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const threadBurnRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.computationThread.policyId,
      `${WITHDRAWN_INPUT_CATEGORY_LABEL} thread burn`,
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
      `${WITHDRAWN_INPUT_CATEGORY_LABEL} fraud-proof mint`,
    );
    computationThreadMintRedeemerIndex = requireMintRedeemerIndex(
      ctx,
      contracts.computationThread.policyId,
      `${WITHDRAWN_INPUT_CATEGORY_LABEL} thread burn`,
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

  const computationThreadMintCarriage = witnessMintingPolicyCarriageV1({
    script: contracts.computationThread.mintingScript,
    referenceUtxo: witnessReferenceScripts?.computationThreadMint,
    label: "withdrawn-input step-03 computation-thread mint",
  });
  const fraudProofMintCarriage = witnessMintingPolicyCarriageV1({
    script: contracts.fraudProof.mintingScript,
    referenceUtxo: witnessReferenceScripts?.fraudProofMint,
    label: "withdrawn-input step-03 fraud-proof mint",
  });
  const referenceInputs = [
    stepReference,
    ...computationThreadMintCarriage.referenceInputs,
    ...fraudProofMintCarriage.referenceInputs,
  ];
  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spendRedeemer)
    .readFrom(referenceInputs)
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
  const tx = fraudProofMintCarriage.attach(
    computationThreadMintCarriage.attach(base),
  );
  const unsigned = await tx.complete({ localUPLCEval: true });
  if (
    layout === undefined ||
    computationThreadMintRedeemerIndex === undefined
  ) {
    throw withdrawnInputSubmitError("step-03 layout was not resolved.");
  }
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundaryV1({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransactionV1({
      signed,
      candidates: [
        {
          role: "V1 fraud-proof withdrawn-input step-03",
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
      ],
    }),
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw withdrawnInputSubmitError(
      `step-03 provider returned ${txHash}, expected ${expectedTxHash}.`,
    );
  }
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    fraudProofOutRef: `${txHash}#${layout.outputIndex.toString()}`,
    fraudProofUnit,
    fraudProofAddress: contracts.fraudProof.spendingScriptAddress,
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
  };
};
