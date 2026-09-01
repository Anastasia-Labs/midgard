import {
  DoubleWithdrawStep02Datum,
  DoubleWithdrawStep02SpendRedeemer,
  type DoubleWithdrawStep02State,
  FraudProofComputationThreadRedeemer,
  FraudProofTokenDatum,
  FraudProofTokenMintRedeemer,
  getHeaderV1FromStateQueueDatum,
  getLinkedListNodeViewFromUTxO,
  HUB_ORACLE_ASSET_NAME,
  isDoubleWithdrawFaultV1,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
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
  DEFAULT_CONFIRMATION_POLL_MS,
  fetchUtxoByOutRef,
  parseOutRef,
  requireSingletonUtxo,
  type ResolvedProverSigner,
  resolveFraudulentHeaderHash,
} from "../runtime.js";
import { selectFeeInput } from "../submit-step-01.js";
import { outputWithDatumAndUnitPredicate } from "../tx-layout.js";
import {
  type FaultProofWitnessReferenceScriptsV1,
  witnessMintingPolicyCarriageV1,
} from "../witness-reference-scripts-v1.js";
import type { FraudProofPreSubmitBoundaryV1 } from "../workflow/transaction-boundary-v1.js";
import {
  reachFraudProofPreSubmitBoundaryV1,
  workflowReferenceScriptsUsedByTransactionV1,
} from "../workflow/transaction-boundary-v1.js";
import type { DoubleWithdrawContractsV1 } from "./contracts-v1.js";
import {
  doubleWithdrawSubmitError,
  requireDoubleWithdrawReferenceScriptV1,
  requireDoubleWithdrawStepStateV1,
  requireDoubleWithdrawThreadUtxoV1,
} from "./submit-common-v1.js";
import {
  deriveDoubleWithdrawMembershipV1,
  type SubmitDoubleWithdrawInclusionV1,
} from "./submit-double-withdraw-step-01.js";

export const assertDoubleWithdrawFinalizableV1 = ({
  state,
  fraudulentHeaderHash,
  second,
}: {
  readonly state: DoubleWithdrawStep02State;
  readonly fraudulentHeaderHash: string;
  readonly second: Parameters<typeof isDoubleWithdrawFaultV1>[1];
}): void => {
  if (state.challenged_header_hash !== fraudulentHeaderHash) {
    throw doubleWithdrawSubmitError(
      `step-02 state names ${state.challenged_header_hash}, but the thread names ${fraudulentHeaderHash}.`,
    );
  }
  if (!isDoubleWithdrawFaultV1(state, second)) {
    throw doubleWithdrawSubmitError(
      "step-02 refuses to finalize: the second leaf is identical, drains a different L2 outref, or is not payable.",
    );
  }
};

export type SubmitDoubleWithdrawStep02Result = {
  readonly txHash: string;
  readonly fraudProofOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadUnit: string;
  readonly fraudProofUnit: string;
  readonly fraudProofAddress: string;
  readonly secondStepAddress: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly computationThreadMintRedeemerIndex: number;
  readonly fraudProofMintRedeemerIndex: number;
  readonly awaitedConfirmation: boolean;
};

export const submitDoubleWithdrawStep02 = async ({
  lucid,
  contracts,
  categoryId,
  network,
  signer,
  threadOutRef,
  stateQueueBlockOutRef,
  inclusion,
  referenceScriptUtxo,
  witnessReferenceScripts,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: DoubleWithdrawContractsV1;
  readonly categoryId: string;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly stateQueueBlockOutRef: string;
  readonly inclusion: SubmitDoubleWithdrawInclusionV1;
  readonly referenceScriptUtxo: UTxO;
  /** Required published witness reference scripts for this transaction. */
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScriptsV1;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitDoubleWithdrawStep02Result> => {
  const [{ threadUtxo, threadToken }, hubOracleUtxo, stateQueueBlockUtxo] =
    await Promise.all([
      requireDoubleWithdrawThreadUtxoV1({
        lucid,
        contracts,
        categoryId,
        stepIndex: 1,
        threadOutRef,
      }),
      requireSingletonUtxo({
        lucid,
        address: credentialToAddress(
          network,
          scriptHashToCredential(contracts.hubOraclePolicyId),
        ),
        unit: toUnit(contracts.hubOraclePolicyId, HUB_ORACLE_ASSET_NAME),
        label: "double-withdraw step-02 hub oracle",
      }),
      fetchUtxoByOutRef({
        lucid,
        outRef: parseOutRef(
          stateQueueBlockOutRef,
          "--state-queue-block-out-ref",
        ),
        label: "double-withdraw step-02 state-queue block",
      }),
    ]);
  const state = requireDoubleWithdrawStepStateV1({
    threadUtxo,
    signer,
    schema: DoubleWithdrawStep02Datum,
    stepIndex: 1,
  });
  const stateQueueHeaderHash = resolveFraudulentHeaderHash({
    stateQueuePolicyId: contracts.stateQueuePolicyId,
    fraudulentBlockUtxo: stateQueueBlockUtxo,
  });
  if (stateQueueHeaderHash !== threadToken.fraudulentHeaderHash) {
    throw doubleWithdrawSubmitError(
      `state-queue header ${stateQueueHeaderHash} does not match thread ${threadToken.fraudulentHeaderHash}.`,
    );
  }
  const node = await Effect.runPromise(
    getLinkedListNodeViewFromUTxO(stateQueueBlockUtxo),
  );
  const header = await Effect.runPromise(getHeaderV1FromStateQueueDatum(node));
  const { committedWithdrawal } = await deriveDoubleWithdrawMembershipV1({
    header,
    inclusion,
  });
  assertDoubleWithdrawFinalizableV1({
    state,
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
    second: committedWithdrawal,
  });

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
        inputIndex: bigint;
        outputIndex: bigint;
        hubOracleRefInputIndex: bigint;
        stateQueueNodeRefInputIndex: bigint;
        fraudProofMintRedeemerIndex: bigint;
      }
    | undefined;
  let computationThreadMintRedeemerIndex: bigint | undefined;
  const spendRedeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "double-withdraw step-02");
    layout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, "double-withdraw step-02"),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        outputMatches,
        "double-withdraw step-02 fraud-proof output",
      ),
      hubOracleRefInputIndex: requireReferenceInputIndex(
        ctx,
        hubOracleUtxo,
        "double-withdraw step-02 hub oracle",
      ),
      stateQueueNodeRefInputIndex: requireReferenceInputIndex(
        ctx,
        stateQueueBlockUtxo,
        "double-withdraw step-02 state-queue node",
      ),
      fraudProofMintRedeemerIndex: requireMintRedeemerIndex(
        ctx,
        contracts.fraudProof.policyId,
        "double-withdraw step-02 fraud-proof mint",
      ),
    };
    return Data.to(
      {
        Continue: [
          {
            input_index: layout.inputIndex,
            output_index: layout.outputIndex,
            fraud_proof_mint_redeemer_index: layout.fraudProofMintRedeemerIndex,
            hub_ref_input_index: layout.hubOracleRefInputIndex,
            state_queue_node_ref_input_index:
              layout.stateQueueNodeRefInputIndex,
            committed_withdrawal: committedWithdrawal,
          },
        ],
      },
      DoubleWithdrawStep02SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const threadBurnRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.computationThread.policyId,
      "double-withdraw step-02 thread burn",
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
      "double-withdraw step-02 fraud-proof mint",
    );
    computationThreadMintRedeemerIndex = requireMintRedeemerIndex(
      ctx,
      contracts.computationThread.policyId,
      "double-withdraw step-02 thread burn",
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
    label: "double-withdraw step-02 computation-thread mint",
  });
  const fraudProofMintCarriage = witnessMintingPolicyCarriageV1({
    script: contracts.fraudProof.mintingScript,
    referenceUtxo: witnessReferenceScripts?.fraudProofMint,
    label: "double-withdraw step-02 fraud-proof mint",
  });
  const referenceInputs = [
    hubOracleUtxo,
    stateQueueBlockUtxo,
    requireDoubleWithdrawReferenceScriptV1({
      utxo: referenceScriptUtxo,
      expectedScriptHash: contracts.steps[1].spendingScriptHash,
      stepIndex: 1,
    }),
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
    throw doubleWithdrawSubmitError("step-02 layout was not resolved.");
  }
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundaryV1({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransactionV1({
      signed,
      candidates: [
        {
          role: "V1 fraud-proof double-withdraw step-02",
          utxo: referenceScriptUtxo,
          expectedScript: contracts.steps[1].spendingScript,
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
    throw doubleWithdrawSubmitError(
      `step-02 provider returned ${txHash}, expected ${expectedTxHash}.`,
    );
  }
  if (awaitConfirmation)
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  return {
    txHash,
    fraudProofOutRef: `${txHash}#${layout.outputIndex.toString()}`,
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
    computationThreadUnit: threadToken.unit,
    fraudProofUnit,
    fraudProofAddress: contracts.fraudProof.spendingScriptAddress,
    secondStepAddress: contracts.steps[1].spendingScriptAddress,
    inputIndex: Number(layout.inputIndex),
    outputIndex: Number(layout.outputIndex),
    computationThreadMintRedeemerIndex: Number(
      computationThreadMintRedeemerIndex,
    ),
    fraudProofMintRedeemerIndex: Number(layout.fraudProofMintRedeemerIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};
