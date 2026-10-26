/** `missingRedeemer` terminal: the final state convicts, the computation
 * thread burns and the permanent fraud-proof token mints. */
import * as SDK from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  toUnit,
  type TxSigned,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  requireLinearFaultReferenceScript,
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  type ResolvedProverSigner,
} from "../runtime.js";
import { selectFeeInput } from "../step-support.js";
import { outputWithDatumAndUnitPredicate } from "../tx-layout.js";
import {
  type FaultProofWitnessReferenceScripts,
  witnessMintingPolicyCarriage,
} from "../witness-reference-scripts.js";
import {
  type FraudProofPreSubmitBoundary,
  reachFraudProofPreSubmitBoundary,
  workflowReferenceScriptsUsedByTransaction,
} from "../workflow/transaction-boundary.js";
import {
  MISSING_REDEEMER_CATEGORY_LABEL,
  type MissingRedeemerContracts,
} from "./contracts.js";
import { missingRedeemerConvicts } from "./routes.js";

const TERMINAL_INDEX = SDK.MISSING_REDEEMER_ROLE_ORDER.indexOf("terminal");
const LABEL = `${MISSING_REDEEMER_CATEGORY_LABEL} terminal`;

export const submitMissingRedeemerTerminal = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  referenceScriptUtxo,
  witnessReferenceScripts,
  preSubmitBoundary,
  awaitConfirmation = true,
  onSigned,
  requireConviction = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: MissingRedeemerContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
  readonly onSigned?: (label: string, signed: TxSigned) => void;
  /** False only to reach the validator with a non-convicting state. */
  readonly requireConviction?: boolean;
}) => {
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: MISSING_REDEEMER_CATEGORY_LABEL,
    stepIndex: TERMINAL_INDEX,
    threadOutRef,
  });
  const state = requireLinearFaultStepState({
    threadUtxo,
    signer,
    schema: SDK.DirectClaimFinalDatum,
    family: MISSING_REDEEMER_CATEGORY_LABEL,
    stepIndex: TERMINAL_INDEX,
  });
  if (requireConviction && !missingRedeemerConvicts(state))
    throw new Error(`${LABEL}: the final state does not convict`);
  const step = contracts.steps[TERMINAL_INDEX]!;
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: step.spendingScriptHash,
    family: MISSING_REDEEMER_CATEGORY_LABEL,
    stepIndex: TERMINAL_INDEX,
  });
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const fraudProofUnit = toUnit(
    contracts.fraudProof.policyId,
    threadToken.assetName,
  );
  const fraudProofDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash },
    SDK.FraudProofTokenDatum,
  );
  let outputIndex: bigint | undefined;
  const spend = ((ctx) => {
    SDK.requireOwnSpendPurpose(ctx, threadUtxo, LABEL);
    outputIndex = SDK.requireUniqueOutputIndex(
      ctx.outputs,
      outputWithDatumAndUnitPredicate({
        address: contracts.fraudProof.spendingScriptAddress,
        datum: fraudProofDatum,
        unit: fraudProofUnit,
      }),
      `${LABEL} fraud-proof`,
    );
    return Data.to(
      {
        Continue: [
          {
            input_index: SDK.requireInputIndex(ctx, threadUtxo, LABEL),
            output_index: outputIndex,
            fraud_proof_mint_redeemer_index: SDK.requireMintRedeemerIndex(
              ctx,
              contracts.fraudProof.policyId,
              `${LABEL} fraud-proof`,
            ),
          },
        ],
      },
      SDK.DirectClaimFinalizeRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const burn = ((ctx) => {
    SDK.requireOwnMintPurpose(
      ctx,
      contracts.computationThread.policyId,
      `${LABEL} computation-thread burn`,
    );
    return Data.to(
      { Success: { burning_token_asset_name: threadToken.assetName } },
      SDK.FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const mint = ((ctx) => {
    SDK.requireOwnMintPurpose(
      ctx,
      contracts.fraudProof.policyId,
      `${LABEL} fraud-proof mint`,
    );
    return Data.to(
      {
        computation_thread_token_asset_name: threadToken.assetName,
        computation_thread_mint_redeemer_index: SDK.requireMintRedeemerIndex(
          ctx,
          contracts.computationThread.policyId,
          `${LABEL} computation-thread burn`,
        ),
      },
      SDK.FraudProofTokenMintRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const burnCarriage = witnessMintingPolicyCarriage({
    script: contracts.computationThread.mintingScript,
    referenceUtxo: witnessReferenceScripts?.computationThreadMint,
    label: `${LABEL} computation-thread burn`,
  });
  const mintCarriage = witnessMintingPolicyCarriage({
    script: contracts.fraudProof.mintingScript,
    referenceUtxo: witnessReferenceScripts?.fraudProofMint,
    label: `${LABEL} fraud-proof mint`,
  });
  const unsigned = await lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spend)
    .mintAssets({ [threadToken.unit]: -1n }, burn)
    .mintAssets({ [fraudProofUnit]: 1n }, mint)
    .pay.ToContract(
      contracts.fraudProof.spendingScriptAddress,
      { kind: "inline", value: fraudProofDatum },
      { lovelace: threadUtxo.assets.lovelace ?? 0n, [fraudProofUnit]: 1n },
    )
    .addSignerKey(signer.paymentKeyHash)
    .readFrom([
      stepReference,
      ...burnCarriage.referenceInputs,
      ...mintCarriage.referenceInputs,
    ])
    .complete({ localUPLCEval: true });
  if (outputIndex === undefined)
    throw new Error(`${LABEL}: completed layout unresolved`);
  const signed = await unsigned.sign.withWallet().complete();
  onSigned?.("terminal", signed);
  const expected = await reachFraudProofPreSubmitBoundary({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransaction({
      signed,
      candidates: [
        {
          role: LABEL,
          utxo: stepReference,
          expectedScript: step.spendingScript,
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
  if (txHash !== expected)
    throw new Error(
      `${LABEL}: provider returned ${txHash}, expected ${expected}`,
    );
  if (awaitConfirmation)
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  return {
    txHash,
    finalState: state,
    fraudProofOutRef: `${txHash}#${outputIndex.toString()}`,
    fraudProofUnit,
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
  };
};
