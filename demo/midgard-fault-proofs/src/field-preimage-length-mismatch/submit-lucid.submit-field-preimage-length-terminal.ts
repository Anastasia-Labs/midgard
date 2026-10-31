import {
  FieldPreimageLengthStep01DatumSchema,
  FieldPreimageLengthStep03RedeemerSchema,
  FraudProofComputationThreadRedeemer,
  FraudProofTokenDatum,
  FraudProofTokenMintRedeemer,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import { type BuildTxWithRedeemer, Data, toUnit } from "@lucid-evolution/lucid";

import { DEFAULT_CONFIRMATION_POLL_MS } from "../runtime.js";
import { selectFeeInput } from "../step-support.js";
import { outputWithDatumAndUnitPredicate } from "../tx-layout.js";
import { witnessMintingPolicyCarriage } from "../witness-reference-scripts.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import {
  reachFraudProofPreSubmitBoundary,
  workflowReferenceScriptsUsedByTransaction,
} from "../workflow/transaction-boundary.js";
import type { ManifestBoundFieldPreimageLengthConfig } from "./config.js";
import {
  LABEL,
  requireReference,
  requireThread,
} from "./submit-lucid.submit-field-preimage-length-cancel.js";

export const submitFieldPreimageLengthTerminal = async ({
  config,
  threadOutRef,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly config: ManifestBoundFieldPreimageLengthConfig;
  readonly threadOutRef: string;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  const { threadUtxo, threadToken, step } = await requireThread({
    config,
    threadOutRef,
    stepIndex: 3,
  });
  config.signer.selectWallet(config.lucid);
  const feeInput = selectFeeInput(await config.lucid.wallet().getUtxos());
  const fraudProofUnit = toUnit(
    config.contracts.fraudProof.policyId,
    threadToken.assetName,
  );
  const proofDatum = Data.to(
    { fraud_prover: config.signer.paymentKeyHash },
    FraudProofTokenDatum,
  );
  const outputMatches = outputWithDatumAndUnitPredicate({
    address: config.contracts.fraudProof.spendingScriptAddress,
    datum: proofDatum,
    unit: fraudProofUnit,
  });
  let layout:
    | {
        inputIndex: bigint;
        outputIndex: bigint;
        fraudProofMintRedeemerIndex: bigint;
      }
    | undefined;
  let threadMintIndex: bigint | undefined;
  const spend = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, `${LABEL} terminal`);
    layout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, LABEL),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        outputMatches,
        `${LABEL} proof output`,
      ),
      fraudProofMintRedeemerIndex: requireMintRedeemerIndex(
        ctx,
        config.contracts.fraudProof.policyId,
        `${LABEL} proof mint`,
      ),
    };
    return Data.to(
      {
        Continue: [
          {
            input_index: layout.inputIndex,
            output_index: layout.outputIndex,
            fraud_proof_mint_redeemer_index: layout.fraudProofMintRedeemerIndex,
          },
        ],
      } as never,
      FieldPreimageLengthStep03RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const burn = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      config.contracts.computationThread.policyId,
      `${LABEL} thread burn`,
    );
    return Data.to(
      { Success: { burning_token_asset_name: threadToken.assetName } },
      FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const mint = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      config.contracts.fraudProof.policyId,
      `${LABEL} proof mint`,
    );
    threadMintIndex = requireMintRedeemerIndex(
      ctx,
      config.contracts.computationThread.policyId,
      `${LABEL} thread burn`,
    );
    return Data.to(
      {
        computation_thread_token_asset_name: threadToken.assetName,
        computation_thread_mint_redeemer_index: threadMintIndex,
      },
      FraudProofTokenMintRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const ctCarriage = witnessMintingPolicyCarriage({
    script: config.contracts.computationThread.mintingScript,
    referenceUtxo: config.referenceScripts.witnesses.computationThreadMint,
    label: `${LABEL} thread mint`,
  });
  const proofCarriage = witnessMintingPolicyCarriage({
    script: config.contracts.fraudProof.mintingScript,
    referenceUtxo: config.referenceScripts.witnesses.fraudProofMint,
    label: `${LABEL} proof mint`,
  });
  const reference = requireReference({
    utxo: config.referenceScripts.step03,
    expectedHash: step.spendingScriptHash,
    role: "step-03",
  });
  const base = config.lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spend)
    .mintAssets({ [threadToken.unit]: -1n }, burn)
    .mintAssets({ [fraudProofUnit]: 1n }, mint)
    .pay.ToContract(
      config.contracts.fraudProof.spendingScriptAddress,
      { kind: "inline", value: proofDatum },
      { lovelace: threadUtxo.assets.lovelace ?? 0n, [fraudProofUnit]: 1n },
    )
    .addSignerKey(config.signer.paymentKeyHash)
    .readFrom([
      reference,
      ...ctCarriage.referenceInputs,
      ...proofCarriage.referenceInputs,
    ]);
  const unsigned = await proofCarriage
    .attach(ctCarriage.attach(base))
    .complete({ localUPLCEval: true });
  if (layout === undefined || threadMintIndex === undefined)
    throw new Error(`${LABEL}: terminal layout did not resolve`);
  const resolved = layout;
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundary({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransaction({
      signed,
      candidates: [
        {
          role: "V1 field-preimage-length terminal",
          utxo: reference,
          expectedScript: step.spendingScript,
        },
        {
          role: "thread mint",
          utxo: config.referenceScripts.witnesses.computationThreadMint,
          expectedScript: config.contracts.computationThread.mintingScript,
        },
        {
          role: "proof mint",
          utxo: config.referenceScripts.witnesses.fraudProofMint,
          expectedScript: config.contracts.fraudProof.mintingScript,
        },
      ],
    }),
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash)
    throw new Error(`${LABEL}: provider returned a different transaction id`);
  if (awaitConfirmation)
    await config.lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  return {
    txHash,
    fraudProofOutRef: `${txHash}#${resolved.outputIndex.toString()}`,
    fraudProofUnit,
  };
};

// Keep the initial datum schema live in this production module. It prevents a
// future ABI-only import cleanup from accidentally dropping the generic Init
// schema used by the bound builder set.
export const FIELD_PREIMAGE_LENGTH_INIT_DATUM_SCHEMA =
  FieldPreimageLengthStep01DatumSchema;
