import { encodeMidgardFieldPreimage } from "@al-ft/midgard-core";
import {
  fieldOpeningForField,
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

import { requireLinearFaultThreadUtxo } from "../../src/linear-fault-family.js";
import { selectFeeInput } from "../../src/step-support.js";
import { outputWithDatumAndUnitPredicate } from "../../src/tx-layout.js";
import { witnessMintingPolicyCarriage } from "../../src/witness-reference-scripts.js";
import { ZeroInputStep02RedeemerSchema } from "../../src/zero-input/schemas.js";
import { type ZeroInputStagedThread } from "./zero-input-honest-commitment.js";

const label = "raw zero-input step-02";

type Step02Redeemer = Data.Static<typeof ZeroInputStep02RedeemerSchema>;
const Step02Redeemer =
  ZeroInputStep02RedeemerSchema as unknown as Step02Redeemer;

/**
 * Zero-input step 02 built by hand: the thread is finalized into a proof token
 * with field 0 of the disputed transaction opened inline from `spendInputs`,
 * whatever those items are. No builder check runs on whether the opening is
 * the committed one or whether the finding closes. Returns the transaction id
 * and the proof token's unit.
 */
export const submitRawZeroInputStep02 = async ({
  staged,
  spendInputs,
}: {
  readonly staged: ZeroInputStagedThread;
  readonly spendInputs: readonly Buffer[];
}): Promise<{ txHash: string; fraudProofUnit: string }> => {
  const { harness, contracts } = staged;
  const { proverLucid: lucid, proverSigner: signer } = harness;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId: staged.categoryId,
    family: "zero-input",
    stepIndex: 1,
    threadOutRef: staged.terminalThreadOutRef,
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
  const spendInputsOpening = fieldOpeningForField({
    fieldIndex: 0,
    nativeTxCompactCbor: staged.nativeTxCompactCbor,
    carriage: {
      Inline: {
        preimage: encodeMidgardFieldPreimage([...spendInputs]).toString("hex"),
      },
    },
  });
  const spendRedeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, label);
    return Data.to(
      {
        Continue: [
          {
            input_index: requireInputIndex(ctx, threadUtxo, label),
            output_index: requireUniqueOutputIndex(
              ctx.outputs,
              outputMatches,
              `${label} proof output`,
            ),
            fraud_proof_mint_redeemer_index: requireMintRedeemerIndex(
              ctx,
              contracts.fraudProof.policyId,
              `${label} proof mint`,
            ),
            spend_inputs_opening: spendInputsOpening,
          },
        ],
      },
      Step02Redeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const burnRedeemer = ((ctx) => {
    requireOwnMintPurpose(ctx, contracts.computationThread.policyId, label);
    return Data.to(
      { Success: { burning_token_asset_name: threadToken.assetName } },
      FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const proofMintRedeemer = ((ctx) => {
    requireOwnMintPurpose(ctx, contracts.fraudProof.policyId, label);
    return Data.to(
      {
        computation_thread_token_asset_name: threadToken.assetName,
        computation_thread_mint_redeemer_index: requireMintRedeemerIndex(
          ctx,
          contracts.computationThread.policyId,
          `${label} thread burn`,
        ),
      },
      FraudProofTokenMintRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const threadBurn = witnessMintingPolicyCarriage({
    script: contracts.computationThread.mintingScript,
    referenceUtxo: harness.witnessReferenceScripts.computationThreadMint,
    label: `${label} thread burn`,
  });
  const proofMint = witnessMintingPolicyCarriage({
    script: contracts.fraudProof.mintingScript,
    referenceUtxo: harness.witnessReferenceScripts.fraudProofMint,
    label: `${label} proof mint`,
  });
  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spendRedeemer)
    .readFrom([
      staged.stepReferences[1],
      ...threadBurn.referenceInputs,
      ...proofMint.referenceInputs,
    ])
    .mintAssets({ [threadToken.unit]: -1n }, burnRedeemer)
    .mintAssets({ [fraudProofUnit]: 1n }, proofMintRedeemer)
    .pay.ToContract(
      contracts.fraudProof.spendingScriptAddress,
      { kind: "inline", value: fraudProofDatum },
      { lovelace: threadUtxo.assets.lovelace ?? 0n, [fraudProofUnit]: 1n },
    )
    .addSignerKey(signer.paymentKeyHash);
  const unsigned = await proofMint
    .attach(threadBurn.attach(base))
    .complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  return { txHash, fraudProofUnit };
};
