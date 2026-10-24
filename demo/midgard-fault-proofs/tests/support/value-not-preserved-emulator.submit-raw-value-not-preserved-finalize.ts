import {
  FraudProofComputationThreadRedeemer,
  FraudProofTokenDatum,
  FraudProofTokenMintRedeemer,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireOwnSpendPurpose,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";

import { selectFeeInput } from "../../src/step-support.js";
import { ValueNotPreservedStep04SpendRedeemer } from "../../src/value-not-preserved/schemas.js";
import { requireValueNotPreservedThreadUtxo } from "../../src/value-not-preserved/submit-common.js";
import {
  type FaultProofWitnessReferenceScripts,
  witnessMintingPolicyCarriage,
  witnessSpendingValidatorCarriage,
} from "../../src/witness-reference-scripts.js";
import { type ValueNotPreservedHarness } from "./value-not-preserved-emulator.commit-header-after-anchor-block.js";

/**
 * A raw step-04 finalize: the honest submitter's thread-burn/token-mint
 * transaction minus its local conviction twin, so a thread whose completed
 * fold the validator must refuse — balanced, or unbalanced against the
 * claimed direction — reaches the exact
 * `value_not_preserved_fault_is_established_v1` check on-chain.
 */
export const submitRawValueNotPreservedFinalize = async ({
  harness,
  threadOutRef,
  referenceScriptUtxo,
  witnessReferenceScripts,
}: {
  readonly harness: ValueNotPreservedHarness;
  readonly threadOutRef: string;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
}): Promise<string> => {
  const lucid = harness.proverLucid;
  const signer = harness.proverSigner;
  const contracts = harness.family;
  const { threadUtxo, threadToken } = await requireValueNotPreservedThreadUtxo({
    lucid,
    contracts,
    categoryId: harness.category.categoryId,
    stepIndex: 3,
    threadOutRef,
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
  const spendRedeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "raw value-not-preserved finalize");
    const outputIndex = ctx.outputs.findIndex(
      (output) => output.address === contracts.fraudProof.spendingScriptAddress,
    );
    if (outputIndex < 0) {
      throw new Error("raw finalize built no fraud-proof output");
    }
    return Data.to(
      {
        Continue: [
          {
            input_index: requireInputIndex(
              ctx,
              threadUtxo,
              "raw value-not-preserved finalize",
            ),
            output_index: BigInt(outputIndex),
            fraud_proof_mint_redeemer_index: requireMintRedeemerIndex(
              ctx,
              contracts.fraudProof.policyId,
              "raw value-not-preserved fraud-proof",
            ),
          },
        ],
      },
      ValueNotPreservedStep04SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const threadBurnRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.computationThread.policyId,
      "raw value-not-preserved thread burn",
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
      "raw value-not-preserved fraud-proof mint",
    );
    return Data.to(
      {
        computation_thread_token_asset_name: threadToken.assetName,
        computation_thread_mint_redeemer_index: requireMintRedeemerIndex(
          ctx,
          contracts.computationThread.policyId,
          "raw value-not-preserved thread burn",
        ),
      },
      FraudProofTokenMintRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

  const computationThreadMintCarriage = witnessMintingPolicyCarriage({
    script: contracts.computationThread.mintingScript,
    referenceUtxo: witnessReferenceScripts.computationThreadMint,
    label: "raw value-not-preserved computation-thread mint",
  });
  const fraudProofMintCarriage = witnessMintingPolicyCarriage({
    script: contracts.fraudProof.mintingScript,
    referenceUtxo: witnessReferenceScripts.fraudProofMint,
    label: "raw value-not-preserved fraud-proof mint",
  });
  const stepCarriage = witnessSpendingValidatorCarriage({
    script: contracts.steps[3].spendingScript,
    referenceUtxo: referenceScriptUtxo,
    label: "raw value-not-preserved step-04",
  });
  const referenceInputs = [
    ...stepCarriage.referenceInputs,
    ...computationThreadMintCarriage.referenceInputs,
    ...fraudProofMintCarriage.referenceInputs,
  ];

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
  const withReferences = base.readFrom(referenceInputs);
  const tx = fraudProofMintCarriage.attach(
    computationThreadMintCarriage.attach(stepCarriage.attach(withReferences)),
  );
  const unsigned = await tx.complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  return txHash;
};
