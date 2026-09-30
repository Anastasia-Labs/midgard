import {
  FraudProofComputationThreadRedeemer,
  FraudProofTokenDatum,
  FraudProofTokenMintRedeemer,
  type InputSetUniquenessStep02Args,
  InputSetUniquenessStep02SpendRedeemer,
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

import { requireInputSetUniquenessThreadUtxo } from "../../src/input-set-uniqueness/submit-common.js";
import { selectFeeInput } from "../../src/step-support.js";
import {
  type FaultProofWitnessReferenceScripts,
  witnessMintingPolicyCarriage,
  witnessSpendingValidatorCarriage,
} from "../../src/witness-reference-scripts.js";
import { type InputSetUniquenessHarness } from "./input-set-uniqueness-emulator.build-input-set-uniqueness-fixture.js";
import { type RawInputSetUniquenessFinalizeLayout } from "./input-set-uniqueness-emulator.submit-raw-input-set-uniqueness-bind.js";

/**
 * A raw step-02 finalize: the honest submitter's thread-burn/token-mint
 * transaction with a caller-built `Continue` argument and NONE of the local
 * conviction twins, so a claim the validator must refuse — unequal items,
 * `i >= j`, an out-of-range index — reaches the exact on-chain check.
 */
export const submitRawInputSetUniquenessFinalize = async ({
  harness,
  threadOutRef,
  buildArgs,
  referenceScriptUtxo,
  witnessReferenceScripts,
}: {
  readonly harness: InputSetUniquenessHarness;
  readonly threadOutRef: string;
  readonly buildArgs: (
    layout: RawInputSetUniquenessFinalizeLayout,
  ) => InputSetUniquenessStep02Args;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
}): Promise<string> => {
  const lucid = harness.proverLucid;
  const signer = harness.proverSigner;
  const contracts = harness.family;
  const { threadUtxo, threadToken } = await requireInputSetUniquenessThreadUtxo(
    {
      lucid,
      contracts,
      categoryId: harness.category.categoryId,
      stepIndex: 1,
      threadOutRef,
    },
  );
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
    requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      "raw input-set-uniqueness finalize",
    );
    const outputIndex = ctx.outputs.findIndex(
      (output) => output.address === contracts.fraudProof.spendingScriptAddress,
    );
    if (outputIndex < 0) {
      throw new Error("raw finalize built no fraud-proof output");
    }
    return Data.to(
      {
        Continue: [
          buildArgs({
            inputIndex: requireInputIndex(
              ctx,
              threadUtxo,
              "raw input-set-uniqueness finalize",
            ),
            outputIndex: BigInt(outputIndex),
            fraudProofMintRedeemerIndex: requireMintRedeemerIndex(
              ctx,
              contracts.fraudProof.policyId,
              "raw input-set-uniqueness fraud-proof",
            ),
          }),
        ],
      },
      InputSetUniquenessStep02SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const threadBurnRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.computationThread.policyId,
      "raw input-set-uniqueness thread burn",
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
      "raw input-set-uniqueness fraud-proof mint",
    );
    return Data.to(
      {
        computation_thread_token_asset_name: threadToken.assetName,
        computation_thread_mint_redeemer_index: requireMintRedeemerIndex(
          ctx,
          contracts.computationThread.policyId,
          "raw input-set-uniqueness thread burn",
        ),
      },
      FraudProofTokenMintRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

  const computationThreadMintCarriage = witnessMintingPolicyCarriage({
    script: contracts.computationThread.mintingScript,
    referenceUtxo: witnessReferenceScripts.computationThreadMint,
    label: "raw input-set-uniqueness computation-thread mint",
  });
  const fraudProofMintCarriage = witnessMintingPolicyCarriage({
    script: contracts.fraudProof.mintingScript,
    referenceUtxo: witnessReferenceScripts.fraudProofMint,
    label: "raw input-set-uniqueness fraud-proof mint",
  });
  const stepCarriage = witnessSpendingValidatorCarriage({
    script: contracts.steps[1].spendingScript,
    referenceUtxo: referenceScriptUtxo,
    label: "raw input-set-uniqueness step-02",
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
