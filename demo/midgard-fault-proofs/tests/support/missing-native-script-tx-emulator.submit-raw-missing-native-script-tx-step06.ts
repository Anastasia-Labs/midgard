import { encodeMidgardFieldPreimage } from "@al-ft/midgard-core";
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import {
  faultProofStepRedeemerSchema,
  fieldOpeningForField,
  FraudProofComputationThreadRedeemer,
  FraudProofTokenDatum,
  FraudProofTokenMintRedeemer,
  MIDGARD_FIELD_INDEX,
  MissingNativeScriptTxStep06SpendRedeemer,
  type NativeTxWitnessSetCompact,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";

import { requireMissingNativeScriptTxThreadUtxo } from "../../src/missing-native-script-tx/submit-common.js";
import { selectFeeInput } from "../../src/step-support.js";
import { outputWithDatumAndUnitPredicate } from "../../src/tx-layout.js";
import { witnessMintingPolicyCarriage } from "../../src/witness-reference-scripts.js";
import { makeMissingNativeScriptTxEmulatorHarness } from "./missing-native-script-tx-emulator.setup-missing-native-script-tx-fixture.js";

export const submitRawMissingNativeScriptTxStep06 = async ({
  harness,
  threadOutRef,
  nativeTxCompactCbor,
  witnessSet,
  scriptTxWitsItems,
  referenceScriptUtxo,
}: {
  readonly harness: Awaited<
    ReturnType<typeof makeMissingNativeScriptTxEmulatorHarness>
  >;
  readonly threadOutRef: string;
  readonly nativeTxCompactCbor: string;
  readonly witnessSet: NativeTxWitnessSetCompact;
  readonly scriptTxWitsItems: readonly Uint8Array[];
  readonly referenceScriptUtxo: UTxO;
}): Promise<string> => {
  const { threadUtxo, threadToken } =
    await requireMissingNativeScriptTxThreadUtxo({
      lucid: harness.proverLucid,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      stepIndex: 5,
      threadOutRef,
    });
  const opening = fieldOpeningForField({
    fieldIndex: MIDGARD_FIELD_INDEX.scriptWitnesses,
    nativeTxCompactCbor,
    witnessSet,
    carriage: {
      Inline: {
        preimage: encodeMidgardFieldPreimage(
          scriptTxWitsItems.map((item) => Buffer.from(item)),
        ).toString("hex"),
      },
    },
  });
  harness.proverSigner.selectWallet(harness.proverLucid);
  const feeInput = selectFeeInput(
    await harness.proverLucid.wallet().getUtxos(),
  );
  const fraudProofUnit = toUnit(
    harness.family.fraudProof.policyId,
    threadToken.assetName,
  );
  const fraudProofDatum = Data.to(
    { fraud_prover: harness.proverSigner.paymentKeyHash },
    FraudProofTokenDatum,
  );
  const outputMatches = outputWithDatumAndUnitPredicate({
    address: harness.family.fraudProof.spendingScriptAddress,
    datum: fraudProofDatum,
    unit: fraudProofUnit,
  });
  const spendRedeemer = ((ctx) => {
    requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      "raw missing-native-script-tx step 06",
    );
    return Data.to(
      {
        Continue: [
          {
            DirectFinalize: {
              input_index: requireInputIndex(
                ctx,
                threadUtxo,
                "raw missing-native-script-tx step 06",
              ),
              output_index: requireUniqueOutputIndex(
                ctx.outputs,
                outputMatches,
                "raw missing-native-script-tx fraud proof",
              ),
              fraud_proof_mint_redeemer_index: requireMintRedeemerIndex(
                ctx,
                harness.family.fraudProof.policyId,
                "raw missing-native-script-tx fraud-proof mint",
              ),
              script_tx_wits_opening: opening,
            },
          },
        ],
      },
      MissingNativeScriptTxStep06SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const burnRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      harness.family.computationThread.policyId,
      "raw missing-native-script-tx thread burn",
    );
    return Data.to(
      { Success: { burning_token_asset_name: threadToken.assetName } },
      FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const mintRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      harness.family.fraudProof.policyId,
      "raw missing-native-script-tx fraud-proof mint",
    );
    return Data.to(
      {
        computation_thread_token_asset_name: threadToken.assetName,
        computation_thread_mint_redeemer_index: requireMintRedeemerIndex(
          ctx,
          harness.family.computationThread.policyId,
          "raw missing-native-script-tx thread burn",
        ),
      },
      FraudProofTokenMintRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const computationThreadCarriage = witnessMintingPolicyCarriage({
    script: harness.family.computationThread.mintingScript,
    referenceUtxo: harness.witnessReferenceScripts.computationThreadMint,
    label: "raw missing-native-script-tx step-06 computation-thread mint",
  });
  const fraudProofCarriage = witnessMintingPolicyCarriage({
    script: harness.family.fraudProof.mintingScript,
    referenceUtxo: harness.witnessReferenceScripts.fraudProofMint,
    label: "raw missing-native-script-tx step-06 fraud-proof mint",
  });
  const base = harness.proverLucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spendRedeemer)
    .readFrom([
      referenceScriptUtxo,
      ...computationThreadCarriage.referenceInputs,
      ...fraudProofCarriage.referenceInputs,
    ])
    .mintAssets({ [threadToken.unit]: -1n }, burnRedeemer)
    .mintAssets({ [fraudProofUnit]: 1n }, mintRedeemer)
    .pay.ToContract(
      harness.family.fraudProof.spendingScriptAddress,
      { kind: "inline", value: fraudProofDatum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [fraudProofUnit]: 1n,
      },
    )
    .addSignerKey(harness.proverSigner.paymentKeyHash);
  const unsigned = await fraudProofCarriage
    .attach(computationThreadCarriage.attach(base))
    .complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await harness.proverLucid.awaitTx(txHash);
  return txHash;
};

const RawCancelSpendRedeemerSchema = faultProofStepRedeemerSchema(Data.Any());

type RawCancelSpendRedeemer = Data.Static<typeof RawCancelSpendRedeemerSchema>;

const RawCancelSpendRedeemer = asDataType<RawCancelSpendRedeemer>(
  RawCancelSpendRedeemerSchema,
);

export const submitRawMissingNativeScriptTxOutsiderCancel = async ({
  harness,
  threadOutRef,
  stepIndex,
  referenceScriptUtxo,
}: {
  readonly harness: Awaited<
    ReturnType<typeof makeMissingNativeScriptTxEmulatorHarness>
  >;
  readonly threadOutRef: string;
  readonly stepIndex: 0 | 1 | 2 | 3 | 4 | 5;
  readonly referenceScriptUtxo: UTxO;
}): Promise<string> => {
  const { threadUtxo, threadToken } =
    await requireMissingNativeScriptTxThreadUtxo({
      lucid: harness.outsiderLucid,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      stepIndex,
      threadOutRef,
    });
  harness.outsiderSigner.selectWallet(harness.outsiderLucid);
  const feeInput = selectFeeInput(
    await harness.outsiderLucid.wallet().getUtxos(),
  );
  const spendRedeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "raw outsider cancel");
    return Data.to(
      {
        Cancel: {
          input_index: requireInputIndex(
            ctx,
            threadUtxo,
            "raw outsider cancel",
          ),
          computation_thread_mint_redeemer_index: requireMintRedeemerIndex(
            ctx,
            harness.family.computationThread.policyId,
            "raw outsider cancel burn",
          ),
        },
      },
      RawCancelSpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const burnRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      harness.family.computationThread.policyId,
      "raw outsider cancel burn",
    );
    return Data.to(
      {
        BurnForCancellation: {
          burning_token_asset_name: threadToken.assetName,
        },
      },
      FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const computationThreadCarriage = witnessMintingPolicyCarriage({
    script: harness.family.computationThread.mintingScript,
    referenceUtxo: harness.witnessReferenceScripts.computationThreadMint,
    label: "raw missing-native-script-tx cancel computation-thread mint",
  });
  const base = harness.outsiderLucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spendRedeemer)
    .readFrom([
      referenceScriptUtxo,
      ...computationThreadCarriage.referenceInputs,
    ])
    .mintAssets({ [threadToken.unit]: -1n }, burnRedeemer)
    .addSignerKey(harness.outsiderSigner.paymentKeyHash);
  const unsigned = await computationThreadCarriage
    .attach(base)
    .complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await harness.outsiderLucid.awaitTx(txHash);
  return txHash;
};
