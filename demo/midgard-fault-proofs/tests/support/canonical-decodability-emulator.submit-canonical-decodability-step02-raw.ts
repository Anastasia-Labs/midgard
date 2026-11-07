import {
  CanonicalDecodabilityStep02SpendRedeemer,
  FraudProofComputationThreadRedeemer,
  FraudProofTokenDatum,
  FraudProofTokenMintRedeemer,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  type CanonicalDecodabilityContracts,
  requireCanonicalDecodabilityReferenceScript,
  requireCanonicalDecodabilityThreadUtxo,
} from "../../src/canonical-decodability/index.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  type ResolvedProverSigner,
} from "../../src/runtime.js";
import { selectFeeInput } from "../../src/step-support.js";
import { outputWithDatumAndUnitPredicate } from "../../src/tx-layout.js";
import {
  type FaultProofWitnessReferenceScripts,
  witnessMintingPolicyCarriage,
} from "../../src/witness-reference-scripts.js";

/** Guard-bypassing finalizer used to prove verdict-0 is refused on-chain. */
export const submitCanonicalDecodabilityStep02Raw = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  referenceScriptUtxo,
  witnessReferenceScripts,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: CanonicalDecodabilityContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
}): Promise<string> => {
  const { threadUtxo, threadToken } =
    await requireCanonicalDecodabilityThreadUtxo({
      lucid,
      contracts,
      categoryId,
      stepIndex: 1,
      threadOutRef,
    });
  const stepReference = requireCanonicalDecodabilityReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[1].spendingScriptHash,
    stepIndex: 1,
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
  const spendRedeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "raw canonical step-02");
    return Data.to(
      {
        Continue: [
          {
            input_index: requireInputIndex(
              ctx,
              threadUtxo,
              "raw canonical step-02",
            ),
            output_index: requireUniqueOutputIndex(
              ctx.outputs,
              outputMatches,
              "raw canonical step-02 output",
            ),
            fraud_proof_mint_redeemer_index: requireMintRedeemerIndex(
              ctx,
              contracts.fraudProof.policyId,
              "raw canonical fraud-proof mint",
            ),
          },
        ],
      },
      CanonicalDecodabilityStep02SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const burnRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.computationThread.policyId,
      "raw canonical thread burn",
    );
    return Data.to(
      { Success: { burning_token_asset_name: threadToken.assetName } },
      FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const mintRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.fraudProof.policyId,
      "raw canonical fraud-proof mint",
    );
    return Data.to(
      {
        computation_thread_token_asset_name: threadToken.assetName,
        computation_thread_mint_redeemer_index: requireMintRedeemerIndex(
          ctx,
          contracts.computationThread.policyId,
          "raw canonical thread burn",
        ),
      },
      FraudProofTokenMintRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const computationThreadCarriage = witnessMintingPolicyCarriage({
    script: contracts.computationThread.mintingScript,
    referenceUtxo: witnessReferenceScripts.computationThreadMint,
    label: "raw canonical step-02 computation-thread mint",
  });
  const fraudProofCarriage = witnessMintingPolicyCarriage({
    script: contracts.fraudProof.mintingScript,
    referenceUtxo: witnessReferenceScripts.fraudProofMint,
    label: "raw canonical step-02 fraud-proof mint",
  });
  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spendRedeemer)
    .readFrom([
      stepReference,
      ...computationThreadCarriage.referenceInputs,
      ...fraudProofCarriage.referenceInputs,
    ])
    .mintAssets({ [threadToken.unit]: -1n }, burnRedeemer)
    .mintAssets({ [fraudProofUnit]: 1n }, mintRedeemer)
    .pay.ToContract(
      contracts.fraudProof.spendingScriptAddress,
      { kind: "inline", value: fraudProofDatum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [fraudProofUnit]: 1n,
      },
    )
    .addSignerKey(signer.paymentKeyHash);
  const unsigned = await fraudProofCarriage
    .attach(computationThreadCarriage.attach(base))
    .complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  return txHash;
};
