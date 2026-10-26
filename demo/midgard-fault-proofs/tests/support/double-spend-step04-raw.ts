import { FraudProofTokenDatum, MIDGARD_FIELD_INDEX } from "@al-ft/midgard-sdk";
import { Data, toUnit, type UTxO } from "@lucid-evolution/lucid";

import {
  makeComputationThreadSuccessRedeemer,
  makeFraudProofMintRedeemer,
  makeStep04SpendRedeemer,
} from "../../src/double-spend/submit-step-04.make-step04-spend-redeemer.js";
import {
  faultProofFieldOpening,
  planFaultProofFieldOpening,
} from "../../src/field-opening.js";
import { parseSubmitStep01TxInclusion } from "./legacy-submit-emulator.js";
import { type stageDoubleSpendStep04 } from "./submit-init-emulator-fixtures.stage-double-spend-step04.js";

type StagedDoubleSpend = Awaited<ReturnType<typeof stageDoubleSpendStep04>>;

/**
 * Double-spend step 04 built by hand, without the builder's check that tx2's
 * selected input is the one the thread carries: the transaction a prover
 * that skips that check would submit. Returns the submitted transaction id.
 */
export const submitRawDoubleSpendStep04 = async ({
  staged,
  doubleSpentInputIndex,
}: {
  readonly staged: StagedDoubleSpend;
  readonly doubleSpentInputIndex: bigint;
}): Promise<string> => {
  const {
    proverLucid: lucid,
    proverSigner: signer,
    contracts,
    transactionInclusion,
    submitInitResult,
    fourthStepUtxo: threadUtxo,
    doubleSpendStepReferenceScripts,
    witnessReferenceScripts,
  } = staged;
  const verifiedTx2Id = transactionInclusion.tx2.nativeTxId;
  const planned = planFaultProofFieldOpening({
    anchorSourceKind: 0n,
    fieldIndex: MIDGARD_FIELD_INDEX.spendInputs,
    anchorTxId: verifiedTx2Id,
    nativeTxCompactCbor: parseSubmitStep01TxInclusion(
      transactionInclusion.tx2.inclusion,
    ).nativeTxCompactCbor,
    itemCbors: transactionInclusion.tx2SpendInputCbors.map((inputCbor) =>
      Buffer.from(inputCbor, "hex"),
    ),
    owner: signer.paymentKeyHash,
    publish: false,
    label: "raw double-spend step 04 tx2 spend-inputs",
  });
  const fraudProofUnit = toUnit(
    contracts.fraudProof.policyId,
    submitInitResult.computationThreadAssetName,
  );
  const fraudProofDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash },
    FraudProofTokenDatum,
  );
  const { computationThreadMint, fraudProofMint } = witnessReferenceScripts;
  if (computationThreadMint === undefined || fraudProofMint === undefined)
    throw new Error("double-spend fixture deployed no mint reference scripts");
  const referenceInputs: UTxO[] = [
    doubleSpendStepReferenceScripts["fraudProofDoubleSpendStep04"]!.utxo,
    computationThreadMint,
    fraudProofMint,
  ];
  signer.selectWallet(lucid);
  const unsigned = await lucid
    .newTx()
    .collectFrom(
      [threadUtxo],
      makeStep04SpendRedeemer({
        threadUtxo,
        fraudProofAddress: contracts.fraudProof.spendingScriptAddress,
        fraudProofPolicyId: contracts.fraudProof.policyId,
        fraudProofUnit,
        fraudProofDatum,
        tx2SpendInputsOpeningFor: (ctx) =>
          faultProofFieldOpening({
            planned,
            referenceInputs: ctx.referenceInputs,
            label: "raw double-spend step 04 tx2 spend-inputs",
          }),
        doubleSpentInputIndex,
        onLayout: () => {},
      }),
    )
    .readFrom(referenceInputs)
    .mintAssets(
      { [submitInitResult.computationThreadUnit]: -1n },
      makeComputationThreadSuccessRedeemer({
        computationThreadPolicyId: contracts.computationThread.policyId,
        computationThreadAssetName: submitInitResult.computationThreadAssetName,
      }),
    )
    .mintAssets(
      { [fraudProofUnit]: 1n },
      makeFraudProofMintRedeemer({
        fraudProofPolicyId: contracts.fraudProof.policyId,
        computationThreadPolicyId: contracts.computationThread.policyId,
        computationThreadAssetName: submitInitResult.computationThreadAssetName,
        onComputationThreadMintRedeemerIndex: () => {},
      }),
    )
    .pay.ToContract(
      contracts.fraudProof.spendingScriptAddress,
      { kind: "inline", value: fraudProofDatum },
      { lovelace: threadUtxo.assets.lovelace ?? 0n, [fraudProofUnit]: 1n },
    )
    .addSignerKey(signer.paymentKeyHash)
    .complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  return txHash;
};
