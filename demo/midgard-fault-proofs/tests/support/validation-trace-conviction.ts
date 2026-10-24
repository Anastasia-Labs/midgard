/** Validation-trace (category 00000006) proof tokens for removal tests.
 *
 * The proved fixture deploys the validation-trace dispute chain as its
 * always-succeeds scaffold, so a thread opened by the real computation-thread
 * `Init` can finish at once: the real `Success` burn plus the real fraud-proof
 * mint. The token is the authentic permanent record under the deployed
 * fraud-proof policy; only the dispute game before it is skipped. */
import * as SDK from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  mintingPolicyToId,
  scriptFromNative,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";

import { outRefLabel } from "../../src/runtime.js";
import { submitResolvedInit } from "../../src/submit-init.submit-resolved-init.js";
import { type ProvedDoubleSpendFixture } from "./submit-init-emulator-fixtures.js";
import { network } from "./submit-init-emulator-shared.js";

export const VALIDATION_TRACE_CATEGORY_ID =
  SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.validationTraceDispute;

/** The live queue node holding `unit`, as an Init reference input names it. */
export const liveQueueNodeOutRef = async (
  fixture: ProvedDoubleSpendFixture,
  unit: string,
): Promise<string> => {
  const [node] = await fixture.funderLucid.utxosAtWithUnit(
    fixture.contracts.stateQueue.spendingScriptAddress,
    unit,
  );
  if (node === undefined) throw new Error(`queue node ${unit} is absent`);
  return outRefLabel(node);
};

/** Mint one validation-trace proof token for the queue block at
 * `blockOutRef` and pay it to `home` (the fraud-proof address by default). */
export const mintValidationTraceProof = async (
  fixture: ProvedDoubleSpendFixture,
  {
    blockOutRef,
    home = fixture.step04Result.fraudProofAddress,
  }: { readonly blockOutRef: string; readonly home?: string },
): Promise<UTxO> => {
  const { contracts, catalogue, proverLucid, witnessReferenceScripts } =
    fixture;
  const firstStep = contracts.fraudProofs.validationTraceDispute;
  const category = catalogue.categories.validationTraceDispute;
  if (category === undefined)
    throw new Error("catalogue has no validation-trace category");
  const init = await submitResolvedInit({
    lucid: proverLucid,
    blueprint: fixture.realBlueprint,
    network,
    label: "validation-trace scaffold",
    contracts: {
      steps: [firstStep],
      computationThread: contracts.computationThread,
      hubOraclePolicyId: contracts.hubOracle.policyId,
      stateQueuePolicyId: contracts.stateQueue.policyId,
    },
    category,
    catalogue: {
      policyId: contracts.fraudProofCatalogue.policyId,
      spendingScriptAddress:
        contracts.fraudProofCatalogue.spendingScriptAddress,
      root: catalogue.root,
    },
    signer: fixture.proverSigner,
    fraudulentBlockOutRef: blockOutRef,
    witnessReferenceScripts,
    awaitConfirmation: true,
  });
  const [thread] = await proverLucid.utxosAtWithUnit(
    init.firstStepAddress,
    init.computationThreadUnit,
  );
  const assetName = init.computationThreadAssetName;
  const proofUnit = toUnit(contracts.fraudProof.policyId, assetName);
  const proofMintRedeemer = ((ctx) =>
    Data.to(
      {
        computation_thread_token_asset_name: assetName,
        computation_thread_mint_redeemer_index: SDK.requireMintRedeemerIndex(
          ctx,
          contracts.computationThread.policyId,
          "validation-trace scaffold success",
        ),
      },
      SDK.FraudProofTokenMintRedeemer,
    )) satisfies BuildTxWithRedeemer;
  const tx = await proverLucid
    .newTx()
    .collectFrom([thread!], Data.void())
    .attach.SpendingValidator(firstStep.spendingScript)
    .readFrom([
      witnessReferenceScripts.computationThreadMint!,
      witnessReferenceScripts.fraudProofMint!,
    ])
    .mintAssets(
      { [init.computationThreadUnit]: -1n },
      Data.to(
        { Success: { burning_token_asset_name: assetName } },
        SDK.FraudProofComputationThreadRedeemer,
      ),
    )
    .mintAssets({ [proofUnit]: 1n }, proofMintRedeemer)
    .pay.ToContract(
      home,
      {
        kind: "inline",
        value: Data.to(
          { fraud_prover: init.fraudProver },
          SDK.FraudProofTokenDatum,
        ),
      },
      { [proofUnit]: 1n },
    )
    .complete({ localUPLCEval: true });
  const txHash = await (await tx.sign.withWallet().complete()).submit();
  await proverLucid.awaitTx(txHash);
  const proof = (await proverLucid.utxosAtWithUnit(home, proofUnit)).find(
    (utxo) => utxo.txHash === txHash,
  );
  if (proof === undefined) throw new Error("validation-trace proof absent");
  return proof;
};

/** A token named like a validation-trace proof of `headerHash`, issued by a
 * native policy the prover controls and held at the fraud-proof address. */
export const mintForeignValidationTraceToken = async (
  fixture: ProvedDoubleSpendFixture,
  headerHash: string,
): Promise<UTxO> => {
  const issuer = scriptFromNative({
    type: "sig",
    keyHash: fixture.proverPaymentKeyHash,
  });
  const unit = toUnit(
    mintingPolicyToId(issuer),
    VALIDATION_TRACE_CATEGORY_ID + headerHash,
  );
  const tx = await fixture.proverLucid
    .newTx()
    .mintAssets({ [unit]: 1n })
    .attach.MintingPolicy(issuer)
    .pay.ToContract(
      fixture.step04Result.fraudProofAddress,
      {
        kind: "inline",
        value: Data.to(
          { fraud_prover: fixture.proverPaymentKeyHash },
          SDK.FraudProofTokenDatum,
        ),
      },
      { [unit]: 1n },
    )
    .complete();
  const txHash = await (await tx.sign.withWallet().complete()).submit();
  await fixture.proverLucid.awaitTx(txHash);
  return (await fixture.proverLucid.utxoByUnit(unit))!;
};
