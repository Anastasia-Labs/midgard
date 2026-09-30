import * as SDK from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import type { ResolvedProverSigner } from "../../src/runtime.js";
import { selectFeeInput } from "../../src/step-support.js";
import type { WithdrawnReferenceInputContracts } from "../../src/withdrawn-reference-input/contracts.js";
import {
  requireWithdrawnReferenceInputReferenceScript,
  requireWithdrawnReferenceInputThreadUtxo,
} from "../../src/withdrawn-reference-input/submit-common.js";
import {
  type FaultProofWitnessReferenceScripts,
  witnessMintingPolicyCarriage,
} from "../../src/witness-reference-scripts.js";
import { RawWithdrawnCancelRedeemer } from "./withdrawn-reference-input-emulator.submit-raw-withdrawn-reference-input-step03.js";

/** Test-only cancellation signed by an arbitrary wallet. */
export const submitRawWithdrawnReferenceInputCancel = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  stepIndex,
  threadOutRef,
  referenceScriptUtxo,
  witnessReferenceScripts,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: WithdrawnReferenceInputContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly stepIndex: 0 | 1 | 2;
  readonly threadOutRef: string;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
}): Promise<string> => {
  const { threadUtxo, threadToken } =
    await requireWithdrawnReferenceInputThreadUtxo({
      lucid,
      contracts,
      categoryId,
      stepIndex,
      threadOutRef,
    });
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const spendRedeemer = ((ctx) => {
    SDK.requireOwnSpendPurpose(ctx, threadUtxo, "raw withdrawn cancel");
    return Data.to(
      {
        Cancel: {
          input_index: SDK.requireInputIndex(
            ctx,
            threadUtxo,
            "raw withdrawn cancel",
          ),
          computation_thread_mint_redeemer_index: SDK.requireMintRedeemerIndex(
            ctx,
            contracts.computationThread.policyId,
            "raw withdrawn cancel thread burn",
          ),
        },
      },
      RawWithdrawnCancelRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const threadBurn = ((ctx) => {
    SDK.requireOwnMintPurpose(
      ctx,
      contracts.computationThread.policyId,
      "raw withdrawn cancel thread burn",
    );
    return Data.to(
      {
        BurnForCancellation: {
          burning_token_asset_name: threadToken.assetName,
        },
      },
      SDK.FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const reference = requireWithdrawnReferenceInputReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[stepIndex].spendingScriptHash,
    stepIndex,
  });
  const computationThreadCarriage = witnessMintingPolicyCarriage({
    script: contracts.computationThread.mintingScript,
    referenceUtxo: witnessReferenceScripts.computationThreadMint,
    label: "raw withdrawn-reference-input cancel computation-thread mint",
  });
  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spendRedeemer)
    .readFrom([reference, ...computationThreadCarriage.referenceInputs])
    .mintAssets({ [threadToken.unit]: -1n }, threadBurn)
    .addSignerKey(signer.paymentKeyHash);
  const unsigned = await computationThreadCarriage
    .attach(base)
    .complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  return txHash;
};
