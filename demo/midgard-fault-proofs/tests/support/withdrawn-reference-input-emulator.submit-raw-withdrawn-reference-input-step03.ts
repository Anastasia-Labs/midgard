import "./withdrawn-reference-input-emulator.setup-withdrawn-reference-input-unchecked-scenario.js";

import { asDataType } from "@al-ft/midgard-core/lucid-data";
import * as SDK from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";

import type { ResolvedProverSigner } from "../../src/runtime.js";
import { selectFeeInput } from "../../src/step-support.js";
import {
  computationThreadOutputPredicate,
  outputWithDatumAndUnitPredicate,
} from "../../src/tx-layout.js";
import type { WithdrawnReferenceInputContracts } from "../../src/withdrawn-reference-input/contracts.js";
import {
  requireWithdrawnReferenceInputReferenceScript,
  requireWithdrawnReferenceInputThreadUtxo,
} from "../../src/withdrawn-reference-input/submit-common.js";
import {
  type FaultProofWitnessReferenceScripts,
  witnessMintingPolicyCarriage,
} from "../../src/witness-reference-scripts.js";

export type RawWithdrawnReferenceInputStepLayout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
};

const RawWithdrawnCancelRedeemerSchema = SDK.faultProofStepRedeemerSchema(
  Data.Any(),
);

export type RawWithdrawnCancelRedeemer = Data.Static<
  typeof RawWithdrawnCancelRedeemerSchema
>;

export const RawWithdrawnCancelRedeemer =
  asDataType<RawWithdrawnCancelRedeemer>(RawWithdrawnCancelRedeemerSchema);

/** Test-only advancement that bypasses the honest step-02 guards. */
export const submitRawWithdrawnReferenceInputStep02 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  nextDatumCbor,
  buildRedeemer,
  referenceScriptUtxo,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: WithdrawnReferenceInputContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly nextDatumCbor: string;
  readonly buildRedeemer: (
    layout: RawWithdrawnReferenceInputStepLayout,
  ) => string;
  readonly referenceScriptUtxo: UTxO;
}): Promise<string> => {
  const { threadUtxo, threadToken } =
    await requireWithdrawnReferenceInputThreadUtxo({
      lucid,
      contracts,
      categoryId,
      stepIndex: 1,
      threadOutRef,
    });
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[2].spendingScriptAddress,
    datum: nextDatumCbor,
    unit: threadToken.unit,
  });
  const redeemer = ((ctx) => {
    SDK.requireOwnSpendPurpose(ctx, threadUtxo, "raw withdrawn step 02");
    return buildRedeemer({
      inputIndex: SDK.requireInputIndex(
        ctx,
        threadUtxo,
        "raw withdrawn step 02",
      ),
      outputIndex: SDK.requireUniqueOutputIndex(
        ctx.outputs,
        outputMatches,
        "raw withdrawn step 02 output",
      ),
    });
  }) satisfies BuildTxWithRedeemer;
  signer.selectWallet(lucid);
  const reference = requireWithdrawnReferenceInputReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[1].spendingScriptHash,
    stepIndex: 1,
  });
  const unsigned = await lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .readFrom([reference])
    .pay.ToContract(
      contracts.steps[2].spendingScriptAddress,
      { kind: "inline", value: nextDatumCbor },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [threadToken.unit]: 1n,
      },
    )
    .addSignerKey(signer.paymentKeyHash)
    .complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  return txHash;
};

/** Test-only finalizer that sends an arbitrary membership proof on-chain. */
export const submitRawWithdrawnReferenceInputStep03 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  withdrawalMembership,
  referenceScriptUtxo,
  witnessReferenceScripts,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: WithdrawnReferenceInputContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly withdrawalMembership: SDK.WithdrawalSourceMembershipProof;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
}): Promise<string> => {
  const { threadUtxo, threadToken } =
    await requireWithdrawnReferenceInputThreadUtxo({
      lucid,
      contracts,
      categoryId,
      stepIndex: 2,
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
    SDK.FraudProofTokenDatum,
  );
  const outputMatches = outputWithDatumAndUnitPredicate({
    address: contracts.fraudProof.spendingScriptAddress,
    datum: fraudProofDatum,
    unit: fraudProofUnit,
  });
  const spendRedeemer = ((ctx) => {
    SDK.requireOwnSpendPurpose(ctx, threadUtxo, "raw withdrawn step 03");
    return Data.to(
      {
        Continue: [
          {
            input_index: SDK.requireInputIndex(
              ctx,
              threadUtxo,
              "raw withdrawn step 03",
            ),
            output_index: SDK.requireUniqueOutputIndex(
              ctx.outputs,
              outputMatches,
              "raw withdrawn step 03 output",
            ),
            fraud_proof_mint_redeemer_index: SDK.requireMintRedeemerIndex(
              ctx,
              contracts.fraudProof.policyId,
              "raw withdrawn step 03 fraud proof",
            ),
            withdrawal_membership: withdrawalMembership,
          },
        ],
      },
      SDK.WithdrawnReferenceInputStep03SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const threadBurn = ((ctx) => {
    SDK.requireOwnMintPurpose(
      ctx,
      contracts.computationThread.policyId,
      "raw withdrawn step 03 thread burn",
    );
    return Data.to(
      { Success: { burning_token_asset_name: threadToken.assetName } },
      SDK.FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const fraudMint = ((ctx) =>
    Data.to(
      {
        computation_thread_token_asset_name: threadToken.assetName,
        computation_thread_mint_redeemer_index: SDK.requireMintRedeemerIndex(
          ctx,
          contracts.computationThread.policyId,
          "raw withdrawn step 03 thread burn",
        ),
      },
      SDK.FraudProofTokenMintRedeemer,
    )) satisfies BuildTxWithRedeemer;
  const reference = requireWithdrawnReferenceInputReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[2].spendingScriptHash,
    stepIndex: 2,
  });
  const computationThreadCarriage = witnessMintingPolicyCarriage({
    script: contracts.computationThread.mintingScript,
    referenceUtxo: witnessReferenceScripts.computationThreadMint,
    label: "raw withdrawn-reference-input step-03 computation-thread mint",
  });
  const fraudProofCarriage = witnessMintingPolicyCarriage({
    script: contracts.fraudProof.mintingScript,
    referenceUtxo: witnessReferenceScripts.fraudProofMint,
    label: "raw withdrawn-reference-input step-03 fraud-proof mint",
  });
  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spendRedeemer)
    .readFrom([
      reference,
      ...computationThreadCarriage.referenceInputs,
      ...fraudProofCarriage.referenceInputs,
    ])
    .mintAssets({ [threadToken.unit]: -1n }, threadBurn)
    .mintAssets({ [fraudProofUnit]: 1n }, fraudMint)
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
  await lucid.awaitTx(txHash);
  return txHash;
};
