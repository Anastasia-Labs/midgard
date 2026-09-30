import {
  FraudProofComputationThreadRedeemer,
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
  type UTxO,
} from "@lucid-evolution/lucid";

import type { NativeScriptDecodingContracts } from "../../src/native-script-decoding/contracts.js";
import {
  type NativeScriptDecodingStepIndex,
  requireNativeScriptDecodingReferenceScript,
} from "../../src/native-script-decoding/submit-common.js";
import { type ResolvedProverSigner } from "../../src/runtime.js";
import { excludeUtxo } from "../../src/spend-input-witness.js";
import { selectFeeInput } from "../../src/step-support.js";
import { computationThreadOutputPredicate } from "../../src/tx-layout.js";
import { witnessMintingPolicyCarriage } from "../../src/witness-reference-scripts.js";
import {
  makeDecodingEmulatorHarness,
  RawCancelSpendRedeemer,
} from "./native-script-decoding-emulator.setup-decoding-scenario.js";

/** The thread layout a raw redeemer builder is handed. */
export type RawDecodingStepLayout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
};

/**
 * A test-only raw thread advancement — the same transaction shape the
 * step-02/step-03 submitters build (fee input, thread spend, advanced state
 * paid to `destinationAddress`, carriage read as reference inputs, Q3
 * step-script sourcing), with NONE of their fail-closed pre-checks.
 *
 * The adversarial suite needs this because every attack it exercises is one
 * the honest submitters refuse locally, before anything is paid for. To
 * observe the ON-CHAIN refusal — the check that actually protects an honest
 * operator against a prover who patched their own tooling — the transaction
 * has to be built past those guards. Production code never takes this path.
 */
export const submitRawDecodingStep = async ({
  lucid,
  contracts,
  signer,
  stepIndex,
  threadUtxo,
  threadUnit,
  destinationAddress,
  nextDatumCbor,
  buildRedeemer,
  carriageUtxos = [],
  referenceScriptUtxo,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: NativeScriptDecodingContracts;
  readonly signer: ResolvedProverSigner;
  readonly stepIndex: number;
  readonly threadUtxo: UTxO;
  readonly threadUnit: string;
  readonly destinationAddress: string;
  readonly nextDatumCbor: string;
  readonly buildRedeemer: (layout: RawDecodingStepLayout) => string;
  readonly carriageUtxos?: readonly UTxO[];
  readonly referenceScriptUtxo: UTxO;
}): Promise<string> => {
  signer.selectWallet(lucid);
  const walletUtxos = await lucid.wallet().getUtxos();
  const walletUtxosSansCarriage = carriageUtxos.reduce<readonly UTxO[]>(
    (candidates, utxo) => excludeUtxo(candidates, utxo),
    walletUtxos,
  );
  const feeInput = selectFeeInput(walletUtxosSansCarriage);
  const outputMatches = computationThreadOutputPredicate({
    address: destinationAddress,
    datum: nextDatumCbor,
    unit: threadUnit,
  });
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "raw decoding step");
    return buildRedeemer({
      inputIndex: requireInputIndex(ctx, threadUtxo, "raw decoding step"),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        outputMatches,
        "raw decoding step output",
      ),
    });
  }) satisfies BuildTxWithRedeemer;
  const stepContract = contracts.steps[stepIndex];
  if (stepContract === undefined || stepIndex < 0 || stepIndex > 5) {
    throw new Error(
      `raw decoding step index ${stepIndex.toString()} is invalid`,
    );
  }
  const stepReference = requireNativeScriptDecodingReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: stepContract.spendingScriptHash,
    stepIndex: stepIndex as NativeScriptDecodingStepIndex,
  });

  const withReferences = (() => {
    const base = lucid
      .newTx()
      .collectFrom([feeInput])
      .collectFrom([threadUtxo], redeemer);
    const referenceInputs = [...carriageUtxos, stepReference];
    return referenceInputs.length === 0 ? base : base.readFrom(referenceInputs);
  })();
  const paid = withReferences.pay
    .ToContract(
      destinationAddress,
      { kind: "inline", value: nextDatumCbor },
      { lovelace: threadUtxo.assets.lovelace ?? 0n, [threadUnit]: 1n },
    )
    .addSignerKey(signer.paymentKeyHash);
  const tx = paid;

  const unsigned = await tx.complete({
    localUPLCEval: true,
    ...(carriageUtxos.length === 0
      ? {}
      : { presetWalletInputs: walletUtxosSansCarriage as UTxO[] }),
  });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  return txHash;
};

/**
 * A test-only raw `ct.Cancel` — the cancel submitter's transaction without
 * its "only the named prover can cancel" pre-check, so a third party's
 * attempt reaches the validator's own signature demand.
 */
export const submitRawDecodingCancel = async ({
  lucid,
  contracts,
  signer,
  stepIndex,
  threadUtxo,
  threadUnit,
  threadAssetName,
  referenceScriptUtxo,
  computationThreadReferenceUtxo,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: NativeScriptDecodingContracts;
  readonly signer: ResolvedProverSigner;
  readonly stepIndex: number;
  readonly threadUtxo: UTxO;
  readonly threadUnit: string;
  readonly threadAssetName: string;
  readonly referenceScriptUtxo: UTxO;
  readonly computationThreadReferenceUtxo: UTxO;
}): Promise<string> => {
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const spendRedeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "raw decoding cancel");
    return Data.to(
      {
        Cancel: {
          input_index: requireInputIndex(
            ctx,
            threadUtxo,
            "raw decoding cancel",
          ),
          computation_thread_mint_redeemer_index: requireMintRedeemerIndex(
            ctx,
            contracts.computationThread.policyId,
            "raw decoding cancel burn",
          ),
        },
      },
      RawCancelSpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const burnRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.computationThread.policyId,
      "raw decoding cancel burn",
    );
    return Data.to(
      { BurnForCancellation: { burning_token_asset_name: threadAssetName } },
      FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const stepContract = contracts.steps[stepIndex];
  if (stepContract === undefined || stepIndex < 0 || stepIndex > 5) {
    throw new Error(
      `raw decoding cancel step index ${stepIndex.toString()} is invalid`,
    );
  }
  const stepReference = requireNativeScriptDecodingReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: stepContract.spendingScriptHash,
    stepIndex: stepIndex as NativeScriptDecodingStepIndex,
  });
  const computationThreadCarriage = witnessMintingPolicyCarriage({
    script: contracts.computationThread.mintingScript,
    referenceUtxo: computationThreadReferenceUtxo,
    label: "raw decoding cancel computation-thread mint",
  });

  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spendRedeemer)
    .mintAssets({ [threadUnit]: -1n }, burnRedeemer)
    .addSignerKey(signer.paymentKeyHash)
    .readFrom([stepReference, ...computationThreadCarriage.referenceInputs]);
  const tx = computationThreadCarriage.attach(base);
  const unsigned = await tx.complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  return txHash;
};

/**
 * Funds the harness's third-party wallet with plain, token-free outputs so
 * its own transactions can always pay their fee.
 *
 * Deliberately NOT folded into the harness: the funder's opening UTxO is the
 * one-shot nonce the contracts are parameterised by, and the setup
 * transaction has to be the one that spends it.
 */
export const fundDecodingOutsider = async (
  harness: Awaited<ReturnType<typeof makeDecodingEmulatorHarness>>,
): Promise<void> => {
  // Both of the outsider's addresses are funded. `selectWallet.fromSeed`
  // derives the seed's base address while `resolveProverSigner` derives its
  // enterprise address, and the raw drivers re-select through the signer, so
  // funding only the base address strands every transaction the outsider
  // builds after that call.
  const outsiderAddress = await harness.outsiderLucid.wallet().address();
  const funding = await harness.funderLucid
    .newTx()
    .pay.ToAddress(outsiderAddress, { lovelace: 1_000_000_000n })
    .pay.ToAddress(outsiderAddress, { lovelace: 1_000_000_000n })
    .pay.ToAddress(harness.outsiderSigner.address, { lovelace: 1_000_000_000n })
    .pay.ToAddress(harness.outsiderSigner.address, { lovelace: 1_000_000_000n })
    .complete();
  const signed = await funding.sign.withWallet().complete();
  await harness.funderLucid.awaitTx(await signed.submit());
};
