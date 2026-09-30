import { asDataType } from "@al-ft/midgard-core/lucid-data";
import * as SDK from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  credentialToAddress,
  Data,
  type LucidEvolution,
  scriptHashToCredential,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { type DoubleWithdrawContracts } from "../src/double-withdraw/contracts.js";
import {
  deriveDoubleWithdrawMembership,
  type SubmitDoubleWithdrawInclusion,
} from "../src/double-withdraw/submit-double-withdraw-step-01.js";
import { fetchUtxoByOutRef, parseOutRef } from "../src/runtime.js";
import {
  requireComputationThreadToken,
  selectFeeInput,
} from "../src/step-support.js";
import { outputWithDatumAndUnitPredicate } from "../src/tx-layout.js";
import { makeHarness } from "./submit-init-emulator-double-withdraw.setup-block.js";
import {
  expectSingleUtxoWithUnit,
  network,
} from "./support/submit-init-emulator-shared.js";

/** Terminal transaction without the submitter's decisive local rule check. */
export const submitRawTerminal = async ({
  harness,
  signer,
  threadOutRef,
  blockOutRef,
  inclusion,
  referenceScript,
}: {
  readonly harness: Awaited<ReturnType<typeof makeHarness>>;
  readonly signer: typeof harness.proverSigner;
  readonly threadOutRef: string;
  readonly blockOutRef: string;
  readonly inclusion: SubmitDoubleWithdrawInclusion;
  readonly referenceScript: UTxO;
}): Promise<string> => {
  const { doubleWithdraw: contracts, category, proverLucid: lucid } = harness;
  const computationThreadReference =
    harness.witnessReferenceScripts.computationThreadMint;
  const fraudProofReference = harness.witnessReferenceScripts.fraudProofMint;
  if (
    computationThreadReference === undefined ||
    fraudProofReference === undefined
  ) {
    throw new Error("double-withdraw witness reference scripts missing");
  }
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "raw terminal thread"),
    label: "raw double-withdraw terminal thread",
  });
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: category.categoryId,
    categoryLabel: "double-withdraw",
  });
  const [hubOracleUtxo, blockUtxo] = await Promise.all([
    expectSingleUtxoWithUnit(
      lucid,
      credentialToAddress(
        network,
        scriptHashToCredential(contracts.hubOraclePolicyId),
      ),
      toUnit(contracts.hubOraclePolicyId, SDK.HUB_ORACLE_ASSET_NAME),
    ),
    fetchUtxoByOutRef({
      lucid,
      outRef: parseOutRef(blockOutRef, "raw terminal block"),
      label: "raw terminal state-queue block",
    }),
  ]);
  const node = await Effect.runPromise(
    SDK.getLinkedListNodeViewFromUTxO(blockUtxo),
  );
  const header = await Effect.runPromise(
    SDK.getHeaderFromStateQueueDatum(node),
  );
  const { committedWithdrawal } = await deriveDoubleWithdrawMembership({
    header,
    inclusion,
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
  const fraudProofOutputMatches = outputWithDatumAndUnitPredicate({
    address: contracts.fraudProof.spendingScriptAddress,
    datum: fraudProofDatum,
    unit: fraudProofUnit,
  });
  let outputIndex: bigint | undefined;
  const spendRedeemer = ((ctx) => {
    const ownInputIndex = SDK.requireInputIndex(
      ctx,
      threadUtxo,
      "raw double-withdraw terminal",
    );
    outputIndex = SDK.requireUniqueOutputIndex(
      ctx.outputs,
      fraudProofOutputMatches,
      "raw double-withdraw fraud-proof output",
    );
    return Data.to(
      {
        Continue: [
          {
            input_index: ownInputIndex,
            output_index: outputIndex,
            fraud_proof_mint_redeemer_index: SDK.requireMintRedeemerIndex(
              ctx,
              contracts.fraudProof.policyId,
              "raw double-withdraw fraud-proof mint",
            ),
            hub_ref_input_index: SDK.requireReferenceInputIndex(
              ctx,
              hubOracleUtxo,
              "raw double-withdraw hub",
            ),
            state_queue_node_ref_input_index: SDK.requireReferenceInputIndex(
              ctx,
              blockUtxo,
              "raw double-withdraw block",
            ),
            committed_withdrawal: committedWithdrawal,
          },
        ],
      },
      SDK.DoubleWithdrawStep02SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const threadBurn = ((ctx) => {
    SDK.requireOwnMintPurpose(
      ctx,
      contracts.computationThread.policyId,
      "raw terminal thread burn",
    );
    return Data.to(
      { Success: { burning_token_asset_name: threadToken.assetName } },
      SDK.FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const proofMint = ((ctx) => {
    SDK.requireOwnMintPurpose(
      ctx,
      contracts.fraudProof.policyId,
      "raw terminal fraud-proof mint",
    );
    return Data.to(
      {
        computation_thread_token_asset_name: threadToken.assetName,
        computation_thread_mint_redeemer_index: SDK.requireMintRedeemerIndex(
          ctx,
          contracts.computationThread.policyId,
          "raw terminal thread burn",
        ),
      },
      SDK.FraudProofTokenMintRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const terminalBase = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spendRedeemer)
    .readFrom([
      hubOracleUtxo,
      blockUtxo,
      referenceScript,
      computationThreadReference,
      fraudProofReference,
    ])
    .mintAssets({ [threadToken.unit]: -1n }, threadBurn)
    .mintAssets({ [fraudProofUnit]: 1n }, proofMint)
    .pay.ToContract(
      contracts.fraudProof.spendingScriptAddress,
      { kind: "inline", value: fraudProofDatum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [fraudProofUnit]: 1n,
      },
    )
    .addSignerKey(signer.paymentKeyHash);
  const unsigned = await terminalBase.complete({ localUPLCEval: true });
  if (outputIndex === undefined) throw new Error("raw terminal layout missing");
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  return txHash;
};

export const submitRawCancel = async ({
  lucid,
  contracts,
  signer,
  threadUtxo,
  categoryId,
  referenceScript,
  computationThreadReference,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: DoubleWithdrawContracts;
  readonly signer: Awaited<ReturnType<typeof makeHarness>>["proverSigner"];
  readonly threadUtxo: UTxO;
  readonly categoryId: string;
  readonly referenceScript: UTxO;
  readonly computationThreadReference: UTxO;
}): Promise<string> => {
  const rawCancelSchema = SDK.faultProofStepRedeemerSchema(Data.Any());
  type RawCancelRedeemer = Data.Static<typeof rawCancelSchema>;
  const RawCancelRedeemer = asDataType<RawCancelRedeemer>(rawCancelSchema);
  const token = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId,
    categoryLabel: "double-withdraw",
  });
  signer.selectWallet(lucid);
  const fee = selectFeeInput(await lucid.wallet().getUtxos());
  const spend = ((ctx) =>
    Data.to(
      {
        Cancel: {
          input_index: SDK.requireInputIndex(ctx, threadUtxo, "raw cancel"),
          computation_thread_mint_redeemer_index: SDK.requireMintRedeemerIndex(
            ctx,
            contracts.computationThread.policyId,
            "raw cancel burn",
          ),
        },
      },
      RawCancelRedeemer,
    )) satisfies BuildTxWithRedeemer;
  const burn = ((ctx) => {
    SDK.requireOwnMintPurpose(
      ctx,
      contracts.computationThread.policyId,
      "raw cancel burn",
    );
    return Data.to(
      { BurnForCancellation: { burning_token_asset_name: token.assetName } },
      SDK.FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const cancelBase = lucid
    .newTx()
    .collectFrom([fee])
    .collectFrom([threadUtxo], spend)
    .readFrom([referenceScript, computationThreadReference])
    .mintAssets({ [token.unit]: -1n }, burn)
    .addSignerKey(signer.paymentKeyHash);
  const unsigned = await cancelBase.complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  return txHash;
};
