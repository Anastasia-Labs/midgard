import { replacePlutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import {
  FabricatedDepositStep01SpendRedeemer,
  FabricatedDepositStep02Datum,
  getHeaderFromStateQueueDatum,
  getLinkedListNodeViewFromUTxO,
  HUB_ORACLE_ASSET_NAME,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  credentialToAddress,
  Data,
  type LucidEvolution,
  type Network,
  scriptHashToCredential,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { fabricatedProofValidity } from "./fabricated-proof-validity.js";
import { requireFabricatedReferenceScript } from "./fabricated-reference-script.js";
import { readJsonFile } from "./json-file.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  fetchUtxoByOutRef,
  makeLucidForSubmit,
  outRefLabel,
  parseOutRef,
  requireSingletonUtxo,
  type ResolvedProverSigner,
  resolveFraudulentHeaderHash,
  resolveProverSigner,
} from "./runtime.js";
import {
  requireComputationThreadToken,
  requireInitialStepDatum,
  selectFeeInput,
} from "./step-support.js";
import {
  deriveFabricatedDepositStep01Handoff,
  FABRICATED_DEPOSIT_CATEGORY_LABEL,
  type FabricatedDepositContracts,
  type FabricatedDepositStep01Layout,
  parseSubmitFabricatedDepositInclusion,
  type SubmitFabricatedDepositInclusion,
  type SubmitFabricatedDepositStep01CliConfig,
  type SubmitFabricatedDepositStep01Result,
} from "./submit-fabricated-deposit-step-01.derive-fabricated-deposit-step01-handoff.js";
import { computationThreadOutputPredicate } from "./tx-layout.js";
import {
  type FraudProofPreSubmitBoundary,
  reachFraudProofPreSubmitBoundary,
  workflowReferenceScript,
} from "./workflow/transaction-boundary.js";

export const submitFabricatedDepositStep01 = async ({
  lucid,
  contracts,
  network,
  signer,
  threadOutRef,
  stateQueueBlockOutRef,
  depositInclusion,
  referenceScriptUtxo,
  preSubmitBoundary,
  awaitConfirmation = true,
  now = Date.now,
}: {
  readonly now?: () => number;
  readonly lucid: LucidEvolution;
  readonly contracts: FabricatedDepositContracts;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly stateQueueBlockOutRef: string;
  readonly depositInclusion: SubmitFabricatedDepositInclusion;
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitFabricatedDepositStep01Result> => {
  const parsedThreadOutRef = parseOutRef(threadOutRef, "--thread-out-ref");
  const parsedStateQueueBlockOutRef = parseOutRef(
    stateQueueBlockOutRef,
    "--state-queue-block-out-ref",
  );
  const [threadUtxo, hubOracleUtxo, stateQueueBlockUtxo] = await Promise.all([
    fetchUtxoByOutRef({
      lucid,
      outRef: parsedThreadOutRef,
      label: "fabricated-deposit step-01 computation-thread UTxO",
    }),
    requireSingletonUtxo({
      lucid,
      address: credentialToAddress(
        network,
        scriptHashToCredential(contracts.hubOraclePolicyId),
      ),
      unit: toUnit(contracts.hubOraclePolicyId, HUB_ORACLE_ASSET_NAME),
      label: "hub oracle",
    }),
    fetchUtxoByOutRef({
      lucid,
      outRef: parsedStateQueueBlockOutRef,
      label: "state-queue block UTxO",
    }),
  ]);
  if (threadUtxo.address !== contracts.steps[0].spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at fabricated-deposit step 01.`,
    );
  }
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: contracts.categoryId,
    categoryLabel: FABRICATED_DEPOSIT_CATEGORY_LABEL,
  });
  requireInitialStepDatum({ threadUtxo, signer });
  const stateQueueHeaderHash = resolveFraudulentHeaderHash({
    stateQueuePolicyId: contracts.stateQueuePolicyId,
    fraudulentBlockUtxo: stateQueueBlockUtxo,
  });
  if (stateQueueHeaderHash !== threadToken.fraudulentHeaderHash) {
    throw new Error(
      `State-queue block header hash ${stateQueueHeaderHash} does not match computation-thread header hash ${threadToken.fraudulentHeaderHash}.`,
    );
  }

  const stateQueueNodeView = await Effect.runPromise(
    getLinkedListNodeViewFromUTxO(stateQueueBlockUtxo),
  );
  const header = await Effect.runPromise(
    getHeaderFromStateQueueDatum(stateQueueNodeView),
  );
  const { committedDeposit, step02State } =
    await deriveFabricatedDepositStep01Handoff({
      stateQueuePolicyId: contracts.stateQueuePolicyId,
      header,
      headerHash: stateQueueHeaderHash,
      inclusion: depositInclusion,
    });

  const validity = fabricatedProofValidity(header.endTime, now());
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const referenceInputs = [hubOracleUtxo, stateQueueBlockUtxo];
  const step02Datum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: step02State },
    FabricatedDepositStep02Datum,
  );
  const step02OutputMatches = computationThreadOutputPredicate({
    address: contracts.steps[1].spendingScriptAddress,
    datum: step02Datum,
    unit: threadToken.unit,
  });
  let resolvedLayout: FabricatedDepositStep01Layout | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "fabricated-deposit step 01");
    const layout: FabricatedDepositStep01Layout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        "fabricated-deposit step 01",
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        step02OutputMatches,
        "fabricated-deposit step 01 output",
      ),
      hubOracleRefInputIndex: requireReferenceInputIndex(
        ctx,
        hubOracleUtxo,
        "fabricated-deposit step 01 hub oracle",
      ),
      stateQueueNodeRefInputIndex: requireReferenceInputIndex(
        ctx,
        stateQueueBlockUtxo,
        "fabricated-deposit step 01 state-queue node",
      ),
    };
    resolvedLayout = layout;
    return replacePlutusConstrFieldCbor(
      Data.to(
        {
          Continue: [
            {
              input_index: layout.inputIndex,
              output_index: layout.outputIndex,
              hub_ref_input_index: layout.hubOracleRefInputIndex,
              state_queue_node_ref_input_index:
                layout.stateQueueNodeRefInputIndex,
              committed_deposit: committedDeposit,
            },
          ],
        },
        FabricatedDepositStep01SpendRedeemer,
      ),
      [0, 4, 5],
      depositInclusion.committedDepositInfoCbor,
    );
  }) satisfies BuildTxWithRedeemer;
  const threadAssets = {
    lovelace: threadUtxo.assets.lovelace ?? 0n,
    [threadToken.unit]: 1n,
  };

  const tx = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .readFrom([
      ...referenceInputs,
      requireFabricatedReferenceScript({
        utxo: referenceScriptUtxo,
        expectedScriptHash: contracts.steps[0].spendingScriptHash,
        categoryLabel: FABRICATED_DEPOSIT_CATEGORY_LABEL,
        stepIndex: 0,
      }),
    ])
    .pay.ToContract(
      contracts.steps[1].spendingScriptAddress,
      { kind: "inline", value: step02Datum },
      threadAssets,
    )
    .addSignerKey(signer.paymentKeyHash)
    .validFrom(validity.validFrom)
    .validTo(validity.validTo);

  const unsigned = await tx.complete({ localUPLCEval: true });
  if (resolvedLayout === undefined) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve fabricated-deposit step 01 layout.",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundary({
    signed,
    referenceScripts: [
      workflowReferenceScript({
        role: "V1 fraud-proof fabricated-deposit step-01",
        utxo: referenceScriptUtxo,
        expectedScript: contracts.steps[0].spendingScript,
      }),
    ],
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw new Error(
      `fabricated-deposit step-01 provider returned ${txHash}, expected ${expectedTxHash}.`,
    );
  }
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }

  return {
    txHash,
    walletSource: signer.source,
    proverAddress: signer.address,
    fraudProver: signer.paymentKeyHash,
    threadOutRef,
    nextThreadOutRef: `${txHash}#${resolvedLayout.outputIndex.toString()}`,
    stateQueueBlockOutRef,
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
    computationThreadPolicyId: contracts.computationThread.policyId,
    computationThreadAssetName: threadToken.assetName,
    computationThreadUnit: threadToken.unit,
    firstStepAddress: contracts.steps[0].spendingScriptAddress,
    secondStepAddress: contracts.steps[1].spendingScriptAddress,
    committedDepositIdCbor: depositInclusion.committedDepositIdCbor,
    committedDepositInfoHash: step02State.committed_deposit_info_hash,
    depositsPhasRoot: depositInclusion.depositsPhasRoot,
    committedDepositsRoot: header.depositsRoot,
    inputIndex: Number(resolvedLayout.inputIndex),
    outputIndex: Number(resolvedLayout.outputIndex),
    hubOracleRefInputIndex: Number(resolvedLayout.hubOracleRefInputIndex),
    stateQueueNodeRefInputIndex: Number(
      resolvedLayout.stateQueueNodeRefInputIndex,
    ),
    awaitedConfirmation: awaitConfirmation,
  };
};

export const submitFabricatedDepositStep01FromFiles = async (
  config: SubmitFabricatedDepositStep01CliConfig & {
    readonly contracts: FabricatedDepositContracts;
    readonly referenceScriptUtxo: UTxO;
  },
): Promise<SubmitFabricatedDepositStep01Result> => {
  const [depositInclusionJson, lucid] = await Promise.all([
    readJsonFile(config.depositInclusionPath),
    makeLucidForSubmit(config),
  ]);
  const signer = resolveProverSigner(config);
  return await submitFabricatedDepositStep01({
    lucid,
    contracts: config.contracts,
    network: config.network,
    signer,
    threadOutRef: config.threadOutRef,
    stateQueueBlockOutRef: config.stateQueueBlockOutRef,
    depositInclusion:
      parseSubmitFabricatedDepositInclusion(depositInclusionJson),
    referenceScriptUtxo: config.referenceScriptUtxo,
    awaitConfirmation: config.awaitConfirmation,
  });
};
