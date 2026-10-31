import * as SDK from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type Emulator,
  type LucidEvolution,
  toUnit,
} from "@lucid-evolution/lucid";
import { Effect, Either } from "effect";

import type { ReferenceScriptPublishingContracts } from "./emulator/reference-script-publisher.js";
import {
  publishPlainReferenceScriptUtxo,
  publishStateQueueYieldReferenceScript,
} from "./emulator/reference-scripts.js";

/** The deployment pieces settlement reads: real state-queue, merge yield and
 * settlement scripts over the scaffold DA policy. */
export type QueueSettlementSubject = {
  readonly contracts: ReferenceScriptPublishingContracts;
  readonly funderLucid: LucidEvolution;
  readonly emulator: Emulator;
};

const queueFetchConfig = (h: QueueSettlementSubject) => ({
  stateQueueAddress: h.contracts.stateQueue.spendingScriptAddress,
  stateQueuePolicyId: h.contracts.stateQueue.policyId,
});

/** Attach a published DA marker to the oldest queued header, keeping every
 * other node field (including any completed-fraud marker). */
export const publishOldestQueueHeaderDa = async (h: QueueSettlementSubject) => {
  const pair = await Effect.runPromise(
    SDK.fetchConfirmedStateAndItsLinkProgram(
      h.funderLucid,
      queueFetchConfig(h),
    ),
  );
  const node = Data.from(Data.to(pair.link.datum.data), SDK.StateQueueNode);
  const nextDatum = SDK.encodeLinkedListNodeView({
    ...pair.link.datum,
    data: Data.castTo(
      {
        ...node,
        da_attestation: { Published: { terminal_commitment: "00".repeat(32) } },
      },
      SDK.StateQueueNode,
    ),
  });
  const daRedeemer = ((ctx) =>
    Data.to(
      {
        ApplyToStateQueue: {
          da_attestation_input_index: 0n,
          da_params_ref_input_index: 0n,
          state_queue_input_index: SDK.requireInputIndex(
            ctx,
            pair.link.utxo,
            "attachment",
          ),
          state_queue_output_index: 0n,
          state_queue_mint_ref_script_input_index: 0n,
          pool_ref_input_index: 0n,
          refund_output_index: 0n,
        },
      },
      SDK.DaAttestationMintRedeemer,
    )) satisfies BuildTxWithRedeemer;
  // The shared harness's DA policy remains a test transport boundary. The
  // state-queue spend, merge yield and settlement mint below are real scripts.
  const changed = await h.funderLucid
    .newTx()
    .collectFrom([pair.link.utxo], ((ctx) =>
      Data.to(
        {
          AttachDaAttestation: {
            state_queue_input_index: SDK.requireInputIndex(
              ctx,
              pair.link.utxo,
              "attachment",
            ),
            da_attestation_mint_redeemer_index: SDK.requireMintRedeemerIndex(
              ctx,
              h.contracts.daAttestation.policyId,
              "attachment",
            ),
          },
        },
        SDK.StateQueueSpendRedeemer,
      )) satisfies BuildTxWithRedeemer)
    .pay.ToContract(
      pair.link.utxo.address,
      { kind: "inline", value: nextDatum },
      pair.link.utxo.assets,
    )
    .mintAssets(
      { [toUnit(h.contracts.daAttestation.policyId, "00")]: 1n },
      daRedeemer,
    )
    .attach.MintingPolicy(h.contracts.daAttestation.mintingScript)
    .attach.SpendingValidator(h.contracts.stateQueue.spendingScript)
    .complete({ localUPLCEval: true });
  await h.funderLucid.awaitTx(
    await (await changed.sign.withWallet().complete()).submit(),
  );
};

/** Build (not submit) the merge of the oldest queued header once it is
 * mature. `presentedNode` replaces only the off-chain view the builder
 * inspects; the spent UTxO, and so what the validators see, stays the real
 * one. */
export const buildOldestQueueHeaderMerge = async (
  h: QueueSettlementSubject,
  {
    presentedNode,
  }: {
    readonly presentedNode?: (node: SDK.StateQueueNode) => SDK.StateQueueNode;
  } = {},
) => {
  const config = queueFetchConfig(h);
  const mergeRef = (
    await publishStateQueueYieldReferenceScript({
      lucid: h.funderLucid,
      contracts: h.contracts,
      arm: "merge",
    })
  ).utxo;
  const mintRef = (
    await publishPlainReferenceScriptUtxo({
      lucid: h.funderLucid,
      script: h.contracts.settlement.mintingScript,
      label: "real settlement mint",
    })
  ).utxo;
  const stateMintRef = (
    await publishPlainReferenceScriptUtxo({
      lucid: h.funderLucid,
      script: h.contracts.stateQueue.mintingScript,
      label: "real state mint",
    })
  ).utxo;
  const stateSpendRef = (
    await publishPlainReferenceScriptUtxo({
      lucid: h.funderLucid,
      script: h.contracts.stateQueue.spendingScript,
      label: "real state spend",
    })
  ).utxo;
  const hub = (
    await h.funderLucid.utxosAtWithUnit(
      h.contracts.hubOracle.spendingScriptAddress,
      toUnit(h.contracts.hubOracle.policyId, SDK.HUB_ORACLE_ASSET_NAME),
    )
  )[0]!;
  const correction = await Effect.runPromise(
    SDK.fetchCorrectionLockUTxOProgram(h.funderLucid, {
      correctionLockAddress: h.contracts.correctionLock.spendingScriptAddress,
      hubOraclePolicyId: h.contracts.hubOracle.policyId,
    }),
  );
  h.emulator.awaitSlot(604_900);
  const pair = await Effect.runPromise(
    SDK.fetchConfirmedStateAndItsLinkProgram(h.funderLucid, config),
  );
  const firstBlockUTxO =
    presentedNode === undefined
      ? pair.link
      : {
          ...pair.link,
          datum: {
            ...pair.link.datum,
            data: SDK.castStateQueueNodeToData(
              presentedNode(
                Effect.runSync(
                  SDK.getStateQueueNodeFromStateQueueDatum(pair.link.datum),
                ),
              ),
            ) as SDK.LinkedListNodeView["data"],
          },
        };
  const merged = await Effect.runPromise(
    Effect.either(
      SDK.buildMergeToConfirmedStateTxProgram({
        lucid: h.funderLucid,
        fetchConfig: config,
        contracts: h.contracts,
        confirmedUTxO: pair.confirmed,
        firstBlockUTxO,
        validFrom: h.emulator.now() - 1000,
        hubOracleRefInput: hub,
        correctionLockRefInput: correction,
        stateQueueMergeYieldRefInput: mergeRef,
        referenceScripts: {
          settlementMinting: mintRef,
          stateQueueMinting: stateMintRef,
          stateQueueSpending: stateSpendRef,
        },
      }),
    ),
  );
  // Surface the evaluator's own report, which the builder keeps in `cause`.
  if (Either.isLeft(merged))
    throw new Error(
      `${merged.left.message}: ${JSON.stringify(merged.left.cause)}`,
    );
  return merged.right;
};

export const settleOldestCrossBlockHeader = async (
  h: QueueSettlementSubject,
) => {
  await publishOldestQueueHeaderDa(h);
  const merged = await buildOldestQueueHeaderMerge(h);
  await h.funderLucid.awaitTx(
    await (await merged.tx.sign.withWallet().complete()).submit(),
  );
  return merged;
};
