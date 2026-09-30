import {
  plutusConstrFieldCbor,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import { EventHistoryNode, EventHistoryObserve } from "@al-ft/midgard-sdk";
import { makeNativeTx } from "@al-ft/midgard-validation/tests/validation-fixtures";
import { CML, Data } from "@lucid-evolution/lucid";

import { createWatcherLocalUserEventPublisher } from "../../src/indexers/user-event-history.js";
import type { WatcherLocalUserEventAuthority } from "../../src/indexers/user-event-indexer.js";
import { type WatcherUserEventOriginFacts } from "../../src/indexers/user-event-origin.js";
import { readWatcherLocalBackfillFinality } from "../../src/l1/finality-engine.js";
import {
  computeWatcherRuleBundleCommitment,
  makeWatcherCanonicalRuleBundle,
} from "../../src/verification/rule-bundle.js";
import {
  durableFixture,
  openOrigin,
} from "./local-user-event-authority-fixture.durable-fixture.js";
import { historyLifecycle } from "./local-user-event-authority-fixture.history-lifecycle.js";
import {
  type LocalReplayUserEventAuthorities,
  type LocalReplayUserEventRequest,
  ordinaryLocalOrderCreation,
} from "./local-user-event-authority-fixture.ordinary-local-order-creation.js";
import {
  syntheticUserEventTransaction,
  transactionInput,
} from "./local-user-event-authority-fixture.synthetic-user-event-transaction.js";
import { createSyntheticUserEventOriginFixture } from "./user-event-origin-fixture.js";

/**
 * Publishes the requested ordinary orders in one synthetic finalized block and
 * returns private local replay capabilities for each. The publisher stays open
 * until `close`, so the capabilities remain current for replay.
 */
export const createLocalReplayUserEventAuthorities = async (
  request: LocalReplayUserEventRequest,
): Promise<LocalReplayUserEventAuthorities> => {
  const fixture = await createSyntheticUserEventOriginFixture();
  let publisher:
    | Awaited<ReturnType<typeof createWatcherLocalUserEventPublisher>>
    | undefined;
  try {
    const { pair, input, origin, facts } = await openOrigin(fixture);
    const durable = await durableFixture(
      readWatcherLocalBackfillFinality(pair.finality).policy,
    );
    publisher = await createWatcherLocalUserEventPublisher({
      ...input,
      origin,
      runtime: durable.runtime,
      archive: durable.archive,
    });
    await publisher.publish(pair);
    await publisher.publish(
      await fixture.openFinalizedBlock(fixture.emptySuccessorBlock),
    );
    const deposit =
      request.deposit === undefined
        ? null
        : historyLifecycle(facts, false, {
            ...request.deposit,
            structuralLovelace: 1_000_000n,
          });
    const placeholderNative = makeNativeTx().txCbor;
    const withdrawals = (request.withdrawals ?? []).map((withdrawal) => ({
      key: withdrawal.key,
      order: ordinaryLocalOrderCreation(
        facts,
        "withdrawal",
        placeholderNative,
        {
          nonceByte: withdrawal.nonceByte,
          withdrawalL2OutRef: withdrawal.l2OutRef,
          withdrawalInfo: withdrawal.info,
          structuralLovelace: 1_000_000n,
        },
      ),
    }));
    const forcedOrders = (request.forcedOrders ?? []).map((forced) => ({
      key: forced.key,
      order: ordinaryLocalOrderCreation(
        facts,
        "forced_order",
        placeholderNative,
        { nonceByte: forced.nonceByte, forcedPayload: forced.payload },
      ),
    }));
    const block = await fixture.makeBlock({
      parent: fixture.emptySuccessorBlock,
      transactions: [
        ...(deposit === null ? [] : [deposit.create]),
        ...withdrawals.map(({ order }) => order.cbor),
        ...forcedOrders.map(({ order }) => order.cbor),
      ],
      creatingBodies: [fixture.initializationBodyCbor],
    });
    await publisher.publish(await fixture.openFinalizedBlock(block));
    const fresh = await fixture.openFinalizedBlock(block);
    const active = publisher;
    const depositAuthority =
      deposit === null
        ? null
        : await active.eventAuthority({
            ...fresh,
            kind: "deposit",
            eventId: deposit.expectedEventId,
          });
    const withdrawalAuthorities: Record<
      string,
      WatcherLocalUserEventAuthority
    > = {};
    for (const { key, order } of withdrawals)
      withdrawalAuthorities[key] = await active.eventAuthority({
        ...fresh,
        kind: "withdrawal",
        eventId: order.eventIdCborHex,
      });
    const forcedAuthorities: Record<string, WatcherLocalUserEventAuthority> =
      {};
    for (const { key, order } of forcedOrders)
      forcedAuthorities[key] = await active.eventAuthority({
        ...fresh,
        kind: "forced_order",
        eventId: order.eventIdCborHex,
      });
    const ruleBundle = makeWatcherCanonicalRuleBundle({
      constructionIdentity: {
        manifestId: fixture.deploymentIdentity.manifestId,
        blueprintHash: fixture.deploymentIdentity.blueprintHash,
        network: fixture.deploymentIdentity.network,
        programCommitments: fixture.deploymentIdentity.programCommitments,
      },
      targetParameterSnapshot: { finalityDepth: 12 },
    });
    return Object.freeze({
      deposit: depositAuthority,
      withdrawals: Object.freeze(withdrawalAuthorities),
      forcedOrders: Object.freeze(forcedAuthorities),
      ruleBundle,
      ruleBundleCommitment: computeWatcherRuleBundleCommitment(ruleBundle),
      close: async () => {
        active.close();
        await fixture.close();
      },
    });
  } catch (error) {
    publisher?.close();
    await fixture.close();
    throw error;
  }
};

export const historyPointerContinuation = (
  facts: WatcherUserEventOriginFacts,
  lifecycle: ReturnType<typeof historyLifecycle>,
  kind: "deposit" | "withdrawal",
  current?: Readonly<{ outRef: string; outputCborHex: string }>,
): string => {
  const original =
    current === undefined
      ? CML.Transaction.from_cbor_hex(lifecycle.create).body().outputs().get(0)
      : CML.TransactionOutput.from_cbor_hex(current.outputCborHex);
  const node = Data.from(
    original.datum()!.as_datum()!.to_cbor_hex(),
    EventHistoryNode,
  );
  const fillerKey =
    current === undefined
      ? "ff".repeat(32)
      : (
          (BigInt(
            "0x" + (node.position === "Root" ? "0" : node.position.Key[0]),
          ) +
            BigInt("0x" + (node.next ?? "ff".repeat(32)))) /
          2n
        )
          .toString(16)
          .padStart(64, "0");
  const outputs = CML.TransactionOutputList.new();
  outputs.add(
    CML.TransactionOutput.new(
      original.address(),
      original.amount(),
      CML.DatumOption.new_datum(
        CML.PlutusData.from_cbor_hex(
          replacePlutusConstrFieldCbor(
            Data.to(
              {
                ...node,
                next: fillerKey,
                protected_until: node.protected_until + 1n,
              },
              EventHistoryNode,
            ),
            [3],
            plutusConstrFieldCbor(original.datum()!.as_datum()!.to_cbor_hex(), [
              3,
            ]),
          ),
        ),
      ),
    ),
  );
  const fillerAssets = CML.MultiAsset.new();
  fillerAssets.set(
    CML.ScriptHash.from_hex(facts.scripts[kind].policyId),
    CML.AssetName.from_hex(fillerKey),
    1n,
  );
  outputs.add(
    CML.TransactionOutput.new(
      original.address(),
      CML.Value.new(2_000_000n, fillerAssets),
      CML.DatumOption.new_datum(
        CML.PlutusData.from_cbor_hex(
          Data.to(
            {
              position: { Key: [fillerKey] },
              next: node.next,
              protected_until: node.protected_until + 1n,
              payload: { Filler: { refund_key: "88".repeat(28) } },
            },
            EventHistoryNode,
          ),
        ),
      ),
    ),
  );
  const inputs = CML.TransactionInputList.new();
  inputs.add(transactionInput(current?.outRef ?? `${lifecycle.createId}#0`));
  const body = CML.TransactionBody.new(inputs, outputs, 200_000n);
  const mint = CML.Mint.new();
  mint.set(
    CML.ScriptHash.from_hex(facts.scripts[kind].policyId),
    CML.AssetName.from_hex(fillerKey),
    1n,
  );
  body.set_mint(mint);
  const refs = CML.TransactionInputList.new();
  refs.add(transactionInput(facts.activation.hubOutRef));
  body.set_reference_inputs(refs);
  const withdrawals = CML.MapRewardAccountToCoin.new();
  withdrawals.insert(
    CML.RewardAddress.new(
      0,
      CML.Credential.new_script(
        CML.ScriptHash.from_hex(facts.scripts[kind].policyId),
      ),
    ),
    0n,
  );
  body.set_withdrawals(withdrawals);
  return syntheticUserEventTransaction(
    body,
    [
      { tag: CML.RedeemerTag.Mint, index: 0n, cbor: Data.to(0n) },
      { tag: CML.RedeemerTag.Spend, index: 0n, cbor: Data.to(0n) },
      {
        tag: CML.RedeemerTag.Reward,
        index: 0n,
        cbor: Data.to(
          {
            Apply: {
              hub_reference_index: 0n,
              operation: {
                InsertFiller: {
                  predecessor_input_index: 0n,
                  predecessor_output_index: 0n,
                  filler_output_index: 1n,
                },
              },
            },
          },
          EventHistoryObserve,
        ),
      },
    ],
    true,
  );
};
