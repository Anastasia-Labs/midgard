import type { L1SlotConfig } from "@al-ft/midgard-core/l1-slot-config";
import { depositEventsRetainedBlock } from "@al-ft/midgard-fault-proofs/test-support/transition-trace-retained";
import type * as SDKDa from "@al-ft/midgard-sdk/da";
import * as SDK from "@al-ft/midgard-sdk/users";
import {
  credentialToAddress,
  type LucidEvolution,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  buildPastProtectedPredecessor,
  ledgerClock,
} from "./history-predecessor-hold.js";
import {
  capturePublishedDepositHistory,
  readPublishedDepositHistory,
} from "./published-deposit-history.js";
import type {
  Published,
  PublishedDepositTraceCheckpoint,
} from "./published-deposit-trace.published-deposit-trace-checkpoint.js";

/** C3's first predecessor contains real authenticated admission material, including on a fresh deployment. */
export const preparePredecessorDeposits = async (input: {
  deployment: Pick<Published, "contracts" | "chain" | "references">;
  lucid: LucidEvolution;
  operatorVkey: string;
  onStage: (name: string) => void;
  /** Keep only deposits due after this time; earlier ones are absorbed. */
  dueAfter?: bigint;
}) => {
  const { contracts, chain, references } = input.deployment;
  const read = async () => {
    const orders = await Effect.runPromise(
      SDK.fetchDepositUTxOsProgram(
        input.lucid,
        SDK.eventHistoryDeploymentFromContracts(
          SDK.requireEventHistoryContracts(contracts).deposit,
        ),
      ),
    );
    const { dueAfter } = input;
    return dueAfter === undefined
      ? orders
      : orders.filter((order) => order.facts.inclusion_time > dueAfter);
  };
  let priorOrders = await read();
  if (priorOrders.length === 0) {
    const reference = references.get("depositMint");
    if (reference === undefined)
      throw new Error("Missing actual publication depositMint");
    const admitted = await buildPastProtectedPredecessor(
      () =>
        Effect.runPromise(
          SDK.buildUnsignedDepositTxWithMetadataProgram(
            input.lucid,
            contracts,
            {
              l2Address: credentialToAddress("Preprod", {
                type: "Key",
                hash: input.operatorVkey,
              }),
              l2Datum: null,
              lovelace: 2_000_000n,
              additionalAssets: {},
              referenceScripts: { depositMinting: reference },
            },
          ),
        ),
      ledgerClock(chain),
    );
    input.onStage("honest predecessor deposit publication");
    const signed = await admitted.tx.sign.withWallet().complete();
    const txHash = await signed.submit();
    await input.lucid.awaitTx(txHash, 500);
    priorOrders = await read();
    if (
      !priorOrders.some(
        (order) => order.assetName === admitted.metadata.depositAssetName,
      )
    )
      throw new Error(
        "Published predecessor deposit is absent from authenticated history",
      );
  }
  const priorDeposits: NonNullable<
    PublishedDepositTraceCheckpoint["priorDeposits"]
  > = priorOrders.map((order) => ({
    event: order.utxo,
    history: capturePublishedDepositHistory(order, contracts.deposit.policyId),
    metadata: {
      depositAuthUnit: contracts.deposit.policyId + order.assetName,
      depositAssetName: order.assetName,
      inclusionTime: Number(order.facts.inclusion_time),
    },
  }));
  return { priorOrders, priorDeposits };
};

/** Reconstruct fixture state from captured authenticated admissions; neither DA nor a persisted ledger is its authority. */
export const publishedPredecessorLedger = async (
  /** The slot mapping of the chain the predecessor committed on. */
  l1SlotConfig: L1SlotConfig,
  predecessor: PublishedDepositTraceCheckpoint["predecessor"],
  priorDeposits: PublishedDepositTraceCheckpoint["priorDeposits"],
  /** The ledger the predecessor extends; its own header root checks it. */
  priorLedger: readonly SDKDa.DaPayloadEntry[] = [],
): Promise<readonly SDKDa.DaPayloadEntry[]> => {
  if (priorDeposits === undefined || priorDeposits.length === 0)
    throw new Error(
      "A nonempty predecessor's authenticated admissions are required",
    );
  const header = predecessor.header;
  const built = await depositEventsRetainedBlock({
    l1SlotConfig,
    operatorVkey: header.operatorVkey,
    startTime: header.startTime,
    endTime: header.endTime,
    blockSlot: header.blockSlot,
    prevHeaderHash: header.prevHeaderHash,
    prevUtxosRoot: header.prevUtxosRoot,
    priorLedger,
    events: priorDeposits.map((prior) => ({
      ...readPublishedDepositHistory(prior.history, prior.metadata),
      honest: true,
    })),
  });
  if (built.headerHash !== predecessor.headerHash)
    throw new Error(
      "Predecessor reconstruction differs from its committed header",
    );
  return built.postLedgerEntries.map(({ outRef, output }) => [
    outRef.toString("hex"),
    output.toString("hex"),
  ]);
};

/** Build a later deposit block from that exact reconstructed predecessor. */
export const buildPublishedDepositBlock = async (input: {
  /** The slot mapping of the chain the block commits on. */
  l1SlotConfig: L1SlotConfig;
  predecessor: PublishedDepositTraceCheckpoint["predecessor"];
  /** The ledger the predecessor extends; empty for the confirmed genesis. */
  predecessorPriorLedger?: readonly SDKDa.DaPayloadEntry[];
  priorDeposits: PublishedDepositTraceCheckpoint["priorDeposits"];
  event: SDK.DepositUTxO;
  operatorVkey: string;
  endTime: bigint;
  blockSlot: bigint;
  honest: boolean;
}) =>
  depositEventsRetainedBlock({
    l1SlotConfig: input.l1SlotConfig,
    operatorVkey: input.operatorVkey,
    prevHeaderHash: input.predecessor.headerHash,
    prevUtxosRoot: input.predecessor.header.utxosRoot,
    priorLedger: await publishedPredecessorLedger(
      input.l1SlotConfig,
      input.predecessor,
      input.priorDeposits,
      input.predecessorPriorLedger,
    ),
    startTime: input.predecessor.header.endTime,
    endTime: input.endTime,
    blockSlot: input.blockSlot,
    events: [
      {
        event: input.event.event,
        originalAssets: input.event.originalAssets,
        honest: input.honest,
      },
    ],
  });
