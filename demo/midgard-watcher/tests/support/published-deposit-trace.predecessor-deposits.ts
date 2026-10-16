import { depositEventsRetainedBlock } from "@al-ft/midgard-fault-proofs/test-support/transition-trace-retained";
import * as SDK from "@al-ft/midgard-sdk";
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
  deployment: Published;
  lucid: LucidEvolution;
  operatorVkey: string;
  onStage: (name: string) => void;
}) => {
  const { contracts, chain, references } = input.deployment;
  const read = () =>
    Effect.runPromise(
      SDK.fetchDepositUTxOsProgram(
        input.lucid,
        SDK.eventHistoryDeploymentFromContracts(
          SDK.requireEventHistoryContracts(contracts).deposit,
        ),
      ),
    );
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
  predecessor: PublishedDepositTraceCheckpoint["predecessor"],
  priorDeposits: PublishedDepositTraceCheckpoint["priorDeposits"],
): Promise<readonly SDK.DaPayloadEntry[]> => {
  if (priorDeposits === undefined || priorDeposits.length === 0)
    throw new Error(
      "A nonempty predecessor's authenticated admissions are required",
    );
  const header = predecessor.header;
  const built = await depositEventsRetainedBlock({
    operatorVkey: header.operatorVkey,
    startTime: header.startTime,
    endTime: header.endTime,
    blockSlot: header.blockSlot,
    prevHeaderHash: header.prevHeaderHash,
    prevUtxosRoot: header.prevUtxosRoot,
    priorLedger: [],
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
  predecessor: PublishedDepositTraceCheckpoint["predecessor"];
  priorDeposits: PublishedDepositTraceCheckpoint["priorDeposits"];
  event: SDK.DepositUTxO;
  operatorVkey: string;
  endTime: bigint;
  blockSlot: bigint;
  honest: boolean;
}) =>
  depositEventsRetainedBlock({
    operatorVkey: input.operatorVkey,
    prevHeaderHash: input.predecessor.headerHash,
    prevUtxosRoot: input.predecessor.header.utxosRoot,
    priorLedger: await publishedPredecessorLedger(
      input.predecessor,
      input.priorDeposits,
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
