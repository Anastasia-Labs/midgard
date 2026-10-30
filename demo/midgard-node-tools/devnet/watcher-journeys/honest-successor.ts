import { existsSync } from "node:fs";
import { appendFile, readFile } from "node:fs/promises";
import { join } from "node:path";
import { setTimeout as pause } from "node:timers/promises";

import {
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFullFromCanonicalCbor,
} from "@al-ft/midgard-core/codec";
import { unwrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { DA_TRANSPORT_LIMITS } from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, type Network, walletFromSeed } from "@lucid-evolution/lucid";
import { buildTransferTxWithMinFee } from "midgard-node/commands/submit-l2-transfer";
import type { ResolvedTxStatus } from "midgard-node/commands/tx-status";

import { type JourneyLedger } from "../../src/full-stack/journey.js";
import { readJourneyArtifact, writeJourneyArtifact } from "./artifacts.js";
import type {
  JourneyBlock,
  JourneySuccessor,
  JourneySuccessorCheckpoint,
  StagedJourney,
} from "./fixture.js";
import {
  assertNoHonestHeaderOnFault,
  classifyNodeFaultHold,
  LANDED_BLOCK_INVALID,
  type NodeFaultHold,
  readIndexedStateQueueHeader,
} from "./honest-successor.observables.js";

/**
 * How a family's honest successor comes about once the fault is on L1.
 * - `afterFaultLanded`: the fault is the queue's tail; give our node work.
 * - `commit`: the watcher removed the fault; return the successor block.
 * - `close`: stop anything `afterFaultLanded` left running.
 */
export type JourneySuccessorPort = Readonly<{
  afterFaultLanded(): Promise<void>;
  commit(): Promise<JourneySuccessor>;
  close(): Promise<void>;
}>;

const SUCCESSOR = "successor.json";

/**
 * The devnet successor: the journey commits it itself as the honest
 * operator, through the fixture's own builder, after the correction.
 */
export const devnetSuccessorPort = (
  input: Readonly<{
    directory: string;
    staged: Pick<StagedJourney, "commitHonestSuccessor">;
    /** Retains the successor's payload before its commit is signed. */
    retainPayload(block: JourneyBlock): Promise<void>;
    /** Retains the committed successor in the journey's retained DA. */
    retain(block: JourneyBlock, commitTxHash: string): Promise<void>;
    stage<T>(name: string, action: () => Promise<T>): Promise<T>;
  }>,
): JourneySuccessorPort => ({
  afterFaultLanded: async () => {},
  commit: async () => {
    const successorPath = join(input.directory, SUCCESSOR);
    const progressPath = join(input.directory, "successor-progress.json");
    const successor = existsSync(successorPath)
      ? await readJourneyArtifact<JourneySuccessor>(successorPath)
      : await input.stage("honest successor commitment", async () =>
          input.staged.commitHonestSuccessor({
            beforeCommit: input.retainPayload,
            resume: existsSync(progressPath)
              ? await readJourneyArtifact<JourneySuccessorCheckpoint>(
                  progressPath,
                )
              : undefined,
            onCheckpoint: (checkpoint) =>
              writeJourneyArtifact(progressPath, checkpoint),
          }),
        );
    await writeJourneyArtifact(successorPath, successor);
    await input.retain(successor, successor.commitTxHash);
    return successor;
  },
  close: async () => {},
});

type TransferIntent = Readonly<{
  txId: string;
  signedTxCbor: string;
  fee: string;
}>;

/** One `/readyz` hold observation, as `node-hold-observations.ndjson` keeps it. */
type HoldObservation = Readonly<{
  observedAt: string;
  hold?: NodeFaultHold;
  unreachable?: string;
}>;

const HOLD_OBSERVATIONS = "node-hold-observations.ndjson";

/**
 * The stack successor: our own node builds it. After the fault lands the
 * journey submits one L2 transfer to our node, which gives it work while the
 * fault is the tail, and watches our node's `/readyz` for its hold on the
 * fault. After the watcher's removal it proves the three refusal
 * observables:
 * 1. our node held `landed_block_invalid` on the fault while it was the tail;
 * 2. no header of our operator on L1 names the fault as its parent;
 * 3. the hold cleared, and our node's next header links to the fault's
 *    predecessor and carries the transfer.
 */
export const stackSuccessorPort = (
  input: Readonly<{
    directory: string;
    /** Our node's HTTP endpoint. */
    nodeEndpoint: string;
    ledger: JourneyLedger;
    kupoUrl: string;
    stateQueuePolicyId: string;
    honestOperatorKeyHash: string;
    /** The drill's L2 owner, which sends the transfer to itself. */
    ledgerOwner: Readonly<{ seedPhrase: string; address: string }>;
    network: Network;
    transferLovelace: bigint;
    /** The queue's tail once the fault is removed. */
    predecessor: JourneyBlock;
    fault: JourneyBlock;
    /** The public DA copy of a header's payload envelope, verified against it. */
    fetchPayloadEnvelope(headerHash: string): Promise<Uint8Array>;
    poll<T>(
      name: string,
      action: () => Promise<T | undefined>,
      timeoutMs?: number,
    ): Promise<T>;
    /** How long our node may take to commit the transfer after the removal. */
    commitTimeoutMs: number;
    holdPollIntervalMs?: number;
    fetchImpl?: typeof fetch;
  }>,
): JourneySuccessorPort => {
  const fetchImpl = input.fetchImpl ?? fetch;
  const intentPath = join(input.directory, "successor-transfer.json");
  const observationsPath = join(input.directory, HOLD_OBSERVATIONS);
  const readyz = async (): Promise<HoldObservation> => {
    const observedAt = new Date().toISOString();
    let body: unknown;
    try {
      const response = await fetchImpl(`${input.nodeEndpoint}/readyz`, {
        signal: AbortSignal.timeout(10_000),
      });
      body = await response.json();
    } catch (cause) {
      return { observedAt, unreachable: String(cause) };
    }
    return {
      observedAt,
      hold: classifyNodeFaultHold(body, input.fault.headerHash),
    };
  };
  const txStatus = async (txId: string) => {
    const response = await fetchImpl(
      `${input.nodeEndpoint}/tx-status?tx_hash=${txId}`,
      { signal: AbortSignal.timeout(10_000) },
    );
    if (!response.ok)
      throw new Error(
        `Our node's /tx-status answered ${response.status.toString()}`,
      );
    const status = (await response.json()) as ResolvedTxStatus;
    if (status.status === "rejected")
      throw new Error(
        `Our node rejected the drill's transfer ${txId}: ${status.reasonCode}`,
      );
    return status;
  };
  const savedIntent = async (): Promise<TransferIntent | undefined> => {
    if (!existsSync(intentPath)) return undefined;
    const intent = await readJourneyArtifact<TransferIntent>(intentPath);
    const decoded = decodeMidgardNativeTxFullFromCanonicalCbor(
      Buffer.from(intent.signedTxCbor, "hex"),
    );
    if (
      computeMidgardNativeTxId(decoded).toString("hex") !== intent.txId ||
      decoded.body.fee !== BigInt(intent.fee)
    )
      throw new Error(
        "Saved successor transfer differs from its exact transaction identity or fee",
      );
    return intent;
  };
  const monitor = new AbortController();
  let monitoring: Promise<void> | undefined;
  let monitorFailure: Error | undefined;
  // Appends each change of our node's hold; a hold on another block, or a
  // failure to record, stops the watch and fails the successor step.
  const watchHold = async () => {
    let last: string | undefined;
    try {
      while (!monitor.signal.aborted) {
        const observation = await readyz();
        const summary = JSON.stringify({
          state: observation.hold?.state,
          binding: observation.hold?.binding,
          unreachable: observation.unreachable !== undefined,
        });
        if (summary !== last) {
          last = summary;
          await appendFile(
            observationsPath,
            `${JSON.stringify(observation)}\n`,
          );
        }
        await pause(input.holdPollIntervalMs ?? 5000, undefined, {
          signal: monitor.signal,
        }).catch(() => undefined);
      }
    } catch (cause) {
      monitorFailure =
        cause instanceof Error
          ? cause
          : new Error(`Our node's hold watch failed: ${JSON.stringify(cause)}`);
    }
  };
  const stopMonitor = async () => {
    monitor.abort();
    await monitoring;
  };
  return {
    afterFaultLanded: async () => {
      if (existsSync(join(input.directory, SUCCESSOR))) return;
      let intent = await savedIntent();
      if (intent === undefined) {
        const { header } = input.predecessor;
        const built = await buildTransferTxWithMinFee({
          senderAddress: input.ledgerOwner.address,
          destinationAddress: input.ledgerOwner.address,
          signer: walletFromSeed(input.ledgerOwner.seedPhrase, {
            network: input.network,
          }).paymentKey,
          availableUtxos: await input.ledger.utxos(input.ledgerOwner.address),
          requestedAssets: { lovelace: input.transferLovelace },
          network: input.network,
          networkId: header.expectedNetworkId,
          minFeeA: header.minFeeA,
          minFeeB: header.minFeeB,
        });
        intent = {
          txId: built.txIdHex,
          signedTxCbor: built.txHex,
          fee: built.fee.toString(),
        };
        // This checkpoint precedes the first send, including a lost response.
        await writeJourneyArtifact(intentPath, intent);
      }
      if ((await txStatus(intent.txId)).status === "not_found")
        await input.ledger.submitTransfer(intent.signedTxCbor, intent.txId);
      monitoring ??= watchHold();
    },
    commit: async () => {
      await stopMonitor();
      if (monitorFailure !== undefined) throw monitorFailure;
      const successorPath = join(input.directory, SUCCESSOR);
      if (existsSync(successorPath))
        return await readJourneyArtifact<JourneySuccessor>(successorPath);
      const intent = await savedIntent();
      if (intent === undefined)
        throw new Error("The drill's successor transfer was never submitted");
      // Observable 1: the hold was on the fault while it was the tail.
      const observations = existsSync(observationsPath)
        ? (await readFile(observationsPath, "utf8"))
            .split("\n")
            .filter((line) => line !== "")
            .map((line) => JSON.parse(line) as HoldObservation)
        : [];
      if (!observations.some(({ hold }) => hold?.state === "held"))
        throw new Error(
          `Our node never reported ${LANDED_BLOCK_INVALID} for the fault ${input.fault.headerHash} while it was the tail; the refusal was not observed`,
        );
      const committed = await input.poll(
        "our node's successor commitment",
        async () => {
          const status = await txStatus(intent.txId);
          return status.status === "committed" &&
            status.headerHash !== undefined
            ? status.headerHash
            : undefined;
        },
        input.commitTimeoutMs,
      );
      // Observable 3: our node's block links to the predecessor and carries
      // the transfer, read from the payload the committee published.
      const payloadEnvelopeCbor = Buffer.from(
        await input.fetchPayloadEnvelope(committed),
      );
      const payload = SDK.decodeDaPayload(
        Buffer.from(
          (
            await unwrapDaPayload(payloadEnvelopeCbor, {
              maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
            })
          ).innerBytes,
        ),
      );
      const { header, header_hash: headerHash } = payload.block_body;
      if (headerHash !== committed)
        throw new Error(
          `The published payload for ${committed} names header ${headerHash}`,
        );
      if (header.prevHeaderHash !== input.predecessor.headerHash)
        throw new Error(
          `Our node's header ${committed} links to ${header.prevHeaderHash}, not to the fault's predecessor ${input.predecessor.headerHash}`,
        );
      if (header.operatorVkey !== input.honestOperatorKeyHash)
        throw new Error(
          `Header ${committed} carrying the drill's transfer is not our operator's`,
        );
      if (!payload.block_body.transactions.some(([id]) => id === intent.txId))
        throw new Error(
          `Our node's header ${committed} does not carry the drill's transfer ${intent.txId}`,
        );
      const [onL1] = await input.poll(
        "our node's successor on L1",
        async () => {
          const indexed = await readIndexedStateQueueHeader(
            {
              kupoUrl: input.kupoUrl,
              policyId: input.stateQueuePolicyId,
              headerHash: committed,
            },
            fetchImpl,
          );
          return indexed.length > 0 ? indexed : undefined;
        },
        input.commitTimeoutMs,
      );
      if (Data.to(onL1!.header, SDK.Header) !== Data.to(header, SDK.Header))
        throw new Error(
          `Header ${committed} on L1 differs from its published payload's`,
        );
      // Observable 2, over the whole window from the fault's commit.
      const linkage = await assertNoHonestHeaderOnFault(
        {
          kupoUrl: input.kupoUrl,
          policyId: input.stateQueuePolicyId,
          faultHeaderHash: input.fault.headerHash,
          honestOperatorKeyHash: input.honestOperatorKeyHash,
        },
        fetchImpl,
      );
      // Observable 3: the hold cleared once our node built past the fault.
      const after = await readyz();
      if (after.hold === undefined || after.hold.state !== "not_held")
        throw new Error(
          `Our node still holds the fault after its successor: ${JSON.stringify(after)}`,
        );
      const successor: JourneySuccessor = {
        header,
        headerHash: committed,
        payloadEnvelopeCbor,
        commitTxHash: onL1!.txHash,
      };
      await writeJourneyArtifact(join(input.directory, "refusal.json"), {
        fault: input.fault.headerHash,
        holdObservations: observations.length,
        holdBinding: observations.find(({ hold }) => hold?.state === "held")
          ?.hold?.binding,
        linkage,
        transfer: intent.txId,
        successor: committed,
        commitTxHash: onL1!.txHash,
      });
      await writeJourneyArtifact(successorPath, successor);
      return successor;
    },
    close: stopMonitor,
  };
};
