import { existsSync } from "node:fs";
import { join } from "node:path";
import { setTimeout as pause } from "node:timers/promises";

import {
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardTxOutput,
  encodeMidgardAddressText,
} from "@al-ft/midgard-core/codec";
import { type Network, walletFromSeed } from "@lucid-evolution/lucid";
import { buildTransferTxWithMinFee } from "midgard-node/commands/submit-l2-transfer";
import type { ResolvedTxStatus } from "midgard-node/commands/tx-status";

import { type JourneyLedger } from "../../src/full-stack/journey.js";
import { readJourneyArtifact, writeJourneyArtifact } from "./artifacts.js";
import type { JourneyBlock } from "./fixture.js";

/**
 * The drill's transfer to our node, saved for the predecessor it builds on;
 * one saved before the transfer was handed over ahead of the fault names the
 * fault instead.
 */
type TransferIntent = Readonly<{
  predecessorHeaderHash?: string;
  faultHeaderHash?: string;
  txId: string;
  signedTxCbor: string;
  fee: string;
}>;

/** What the drill's transfer to our node is built and sent with. */
export type SuccessorTransferInput = Readonly<{
  directory: string;
  /** Our node's HTTP endpoint. */
  nodeEndpoint: string;
  ledger: JourneyLedger;
  /** The drill's L2 owner, which sends the transfer to itself. */
  ledgerOwner: Readonly<{ seedPhrase: string; address: string }>;
  network: Network;
  transferLovelace: bigint;
  fetchImpl?: typeof fetch;
}>;

/**
 * The drill's one transfer to our node: saved before its first send,
 * including a lost response, and sent again only while our node does not
 * know it.
 */
export const successorTransfer = (input: SuccessorTransferInput) => {
  const fetchImpl = input.fetchImpl ?? fetch;
  const intentPath = join(input.directory, "successor-transfer.json");
  const txStatus = async (txId: string) => {
    const response = await fetchImpl(
      `${input.nodeEndpoint}/tx-status?tx_hash=${txId}`,
      { signal: AbortSignal.timeout(10_000) },
    );
    const text = await response.text();
    let status: ResolvedTxStatus | undefined;
    try {
      status = JSON.parse(text) as ResolvedTxStatus;
    } catch {
      status = undefined;
    }
    // Our node answers a transaction it does not know with 404 and the
    // `not_found` status; any other answer must name this transaction.
    const known =
      status?.txId === txId &&
      (response.ok
        ? status.status !== "not_found"
        : response.status === 404 && status.status === "not_found");
    if (!known || status === undefined)
      throw new Error(
        `Our node's /tx-status answered ${response.status.toString()} for ${txId}: ${text}`,
      );
    if (status.status === "rejected")
      throw new Error(
        `Our node rejected the drill's transfer ${txId}: ${status.reasonCode}`,
      );
    return status;
  };
  return {
    txStatus,
    /**
     * The transfer saved for `predecessor`, or for `faultHeaderHash` by a
     * run that handed it over after the fault landed. One saved for another
     * predecessor, staged by an earlier attempt, is not this fault's work.
     */
    saved: async (
      predecessor: JourneyBlock,
      faultHeaderHash?: string,
    ): Promise<TransferIntent | undefined> => {
      if (!existsSync(intentPath)) return undefined;
      const intent = await readJourneyArtifact<TransferIntent>(intentPath);
      if (
        intent.predecessorHeaderHash === undefined
          ? faultHeaderHash === undefined ||
            intent.faultHeaderHash !== faultHeaderHash
          : intent.predecessorHeaderHash !== predecessor.headerHash
      )
        return undefined;
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
    },
    /** Builds the transfer on our node's ledger and saves it. */
    build: async (predecessor: JourneyBlock): Promise<TransferIntent> => {
      const { header } = predecessor;
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
      const intent = {
        predecessorHeaderHash: predecessor.headerHash,
        txId: built.txIdHex,
        signedTxCbor: built.txHex,
        fee: built.fee.toString(),
      };
      // This checkpoint precedes the first send, including a lost response.
      await writeJourneyArtifact(intentPath, intent);
      return intent;
    },
    send: async (intent: TransferIntent) => {
      if ((await txStatus(intent.txId)).status === "not_found")
        await input.ledger.submitTransfer(intent.signedTxCbor, intent.txId);
    },
  };
};

/** The fault's predecessor with its replayed ledger, as staging holds it. */
export type ReplayedPredecessor = JourneyBlock &
  Readonly<{
    ledger: Readonly<{
      entries: readonly Readonly<{ outRef: Uint8Array; output: Uint8Array }>[];
    }>;
  }>;

/**
 * Our node's honest transfer, handed over before the fault's commit on an
 * attached stack, so our node holds work from its first shift after the
 * fault. Readied once, before the late-shift wait: built only when our
 * node's ledger shows the drill's L2 owner holding exactly its outputs in
 * the predecessor's replayed ledger, the state the transfer builds on. The
 * returned handover sends it, at most once, on each commit attempt. The
 * successor port then finds it saved for the same predecessor. Readiness
 * waits no later than `readyByMs`, when the fault's late-shift window opens,
 * so it never delays the commit: a ledger still behind then leaves nothing
 * to hand over, recorded in `successor-transfer-deferred.json`, and the
 * successor port builds and sends the transfer once the fault lands.
 */
export const stackTransferBeforeFault =
  (
    input: SuccessorTransferInput &
      Readonly<{
        /** How long our node's ledger may take to reach the predecessor. */
        timeoutMs: number;
        pollIntervalMs?: number;
        /** Ledger time in milliseconds. */
        now(): number;
        /** The ledger time the fault's late-shift window opens at. */
        readyByMs(): Promise<number>;
        onStage(name: string): void;
      }>,
  ) =>
  async (predecessor: ReplayedPredecessor): Promise<() => Promise<void>> => {
    const transfer = successorTransfer(input);
    let intent = await transfer.saved(predecessor);
    if (intent === undefined) {
      const owned = (outRefs: readonly Uint8Array[]) =>
        outRefs
          .map((outRef) => Buffer.from(outRef).toString("hex"))
          .sort()
          .join(",");
      const expected = owned(
        predecessor.ledger.entries
          .filter(
            ({ output }) =>
              encodeMidgardAddressText(
                decodeMidgardTxOutput(output).address,
              ) === input.ledgerOwner.address,
          )
          .map(({ outRef }) => outRef),
      );
      const deadline = performance.now() + input.timeoutMs;
      const readyByMs = await input.readyByMs();
      for (;;) {
        const ours = owned(
          (await input.ledger.utxos(input.ledgerOwner.address)).map(
            ({ outrefCbor }) => outrefCbor,
          ),
        );
        if (ours === expected) break;
        if (input.now() >= readyByMs) {
          input.onStage(
            `Our node's ledger did not show the drill's L2 owner's outputs at the fault's predecessor ${predecessor.headerHash} by ${readyByMs.toString()}, when the fault's late-shift window opens; its transfer is handed over once the fault lands`,
          );
          await writeJourneyArtifact(
            join(input.directory, "successor-transfer-deferred.json"),
            { predecessorHeaderHash: predecessor.headerHash, readyByMs },
          );
          return () => Promise.resolve();
        }
        if (performance.now() >= deadline)
          throw new Error(
            `Our node's ledger did not show the drill's L2 owner's outputs at the fault's predecessor ${predecessor.headerHash} within ${input.timeoutMs.toString()} ms; its honest transfer would not build on it`,
          );
        await pause(input.pollIntervalMs ?? 1_000);
      }
      intent = await transfer.build(predecessor);
    }
    const ready = intent;
    return () => transfer.send(ready);
  };
