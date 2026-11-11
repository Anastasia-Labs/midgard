import { existsSync } from "node:fs";
import { mkdtemp, readFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { setTimeout as pause } from "node:timers/promises";

import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import * as SDK from "@al-ft/midgard-sdk";
import {
  assetsToValue,
  CML,
  generateSeedPhrase,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import {
  makeMidgardTxOutput,
  makeOutRefCbor,
} from "midgard-node/tests/midgard-output-helpers";

import type { JourneyLedger } from "../../src/full-stack/journey.js";
import type { JourneyBlock } from "./fixture.js";
import { stackSuccessorPort } from "./honest-successor.js";
import {
  headerOn,
  honestOperator,
  type IndexedAppointment,
  type IndexedOutput,
  kupo,
  operator2,
  policyId,
  schedulerPolicyId,
  serve,
} from "./honest-successor.test-support.js";
import { createNodeHoldWatch } from "./node-hold-watch.js";

/** Every drill's directory, for the suite to remove. */
export const drillDirectories: string[] = [];

/** A shift of the scripted scheduler: five slots. */
const SHIFT_MS = 5_000;

/**
 * Our node and its L1, scripted, around one drill family. Slots are seconds
 * of wall-clock time, which our node's hold observations are stamped with:
 * the drill starts at `startSlot`, by default the fault's commit slot.
 */
export const stackDrill = async (options: { startSlot?: number } = {}) => {
  const epochMs = Date.now() - (options.startSlot ?? 10) * 1_000;
  /** The time of `slot`. */
  const at = (slot: number) => epochMs + slot * 1_000;
  const directory = await mkdtemp(join(tmpdir(), "honest-successor-"));
  drillDirectories.push(directory);
  const seedPhrase = generateSeedPhrase();
  const owner = walletFromSeed(seedPhrase, { network: "Preprod" }).address;
  const predecessor = await headerOn("00".repeat(28), honestOperator, 1n);
  const fault = await headerOn(predecessor.headerHash, operator2, 2n);
  const node = {
    readyz: { ready: false, reasons: ["landed_block_awaiting_da"] } as unknown,
    status: { status: "not_found" } as Record<string, unknown>,
  };
  const nodeEndpoint = await serve((url) =>
    url.pathname === "/readyz"
      ? { status: 503, body: node.readyz }
      : url.pathname === "/tx-status"
        ? {
            // Our node answers an unknown transaction with 404 and its status.
            status: node.status["status"] === "not_found" ? 404 : 200,
            body: { txId: url.searchParams.get("tx_hash"), ...node.status },
          }
        : { status: 404, body: {} },
  );
  const indexed: IndexedOutput[] = [
    { block: predecessor, slot: 5 },
    { block: fault, slot: 10 },
  ];
  const appointments: IndexedAppointment[] = [];
  const kupoUrl = await kupo(
    () => indexed,
    () => appointments,
  );
  const sent: { cbor: string; txId: string }[] = [];
  const ledger: JourneyLedger = {
    utxos: (address) => {
      const txHash = "44".repeat(32);
      const assets = { lovelace: 50_000_000n };
      return Promise.resolve([
        {
          txHash,
          outputIndex: 0,
          outrefCbor: makeOutRefCbor(txHash, 0),
          outputCbor: Buffer.from(
            makeMidgardTxOutput(
              CML.Address.from_bech32(address),
              assetsToValue(assets),
            ).to_cbor_bytes(),
          ),
          address,
          assets,
        },
      ]);
    },
    submitTransfer: (cbor, txId) => {
      sent.push({ cbor, txId });
      return Promise.resolve({ txId, status: "queued" });
    },
  };
  const envelopes = new Map<string, Buffer>();
  /** Our node's hold watch, as the drill starts it at the fault's commit. */
  const holdWatch = () =>
    createNodeHoldWatch({ directory, nodeEndpoint, pollIntervalMs: 5 });
  const port = (watch?: ReturnType<typeof holdWatch>) =>
    stackSuccessorPort({
      ...(watch === undefined ? {} : { holdWatch: watch }),
      directory,
      nodeEndpoint,
      ledger,
      kupoUrl,
      stateQueuePolicyId: policyId,
      honestOperatorKeyHash: honestOperator,
      ledgerOwner: { seedPhrase, address: owner },
      network: "Preprod",
      transferLovelace: 5_000_000n,
      predecessor,
      fault,
      fetchPayloadEnvelope: (headerHash) => {
        const envelope = envelopes.get(headerHash);
        return envelope === undefined
          ? Promise.reject(new Error(`no public DA copy of ${headerHash}`))
          : Promise.resolve(envelope);
      },
      poll: async (name, action, timeoutMs = 5_000) => {
        const deadline = performance.now() + timeoutMs;
        for (;;) {
          const result = await action();
          if (result !== undefined) return result;
          if (performance.now() >= deadline)
            throw new Error(`Timed out waiting for ${name}`);
          await pause(5);
        }
      },
      commitTimeoutMs: 2_000,
      holdPollIntervalMs: 5,
      schedulerPolicyId,
      shiftDurationMs: SHIFT_MS,
      slotToUnixTime: at,
    });
  /** Our node commits `block` carrying `transactions`, published and on L1. */
  const commits = async (
    block: JourneyBlock,
    transactions: readonly string[],
  ) => {
    const payload: SDK.DaPayload = {
      version: SDK.DA_PAYLOAD_VERSION,
      block_body: {
        header_hash: block.headerHash,
        header: block.header,
        withdrawals: [],
        forced_transactions: [],
        transactions: transactions.map((txId) => [txId, "00"]),
        transaction_preimages: [],
        forced_transaction_preimages: [],
        deposits: [],
        transition_trace: [],
        event_to_step: [],
        validation_traces: [],
        validation_trace_witnesses: [],
        counts: {
          withdrawalCount: 0n,
          forcedTransactionCount: 0n,
          l2TransactionCount: BigInt(transactions.length),
          depositCount: 0n,
          totalEventCount: BigInt(transactions.length),
          transitionStepCount: BigInt(transactions.length),
          validationTraceCount: 0n,
        },
      },
    };
    envelopes.set(
      block.headerHash,
      await wrapDaPayload(SDK.encodeDaPayload(payload), { mode: "identity" }),
    );
    indexed.push({ block, slot: 30 });
    node.status = { status: "committed", headerHash: block.headerHash };
  };
  const observedHold = async () => {
    const path = join(directory, "node-hold-observations.ndjson");
    for (;;) {
      if (
        existsSync(path) &&
        (await readFile(path, "utf8")).includes('"state":"held"')
      )
        return;
      await pause(5);
    }
  };
  /** The watcher removed the fault at `slot`. */
  const removed = (slot: number) => {
    indexed[1] = { ...indexed[1]!, spentAt: slot };
  };
  /** The scheduler appoints `operator` from `slot`, one second a slot. */
  const appoints = (operator: string, slot: number) =>
    appointments.push({ operator, startTime: at(slot), slot });
  return {
    at,
    directory,
    predecessor,
    fault,
    node,
    indexed,
    sent,
    port,
    holdWatch,
    commits,
    observedHold,
    removed,
    appoints,
  };
};
