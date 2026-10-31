import { existsSync } from "node:fs";
import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
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
import { afterAll, afterEach, describe, expect, it } from "vitest";

import type { JourneyLedger } from "../../src/full-stack/journey.js";
import type { JourneyBlock } from "./fixture.js";
import { stackSuccessorPort } from "./honest-successor.js";
import {
  closeServers,
  faultHold,
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

const directories: string[] = [];
afterAll(() =>
  Promise.all(
    directories.map((path) => rm(path, { recursive: true, force: true })),
  ),
);
afterEach(closeServers);

/** A shift of the scripted scheduler: five slots. */
const SHIFT_MS = 5_000;

/** Our node and its L1, scripted, around one drill family. */
const stackDrill = async () => {
  const directory = await mkdtemp(join(tmpdir(), "honest-successor-"));
  directories.push(directory);
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
        ? { body: { txId: url.searchParams.get("tx_hash"), ...node.status } }
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
      slotToUnixTime: (slot) => slot * 1_000,
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
    appointments.push({ operator, startTime: slot * 1_000, slot });
  return {
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

describe("stack successor", () => {
  it("gives our node work while the fault is the tail, then proves all three refusal observables", async () => {
    const drill = await stackDrill();
    const port = drill.port();
    await port.afterFaultLanded();
    expect(drill.sent).toHaveLength(1);
    const { txId } = drill.sent[0]!;
    drill.node.readyz = faultHold(drill.fault.headerHash);
    await drill.observedHold();
    // Our node's shift began while the fault was the tail (committed at
    // slot 10); then the watcher removed the fault and our node built on its
    // predecessor.
    drill.appoints(operator2, 9);
    drill.appoints(honestOperator, 14);
    drill.removed(20);
    drill.node.readyz = { ready: true, reasons: [] };
    const ours = await headerOn(
      drill.predecessor.headerHash,
      honestOperator,
      3n,
    );
    await drill.commits(ours, [txId]);
    const successor = await port.commit();
    expect(successor).toMatchObject({
      headerHash: ours.headerHash,
      commitTxHash: "03".padStart(64, "0"),
    });
    expect(
      JSON.parse(await readFile(join(drill.directory, "refusal.json"), "utf8")),
    ).toMatchObject({
      fault: drill.fault.headerHash,
      holdBinding: "header",
      transfer: txId,
      successor: ours.headerHash,
      faultTail: {
        fromMs: 10_000,
        toMs: 20_000,
        ourShifts: [{ startTime: 14_000, endTime: 19_000 }],
      },
    });
    // A resumed run reads the proven successor back.
    const resumed = drill.port();
    await resumed.afterFaultLanded();
    expect(drill.sent).toHaveLength(1);
    expect((await resumed.commit()).headerHash).toBe(ours.headerHash);
  });

  it("sees our node's hold from the fault's commit, though the watcher corrected the fault before staging ended", async () => {
    const drill = await stackDrill();
    const watch = drill.holdWatch();
    watch.start(drill.fault.headerHash);
    drill.node.readyz = faultHold(drill.fault.headerHash);
    await drill.observedHold();
    // Corrected before the runner gives our node work, in our node's shift:
    // the hold is gone.
    drill.appoints(honestOperator, 11);
    drill.removed(12);
    drill.node.readyz = { ready: true, reasons: [] };
    const port = drill.port(watch);
    await port.afterFaultLanded();
    const ours = await headerOn(
      drill.predecessor.headerHash,
      honestOperator,
      3n,
    );
    await drill.commits(ours, [drill.sent[0]!.txId]);
    await expect(port.commit()).resolves.toMatchObject({
      headerHash: ours.headerHash,
    });
  });

  it("ignores hold observations and a saved transfer of another fault", async () => {
    const drill = await stackDrill();
    const other = await headerOn(drill.predecessor.headerHash, operator2, 9n);
    const observedAt = new Date().toISOString();
    await writeFile(
      join(drill.directory, "node-hold-observations.ndjson"),
      [
        {
          faultHeaderHash: other.headerHash,
          observedAt,
          hold: { state: "held", binding: "header", reasons: [] },
        },
        {
          observedAt,
          hold: { state: "held", binding: "reason_only", reasons: [] },
        },
      ]
        .map((entry) => JSON.stringify(entry))
        .join("\n") + "\n",
    );
    const first = drill.port();
    await first.afterFaultLanded();
    await first.close();
    const path = join(drill.directory, "successor-transfer.json");
    const saved = JSON.parse(await readFile(path, "utf8")) as Record<
      string,
      unknown
    >;
    expect(saved["faultHeaderHash"]).toBe(drill.fault.headerHash);
    // A transfer saved for another fault is not this fault's.
    await writeFile(
      path,
      JSON.stringify({ ...saved, faultHeaderHash: other.headerHash }),
    );
    const port = drill.port();
    await port.afterFaultLanded();
    expect(drill.sent).toHaveLength(2);
    await pause(30);
    await expect(port.commit()).rejects.toThrow(
      /never reported landed_block_invalid/u,
    );
  });

  it("takes our node's commit_base_pending while the fault was the tail for its shift", async () => {
    const drill = await stackDrill();
    const port = drill.port();
    await port.afterFaultLanded();
    const hold = faultHold(drill.fault.headerHash);
    drill.node.readyz = {
      ...hold,
      reasons: [...hold.reasons, "commit_base_pending"],
    };
    await drill.observedHold();
    drill.removed(20);
    drill.node.readyz = { ready: true, reasons: [] };
    const ours = await headerOn(
      drill.predecessor.headerHash,
      honestOperator,
      3n,
    );
    await drill.commits(ours, [drill.sent[0]!.txId]);
    await port.commit();
    expect(
      JSON.parse(await readFile(join(drill.directory, "refusal.json"), "utf8")),
    ).toMatchObject({ faultTail: { ourShifts: [] } });
    expect(
      JSON.parse(await readFile(join(drill.directory, "refusal.json"), "utf8"))
        .faultTail.commitBasePendingAt,
    ).toEqual(expect.any(String));
  });

  it("fails when our node had no shift while the fault was the tail and never reported commit_base_pending", async () => {
    const drill = await stackDrill();
    const port = drill.port();
    await port.afterFaultLanded();
    drill.node.readyz = faultHold(drill.fault.headerHash);
    await drill.observedHold();
    // Operator-2 held the shift until the watcher removed the fault; our
    // node's next shift came after it, and one before the fault ended in
    // time.
    drill.appoints(honestOperator, 1);
    drill.appoints(operator2, 9);
    drill.removed(13);
    drill.appoints(honestOperator, 14);
    drill.node.readyz = { ready: true, reasons: [] };
    await drill.commits(
      await headerOn(drill.predecessor.headerHash, honestOperator, 3n),
      [drill.sent[0]!.txId],
    );
    await expect(port.commit()).rejects.toThrow(
      /had no shift while the fault .* was the tail/u,
    );
  });

  it("fails when our node never held the fault", async () => {
    const drill = await stackDrill();
    const port = drill.port();
    await port.afterFaultLanded();
    await pause(30);
    await expect(port.commit()).rejects.toThrow(
      /never reported landed_block_invalid/u,
    );
  });

  it("fails when our node holds another block than the fault", async () => {
    const drill = await stackDrill();
    const port = drill.port();
    await port.afterFaultLanded();
    const other = await headerOn(drill.predecessor.headerHash, operator2, 9n);
    drill.node.readyz = faultHold(other.headerHash);
    await pause(30);
    await expect(port.commit()).rejects.toThrow(/not on the fault/u);
  });

  it.each([
    [
      "links to the fault",
      async (drill: Awaited<ReturnType<typeof stackDrill>>, txId: string) =>
        drill.commits(
          await headerOn(drill.fault.headerHash, honestOperator, 3n),
          [txId],
        ),
      /links to .* not to the fault's predecessor/u,
    ],
    [
      "does not carry the transfer",
      async (drill: Awaited<ReturnType<typeof stackDrill>>) =>
        drill.commits(
          await headerOn(drill.predecessor.headerHash, honestOperator, 3n),
          ["77".repeat(32)],
        ),
      /does not carry the drill's transfer/u,
    ],
    [
      "is not our operator's",
      async (drill: Awaited<ReturnType<typeof stackDrill>>, txId: string) =>
        drill.commits(
          await headerOn(drill.predecessor.headerHash, operator2, 3n),
          [txId],
        ),
      /is not our operator's/u,
    ],
  ] as const)(
    "fails when our node's next header %s",
    async (_label, commit, refusal) => {
      const drill = await stackDrill();
      const port = drill.port();
      await port.afterFaultLanded();
      drill.node.readyz = faultHold(drill.fault.headerHash);
      await drill.observedHold();
      drill.node.readyz = { ready: true, reasons: [] };
      await commit(drill, drill.sent[0]!.txId);
      await expect(port.commit()).rejects.toThrow(refusal);
    },
  );

  it("fails when an honest header on the fault reached L1, or the hold stays", async () => {
    const drill = await stackDrill();
    const port = drill.port();
    await port.afterFaultLanded();
    drill.node.readyz = faultHold(drill.fault.headerHash);
    await drill.observedHold();
    drill.indexed.push({
      block: await headerOn(drill.fault.headerHash, honestOperator, 8n),
      slot: 15,
    });
    await drill.commits(
      await headerOn(drill.predecessor.headerHash, honestOperator, 3n),
      [drill.sent[0]!.txId],
    );
    await expect(port.commit()).rejects.toThrow(
      /built header .* on the fault/u,
    );

    const held = await stackDrill();
    const stays = held.port();
    await stays.afterFaultLanded();
    held.node.readyz = faultHold(held.fault.headerHash);
    await held.observedHold();
    await held.commits(
      await headerOn(held.predecessor.headerHash, honestOperator, 3n),
      [held.sent[0]!.txId],
    );
    await expect(stays.commit()).rejects.toThrow(/still holds the fault/u);
  });

  it("never resends a transfer our node knows, and refuses a changed saved transfer", async () => {
    const drill = await stackDrill();
    const first = drill.port();
    await first.afterFaultLanded();
    await first.close();
    drill.node.status = { status: "queued" };
    const again = drill.port();
    await again.afterFaultLanded();
    await again.close();
    expect(drill.sent).toHaveLength(1);
    const path = join(drill.directory, "successor-transfer.json");
    const saved = JSON.parse(await readFile(path, "utf8")) as {
      fee: string;
    };
    await writeFile(
      path,
      JSON.stringify({ ...saved, fee: (BigInt(saved.fee) + 1n).toString() }),
    );
    await expect(drill.port().afterFaultLanded()).rejects.toThrow(
      /differs from its exact transaction identity or fee/u,
    );
  });
});
