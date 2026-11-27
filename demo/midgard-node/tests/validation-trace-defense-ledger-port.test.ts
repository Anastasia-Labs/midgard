/**
 * The defense material build over its read channel, in process (no worker,
 * no emulator): the build the material worker runs, its predecessor read
 * from a store ledger served over a `MessageChannel` by `serveLedgerReads`.
 *
 * - On the one-event Plutus block fixture, passing at several step counts
 *   and failing, its outcome is byte for byte the one the fault-proofs
 *   builder gives over the whole predecessor admitted to a memory store.
 * - Padding the predecessor from 16 to 4,096 entries no event touches
 *   leaves the outputs served unchanged and adds no more trie nodes than
 *   the deeper trie's added levels (each branch with its children) on each
 *   key the event touches.
 * - A served read that fails, at whichever read it is, makes the outcome
 *   `unavailable` or leaves it the reference material; never another
 *   verdict.
 * - The worker's side gets each output as a `Buffer` of the served bytes
 *   (a port carries a `Buffer` as a plain `Uint8Array`, whose
 *   `toString("hex")` is not hex).
 */
import { MessageChannel } from "node:worker_threads";

import { ledgerEd25519SignatureReady } from "@al-ft/midgard-core";
import {
  createLedgerStore,
  createMemoryLedgerStorage,
  ledgerReads,
  type LedgerSource,
} from "@al-ft/midgard-fault-proofs/ledger-store";
import {
  filler,
  l1SlotConfig,
  plutusExecutionBlockFixture,
} from "@al-ft/midgard-fault-proofs/test-support/validation-trace-defender-scene";
import { buildPortableValidationTraceDefenseMaterial } from "@al-ft/midgard-fault-proofs/validation-trace-dispute/defender";
import * as SDKDa from "@al-ft/midgard-sdk/da";
import { beforeAll, describe, expect, it } from "vitest";

import {
  ledgerReadClient,
  serveLedgerReads,
} from "../src/validation-trace-defense/ledger-port.js";
import {
  buildPortableDefenseOutcome,
  type PortableDefenseOutcome,
} from "../src/validation-trace-defense/material-build.js";

type Block = Awaited<ReturnType<typeof plutusExecutionBlockFixture>>;

/** The store ledger at `block`'s parent: its predecessor admitted to a
 * memory store, then read back by its root as the node's store gives it. */
const storeLedger = async (block: Block) => {
  const store = createLedgerStore(createMemoryLedgerStorage());
  const root = block.evidence.header.prevUtxosRoot;
  await store.admit(block.predecessorEntries, root);
  return await store.at(root);
};

/** The build the material worker runs, reading `source` over a channel. */
const servedBuild = async (
  block: Block,
  stepCount: number,
  source: LedgerSource,
): Promise<PortableDefenseOutcome> => {
  const channel = new MessageChannel();
  serveLedgerReads(channel.port1, source);
  try {
    return await buildPortableDefenseOutcome({
      payloadEnvelopeCbor: block.payloadEnvelopeCbor,
      schemaVersion: Number(SDKDa.DA_PAYLOAD_VERSION),
      point: { slot: 1, hash: "00".repeat(32) },
      headerHash: block.evidence.headerHash,
      parentHeaderHash: block.evidence.header.prevHeaderHash,
      parentUtxosRoot: block.evidence.header.prevUtxosRoot,
      ledgerPort: channel.port2,
      eventKey: block.eventKey,
      stepCount,
      l1SlotConfig,
    });
  } finally {
    channel.port1.close();
  }
};

/** The fault-proofs builder's material over the whole predecessor admitted
 * to a memory store. */
const reference = async (block: Block, stepCount: number) =>
  await buildPortableValidationTraceDefenseMaterial({
    evidence: block.evidence,
    predecessor: await createLedgerStore(createMemoryLedgerStorage()).admit(
      block.predecessorEntries,
      block.evidence.header.prevUtxosRoot,
    ),
    eventKey: block.eventKey,
    stepCount,
    l1SlotConfig,
  });

/** `source` with its reads counted: trie nodes asked for, output keys,
 * and calls. */
const counted = (source: LedgerSource) => {
  const count = { nodes: 0, outputs: 0, calls: 0 };
  return {
    count,
    source: {
      node: async (hash) => {
        count.nodes += 1;
        count.calls += 1;
        return await source.node(hash);
      },
      outputs: async (outRefs) => {
        count.outputs += outRefs.length;
        count.calls += 1;
        return await source.outputs(outRefs);
      },
    } satisfies LedgerSource,
  };
};

const defense = (outcome: PortableDefenseOutcome) => {
  if (outcome.kind !== "defend")
    throw new Error(`expected defense material, got ${outcome.kind}`);
  return outcome.defense;
};

beforeAll(async () => {
  await ledgerEd25519SignatureReady();
});

describe("the defense material build over its read channel", () => {
  it("gives the reference material byte for byte at every step count, and the operator's fault on a failing block", async () => {
    const passing = await plutusExecutionBlockFixture("passing");
    const first = await reference(passing, 1);
    if (first.kind !== "defend") throw new Error("expected a passing defense");
    const honest = first.honestStepCount;
    expect(honest).toBeGreaterThan(2);
    const ledger = await storeLedger(passing);
    for (const stepCount of [1, honest - 1, honest, honest + 5])
      expect(
        defense(await servedBuild(passing, stepCount, ledgerReads(ledger))),
      ).toStrictEqual(await reference(passing, stepCount));

    const failing = await plutusExecutionBlockFixture("failing");
    expect((await reference(failing, 1)).kind).toBe("operator_fault");
    expect(
      (await servedBuild(failing, 1, ledgerReads(await storeLedger(failing))))
        .kind,
    ).toBe("operator_fault");
  });

  it("serves the same outputs and about one trie level more per touched key from a predecessor 256 times larger", async () => {
    const base = await plutusExecutionBlockFixture("passing");
    const padded = async (count: number) => {
      const block = await plutusExecutionBlockFixture("passing", {
        predecessorEntries: [...base.predecessorEntries, ...filler(count)],
      });
      const reads = counted(ledgerReads(await storeLedger(block)));
      // The material is the reference over the same padded ledger (the
      // block's roots, and so its trace, move with the padding).
      expect(defense(await servedBuild(block, 1, reads.source))).toStrictEqual(
        await reference(block, 1),
      );
      return { block, count: reads.count };
    };
    const small = await padded(16);
    const large = await padded(4_096);
    expect(large.count.outputs).toBe(small.count.outputs);
    // The keys the event touches: those it spends and those it creates.
    const held = (entries: Block["predecessorEntries"]) =>
      new Set(entries.map(({ outRef }) => outRef.toString("hex")));
    const before = held(small.block.predecessorEntries);
    const after = held(small.block.postEntries);
    const touched =
      [...before].filter((key) => !after.has(key)).length +
      [...after].filter((key) => !before.has(key)).length;
    // A 16-ary trie 256 times larger is two levels deeper, and the trie
    // library reads every child of each branch on a key's path: at most 16
    // more nodes per added level on each touched key.
    expect(large.count.nodes - small.count.nodes).toBeLessThanOrEqual(
      2 * 16 * touched,
    );
    expect(large.count.nodes).toBeLessThan(4_096 / 16);
  });

  it("is unavailable or the reference material when the served ledger fails at any one read", async () => {
    const block = await plutusExecutionBlockFixture("passing");
    const ledger = await storeLedger(block);
    const expected = await reference(block, 1);
    const all = counted(ledgerReads(ledger));
    await servedBuild(block, 1, all.source);
    const reads = all.count.calls;
    expect(reads).toBeGreaterThan(0);
    const kinds = new Set<string>();
    for (let failAt = 1; failAt <= reads; failAt += 1) {
      let read = 0;
      const source = ledgerReads(ledger);
      const failing = async <T>(run: () => Promise<T>) => {
        read += 1;
        if (read === failAt)
          throw new Error(`the served ledger fails at read ${failAt}`);
        return await run();
      };
      const outcome = await servedBuild(block, 1, {
        node: (hash) => failing(() => source.node(hash)),
        outputs: (outRefs) => failing(() => source.outputs(outRefs)),
      });
      kinds.add(outcome.kind);
      if (outcome.kind === "unavailable")
        expect(outcome.detail).toMatch(/the served ledger fails at read/u);
      else expect(defense(outcome)).toStrictEqual(expected);
    }
    expect(kinds.has("unavailable")).toBe(true);
  });

  it("gives the worker each output as a Buffer of the served bytes, and undefined for one the ledger lacks", async () => {
    const block = await plutusExecutionBlockFixture("passing");
    const held = block.predecessorEntries[0]!;
    const channel = new MessageChannel();
    serveLedgerReads(channel.port1, ledgerReads(await storeLedger(block)));
    const client = ledgerReadClient(channel.port2);
    try {
      const [output, absent] = await client.outputs([
        held.outRef.toString("hex"),
        "ff".repeat(34),
      ]);
      expect(Buffer.isBuffer(output)).toBe(true);
      expect(output?.toString("hex")).toBe(held.output.toString("hex"));
      expect(absent).toBeUndefined();
    } finally {
      client.close();
      channel.port1.close();
    }
  });
});
