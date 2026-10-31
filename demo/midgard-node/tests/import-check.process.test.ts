/**
 * The import checker's set as the landed-block driver keeps it: processing
 * a foreign block with Plutus transactions records each of them due at its
 * block's deadline, and the rebase that takes a block off the landed chain
 * takes its entries out with it.
 */
import "./utils.js";

import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  type ImportCheckState,
  recordChecks,
  retrieveChecks,
} from "../src/database/importChecks.js";
import { importCheckDeadlineMs } from "../src/import-check/deadline.js";
import { processLandedQueue } from "../src/landed-blocks/process.js";
import { rollBackRows } from "../src/landed-blocks/settlements.js";
import { retrieveRows } from "../src/landed-blocks/store.js";
import { testWrite } from "./helpers/driver-recompute.js";
import { committedBlock, fundedBy } from "./import-check.fixture.js";
import {
  block,
  confirmedAt,
  F1,
  G0,
  GENESIS,
  harness,
  inNode,
  ports,
  queueOf,
  rootOf,
} from "./landed-blocks-own.fixture.js";
import {
  attempt,
  BLOCK,
  entry,
  freshNative,
  hex,
  land,
  processOf,
  root,
  run,
  seed,
  sqlRun,
} from "./landed-blocks-rebase.fixture.js";

const ALL: readonly ImportCheckState[] = [
  "pending",
  "cap_skipped",
  "cache_skipped",
  "faulted",
  "rejected",
];

/** The import of a block of one Plutus spend: its checks. */
const plutusChecks = async () => {
  const parent = [fundedBy(0x41, "reject")];
  const built = await committedBlock(parent, [
    { input: parent[0]!.outRef, body: "reject", next: "accept" },
  ]);
  const imported = await built.imported();
  if (imported._tag === "Left") throw imported.left;
  expect(imported.right.scriptChecks.checks).toHaveLength(1);
  return { scriptChecks: imported.right.scriptChecks, txId: built.txIds[0]! };
};

describe("import checks on the landed chain", () => {
  it("records a processed foreign block's Plutus transaction, due at the block's deadline", async () => {
    const { scriptChecks, txId } = await plutusChecks();
    const genesisRoot = await rootOf(GENESIS);
    const f = block(
      2,
      { hash: SDK.GENESIS_HEADER_HASH, root: genesisRoot, endTime: 0n },
      await rootOf([G0, F1]),
    );
    const state = harness();
    const stub = ports(state);
    const withChecks: typeof stub = {
      ...stub,
      replay: (input) =>
        Effect.map(stub.replay(input), (outcome) =>
          outcome.kind === "replayed" ? { ...outcome, scriptChecks } : outcome,
        ),
    };
    const recorded = await inNode(
      Effect.gen(function* () {
        yield* processLandedQueue(
          withChecks,
          queueOf(confirmedAt("genesis", genesisRoot), [f]),
        );
        return yield* retrieveChecks(ALL);
      }),
    );
    expect(state.replays.map((input) => input.headerHash)).toEqual([f.hash]);
    expect(recorded).toEqual([
      expect.objectContaining({
        headerHash: f.hash,
        transactionId: txId,
        state: "pending",
        deadlineMs: importCheckDeadlineMs(f.header.endTime),
      }),
    ]);
  }, 120_000);

  it("takes a block's entries out when the rebase removes the block from the landed chain", async () => {
    const { scriptChecks } = await plutusChecks();
    const native = freshNative();
    const globals = await processOf(native);
    await seed(globals);
    await sqlRun(globals, () =>
      recordChecks(BLOCK, Date.now() + 3_600_000, scriptChecks),
    );
    expect(await run(globals, retrieveChecks(ALL))).toHaveLength(1);
    expect((await attempt(globals)).applied).toEqual([true]);

    // The block leaves the landed chain; another lands in its place.
    const left = await run(globals, retrieveRows);
    await run(globals, testWrite(rollBackRows(left, [])));
    const R3 = root(0x13);
    const E3 = entry("e3", 5_000_000n);
    await land(globals, {
      headerHash: "c3".repeat(28),
      utxosRoot: R3,
      produced: [E3],
    });
    native.reaches = R3;
    const shown = await attempt(globals);
    expect(shown.hold).toBeUndefined();
    expect(shown.working).toContain(hex(E3.outref));
    expect(
      (await run(globals, retrieveRows)).map((row) => row.headerHash),
    ).toEqual(["c3".repeat(28)]);
    expect(await run(globals, retrieveChecks(ALL))).toEqual([]);
  }, 120_000);
});
