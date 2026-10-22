/**
 * A foreign block's replay failing where the block's input cannot make it
 * fail is this node's own fault, never a verdict on the block: the import
 * is `local_fault`, held on a retried hold whose subject is the block, and
 * the same block imports once the computation works again. Each site:
 * - the import check decoding a transaction the replay already accepted;
 * - the event replay dying (a throw past Phase A, which fails typed);
 * - encoding the event-to-step value of a phase the replay already read.
 *
 * A rejection the event replay derives from the block's transaction and
 * ledger stays the block's: a normal transaction it rejects is `invalid`,
 * and a forced transaction committed with its rejection imports.
 */
import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

/** What each site throws when the test arms it, and how often it did. */
const inject = vi.hoisted(() => ({
  decode: undefined as (() => unknown) | undefined,
  replay: undefined as (() => unknown) | undefined,
  encode: undefined as (() => unknown) | undefined,
  thrown: 0,
}));
const armed = (site: (() => unknown) | undefined) => {
  if (site === undefined) return;
  inject.thrown += 1;
  throw site();
};
vi.mock("@al-ft/midgard-validation", async (importOriginal) => {
  const actual =
    await importOriginal<typeof import("@al-ft/midgard-validation")>();
  return {
    ...actual,
    decodeMidgardLedgerTxFromCanonicalCbor: (
      ...args: Parameters<typeof actual.decodeMidgardLedgerTxFromCanonicalCbor>
    ) => {
      armed(inject.decode);
      return actual.decodeMidgardLedgerTxFromCanonicalCbor(...args);
    },
  };
});
vi.mock("@al-ft/midgard-validation/phase-b", async (importOriginal) => {
  const actual =
    await importOriginal<typeof import("@al-ft/midgard-validation/phase-b")>();
  return {
    ...actual,
    runPhaseBValidationWithPatch: (
      ...args: Parameters<typeof actual.runPhaseBValidationWithPatch>
    ) => {
      armed(inject.replay);
      return actual.runPhaseBValidationWithPatch(...args);
    },
  };
});
vi.mock(
  "../../midgard-fault-proofs/src/block-replay.encoding.js",
  async (importOriginal) => {
    const actual =
      await importOriginal<
        typeof import("../../midgard-fault-proofs/src/block-replay.encoding.js")
      >();
    return {
      ...actual,
      encodeEventToStepValueCbor: (
        ...args: Parameters<typeof actual.encodeEventToStepValueCbor>
      ) => {
        armed(inject.encode);
        return actual.encodeEventToStepValueCbor(...args);
      },
    };
  },
);

import { isRetriedHold, localFaultSubject } from "../src/l1-events/driver.js";
import {
  combineHolds,
  LANDED_BLOCK_LOCAL_FAULT,
} from "../src/landed-blocks/holds.js";
import { replayOutcomeHold } from "../src/landed-blocks/replay-outcome-hold.js";
import { computeLedgerMpfRootFromLedgerEntries } from "../src/mpf/ledger-hydration.js";
import { ForeignBlockVerificationError } from "../src/mpf/verified-block-import.js";
import { rebind, verdict } from "./foreign-block-import.fixture.js";
import {
  committedBlock,
  forcedBlock,
  fundedBy,
  type LedgerEntry,
} from "./import-check.fixture.js";

type Block = Awaited<ReturnType<typeof committedBlock>>;
type Site = "decode" | "replay" | "encode";

const oneSpend = async (byte: number): Promise<Block> => {
  const parent = [fundedBy(byte, "accept")];
  return committedBlock(parent, [
    { input: parent[0]!.outRef, body: "accept", next: "accept" },
  ]);
};

const siteFault = () => new Error("site fault injected by the test");

/** The import with `site` armed: its outcome, and how often the site threw. */
const importArmed = async (block: Block, site: Site) => {
  const thrownBefore = inject.thrown;
  inject[site] = siteFault;
  const result = await block.imported().finally(() => {
    inject[site] = undefined;
  });
  if (result._tag === "Right") throw new Error("the import succeeded");
  const error = result.left;
  if (!(error instanceof ForeignBlockVerificationError))
    throw new Error(`the import failed outside its outcomes: ${String(error)}`);
  return { error, thrown: inject.thrown - thrownBefore };
};

describe("a foreign block import failing locally at a replay site", () => {
  it.each([
    [
      "the import check decoding a replayed transaction again",
      "decode",
      0x71,
      "import check bookkeeping failed locally",
    ],
    ["the event replay dying", "replay", 0x72, "event replay failed locally"],
    [
      "encoding an event-to-step value",
      "encode",
      0x73,
      "event-to-step encoding failed locally",
    ],
  ] as const)(
    "holds on %s, then imports the block",
    async (_name, site, byte, named) => {
      const block = await oneSpend(byte);
      const { error, thrown } = await importArmed(block, site);
      expect(thrown).toBe(1);
      expect([error.reason, error.foreignHeaderHash]).toEqual([
        "local_fault",
        block.headerHash,
      ]);
      expect(error.detail).toContain(named);
      expect(error.detail).toContain("site fault injected by the test");
      const hold = combineHolds([
        replayOutcomeHold(
          "local_fault",
          `block ${block.headerHash}: ${error.detail}`,
          block.headerHash,
        ),
      ])!;
      expect([
        hold.reason,
        isRetriedHold(hold),
        localFaultSubject(hold),
      ]).toEqual([LANDED_BLOCK_LOCAL_FAULT, true, block.headerHash]);
      expect((await block.imported())._tag).toBe("Right");
    },
  );
});

/** `block`'s payload on `parent` instead of its own parent. */
const onParent = async (block: Block, parent: readonly LedgerEntry[]) => {
  const parentEntries = parent.map((entry) => ({
    outref: entry.outRef,
    output: entry.output,
  }));
  const parentUtxosRoot = await Effect.runPromise(
    computeLedgerMpfRootFromLedgerEntries(parentEntries),
  );
  const payload = await rebind({
    ...block.payload,
    block_body: {
      ...block.payload.block_body,
      header: {
        ...block.payload.block_body.header,
        prevUtxosRoot: parentUtxosRoot,
      },
    },
  });
  return verdict(payload, { parentUtxosRoot, parentEntries });
};

describe("a rejection the event replay derives from the block", () => {
  it("refuses a normal transaction the replay rejects on the block's parent, which imports on the ledger it spends", async () => {
    const block = await oneSpend(0x74);
    expect((await block.imported())._tag).toBe("Right");
    // The same out-ref, locked by another script than the one the
    // transaction carries.
    const result = await onParent(block, [fundedBy(0x74, "reject")]);
    expect(result).toMatchObject({
      _tag: "Left",
      left: {
        reason: "invalid",
        detail:
          "BlockReplayFault: foreign normal transaction was rejected by replay",
      },
    });
  });

  it("imports a forced transaction committed with the rejection its replay derives", async () => {
    const parent = [fundedBy(0x75, "reject")];
    const block = await forcedBlock(parent, {
      input: parent[0]!.outRef,
      body: "reject",
      next: "accept",
    });
    expect(block.rejection.code).toBe("E_PLUTUS_SCRIPT_INVALID");
    expect((await block.imported())._tag).toBe("Right");
  });
});
