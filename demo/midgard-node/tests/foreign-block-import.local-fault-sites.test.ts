/**
 * A foreign block's replay failing locally is this node's own fault, never a
 * verdict on the block, and the same block imports once the computation
 * works again. Where the block's input cannot make it fail the import is
 * `local_fault`, held on a retried hold whose subject is the block (bounded
 * by the coalesced runner's local-fault exit):
 * - the import check decoding a transaction the replay already accepted;
 * - encoding the event-to-step value of a phase the replay already read.
 * The event replay dying (a throw past Phase A, which fails typed), which
 * the block's input can reach, is `replay_held`: a plain retried hold the
 * node never exits on. The block is held and replayed again on a 1 s to
 * 60 s backoff, named stuck after `LOCAL_FAULT_RUN_LIMIT` identical
 * failures, while the runner keeps running.
 *
 * A rejection the event replay derives from the block's transaction and
 * ledger stays the block's: a normal transaction it rejects is `invalid`,
 * and a forced transaction committed with its rejection imports.
 */
import { LOCAL_FAULT_RUN_LIMIT } from "@al-ft/midgard-fault-proofs";
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

import {
  type DriverHold,
  isRetriedHold,
  localFaultSubject,
} from "../src/l1-events/driver.js";
import {
  combineHolds,
  LANDED_BLOCK_LOCAL_FAULT,
  LANDED_BLOCK_REPLAY_HELD,
  LANDED_BLOCK_REPLAY_STUCK,
} from "../src/landed-blocks/holds.js";
import type { ReplayOutcome } from "../src/landed-blocks/replay.js";
import { heldReplays } from "../src/landed-blocks/replay-held.js";
import { replayOutcomeHold } from "../src/landed-blocks/replay-outcome-hold.js";
import { computeLedgerMpfRootFromLedgerEntries } from "../src/mpf/ledger-hydration.js";
import { ForeignBlockVerificationError } from "../src/mpf/verified-block-import.js";
import {
  coalescedRunner,
  localFaultProgress,
} from "../src/services/l1-follower.coalesced-runner.js";
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
      "local_fault",
      LANDED_BLOCK_LOCAL_FAULT,
      true,
    ],
    [
      "the event replay dying",
      "replay",
      0x72,
      "event replay failed locally",
      "replay_held",
      LANDED_BLOCK_REPLAY_HELD,
      false,
    ],
    [
      "encoding an event-to-step value",
      "encode",
      0x73,
      "event-to-step encoding failed locally",
      "local_fault",
      LANDED_BLOCK_LOCAL_FAULT,
      true,
    ],
  ] as const)(
    "holds on %s, then imports the block",
    async (_name, site, byte, named, reason, holdReason, bounded) => {
      const block = await oneSpend(byte);
      const { error, thrown } = await importArmed(block, site);
      expect(thrown).toBe(1);
      expect([error.reason, error.foreignHeaderHash]).toEqual([
        reason,
        block.headerHash,
      ]);
      expect(error.detail).toContain(named);
      expect(error.detail).toContain("site fault injected by the test");
      const hold = combineHolds([
        replayOutcomeHold(
          reason,
          `block ${block.headerHash}: ${error.detail}`,
          block.headerHash,
        ),
      ])!;
      expect([
        hold.reason,
        isRetriedHold(hold),
        localFaultSubject(hold),
      ]).toEqual([holdReason, true, bounded ? block.headerHash : undefined]);
      expect((await block.imported())._tag).toBe("Right");
    },
  );
});

/** The import as the landed-block replay port sees it. */
const replayOf = (block: Block) => (): Effect.Effect<ReplayOutcome> =>
  Effect.promise(() => block.imported()).pipe(
    Effect.map((result) =>
      result._tag === "Right"
        ? ({ kind: "replayed" } as unknown as ReplayOutcome)
        : (() => {
            const error = result.left;
            if (!(error instanceof ForeignBlockVerificationError))
              throw new Error(`the import failed: ${String(error)}`);
            return { kind: error.reason, detail: error.detail };
          })(),
    ),
  );

describe("a foreign block whose event replay keeps dying", () => {
  it("is held past the local-fault bound on a backoff, named stuck, with the runner still running, and imports once the replay works", async () => {
    const block = await oneSpend(0x76);
    const clock = { ms: 0 };
    const attempts: number[] = [];
    const replay = heldReplays(replayOf(block), () => clock.ms);
    const runs: DriverHold[][] = [];
    const exhausted: (readonly DriverHold[])[] = [];
    let running = false;
    const abort = new AbortController();
    inject.replay = siteFault;
    // Only the runner's backoff is faked; the import's own turns stay real.
    vi.useFakeTimers({ toFake: ["setTimeout", "clearTimeout"] });
    try {
      const trigger = coalescedRunner(
        async () => {
          running = true;
          try {
            const thrown = inject.thrown;
            const outcome = await Effect.runPromise(
              replay({ headerHash: block.headerHash } as never),
            );
            if (inject.thrown > thrown) attempts.push(clock.ms);
            const holds =
              outcome.kind === "replayed"
                ? []
                : [
                    replayOutcomeHold(
                      outcome.kind as Parameters<typeof replayOutcomeHold>[0],
                      `block ${block.headerHash}: ${outcome.detail}`,
                      block.headerHash,
                    ),
                  ];
            runs.push(holds);
            return holds;
          } finally {
            running = false;
          }
        },
        abort.signal,
        {
          progress: localFaultProgress(),
          onExhausted: (holds) => exhausted.push(holds),
        },
      );
      const drive = async (steps: number, until: () => boolean) => {
        for (let step = 0; step < steps && !until(); step += 1) {
          clock.ms += 1_000;
          await vi.advanceTimersByTimeAsync(1_000);
          await new Promise<void>((resolve) => setImmediate(resolve));
        }
      };
      trigger();
      await drive(
        900,
        () => attempts.length >= LOCAL_FAULT_RUN_LIMIT + 2 && !running,
      );
      expect(exhausted).toEqual([]);
      expect(attempts).toHaveLength(LOCAL_FAULT_RUN_LIMIT + 2);
      // One import per backoff: 1 s doubling to 60 s between attempts.
      attempts.slice(1).forEach((at, index) => {
        expect(at - attempts[index]!).toBeGreaterThanOrEqual(
          Math.min(1_000 * 2 ** index, 60_000),
        );
      });
      expect(runs.length).toBeGreaterThan(attempts.length);
      const last = runs.at(-1)!;
      expect(
        last.map((hold) => [
          hold.reason,
          isRetriedHold(hold),
          localFaultSubject(hold),
        ]),
      ).toEqual([[LANDED_BLOCK_REPLAY_STUCK, true, undefined]]);
      // The replay works again: the next attempt imports the block.
      inject.replay = undefined;
      await drive(120, () => runs.at(-1)?.length === 0 && !running);
      expect(runs.at(-1)).toEqual([]);
      expect(exhausted).toEqual([]);
    } finally {
      inject.replay = undefined;
      abort.abort();
      vi.useRealTimers();
    }
  }, 300_000);
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
