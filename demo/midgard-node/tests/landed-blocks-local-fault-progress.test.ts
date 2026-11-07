/**
 * Processing's report of the blocks it commits (`ProcessOptions.committed`),
 * which the follower feeds to the coalesced runner's local-fault bound, on
 * the node database: a block held on a verifier fault is not reported and
 * its hold is a local fault on its header hash, whatever the fault's text;
 * once the fault clears and processing commits the block, it is reported,
 * and the runner's progress watching that block says it passed. A block
 * naming an event the facts lack, short of covering its range, is held as a
 * local fault on it too, and so is a block whose admitted forced order the
 * facts cannot read back.
 */
import "./utils.js";

import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { localFaultSubject } from "../src/l1-events/driver.js";
import {
  LANDED_BLOCK_EVENT_UNKNOWN,
  LANDED_BLOCK_FORCED_ORDER_PENDING,
  LANDED_BLOCK_VERIFIER_UNAVAILABLE,
} from "../src/landed-blocks/holds.js";
import { processLandedQueue } from "../src/landed-blocks/process.js";
import { retrieveRows } from "../src/landed-blocks/store.js";
import { localFaultProgress } from "../src/services/l1-follower.coalesced-runner.js";
import {
  type Block,
  fixture,
  harness,
  inNode,
  ports,
  queueOf,
} from "./landed-blocks-own.fixture.js";

describe("processing reports the blocks it commits", () => {
  it("reports a block held on a verifier fault only once it commits it", async () => {
    const { a, f, genesisState, journalA } = await fixture();
    const state = harness();
    state.journals.set(a.hash, journalA);
    const committed: string[] = [];
    const progress = localFaultProgress();
    await inNode(
      Effect.gen(function* () {
        const process = (nodes: readonly Block[]) =>
          processLandedQueue(ports(state), queueOf(genesisState, nodes), {
            committed: (headerHash) => {
              committed.push(headerHash);
              progress.committed(headerHash);
            },
          });
        state.replayEnds = Effect.succeed({
          kind: "verifier_unavailable",
          detail: "LedgerSignatureVerifierUnavailableError: injected",
        } as const);
        const held = yield* process([a, f]);
        expect(held?.reason).toBe(LANDED_BLOCK_VERIFIER_UNAVAILABLE);
        expect(localFaultSubject(held!)).toBe(f.hash);
        expect(committed).toEqual([a.hash]);
        progress.watch([localFaultSubject(held!)!]);
        expect(progress.passed()).toBe(false);

        // Still faulted, with another text: the same subject, and no
        // progress past the block.
        state.replayEnds = Effect.succeed({
          kind: "verifier_unavailable",
          detail: "LedgerSignatureVerifierUnavailableError: raised again",
        } as const);
        const again = yield* process([a, f]);
        expect(again?.detail).not.toBe(held?.detail);
        expect(localFaultSubject(again!)).toBe(f.hash);
        expect(committed).toEqual([a.hash]);
        expect(progress.passed()).toBe(false);

        state.replayEnds = undefined;
        yield* process([a, f]);
        expect(committed).toEqual([a.hash, f.hash]);
        expect(
          (yield* retrieveRows).map((row) => [row.headerHash, row.state]),
        ).toEqual([
          [a.hash, "processed"],
          [f.hash, "processed"],
        ]);
        expect(progress.passed()).toBe(true);
        // Said once: the next call watches nothing.
        expect(progress.passed()).toBe(false);
      }),
    );
  }, 120_000);

  it("holds a block naming an event the facts lack, short of covering its range, as a local fault on it", async () => {
    const { a, f, genesisState, journalA } = await fixture();
    const state = harness();
    state.journals.set(a.hash, journalA);
    await inNode(
      Effect.gen(function* () {
        state.replayEnds = Effect.succeed({
          kind: "event_unknown",
          detail: "deposit 00 is not known at the view: injected",
        } as const);
        const held = yield* processLandedQueue(
          ports(state),
          queueOf(genesisState, [a, f]),
        );
        expect(held?.reason).toBe(LANDED_BLOCK_EVENT_UNKNOWN);
        expect(localFaultSubject(held!)).toBe(f.hash);
      }),
    );
  }, 120_000);

  it("holds a block whose admitted forced order the facts cannot read back as a local fault on it", async () => {
    const { a, f, genesisState, journalA } = await fixture();
    const state = harness();
    state.journals.set(a.hash, journalA);
    await inNode(
      Effect.gen(function* () {
        state.replayEnds = Effect.succeed({
          kind: "forced_order_pending",
          detail: "forced order 00 cannot be read back: injected",
        } as const);
        const held = yield* processLandedQueue(
          ports(state),
          queueOf(genesisState, [a, f]),
        );
        expect(held?.reason).toBe(LANDED_BLOCK_FORCED_ORDER_PENDING);
        expect(localFaultSubject(held!)).toBe(f.hash);
      }),
    );
  }, 120_000);
});
