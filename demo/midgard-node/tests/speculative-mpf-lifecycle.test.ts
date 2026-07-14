import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

import {
  parkSpeculativeMpfsForConfirmationWait,
  resumeSpeculativeMpfsForSubmission,
} from "@/workers/commit-block-header.js";
import {
  MidgardMpf,
  MpfError,
  type ParkedEventFlatOverlayV2,
  type ParkedMpfOverlayV1,
} from "@/workers/utils/mpf.js";

const artifact = (trieName: string): ParkedMpfOverlayV1 => ({
  schemaVersion: 1,
  trieName,
  baseRoot: new ArrayBuffer(32),
  candidateRoot: new ArrayBuffer(32),
  closureDigest: new ArrayBuffer(32),
  nodeCount: 0,
  nodeHashes: new ArrayBuffer(0),
  nodeValues: new ArrayBuffer(0),
  nodeValueOffsets: new ArrayBuffer(0),
  encodedBytes: 0,
});

const eventFlatArtifact = (trieName: string): ParkedEventFlatOverlayV2 => ({
  schemaVersion: 2,
  trieName,
  baseRoot: new ArrayBuffer(32),
  candidateRoot: new ArrayBuffer(32),
  closureDigest: new ArrayBuffer(32),
  nodeCount: 0,
  shards: [
    {
      nodeCount: 0,
      nodeHashes: new ArrayBuffer(0),
      nodeValues: new ArrayBuffer(0),
      nodeValueOffsets: new ArrayBuffer(0),
      digest: new ArrayBuffer(32),
      encodedBytes: 0,
    },
  ],
  encodedBytes: 0,
});

const failure = (message: string) => new MpfError({ message, cause: message });

const fakeMpf = ({
  name,
  events,
  parkedArtifact,
  parkedEventFlatArtifact,
  eventFlat = false,
  parkFailure,
}: {
  readonly name: string;
  readonly events: string[];
  readonly parkedArtifact?: ParkedMpfOverlayV1;
  readonly parkedEventFlatArtifact?: ParkedEventFlatOverlayV2;
  readonly eventFlat?: boolean;
  readonly parkFailure?: MpfError;
}): MidgardMpf =>
  ({
    usesEventFlatEngine: () => eventFlat,
    parkEventFlatOverlayV2: () =>
      Effect.sync(() => events.push(`${name}:park-v2`)).pipe(
        Effect.zipRight(
          parkFailure === undefined
            ? Effect.succeed(parkedEventFlatArtifact ?? eventFlatArtifact(name))
            : Effect.fail(parkFailure),
        ),
      ),
    parkBlockOverlay: () =>
      Effect.sync(() => events.push(`${name}:park`)).pipe(
        Effect.zipRight(
          parkFailure === undefined
            ? Effect.succeed(parkedArtifact ?? artifact(name))
            : Effect.fail(parkFailure),
        ),
      ),
    discardBlockOverlayIfActive: () =>
      Effect.sync(() => events.push(`${name}:discard`)),
    close: () => Effect.sync(() => events.push(`${name}:close`)),
    flushBlockOverlay: vi.fn(() => Effect.void),
  }) as unknown as MidgardMpf;

describe("speculative MPF lifecycle", () => {
  it("parks both artifacts before discarding the parent and closing owners", async () => {
    const events: string[] = [];
    const ledgerArtifact = artifact("ledger");
    const transactionsArtifact = artifact("speculative-transactions");
    const ledgerFork = fakeMpf({
      name: "ledger-fork",
      events,
      parkedArtifact: ledgerArtifact,
    });
    const transactionsScratch = fakeMpf({
      name: "transactions-scratch",
      events,
      parkedArtifact: transactionsArtifact,
    });
    const ledgerParent = fakeMpf({ name: "ledger-parent", events });

    const parked = await Effect.runPromise(
      parkSpeculativeMpfsForConfirmationWait({
        ledgerFork,
        transactionsScratch,
        ledgerParent,
        closeOwningParents: Effect.sync(() => events.push("owners:close")),
      }),
    );

    expect(parked).toEqual({
      engine: "overlay",
      ledger: ledgerArtifact,
      transactions: transactionsArtifact,
    });
    expect(events).toEqual([
      "ledger-fork:park",
      "transactions-scratch:park",
      "ledger-parent:discard",
      "ledger-fork:close",
      "transactions-scratch:close",
      "owners:close",
    ]);
  });

  it("routes event-flat ledger parking and resume without early promotion", async () => {
    const events: string[] = [];
    const ledgerArtifact = eventFlatArtifact("ledger");
    const transactionsArtifact = artifact("speculative-transactions");
    const ledgerFork = fakeMpf({
      name: "ledger",
      events,
      eventFlat: true,
      parkedEventFlatArtifact: ledgerArtifact,
    });
    const transactionsScratch = fakeMpf({
      name: "transactions",
      events,
      parkedArtifact: transactionsArtifact,
    });
    const ledgerParent = fakeMpf({ name: "parent", events });
    const parked = await Effect.runPromise(
      parkSpeculativeMpfsForConfirmationWait({
        ledgerFork,
        transactionsScratch,
        ledgerParent,
        closeOwningParents: Effect.sync(() => events.push("owners:close")),
      }),
    );
    expect(parked).toEqual({
      engine: "event_flat",
      ledger: ledgerArtifact,
      transactions: transactionsArtifact,
    });
    expect(events).toEqual([
      "ledger:park-v2",
      "transactions:park",
      "parent:discard",
      "ledger:close",
      "transactions:close",
      "owners:close",
    ]);

    const resumedLedger = fakeMpf({ name: "resumed-ledger", events });
    const resumedTransactions = fakeMpf({
      name: "resumed-transactions",
      events,
    });
    const resumeV2 = vi.fn(() => Effect.succeed(resumedLedger));
    const resumeV1 = vi.fn(() => Effect.succeed(resumedTransactions));
    const resumed = await Effect.runPromise(
      resumeSpeculativeMpfsForSubmission({
        artifacts: parked,
        ledgerMpfPath: "/tmp/injected-event-flat",
        needsLocalFinalizationTransactionsMpf: false,
        resumeParkedOverlay: resumeV1,
        resumeParkedEventFlatOverlay: resumeV2,
        openLocalFinalizationTransactionsMpf: () =>
          Effect.fail(failure("must not open local finalization")),
      }),
    );
    expect(resumed.ledgerMpf).toBe(resumedLedger);
    expect(resumed.transactionsMpf).toBe(resumedTransactions);
    expect(resumeV2).toHaveBeenCalledOnce();
    expect(resumeV1).toHaveBeenCalledOnce();
    expect(resumedLedger.flushBlockOverlay).not.toHaveBeenCalled();
  });

  it("does not resume transactions after an event-flat ledger resume failure", async () => {
    const resumeV2 = vi.fn(() =>
      Effect.fail(failure("event-flat resume failed")),
    );
    const resumeV1 = vi.fn(() =>
      Effect.succeed(fakeMpf({ name: "unexpected-transactions", events: [] })),
    );
    const result = await Effect.runPromise(
      Effect.either(
        resumeSpeculativeMpfsForSubmission({
          artifacts: {
            engine: "event_flat",
            ledger: eventFlatArtifact("ledger"),
            transactions: artifact("speculative-transactions"),
          },
          ledgerMpfPath: "/tmp/injected-event-flat-failure",
          needsLocalFinalizationTransactionsMpf: false,
          resumeParkedOverlay: resumeV1,
          resumeParkedEventFlatOverlay: resumeV2,
          openLocalFinalizationTransactionsMpf: () =>
            Effect.fail(failure("must not open local finalization")),
        }),
      ),
    );
    expect(result._tag).toBe("Left");
    expect(resumeV2).toHaveBeenCalledOnce();
    expect(resumeV1).not.toHaveBeenCalled();
  });

  it("closes every handle and exposes no artifact when the second park fails", async () => {
    const events: string[] = [];
    const ledgerFork = fakeMpf({ name: "ledger-fork", events });
    const transactionsScratch = fakeMpf({
      name: "transactions-scratch",
      events,
      parkFailure: failure("second park failed"),
    });
    const ledgerParent = fakeMpf({ name: "ledger-parent", events });

    const parked = await Effect.runPromise(
      Effect.either(
        parkSpeculativeMpfsForConfirmationWait({
          ledgerFork,
          transactionsScratch,
          ledgerParent,
          closeOwningParents: Effect.sync(() => events.push("owners:close")),
        }),
      ),
    );

    expect(parked._tag).toBe("Left");
    expect(events).toEqual([
      "ledger-fork:park",
      "transactions-scratch:park",
      "ledger-fork:discard",
      "transactions-scratch:discard",
      "ledger-parent:discard",
      "ledger-fork:close",
      "transactions-scratch:close",
      "owners:close",
    ]);
    expect(ledgerFork.flushBlockOverlay).not.toHaveBeenCalled();
    expect(transactionsScratch.flushBlockOverlay).not.toHaveBeenCalled();
  });

  it("closes the first resumed handle when the second resume fails", async () => {
    const events: string[] = [];
    const resumedLedger = fakeMpf({ name: "resumed-ledger", events });
    const resume = vi.fn(
      (trieName: string): Effect.Effect<MidgardMpf, MpfError> =>
        trieName === "ledger"
          ? Effect.succeed(resumedLedger)
          : Effect.fail(failure("second resume failed")),
    );
    const openLocalFinalizationTransactionsMpf = vi.fn(() =>
      Effect.succeed(fakeMpf({ name: "local-transactions", events })),
    );

    const result = await Effect.runPromise(
      Effect.either(
        resumeSpeculativeMpfsForSubmission({
          artifacts: {
            engine: "overlay",
            ledger: artifact("ledger"),
            transactions: artifact("speculative-transactions"),
          },
          ledgerMpfPath: "/tmp/not-opened-by-injected-resume",
          needsLocalFinalizationTransactionsMpf: true,
          resumeParkedOverlay: resume,
          openLocalFinalizationTransactionsMpf,
        }),
      ),
    );

    expect(result._tag).toBe("Left");
    expect(resume).toHaveBeenCalledTimes(2);
    expect(openLocalFinalizationTransactionsMpf).not.toHaveBeenCalled();
    expect(events).toEqual(["resumed-ledger:close"]);
    expect(resumedLedger.flushBlockOverlay).not.toHaveBeenCalled();
  });

  it("releases real parent Level locks after parking a fork", async () => {
    const directory = await mkdtemp(
      join(tmpdir(), "midgard-speculative-mpf-lifecycle-"),
    );
    const ledgerPath = join(directory, "ledger");
    const transactionsPath = join(directory, "transactions");
    try {
      const ledgerParent = await Effect.runPromise(
        MidgardMpf.create("ledger", ledgerPath, { engine: "overlay" }),
      );
      const transactionsParent = await Effect.runPromise(
        MidgardMpf.create("transactions", transactionsPath, {
          engine: "overlay",
        }),
      );
      await Effect.runPromise(
        ledgerParent.applyBatch([
          {
            type: "insert",
            key: Buffer.alloc(32, 0x11),
            value: Buffer.from("durable-base"),
          },
        ]),
      );
      await Effect.runPromise(ledgerParent.beginBlockOverlay());
      const ledgerFork = await Effect.runPromise(
        ledgerParent.forkBlockOverlay(),
      );
      await Effect.runPromise(
        ledgerFork.applyBatch([
          {
            type: "insert",
            key: Buffer.alloc(32, 0x22),
            value: Buffer.from("speculative-delta"),
          },
        ]),
      );
      const transactionsScratch = await Effect.runPromise(
        MidgardMpf.createScratch("speculative-transactions", {
          engine: "overlay",
        }),
      );
      await Effect.runPromise(transactionsScratch.beginBlockOverlay());

      await Effect.runPromise(
        parkSpeculativeMpfsForConfirmationWait({
          ledgerFork,
          transactionsScratch,
          ledgerParent,
          closeOwningParents: Effect.all(
            [ledgerParent.close(), transactionsParent.close()],
            { discard: true },
          ).pipe(Effect.catchAll(() => Effect.void)),
        }),
      );

      const reopenedTransactions = await Effect.runPromise(
        MidgardMpf.create("transactions-reopened", transactionsPath, {
          engine: "overlay",
        }),
      );
      await Effect.runPromise(reopenedTransactions.close());
      const reopenedLedger = await Effect.runPromise(
        MidgardMpf.create("ledger-reopened", ledgerPath, {
          engine: "overlay",
        }),
      );
      await Effect.runPromise(reopenedLedger.close());
    } finally {
      await rm(directory, { recursive: true, force: true });
    }
  });
});
