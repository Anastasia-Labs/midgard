import { SqlClient } from "@effect/sql";
import { Cause, Effect, Exit, Option } from "effect";
import { beforeEach, describe, expect, it } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import { SignedIntentReplacementIntegrityError } from "../src/services/canonical-journal-recovery.js";
import { sameBaseJournals } from "../src/services/history-expired-intent-release.base-spend.js";
import {
  displacement,
  type ObserverView,
} from "../src/services/history-expired-intent-release.displacement.js";
import {
  BASE_HEADER,
  bytes,
  hex,
  insertJournal,
  ROOT_HEADER,
  run,
  seed,
  signedCommit,
  TTL,
  UTXOS_ROOT,
} from "./helpers/history-expired-intent-release-before-ttl.js";
import {
  depthOf,
  journal,
  S_COMMIT,
  S_HEADER,
  W_COMMIT,
  W_HEADER,
  W_NODE_TX,
  wHoldsTheSlot,
} from "./helpers/history-expired-intent-release-displaced-sibling.js";

/**
 * Each guard of `displacement` (which locally finalized siblings an L1
 * rollback displaced from under the winner W, and so may be abandoned) and of
 * the sibling query it starts from, in both polarities with the exact reason.
 * Base D; W abandoned for replacement and holding D's slot; S locally
 * finalized on D, changing no ledger state.
 */

const S = S_HEADER.toString("hex");
const S_NODE_OUT = `${hex("displaced:s-node-tx")}#0`;
const T_HEADER = bytes("displaced:t-header", 28);
const T = T_HEADER.toString("hex");
const W_NODE = wHoldsTheSlot.nodes[2]!;

const none: ObserverView = { kind: "blocked", reason: "no observer state" };
const observed = (
  transitionKind: string,
  removed: string,
  admitted: boolean,
): ObserverView => {
  const transition = {
    transitionKind,
    removedHeaderHashes: [removed],
    transactionHash: hex(`${transitionKind}:${admitted.toString()}`),
  };
  return {
    kind: "observed",
    state: {
      admitted: admitted ? [transition] : [],
      pending: admitted ? [] : [transition],
    },
  } as unknown as ObserverView;
};

/** One `displacement` over S, read in one SQL session. */
const displace = (
  input: {
    observer?: ObserverView;
    queued?: boolean;
    depths?: Record<string, bigint>;
  } = {},
) =>
  run(
    Effect.gen(function* () {
      const winner = Option.getOrThrow(
        yield* Pending.retrieveByHeaderHash(W_HEADER),
      );
      const exit = yield* Effect.exit(
        displacement({
          blocking: [S],
          node: W_NODE,
          queued: input.queued ?? true,
          winner,
          base: BASE_HEADER.toString("hex"),
          baseRoot: UTXOS_ROOT,
          queue: wHoldsTheSlot,
          observer: input.observer ?? none,
          depth: depthOf(input.depths ?? { [W_NODE_TX]: 6n }),
          required: 3n,
        }),
      );
      if (Exit.isSuccess(exit))
        return typeof exit.value === "string"
          ? { wait: exit.value }
          : {
              displaced: exit.value.map((record) =>
                record[Pending.Columns.HEADER_HASH].toString("hex"),
              ),
            };
      const failure = Option.getOrUndefined(Cause.failureOption(exit.cause));
      expect(failure).toBeInstanceOf(SignedIntentReplacementIntegrityError);
      return { integrity: (failure as Error).message };
    }),
  );

/** T, built on S, at `status`, changing no ledger state. */
const child = (status: Pending.Status) =>
  insertJournal({
    header: T_HEADER,
    status,
    commit: signedCommit(S_NODE_OUT, TTL + 3),
    baseOut: S_NODE_OUT,
    baseHeader: S_HEADER,
    createdAt: new Date(3_500_000),
  }).pipe(
    Effect.zipRight(
      Effect.flatMap(
        SqlClient.SqlClient,
        (sql) => sql`UPDATE pending_block_finalizations
          SET block_end_time = block_start_time + INTERVAL '1 second',
            expected_utxos_root = base_utxos_root
          WHERE header_hash = ${T_HEADER}`,
      ),
    ),
  );

beforeEach(async () => {
  await run(seed);
  await run(
    Effect.gen(function* () {
      yield* journal(W_HEADER, Pending.Status.Abandoned, W_COMMIT, 2_000_000, {
        abandonment: "replacement",
      });
      yield* journal(S_HEADER, Pending.Status.Finalized, S_COMMIT, 3_000_000, {
        empty: true,
      });
    }),
  );
});

describe("displacement", () => {
  it("abandons S with no recorded removal (the baseline every guard departs from)", async () => {
    expect(await displace()).toEqual({ displaced: [S] });
  });

  it("waits on a descendant that is not locally finalized, and takes a locally finalized one after S", async () => {
    await run(child(Pending.Status.PendingSubmission));
    expect(await displace()).toEqual({
      wait: `block ${T} built over it has journal status ${Pending.Status.PendingSubmission}, not locally finalized`,
    });
    await run(
      Effect.flatMap(
        SqlClient.SqlClient,
        (sql) => sql`UPDATE pending_block_finalizations
          SET status = ${Pending.Status.Finalized} WHERE header_hash = ${T_HEADER}`,
      ),
    );
    expect(await displace()).toEqual({ displaced: [S, T] });
  });

  it("refuses a root-preserving descendant whose base root is not its parent's root", async () => {
    await run(child(Pending.Status.Finalized));
    await run(
      Effect.flatMap(
        SqlClient.SqlClient,
        (sql) => sql`UPDATE pending_block_finalizations
        SET base_utxos_root = ${"91".repeat(32)}, expected_utxos_root = ${"91".repeat(32)}
        WHERE header_hash = ${T_HEADER}`,
      ),
    );
    expect((await displace()).integrity).toContain(T);
  });

  it("is the integrity failure when an admitted merge folded S, and waits on a pending one", async () => {
    const admitted = await displace({ observer: observed("merge", S, true) });
    expect(admitted.integrity).toContain(
      `replaced block ${S} won its state-queue slot on the observed chain, but it was merged while block ${W_HEADER.toString("hex")} of this node holds the slot`,
    );
    expect(await displace({ observer: observed("merge", S, false) })).toEqual({
      wait: `pending state-queue merge ${hex("merge:false")} names block ${S}; it is not admitted yet`,
    });
    // A merge of another block is no evidence about S.
    expect(
      await displace({ observer: observed("merge", hex("other"), true) }),
    ).toEqual({ displaced: [S] });
  });

  it("leaves S to the correction path when a correction removed it, admitted or pending", async () => {
    expect(await displace({ observer: observed("timeout", S, true) })).toEqual({
      wait: `admitted state-queue correction ${hex("timeout:true")} removed block ${S}, which the correction path reconciles`,
    });
    expect(await displace({ observer: observed("timeout", S, false) })).toEqual(
      {
        wait: `pending state-queue correction ${hex("timeout:false")} removed block ${S}, which the correction path reconciles`,
      },
    );
  });

  it("reads an unqueued winner absent from the canonical history as not shown deep, and a queued one as deeper than all of it", async () => {
    expect(await displace({ queued: false, depths: {} })).toEqual({
      wait: "the commit holding the slot is not in the journaled canonical history, short of the confirmation depth 3",
    });
    expect(await displace({ queued: true, depths: {} })).toEqual({
      displaced: [S],
    });
    expect(
      await displace({ queued: false, depths: { [W_NODE_TX]: 3n } }),
    ).toEqual({ displaced: [S] });
  });
});

describe("the same-base journals of a root-tail base", () => {
  const ROOT_OUT_A = `${hex("root-out-a")}#0`;
  const ROOT_OUT_B = `${hex("root-out-b")}#0`;
  const rootJournal = (label: string, baseOut: string, at: number) =>
    insertJournal({
      header: bytes(label, 28),
      status: Pending.Status.Abandoned,
      commit: signedCommit(baseOut, TTL + at),
      baseOut,
      baseHeader: ROOT_HEADER,
      createdAt: new Date(5_000_000 + at),
    });
  const siblingsOf = (
    base: { outRef: string; headerHash: Buffer },
    self: Buffer,
  ) =>
    run(
      Effect.map(
        sameBaseJournals({ ...base, utxosRoot: UTXOS_ROOT }, [self]),
        (rows) => rows.map(({ header_hash }) => header_hash.toString("hex")),
      ),
    );

  it("match by output reference only: every root tail shares the root's header and may share a root", async () => {
    await run(rootJournal("root:a", ROOT_OUT_A, 1));
    await run(rootJournal("root:b", ROOT_OUT_B, 2));
    await run(rootJournal("root:a2", ROOT_OUT_A, 3));
    expect(
      await siblingsOf(
        { outRef: ROOT_OUT_A, headerHash: ROOT_HEADER },
        bytes("root:a", 28),
      ),
    ).toEqual([bytes("root:a2", 28).toString("hex")]);
  });

  it("match a non-root base by header and root as well, whatever its output reference", async () => {
    // S is on D's output; a journal on D under another output reference (a
    // re-created D node) is its sibling too.
    expect(
      await siblingsOf(
        { outRef: `${hex("another-d-out")}#0`, headerHash: BASE_HEADER },
        W_HEADER,
      ),
    ).toEqual([S]);
  });
});
