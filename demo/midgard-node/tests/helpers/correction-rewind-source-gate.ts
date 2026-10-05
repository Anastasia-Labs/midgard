import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { Level } from "level";
import { expect } from "vitest";

import {
  expectOwedAndUnreincluded,
  failureText,
  nativeRoot,
  openRemovedTailOverRetainedBlock,
  type Scenario,
} from "../attestation-timeout-reinclusion-emulator.expect-rewound-and-recommitted.js";
import {
  closeLifecycle,
  openCorrectionRewindScenario,
  read,
  readSqlLedgerRoot,
} from "./correction-rewind-scenario.js";

export const assertClosedCorrectionGate = async (h: Scenario["h"]) => {
  expect((await Effect.runPromise(h.production.owner.frontier)).ready).toBe(
    false,
  );
  let entered = false;
  await expect(
    read(
      h.production.owner.runProducer(() => {
        entered = true;
        return Effect.succeed(undefined);
      }),
    ),
  ).rejects.toBeDefined();
  expect(entered).toBe(false);
  const state = await read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      return yield* sql<{
        state: string;
      }>`SELECT state FROM event_history_authority`;
    }),
  );
  expect(state).toHaveLength(1);
  expect(state[0]!.state).not.toBe("ready");
};

export const assertUnboundRemovedLocalRoot = async () => {
  const scenario = await openCorrectionRewindScenario({ blocks: 1 });
  const { h } = scenario;
  const root = await nativeRoot(h);
  const sqlRoot = await readSqlLedgerRoot();
  const marker = new Level<string, unknown>(
    h.production.nodeConfig.LEDGER_MPF_DB_PATH,
    { valueEncoding: "json" },
  );
  try {
    const failure = await failureText(
      h.restartRuntime({
        afterStop: async () => {
          await scenario.removeTail(scenario.headers[0]!, { observe: false });
          await read(
            Effect.gen(function* () {
              const sql = yield* SqlClient.SqlClient;
              const rows = yield* sql`UPDATE pending_block_finalizations
            SET base_tail_header_hash = ${Buffer.from("ff".repeat(28), "hex")}
            WHERE header_hash = ${Buffer.from(scenario.headers[0]!, "hex")}
            RETURNING header_hash`;
              expect(rows).toHaveLength(1);
            }),
          );
        },
      }),
    );
    expect(failure).toContain(
      "Foreign native replay does not end at the verified base",
    );
    await marker.open();
    expect(await marker.get("__root__")).toBe(root);
    expect(await readSqlLedgerRoot()).toEqual(sqlRoot);
    await assertClosedCorrectionGate(h);
  } finally {
    await marker.close();
    await closeLifecycle(h);
  }
};

export const assertUnretainedCorrectionRoot = async () => {
  const { scenario, removed } = await openRemovedTailOverRetainedBlock();
  const levelPath = scenario.h.production.nodeConfig.LEDGER_MPF_DB_PATH;
  const target = removed[0]!.base;
  const readMarker = async () => {
    const db = new Level<string, unknown>(levelPath, { valueEncoding: "json" });
    await db.open();
    try {
      return await db.get("__root__");
    } finally {
      await db.close();
    }
  };
  try {
    // Drop the base root's own record while no service holds the store.
    const failure = await failureText(
      scenario.h.restartRuntime({
        afterStop: async () => {
          const db = new Level<string, unknown>(levelPath, {
            valueEncoding: "json",
          });
          await db.open();
          try {
            expect(await db.get(target)).toBeDefined();
            await db.del(target);
          } finally {
            await db.close();
          }
        },
      }),
    );
    expect(failure).toContain(
      `Native MPF canonical recovery target root ${target} is not retained in full; refusing to restore`,
    );
    expect(await readMarker()).toBe(removed[0]!.expected);
    expect((await readSqlLedgerRoot()).root_hex).not.toBe(target);
    await expectOwedAndUnreincluded(removed);
  } finally {
    await closeLifecycle(scenario.h);
  }
};
