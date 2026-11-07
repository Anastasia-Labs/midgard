/**
 * The follow loop's R3 against the deployment's recorded origin: a store
 * whose origin is after it is flagged at the tip, the loop keeps following,
 * and the protocol-init spend being seen does not clear it.
 */
import type { ChainSyncEvent } from "@al-ft/l1-node-transport";
import { describe, expect, it } from "vitest";

import {
  type FactStore,
  type FollowStatus,
  openSqliteFactStore,
  type OriginConfig,
} from "../src/index.js";
import {
  buildForkSteps,
  forkCorpus,
  SIM_ORIGIN,
  simStoreOptions,
} from "../src/testing/index.js";
import {
  appliedAll,
  follow,
  script,
  simOrigin,
} from "./support/follow-loop.js";
import { FIXTURE_PROJECTION, SIM_K } from "./support/fork-sim.js";

const corpus = forkCorpus(SIM_K);
/** A short scenario from the fork corpus. */
const short: readonly ChainSyncEvent[] = buildForkSteps(corpus[0]!.scenario, [
  FIXTURE_PROJECTION,
]).steps.map((step) => step.event);

const openStore = (): FactStore =>
  openSqliteFactStore({
    ...simStoreOptions([FIXTURE_PROJECTION], SIM_K, "sqlite"),
    path: ":memory:",
  });

const reasons = (status: FollowStatus): string[] =>
  status.readiness.map((r) => r.reason);

/** Follows the whole scenario on a fresh store, from the simulated origin. */
const followAll = async (
  origin: OriginConfig | undefined,
  read?: (store: FactStore) => Promise<unknown>,
) => {
  const store = openStore();
  try {
    const s = script(short);
    const { final } = await follow({
      store,
      script: s,
      ...(origin === undefined ? {} : { origin }),
      until: appliedAll(s),
    });
    return { final, read: read === undefined ? undefined : await read(store) };
  } finally {
    await store.close();
  }
};

describe("followChain: R3 against the deployment's recorded origin", () => {
  it("F-a: reports R3 for a store origin after the recorded origin, keeps following, and a seen init spend does not clear it", async () => {
    // An outref a valid tx in the scenario spends stands for hubOracleOneShot.
    const { read } = await followAll(undefined, (store) =>
      store.transaction("read", async (tx) =>
        tx.query(
          `SELECT j.value AS input FROM l1_txs t, json_each(t.inputs) j
            WHERE t.is_valid = 1 ORDER BY t.block_slot LIMIT 1`,
        ),
      ),
    );
    const encoded = Buffer.from(
      String((read as { input: unknown }[])[0]!.input),
      "hex",
    );
    const oneShot = {
      txHash: Buffer.from(encoded.subarray(0, 32)),
      index: encoded.readUInt16BE(32),
    };
    const run = async (recordedOrigin?: OriginConfig["recordedOrigin"]) =>
      (
        await followAll({
          ...simOrigin(oneShot),
          ...(recordedOrigin === undefined ? {} : { recordedOrigin }),
        })
      ).final;

    const unflagged = await run();
    expect(unflagged.protocolInit).toBe("seen");
    expect(unflagged.interventions).toEqual([]);

    const flagged = await run({
      slot: SIM_ORIGIN.point.slot - 1,
      hash: Buffer.alloc(32, 0x9f),
    });
    expect(flagged.state).toBe("stopped");
    expect(flagged.events).toBe(short.length);
    expect(flagged.protocolInit).toBe("pending");
    expect(flagged.interventions.map((i) => i.reason)).toEqual([
      "origin_after_protocol_init",
    ]);
    expect(reasons(flagged)).toEqual(["origin_after_protocol_init"]);

    const accepted = await run(SIM_ORIGIN.point);
    expect(accepted.protocolInit).toBe("seen");
    expect(accepted.interventions).toEqual([]);
  });
});
