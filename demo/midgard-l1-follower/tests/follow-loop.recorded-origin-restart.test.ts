/**
 * The follow loop's R3 against the deployment's recorded origin across a
 * restart: a loop that starts again on a store whose origin is after the
 * recorded one reports R3 from its first status, so it never reads caught
 * up (at the tip with no intervention) without it, while a store at the
 * recorded origin does read caught up once the protocol-init spend is seen.
 */
import { afterAll, describe, expect, it } from "vitest";

import {
  type FactStore,
  type FollowStatus,
  openPostgresFactStore,
  openSqliteFactStore,
  type Point,
} from "../src/index.js";
import {
  SIM_ORIGIN,
  simProtocolInitChain,
  simStoreOptions,
} from "../src/testing/index.js";
import { follow, script } from "./support/follow-loop.js";
import { SIM_K } from "./support/fork-sim.js";
import { testDatabases } from "./support/postgres.js";

const databases = testDatabases();
afterAll(async () => {
  await databases.dropAll();
});

const STORES: readonly (readonly [string, () => Promise<FactStore>])[] = [
  [
    "sqlite",
    () =>
      Promise.resolve(
        openSqliteFactStore({
          ...simStoreOptions([], SIM_K, "sqlite"),
          path: ":memory:",
        }),
      ),
  ],
  [
    "postgres",
    async () =>
      openPostgresFactStore({
        ...simStoreOptions([], SIM_K, "postgres"),
        connection: {
          connectionString: (await databases.create()).url,
          maxConnections: 4,
        },
      }),
  ],
];

const R3 = "origin_after_protocol_init";
const holdsR3 = (status: FollowStatus): boolean =>
  status.interventions.some((i) => i.reason === R3);
const caughtUp = (status: FollowStatus): boolean =>
  status.atTip && status.interventions.length === 0;

/**
 * Follows the chain one block short of the tip (a catch-up the process
 * stops in), then starts a fresh loop on the same store, which reaches the
 * tip; returns every status the second loop reported.
 */
const restartAgainst = async (
  open: () => Promise<FactStore>,
  recordedOrigin: Point,
): Promise<readonly FollowStatus[]> => {
  const { events, hubOracleOneShot } = simProtocolInitChain();
  const origin = {
    origin: SIM_ORIGIN.point,
    hubOracleOneShot,
    recordedOrigin,
  };
  const store = await open();
  try {
    const s = script(events, { limit: events.length - 1 });
    const first = await follow({
      store,
      script: s,
      origin,
      until: (status) => status.events >= events.length - 1,
    });
    expect(first.statuses.some((status) => status.atTip)).toBe(false);
    s.limit = undefined;
    const { statuses } = await follow({
      store,
      script: s,
      origin,
      until: (status) =>
        status.atTip && status.events >= 1 && status.protocolInit !== "unknown",
    });
    return statuses;
  } finally {
    await store.close();
  }
};

describe.each(STORES)(
  "followChain on %s: a restart against the recorded origin",
  (_name, open) => {
    it("F-d: on a store whose origin is after the recorded origin, the first status already reads R3 and no status reads caught up", async () => {
      const statuses = await restartAgainst(open, {
        slot: SIM_ORIGIN.point.slot - 1,
        hash: Buffer.alloc(32, 0x9f),
      });
      expect(statuses[0]!.interventions.map((i) => i.reason)).toEqual([R3]);
      expect(statuses[0]!.readiness.map((r) => r.reason)).toContain(R3);
      expect(statuses.filter((status) => !holdsR3(status))).toEqual([]);
      expect(statuses.some((status) => status.atTip)).toBe(true);
      expect(statuses.some(caughtUp)).toBe(false);
    });

    it("F-d: on a store at the recorded origin, the restarted loop reads caught up once the protocol-init spend is seen", async () => {
      const statuses = await restartAgainst(open, SIM_ORIGIN.point);
      expect(statuses.some(holdsR3)).toBe(false);
      expect(statuses.at(-1)).toMatchObject({
        atTip: true,
        protocolInit: "seen",
        interventions: [],
      });
    });
  },
);
