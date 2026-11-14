/**
 * Stake-credential registration reads (`src/intents/certificates.ts`):
 *
 * - a body's certificates (key 4, plain or under set tag 258) name the
 *   credentials they register or deregister, in body order; other
 *   certificates change nothing;
 * - over the store, on SQLite and Postgres (each dialect's body-contains
 *   prefilter): the latest certificate on a credential among the canonical
 *   phase-2-valid landed transactions decides, a phase-2-failed
 *   transaction's certificates do not count, and a rewind drops the
 *   transactions it removes.
 */
import { afterAll, afterEach, describe, expect, it } from "vitest";

import {
  applyChainSyncEvent,
  credentialsRegisteredBy,
  decodeTransaction,
  type FactStore,
  landedStakeRegistrationsIn,
  openPostgresFactStore,
  openSqliteFactStore,
  type OutRef,
  stakeRegistrationChanges,
} from "../src/index.js";
import {
  cbor as c,
  encodeSimTx,
  SIM_ORIGIN,
  SimChain,
  simStoreOptions,
  type SimTx,
  simUniverse,
} from "../src/testing/index.js";
import { testDatabases } from "./support/postgres.js";

const universe = simUniverse();
const databases = testDatabases();
const opened: FactStore[] = [];
afterEach(async () => {
  await Promise.all(opened.splice(0).map((store) => store.close()));
});
afterAll(async () => {
  await databases.dropAll();
});

const keyCredential = (byte: number) =>
  c.array(c.uint(0), c.bytes(Buffer.alloc(28, byte)));
const scriptCredential = (byte: number) =>
  c.array(c.uint(1), c.bytes(Buffer.alloc(28, byte)));
const name = (kind: 0 | 1, byte: number) =>
  `${kind.toString()}:${Buffer.alloc(28, byte).toString("hex")}`;

const DEPOSIT = c.uint(2_000_000);
const POOL = c.bytes(Buffer.alloc(28, 0x99));
const DREP = c.array(c.uint(2));

/** A body map with inputs (key 0, empty) and `certificates` (key 4). */
const body = (certificates: readonly Buffer[], tagged = false): Buffer =>
  c.map(
    [c.uint(0), c.array()],
    [
      c.uint(4),
      tagged ? c.tag(258, c.array(...certificates)) : c.array(...certificates),
    ],
  );

describe("stake registration changes of a body", () => {
  it("names each registering and deregistering certificate's credential, in order, under either array form", () => {
    const certificates = [
      c.array(c.uint(0), keyCredential(0x01)),
      c.array(c.uint(7), scriptCredential(0x02), DEPOSIT),
      c.array(c.uint(11), scriptCredential(0x03), POOL, DEPOSIT),
      c.array(c.uint(12), scriptCredential(0x04), DREP, DEPOSIT),
      c.array(c.uint(13), scriptCredential(0x05), POOL, DREP, DEPOSIT),
      c.array(c.uint(2), scriptCredential(0x06), POOL),
      c.array(c.uint(1), keyCredential(0x01)),
      c.array(c.uint(8), scriptCredential(0x02), DEPOSIT),
    ];
    const expected = [
      { credential: name(0, 0x01), registered: true },
      { credential: name(1, 0x02), registered: true },
      { credential: name(1, 0x03), registered: true },
      { credential: name(1, 0x04), registered: true },
      { credential: name(1, 0x05), registered: true },
      { credential: name(0, 0x01), registered: false },
      { credential: name(1, 0x02), registered: false },
    ];
    expect(stakeRegistrationChanges(body(certificates))).toEqual(expected);
    expect(stakeRegistrationChanges(body(certificates, true))).toEqual(
      expected,
    );
    expect(stakeRegistrationChanges(c.map([c.uint(0), c.array()]))).toEqual([]);
  });

  it("a whole transaction's registered credentials", () => {
    const tx = encodeSimTx({
      inputs: [{ txHash: Buffer.alloc(32, 1), index: 0 }],
      outputs: [],
      certificates: [
        c.array(c.uint(7), scriptCredential(0x02), DEPOSIT),
        c.array(c.uint(8), scriptCredential(0x03), DEPOSIT),
      ],
      nonce: 2,
    });
    expect(credentialsRegisteredBy(tx)).toEqual([name(1, 0x02)]);
  });
});

describe.each(["sqlite", "postgres"] as const)(
  "landed stake registrations (%s)",
  (dialect) => {
    const open = async () => {
      const options = {
        ...simStoreOptions([], 6, dialect),
        trackedSet: universe.tracked,
      };
      const store =
        dialect === "sqlite"
          ? openSqliteFactStore({ ...options, path: ":memory:" })
          : openPostgresFactStore({
              ...options,
              connection: { connectionString: (await databases.create()).url },
            });
      opened.push(store);
      expect(await store.start()).toMatchObject({ kind: "ready" });
      expect(await store.initialize(SIM_ORIGIN)).toMatchObject({
        kind: "initialized",
      });
      const chain = new SimChain(universe, SIM_ORIGIN, universe.tracked);
      const forward = async (txs: readonly SimTx[]) => {
        const step = await applyChainSyncEvent(store, chain.forward(txs).event);
        expect(step.result.kind).toBe("applied");
      };
      const backward = async (depth: number) => {
        const step = await applyChainSyncEvent(store, chain.backward(depth));
        expect(step.result.kind).toBe("rewound");
      };
      /** A tracked funding output, landed in its own block. */
      const fund = async (): Promise<OutRef> => {
        const tx: SimTx = {
          inputs: [chain.outsideInput()],
          outputs: [{ address: universe.trackedAddress, lovelace: 9_000_000n }],
          nonce: chain.nonce(),
        };
        await forward([tx]);
        return { txHash: decodeTransaction(encodeSimTx(tx)).hash, index: 0 };
      };
      /** A tracked transaction carrying `certificates`. */
      const certifying = async (
        certificates: readonly Buffer[],
        extra: Partial<SimTx> = {},
      ): Promise<SimTx> => ({
        inputs: [await fund()],
        outputs: [{ address: universe.trackedAddress, lovelace: 8_000_000n }],
        certificates,
        nonce: chain.nonce(),
        ...extra,
      });
      const landed = (credentials: readonly string[]) =>
        store.transaction("read", (tx) =>
          landedStakeRegistrationsIn(tx, store.dialect, credentials),
        );
      return { forward, backward, fund, certifying, landed };
    };

    const A = name(1, 0x21);
    const B = name(0, 0x22);

    it("the latest canonical valid certificate decides; a failed transaction's and a rewound one's do not count", async () => {
      const s = await open();
      expect(await s.landed([A, B])).toEqual(new Map());
      await s.forward([
        await s.certifying([
          c.array(c.uint(7), scriptCredential(0x21), DEPOSIT),
          c.array(c.uint(0), keyCredential(0x22)),
        ]),
      ]);
      expect(await s.landed([A, B])).toEqual(
        new Map([
          [A, true],
          [B, true],
        ]),
      );
      // A later block deregisters A; a credential of the other kind with
      // the same hash is a different credential.
      await s.forward([
        await s.certifying([
          c.array(c.uint(8), scriptCredential(0x21), DEPOSIT),
          c.array(c.uint(1), keyCredential(0x21)),
        ]),
      ]);
      expect(await s.landed([A])).toEqual(new Map([[A, false]]));
      // A phase-2-failed registration does not count.
      const collateral = await s.fund();
      await s.forward([
        await s.certifying(
          [c.array(c.uint(7), scriptCredential(0x21), DEPOSIT)],
          { collaterals: [collateral], isValid: false },
        ),
      ]);
      expect(await s.landed([A])).toEqual(new Map([[A, false]]));
      // A valid re-registration, then a rewind that removes it.
      await s.forward([
        await s.certifying([
          c.array(c.uint(7), scriptCredential(0x21), DEPOSIT),
        ]),
      ]);
      expect(await s.landed([A])).toEqual(new Map([[A, true]]));
      await s.backward(2);
      expect(await s.landed([A])).toEqual(new Map([[A, false]]));
    });
  },
);
