import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  resolveStateQueueMutationLeaseRequest,
  type StateQueueMutationLeaseEndpointStore,
} from "@/commands/listen-router.js";
import { StateQueueMutationLeasesDB } from "@/database/index.js";

type FakeLeaseEntry = StateQueueMutationLeasesDB.Entry;

const makeLeaseEntry = ({
  token,
  holder,
  status,
  lastError = null,
  expiresAt,
}: {
  readonly token: string;
  readonly holder: string;
  readonly status: StateQueueMutationLeasesDB.Status;
  readonly lastError?: string | null;
  readonly expiresAt?: Date;
}): FakeLeaseEntry => ({
  [StateQueueMutationLeasesDB.Columns.TOKEN]: token,
  [StateQueueMutationLeasesDB.Columns.SCOPE]: "state_queue",
  [StateQueueMutationLeasesDB.Columns.HOLDER]: holder,
  [StateQueueMutationLeasesDB.Columns.STATUS]: status,
  [StateQueueMutationLeasesDB.Columns.ACQUIRED_AT]: new Date(
    "2026-01-01T00:00:00.000Z",
  ),
  [StateQueueMutationLeasesDB.Columns.EXPIRES_AT]: new Date(
    expiresAt ?? "2026-01-01T00:10:00.000Z",
  ),
  [StateQueueMutationLeasesDB.Columns.RELEASED_AT]:
    status === StateQueueMutationLeasesDB.Status.Active
      ? null
      : new Date("2026-01-01T00:01:00.000Z"),
  [StateQueueMutationLeasesDB.Columns.LAST_ERROR]: lastError,
});

const createFakeLeaseStore = () => {
  const calls: Array<{
    readonly action: string;
    readonly token?: string;
    readonly holder?: string;
    readonly ttlMs?: number;
    readonly error?: string;
  }> = [];
  let activeLease: FakeLeaseEntry | undefined;
  const allLeases: FakeLeaseEntry[] = [];
  const store: StateQueueMutationLeaseEndpointStore<never> = {
    inspect: ({ recentLimit = 10 } = {}) =>
      Effect.sync(() => ({
        dbNow: new Date("2026-01-01T00:00:30.000Z"),
        activeLease,
        recentLeases: allLeases.slice(0, recentLimit),
        pendingFinalizations:
          activeLease === undefined
            ? []
            : [
                {
                  headerHash: "11".repeat(28),
                  submittedTxHash: null,
                  status:
                    "pending_submission" as StateQueueMutationLeasesDB.LeaseInspection["pendingFinalizations"][number]["status"],
                  createdAt: new Date("2026-01-01T00:00:05.000Z"),
                  updatedAt: new Date("2026-01-01T00:00:10.000Z"),
                },
              ],
      })),
    tryAcquire: ({ holder, ttlMs }) =>
      Effect.sync(() => {
        calls.push({ action: "acquire", holder, ttlMs });
        if (activeLease !== undefined) {
          return { _tag: "Busy", activeLease };
        }
        const token = `${holder}:fake-${(allLeases.length + 1).toString()}`;
        activeLease = makeLeaseEntry({
          token,
          holder,
          status: StateQueueMutationLeasesDB.Status.Active,
        });
        allLeases.push(activeLease);
        return { _tag: "Acquired", token };
      }),
    renew: ({ token, ttlMs }) =>
      Effect.sync(() => {
        calls.push({ action: "renew", token, ttlMs });
        if (activeLease?.[StateQueueMutationLeasesDB.Columns.TOKEN] !== token) {
          throw new Error(`inactive token ${token}`);
        }
      }),
    release: (token) =>
      Effect.sync(() => {
        calls.push({ action: "release", token });
        if (activeLease?.[StateQueueMutationLeasesDB.Columns.TOKEN] === token) {
          activeLease = undefined;
        }
      }),
    markFailed: (token, error) =>
      Effect.sync(() => {
        calls.push({ action: "fail", token, error });
        const lease = allLeases.find(
          (entry) => entry[StateQueueMutationLeasesDB.Columns.TOKEN] === token,
        );
        if (lease !== undefined) {
          const failed = makeLeaseEntry({
            token,
            holder: lease[StateQueueMutationLeasesDB.Columns.HOLDER],
            status: StateQueueMutationLeasesDB.Status.Failed,
            lastError: error,
          });
          allLeases[allLeases.indexOf(lease)] = failed;
          if (
            activeLease?.[StateQueueMutationLeasesDB.Columns.TOKEN] === token
          ) {
            activeLease = undefined;
          }
        }
      }),
  };
  return {
    store,
    calls,
    activeLease: () => activeLease,
    allLeases: () => allLeases,
  };
};

const resolveRequest = (
  body: unknown,
  store: StateQueueMutationLeaseEndpointStore<never>,
) => Effect.runPromise(resolveStateQueueMutationLeaseRequest(body, store));

describe("POST /stateQueueMutationLease request handling", () => {
  it("acquires, reports busy, releases, and reacquires leases", async () => {
    const fake = createFakeLeaseStore();

    const first = await resolveRequest(
      { action: "acquire", holder: "fault-proof-cli", ttlMs: 30_000 },
      fake.store,
    );
    expect(first.statusCode).toBe(200);
    expect(first.body).toEqual({
      status: "acquired",
      token: "fault-proof-cli:fake-1",
    });

    const second = await resolveRequest(
      { action: "acquire", holder: "commit-worker" },
      fake.store,
    );
    expect(second.statusCode).toBe(409);
    expect(second.body).toMatchObject({
      status: "busy",
      activeLease: {
        holder: "fault-proof-cli",
        status: StateQueueMutationLeasesDB.Status.Active,
        releasedAt: null,
        lastError: null,
      },
    });

    const released = await resolveRequest(
      { action: "release", token: "fault-proof-cli:fake-1" },
      fake.store,
    );
    expect(released).toEqual({
      statusCode: 200,
      body: { status: "released" },
    });
    expect(fake.activeLease()).toBeUndefined();

    const third = await resolveRequest(
      { action: "acquire", holder: "merge-worker" },
      fake.store,
    );
    expect(third).toEqual({
      statusCode: 200,
      body: { status: "acquired", token: "merge-worker:fake-2" },
    });
  });

  it("renews active leases and marks failed leases with errors", async () => {
    const fake = createFakeLeaseStore();

    await resolveRequest(
      { action: "acquire", holder: "fault-proof-cli", ttlMs: 30_000 },
      fake.store,
    );
    const renewed = await resolveRequest(
      { action: "renew", token: "fault-proof-cli:fake-1", ttlMs: 60_000 },
      fake.store,
    );
    expect(renewed).toEqual({
      statusCode: 200,
      body: { status: "renewed" },
    });
    expect(fake.activeLease()).toBeDefined();

    const failed = await resolveRequest(
      {
        action: "fail",
        token: "fault-proof-cli:fake-1",
        error: "fault-proof removal failed",
      },
      fake.store,
    );
    expect(failed).toEqual({
      statusCode: 200,
      body: { status: "failed" },
    });
    expect(fake.activeLease()).toBeUndefined();
    expect(fake.allLeases()[0]).toMatchObject({
      status: StateQueueMutationLeasesDB.Status.Failed,
      last_error: "fault-proof removal failed",
    });
    expect(fake.calls).toEqual([
      { action: "acquire", holder: "fault-proof-cli", ttlMs: 30_000 },
      { action: "renew", token: "fault-proof-cli:fake-1", ttlMs: 60_000 },
      {
        action: "fail",
        token: "fault-proof-cli:fake-1",
        error: "fault-proof removal failed",
      },
    ]);
  });

  it("inspects active leases without mutating them", async () => {
    const fake = createFakeLeaseStore();

    await resolveRequest(
      { action: "acquire", holder: "commit-worker", ttlMs: 30_000 },
      fake.store,
    );
    const inspected = await resolveRequest(
      { action: "inspect", recentLimit: 1 },
      fake.store,
    );

    expect(inspected.statusCode).toBe(200);
    expect(inspected.body).toMatchObject({
      status: "busy",
      dbNow: "2026-01-01T00:00:30.000Z",
      activeLease: {
        token: "commit-worker:fake-1",
        holder: "commit-worker",
        remainingMs: 570_000,
        expired: false,
        blockedUntil: "2026-01-01T00:10:00.000Z",
      },
      pendingFinalizations: [
        {
          headerHash: "11".repeat(28),
          status: "pending_submission",
          submittedTxHash: null,
        },
      ],
      recentLeases: [
        {
          token: "commit-worker:fake-1",
          holder: "commit-worker",
        },
      ],
    });
    expect(fake.activeLease()).toBeDefined();
  });

  it("rejects invalid request bodies with 400 responses", async () => {
    const fake = createFakeLeaseStore();

    for (const testCase of [
      {
        body: null,
        error: 'Request body must include an "action" field.',
      },
      {
        body: {},
        error: 'Request body must include an "action" field.',
      },
      {
        body: { action: "acquire", ttlMs: 0 },
        error: "ttlMs must be a positive safe integer.",
      },
      {
        body: { action: "renew" },
        error: '"token" must be a non-empty string.',
      },
      {
        body: { action: "release", token: "" },
        error: '"token" must be a non-empty string.',
      },
      {
        body: { action: "fail", token: "   " },
        error: '"token" must be a non-empty string.',
      },
      {
        body: { action: "renew", token: "missing-token", ttlMs: -1 },
        error: "ttlMs must be a positive safe integer.",
      },
      {
        body: { action: "unknown", token: "lease-token" },
        error: "Unsupported action: unknown",
      },
    ]) {
      await expect(resolveRequest(testCase.body, fake.store)).resolves.toEqual({
        statusCode: 400,
        body: { error: testCase.error },
      });
    }
  });
});
