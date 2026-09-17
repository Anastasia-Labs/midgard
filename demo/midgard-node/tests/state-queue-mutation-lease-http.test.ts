import { afterEach, describe, expect, it } from "vitest";

import { StateQueueMutationLeasesDB } from "../src/database/index.js";
import {
  startStateQueueMutationLeaseServer,
  type StateQueueMutationLeaseServer,
} from "./helpers/state-queue-mutation-lease-server.js";

describe("state-queue mutation lease HTTP with Postgres", () => {
  let server: StateQueueMutationLeaseServer | undefined;
  afterEach(async () => {
    await server?.close();
    server = undefined;
  });

  it("authenticates, excludes a competing owner, renews and releases the persisted lease", async () => {
    server = await startStateQueueMutationLeaseServer();
    const endpoint = `${server.url}/stateQueueMutationLease`;
    const post = (body: unknown, adminApiKey = server!.adminApiKey) =>
      fetch(endpoint, {
        method: "POST",
        headers: {
          "content-type": "application/json",
          "x-midgard-admin-key": adminApiKey,
        },
        body: JSON.stringify(body),
      });
    const unauthorized = await post({ action: "acquire" }, "incorrect");
    expect(unauthorized.status).toBe(401);
    expect((await server.inspect()).recentLeases).toHaveLength(0);

    const acquired = await post({
      action: "acquire",
      holder: "watcher",
      ttlMs: 30_000,
    });
    expect(acquired.status).toBe(200);
    const first: { status: string; token: string } = await acquired.json();
    expect(first.status).toBe("acquired");
    expect((await server.inspect()).activeLease).toMatchObject({
      token: first.token,
      holder: "watcher",
      status: StateQueueMutationLeasesDB.Status.Active,
    });

    const competing = await post({
      action: "acquire",
      holder: "commit-worker",
    });
    expect(competing.status).toBe(409);
    expect(await competing.json()).toMatchObject({
      status: "busy",
      activeLease: { token: first.token, holder: "watcher" },
    });
    const renewed = await post({
      action: "renew",
      token: first.token,
      ttlMs: 60_000,
    });
    expect(renewed.status).toBe(200);
    const inspection = await server.inspect();
    expect(
      inspection.activeLease!.expires_at.getTime() - inspection.dbNow.getTime(),
    ).toBeGreaterThan(50_000);

    const released = await post({ action: "release", token: first.token });
    expect(released.status).toBe(200);
    expect((await server.inspect()).activeLease).toBeUndefined();
    const next = await post({ action: "acquire", holder: "commit-worker" });
    expect(next.status).toBe(200);
    const second: { token: string } = await next.json();
    expect(second.token).not.toBe(first.token);
    const failed = await post({
      action: "fail",
      token: second.token,
      error: "test correction failure",
    });
    expect(failed.status).toBe(200);
    expect((await server.inspect()).activeLease).toBeUndefined();

    const read = await fetch(endpoint, {
      headers: { "x-midgard-admin-key": server.adminApiKey },
    });
    expect(read.status).toBe(200);
    expect(await read.json()).toMatchObject({
      status: "idle",
      recentLeases: expect.arrayContaining([
        expect.objectContaining({ token: first.token, status: "released" }),
        expect.objectContaining({ token: second.token, status: "failed" }),
      ]),
    });
  }, 60_000);
});
