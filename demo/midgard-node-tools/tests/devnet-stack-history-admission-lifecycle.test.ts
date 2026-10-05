import { createConnection, createServer } from "node:net";

import { afterEach, expect, it, vi } from "vitest";

import { HistoryAdmissionExpired } from "../src/devnet-stack/history-admission-expired.js";
import { retryHistoryAdmission } from "../src/devnet-stack/history-admission-retry.js";
import { historyChildClient } from "../src/devnet-stack/history-child-client.js";
import {
  HISTORY_CHILD_SCHEMA,
  parseHistoryChildChallenge,
} from "../src/devnet-stack/history-child-evidence.js";
import { startHistoryChildLifecycle } from "../src/devnet-stack/history-child-startup.js";
import { HistoryConfigurationRefusal } from "../src/devnet-stack/history-configuration-refusal.js";
import { untrustedHistoryOffer } from "./helpers/history-untrusted-offer.js";
afterEach(() => vi.restoreAllMocks());
it("joins an uncancellable expired admission before shutdown returns and never starts a successor", async () => {
  const stopped = new AbortController();
  let release: () => void = () => undefined;
  const started = vi.fn(async (cutoff: number) => {
    await new Promise<void>((resolve) => {
      release = resolve;
    });
    vi.spyOn(performance, "now").mockReturnValue(cutoff + 1);
    throw new HistoryAdmissionExpired(cutoff);
  });
  let returned = false;
  const completion = retryHistoryAdmission(
    { admit: started },
    stopped.signal,
  ).then((value) => {
    returned = true;
    return value;
  });
  stopped.abort();
  await Promise.resolve();
  expect(returned).toBe(false);
  release();
  expect(await completion).toBeUndefined();
  expect(started).toHaveBeenCalledTimes(1);
});
it.each([
  new Error("ordinary EACCES"),
  new HistoryConfigurationRefusal("known drift"),
  new HistoryAdmissionExpired(0),
])(
  "never retries a generic, intrinsic, or foreign-deadline failure: %s",
  async (failure) => {
    const admit = vi.fn(async () => {
      throw failure;
    });
    await expect(
      retryHistoryAdmission({ admit }, new AbortController().signal),
    ).rejects.toBe(failure);
    expect(admit).toHaveBeenCalledTimes(1);
  },
);

it("one owned channel stays negative before activation and joins its started callback plus actual pipe close", async () => {
  const server = createServer();
  const accepted = new Promise<import("node:net").Socket>((resolve) =>
    server.once("connection", resolve),
  );
  await new Promise<void>((resolve) => server.listen(0, "127.0.0.1", resolve));
  const address = server.address();
  if (address === null || typeof address === "string")
    throw Error("owned port absent");
  const pipe = createConnection({ host: "127.0.0.1", port: address.port });
  const peer = await accepted;
  const actor = {
    role: "history-archive-a" as const,
    runId: "synthetic",
    deploymentFingerprint: "a".repeat(64),
    codeStamp: "b".repeat(64),
    serviceSpecsDigest: "c".repeat(64),
    attemptId: "27d6c44a-3262-44e1-bf69-19289923903b",
    childPid: process.pid,
  };
  const lifecycle = startHistoryChildLifecycle(actor, pipe);
  const client = historyChildClient({ actor, pipe: peer });
  let release: () => void = () => undefined;
  let entered: () => void = () => undefined;
  const started = new Promise<void>((resolve) => {
    entered = resolve;
  });
  let closed = false;
  let close: Promise<void> | undefined;
  try {
    expect(
      (await client.request("prove", untrustedHistoryOffer(actor), 1000))
        ?.offer,
    ).toBeNull();
    lifecycle.install({
      parse: parseHistoryChildChallenge,
      answer: async (request) => {
        entered();
        await new Promise<void>((resolve) => {
          release = resolve;
        });
        return {
          schema: HISTORY_CHILD_SCHEMA,
          challengeId: request.challengeId,
          actor,
          offer: null,
        };
      },
    });
    const requested = client.request(
      "prove",
      untrustedHistoryOffer(actor),
      1000,
    );
    await started;
    const physicallyClosed = new Promise<void>((resolve) =>
      pipe.once("close", () => resolve()),
    );
    close = lifecycle.close().then(() => {
      closed = true;
    });
    await physicallyClosed;
    await new Promise<void>((resolve) => setImmediate(resolve));
    expect(closed).toBe(false);
    expect(lifecycle.signal.aborted).toBe(true);
    release();
    await close;
    expect(pipe.closed).toBe(true);
    expect(await requested).toBeNull();
  } finally {
    release();
    await (close ?? lifecycle.close());
    client.close();
    peer.destroy();
    await new Promise<void>((resolve) => server.close(() => resolve()));
  }
});
it("joins an idle owned FD3 socket's actual close event before returning", async () => {
  const server = createServer();
  const accepted = new Promise<import("node:net").Socket>((resolve) =>
    server.once("connection", resolve),
  );
  await new Promise<void>((resolve) => server.listen(0, "127.0.0.1", resolve));
  const address = server.address();
  if (address === null || typeof address === "string")
    throw Error("owned port absent");
  const pipe = createConnection({ host: "127.0.0.1", port: address.port });
  const peer = await accepted;
  const lifecycle = startHistoryChildLifecycle(
    {
      role: "history-recorder",
      runId: "synthetic",
      deploymentFingerprint: "a".repeat(64),
      codeStamp: "b".repeat(64),
      serviceSpecsDigest: "c".repeat(64),
      attemptId: "27d6c44a-3262-44e1-bf69-19289923903b",
      childPid: process.pid,
    },
    pipe,
  );
  let observedClose = false;
  pipe.once("close", () => {
    observedClose = true;
  });
  try {
    await lifecycle.close();
    expect(observedClose).toBe(true);
  } finally {
    await lifecycle.close();
    peer.destroy();
    await new Promise<void>((resolve) => server.close(() => resolve()));
  }
});

it.each(["active", "uninstall", "stop", "replacement"])(
  "fences the actual pending socket response when its handler is %s",
  async (withdrawal) => {
    const server = createServer();
    const accepted = new Promise<import("node:net").Socket>((resolve) =>
      server.once("connection", resolve),
    );
    await new Promise<void>((resolve) =>
      server.listen(0, "127.0.0.1", resolve),
    );
    const address = server.address();
    if (!address || typeof address === "string") throw Error("no owned port");
    const pipe = createConnection({ host: "127.0.0.1", port: address.port });
    const peer = await accepted;
    const actor = {
      role: "history-archive-a" as const,
      runId: "synthetic",
      deploymentFingerprint: "a".repeat(64),
      codeStamp: "b".repeat(64),
      serviceSpecsDigest: "c".repeat(64),
      attemptId: "27d6c44a-3262-44e1-bf69-19289923903b",
      childPid: process.pid,
    };
    const lifecycle = startHistoryChildLifecycle(actor, pipe);
    const client = historyChildClient({ actor, pipe: peer });
    let release: () => void = () => undefined;
    let entered: () => void = () => undefined;
    const started = new Promise<void>((resolve) => {
      entered = resolve;
    });
    const offer = untrustedHistoryOffer(actor); // Structural hostile declaration, never native proof.
    const uninstall = lifecycle.install({
      parse: parseHistoryChildChallenge,
      answer: async (request) => {
        entered();
        await new Promise<void>((resolve) => {
          release = resolve;
        });
        return {
          schema: HISTORY_CHILD_SCHEMA,
          challengeId: request.challengeId,
          actor,
          offer,
        };
      },
    });
    try {
      const requested = client.request("prove", offer, 1000);
      await started;
      if (withdrawal === "stop") process.emit("SIGTERM");
      else if (withdrawal === "uninstall") uninstall();
      else if (withdrawal === "replacement")
        lifecycle.install({
          parse: parseHistoryChildChallenge,
          answer: async (request) => ({
            schema: HISTORY_CHILD_SCHEMA,
            challengeId: request.challengeId,
            actor,
            offer: null,
          }),
        });
      expect(pipe.destroyed).toBe(false); // Real command awaits listener/native close before lifecycle.close.
      release();
      const response = await requested;
      expect(response?.actor).toEqual(actor);
      if (withdrawal === "active") expect(response?.offer).toEqual(offer);
      else expect(response?.offer).toBeNull();
    } finally {
      release();
      await lifecycle.close();
      client.close();
      peer.destroy();
      await new Promise<void>((resolve) => server.close(() => resolve()));
    }
  },
);
