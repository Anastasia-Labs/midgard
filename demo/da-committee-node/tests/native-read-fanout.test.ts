import { createServer } from "node:http";

import { expect, it, vi } from "vitest";

import { joinNativeReads } from "../src/l1/provider.join-native-reads.js";
import { createLocalKupmiosStateQueueReplayProvider } from "../src/l1/state-queue-replay-provider.js";
import {
  after,
  before,
  correctionLockAddress,
  deployment,
  fraudPolicy,
  fraudProofAddress,
  hubPolicy,
  policy,
} from "./state-queue-replay-provider.harness.js";

it("preserves the first observed rejection while joining the earlier input", async () => {
  let rejectEarlier!: (reason: unknown) => void;
  const earlier = new Promise<never>((_resolve, reject) => {
    rejectEarlier = reject;
  });
  const firstError = { cause: "later input rejected first" };
  let returned = false;
  const joined = joinNativeReads([earlier, Promise.reject(firstError)]).catch(
    (error: unknown) => {
      returned = true;
      return error;
    },
  );
  await new Promise<void>((resolve) => setImmediate(resolve));
  expect(returned).toBe(false);
  rejectEarlier(new Error("earlier input rejected later"));
  expect(await joined).toBe(firstError);
});

it("adopts a lazy thenable once when joining after a rejection", async () => {
  const then = vi.fn((resolve: (value: number) => void) => resolve(7));
  await expect(
    joinNativeReads([{ then }, Promise.reject(undefined)]),
  ).rejects.toBeUndefined();
  expect(then).toHaveBeenCalledTimes(1);
  const tuple: [number, string] = await joinNativeReads([
    Promise.resolve(7),
    Promise.resolve("point"),
  ]);
  expect(tuple).toEqual([7, "point"]);
});

it("joins the actual nested Kupo spend body before replay rejects its sibling", async () => {
  let started!: () => void;
  const arrived = new Promise<void>((resolve) => {
    started = resolve;
  });
  let failureRead!: () => void;
  const failed = new Promise<void>((resolve) => {
    failureRead = resolve;
  });
  let finishResponse: (() => void) | undefined;
  let readEnded = false;
  const first = before[0]!.outRef.split("#")[0]!;
  const server = createServer((request, response) => {
    response.setHeader("connection", "close");
    if (request.url?.includes(first)) {
      void arrived.then(() => {
        response.statusCode = 503;
        response.end("primary lookup failure");
      });
    } else {
      finishResponse = () => response.end("[]");
      started();
    }
  });
  await new Promise<void>((resolve) => server.listen(0, "127.0.0.1", resolve));
  const address = server.address();
  if (address === null || typeof address === "string")
    throw new Error("test listener missing");
  const replay = createLocalKupmiosStateQueueReplayProvider({
    deploymentIdentityDigest: deployment,
    stateQueuePolicyId: policy,
    stateQueueAddress: "addr_test_state_queue",
    hubOraclePolicyId: hubPolicy,
    correctionLockAddress,
    fraudProofPolicyId: fraudPolicy,
    fraudProofAddress,
    kupoUrl: `http://127.0.0.1:${address.port}`,
    ogmiosUrl: "http://127.0.0.1:1",
    fetchImpl: async (url, init) => {
      const response = await fetch(url, init);
      if (!url.includes(first)) {
        const body = await response.text();
        readEnded = true;
        return new Response(body);
      }
      const body = await response.text();
      failureRead();
      return new Response(body, { status: response.status });
    },
  });
  let returned = false;
  const operation = replay(before.slice(0, 2), after, 100, 1).catch(
    (error: unknown) => {
      returned = true;
      return error;
    },
  );
  try {
    await arrived;
    // Wait until the failing lookup has completed, while its sibling body
    // remains held by this real HTTP endpoint.
    await failed;
    for (let i = 0; i < 5; i++)
      await new Promise<void>((resolve) => setImmediate(resolve));
    expect(readEnded).toBe(false);
    expect(returned).toBe(false);
    finishResponse!();
    expect(await operation).toEqual(
      new Error("state-queue replay HTTP 503: primary lookup failure"),
    );
    expect(readEnded).toBe(true);
  } finally {
    finishResponse?.();
    await operation;
    await new Promise<void>((resolve, reject) =>
      server.close((error) => (error ? reject(error) : resolve())),
    );
  }
});
