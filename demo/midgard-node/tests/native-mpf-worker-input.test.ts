import { MessageChannel, type MessagePort } from "node:worker_threads";

import { Effect, Either } from "effect";
import { describe, expect, it } from "vitest";

import { nativeMpfWorkerInput } from "../src/fibers/native-mpf-worker-input.js";
import type { NativeMpfOwnerDiagnostics } from "../src/services/mpf-native-owner/protocol.js";

const ownerSha = "ab".repeat(32);

const owner = (diagnostics: () => Promise<NativeMpfOwnerDiagnostics>) => {
  const ports: MessagePort[] = [];
  return {
    ports,
    owner: {
      diagnostics,
      createWorkerPort: () => {
        const { port1 } = new MessageChannel();
        ports.push(port1);
        return port1;
      },
    },
  };
};

describe("native MPF commit worker input", () => {
  it("creates no worker port when owner diagnostics fails", async () => {
    const failure = new Error("native owner child is gone");
    const stub = owner(() => Promise.reject(failure));
    const result = await Effect.runPromise(
      Effect.either(
        nativeMpfWorkerInput(stub.owner, "commit-block-header", ownerSha),
      ),
    );
    expect(Either.isLeft(result)).toBe(true);
    expect(Either.isLeft(result) && result.left).toMatchObject({
      _tag: "WorkerError",
      worker: "commit-block-header",
      cause: failure,
    });
    expect(stub.ports).toHaveLength(0);
  });

  it("hands the worker the port it created after reading the durable root", async () => {
    const durableRoot = "cd".repeat(32);
    const stub = owner(() =>
      Promise.resolve({ durableRoot } as NativeMpfOwnerDiagnostics),
    );
    const input = await Effect.runPromise(
      nativeMpfWorkerInput(stub.owner, "speculative-commit-builder", ownerSha),
    );
    expect(stub.ports).toHaveLength(1);
    expect(input).toEqual({
      port: stub.ports[0],
      durableRoot,
      ownerBinarySha256: ownerSha,
    });
    stub.ports[0]!.close();
  });
});
