import { Deferred, Effect, Exit, Fiber, Option } from "effect";
import { describe, expect, it } from "vitest";

import { runAfterL1ControlPlaneRelease } from "../src/fibers/da-publication-trigger.js";

const FINALIZED_HEADER_HASH = "ab".repeat(28);

describe("post-finalization DA publication ordering", () => {
  it("releases the L1 permit and completes the mutation before publication starts", async () => {
    // Ordered runtime probe: every step appends to one log, so the assertion
    // observes the actual interleaving instead of a source-text ordering.
    const trace: string[] = [];
    const publicationStarted = await Effect.runPromise(Deferred.make<void>());
    const releaseDeadPeer = await Effect.runPromise(Deferred.make<void>());
    const l1ControlPlane = await Effect.runPromise(Effect.makeSemaphore(1));
    const publishedHeaderHashes: string[] = [];

    const program = runAfterL1ControlPlaneRelease(
      l1ControlPlane.withPermits(1)(
        Effect.gen(function* () {
          trace.push("mutation-start");
          yield* Effect.yieldNow();
          trace.push("mutation-end");
          return { finalizedHeaderHash: FINALIZED_HEADER_HASH };
        }),
      ),
      (result) => result.finalizedHeaderHash,
      (headerHash) =>
        Effect.gen(function* () {
          trace.push("publish-start");
          publishedHeaderHashes.push(headerHash);
          yield* Deferred.succeed(publicationStarted, undefined);
          yield* Deferred.await(releaseDeadPeer);
          trace.push("publish-end");
          yield* Effect.fail(new Error("dead committee peer timeout"));
        }).pipe(Effect.either, Effect.asVoid),
    );

    const fiber = Effect.runFork(program);
    await Effect.runPromise(Deferred.await(publicationStarted));

    // The L1 control plane permit must already be free while publication is
    // still blocked on a dead peer: a re-nesting of publication inside the
    // permit would leave `withPermitsIfAvailable` empty here.
    const permit = await Effect.runPromise(
      l1ControlPlane.withPermitsIfAvailable(1)(Effect.succeed("reacquired")),
    );
    expect(Option.getOrUndefined(permit)).toBe("reacquired");
    expect(trace).toEqual(["mutation-start", "mutation-end", "publish-start"]);
    expect(publishedHeaderHashes).toEqual([FINALIZED_HEADER_HASH]);

    // Publication is still in flight, so the caller's fiber has not settled.
    expect(Option.isNone(await Effect.runPromise(Fiber.poll(fiber)))).toBe(
      true,
    );

    await Effect.runPromise(Deferred.succeed(releaseDeadPeer, undefined));
    // A failing publication is absorbed: the L1 result is still returned.
    await expect(Effect.runPromise(Fiber.join(fiber))).resolves.toEqual({
      finalizedHeaderHash: FINALIZED_HEADER_HASH,
    });
    expect(trace).toEqual([
      "mutation-start",
      "mutation-end",
      "publish-start",
      "publish-end",
    ]);
  });

  it("does not publish when the L1 result carries no finalized header hash", async () => {
    const publishedHeaderHashes: string[] = [];

    const result = await Effect.runPromise(
      runAfterL1ControlPlaneRelease(
        Effect.succeed({ finalizedHeaderHash: undefined }),
        (value) => value.finalizedHeaderHash,
        (headerHash) =>
          Effect.sync(() => {
            publishedHeaderHashes.push(headerHash);
          }),
      ),
    );

    expect(result).toEqual({ finalizedHeaderHash: undefined });
    expect(publishedHeaderHashes).toEqual([]);
  });

  it("does not publish and releases the permit when the L1 effect fails", async () => {
    const l1ControlPlane = await Effect.runPromise(Effect.makeSemaphore(1));
    const publishedHeaderHashes: string[] = [];
    const l1Failure = new Error("state queue commit rejected");

    const exit = await Effect.runPromiseExit(
      runAfterL1ControlPlaneRelease(
        l1ControlPlane.withPermits(1)(
          Effect.fail(l1Failure) as Effect.Effect<
            { finalizedHeaderHash: string },
            Error
          >,
        ),
        (value) => value.finalizedHeaderHash,
        (headerHash) =>
          Effect.sync(() => {
            publishedHeaderHashes.push(headerHash);
          }),
      ),
    );

    expect(Exit.isFailure(exit)).toBe(true);
    expect(publishedHeaderHashes).toEqual([]);
    const permit = await Effect.runPromise(
      l1ControlPlane.withPermitsIfAvailable(1)(Effect.succeed("reacquired")),
    );
    expect(Option.getOrUndefined(permit)).toBe("reacquired");
  });
});
