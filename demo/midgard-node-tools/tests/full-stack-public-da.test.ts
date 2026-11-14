import { DaRequestResponseProtocol } from "@al-ft/midgard-core/da-transport";
import type {
  RetainedDaFetchAttempt,
  RetainedDaFetchAttemptStatus,
  RetainedDaPayloadSource,
} from "@al-ft/midgard-fault-proofs";
import {
  buildPublicReplayFixture,
  DA_PROVENANCE,
  headerHashOf,
} from "midgard-watcher/tests/support/block-replay-public-fixture";
import { afterEach, describe, expect, it, vi } from "vitest";

import { fetchCommittedPublicDa } from "../src/full-stack/public-da.js";

describe("public DA of a committed block", () => {
  // The node reports a block committed before the committee has stored its
  // payload, so public DA can answer not_found for a while after commit.
  const PEER = "12D3KooWAbcdefghijkmnopqrstuvwxyz12345";
  const STEP_LIMIT_MS = 60_000;
  /**
   * A public DA source that gives `answers` in order and then repeats the
   * last: a list of failure statuses, or the bytes it serves. Like the
   * watcher's source, it serves bytes only when the caller's verifier admits
   * them.
   */
  const scriptedSource = (
    answers: readonly (readonly RetainedDaFetchAttemptStatus[] | Buffer)[],
  ) => {
    const calls: number[] = [];
    let lastAttempts: readonly RetainedDaFetchAttempt[] = [];
    const attempt = (
      status: RetainedDaFetchAttemptStatus,
      detail: string,
    ): RetainedDaFetchAttempt => ({
      sourceId: "public-da",
      sourcePeerId: PEER,
      protocol: DaRequestResponseProtocol.payloadByHeader,
      status,
      detail,
    });
    const source: RetainedDaPayloadSource = {
      sourceId: "public-da",
      fetchPayloadByHeaderHash: async (_headerHash, options) => {
        calls.push(Date.now());
        const answer = answers[Math.min(calls.length, answers.length) - 1]!;
        if (Buffer.isBuffer(answer)) {
          const verdict = await options?.verifyPayload?.(answer);
          if (verdict?.ok !== false)
            return {
              ok: true,
              provenance: DA_PROVENANCE,
              sourceId: "public-da",
              sourcePeerId: PEER,
              payloadEnvelopeCbor: answer,
              attempts: [],
            };
          lastAttempts = [attempt("failed_verification", verdict.reason)];
        } else
          lastAttempts = answer.map((status) =>
            attempt(status, `${status} on call ${calls.length}`),
          );
        return { ok: false, sourceId: "public-da", attempts: lastAttempts };
      },
    };
    return { source, calls, lastAttempts: () => lastAttempts };
  };

  afterEach(() => {
    vi.useRealTimers();
  });

  it("keeps asking while the committee has not stored the payload, then returns what it serves", async () => {
    const block = await buildPublicReplayFixture({
      steps: [],
      priorState: [],
      postState: [],
      requireAcceptedBindings: false,
    });
    const headerHash = headerHashOf(block.header);
    vi.useFakeTimers({ toFake: ["setTimeout", "Date"] });
    const { source, calls } = scriptedSource([
      ["not_found"],
      ["not_found"],
      block.envelope,
    ]);
    const served = fetchCommittedPublicDa(
      source,
      headerHash,
      STEP_LIMIT_MS,
    ).catch((error: Error) => error.message);
    await vi.runAllTimersAsync();
    expect(await served).toMatchObject({
      ok: true,
      sourcePeerId: PEER,
      payloadEnvelopeCbor: block.envelope,
    });
    expect(calls).toHaveLength(3);
  });

  it("fails at once on any answer other than not stored yet", async () => {
    const headerHash = "ab".repeat(28);
    vi.useFakeTimers({ toFake: ["setTimeout", "Date"] });
    for (const statuses of [
      ["conflict"],
      ["invalid_content"],
      ["rejected"],
      ["transport_error"],
      ["timeout"],
      ["not_found", "conflict"],
      [],
    ] as const) {
      const { source, calls, lastAttempts } = scriptedSource([statuses]);
      const failed = fetchCommittedPublicDa(
        source,
        headerHash,
        STEP_LIMIT_MS,
      ).then(
        () => undefined,
        (error: Error) => error.message,
      );
      await vi.runAllTimersAsync();
      expect(await failed).toBe(
        `Public DA retrieval failed for ${headerHash}: ${JSON.stringify(lastAttempts())}`,
      );
      expect(calls).toHaveLength(1);
    }
  });

  it("refuses at once a served copy that does not commit to the header", async () => {
    const headerHash = "ab".repeat(28);
    vi.useFakeTimers({ toFake: ["setTimeout", "Date"] });
    const { source, calls } = scriptedSource([Buffer.from("820102", "hex")]);
    const failed = fetchCommittedPublicDa(
      source,
      headerHash,
      STEP_LIMIT_MS,
    ).then(
      () => undefined,
      (error: Error) => error.message,
    );
    await vi.runAllTimersAsync();
    expect(await failed).toMatch(
      `Public DA retrieval failed for ${headerHash}: [{"sourceId":"public-da","sourcePeerId":"${PEER}","protocol":"payload-by-header","status":"failed_verification"`,
    );
    expect(calls).toHaveLength(1);
  });

  it("gives up at the step's limit with the last answer when the payload never arrives", async () => {
    const headerHash = "ab".repeat(28);
    vi.useFakeTimers({ toFake: ["setTimeout", "Date"] });
    const { source, calls, lastAttempts } = scriptedSource([["not_found"]]);
    const failed = fetchCommittedPublicDa(source, headerHash, 12_000).then(
      () => undefined,
      (error: Error) => error.message,
    );
    await vi.runAllTimersAsync();
    expect(await failed).toBe(
      `Public DA retrieval failed for ${headerHash}: ${JSON.stringify(lastAttempts())}`,
    );
    expect(calls.length).toBeGreaterThan(1);
    expect(calls.at(-1)! - calls[0]!).toBeGreaterThanOrEqual(12_000);
  });
});
