import { DaRequestResponseProtocol } from "@al-ft/midgard-core/da-transport";
import { describe, expect, it } from "vitest";

import {
  fetchRetainedDaPayloadByHeaderHash,
  isRetainedDaPayloadUnavailableError,
  type RetainedDaFetchAttemptStatus,
  type RetainedDaPayloadSource,
  TransitionTraceChallengerError,
} from "../src/transition-trace/index.js";
import { buildPayloadFixture } from "./transition-trace-challenger.build-payload-fixture.js";

const HEADER_HASH = "ab".repeat(28);

const attempt = (sourceId: string, status: RetainedDaFetchAttemptStatus) => ({
  sourceId,
  sourcePeerId: "peer-a",
  protocol: DaRequestResponseProtocol.payloadByHeader,
  status,
  detail: status,
});

/** Answers each call with the next scripted status list; `ok` serves bytes. */
const scriptedSource = (
  sourceId: string,
  script: readonly (readonly RetainedDaFetchAttemptStatus[] | "ok")[],
  servedBytes = Buffer.from("d87980", "hex"),
): RetainedDaPayloadSource & { readonly calls: () => number } => {
  let calls = 0;
  return {
    sourceId,
    calls: () => calls,
    fetchPayloadByHeaderHash: async () => {
      const step = script[Math.min(calls, script.length - 1)]!;
      calls += 1;
      if (step === "ok")
        return {
          ok: true as const,
          provenance: {
            trustClass: "public_or_permissionless_da" as const,
            sourceId: `${sourceId}/peer-a`,
            grade: "security" as const,
          },
          sourceId,
          sourcePeerId: "peer-a",
          payloadEnvelopeCbor: servedBytes,
          attempts: [],
        };
      return {
        ok: false as const,
        sourceId,
        attempts: step.map((status) => attempt(sourceId, status)),
      };
    },
  };
};

const fetchFrom = (sources: readonly RetainedDaPayloadSource[]) =>
  fetchRetainedDaPayloadByHeaderHash({
    headerHash: HEADER_HASH.toUpperCase(),
    sources,
    retries: 1,
  });

describe("retained-DA payload unavailability", () => {
  it.each<
    [
      string,
      readonly (readonly RetainedDaFetchAttemptStatus[])[],
      "not_found" | "unreachable",
    ]
  >([
    ["every attempt is not_found", [["not_found"], ["not_found"]], "not_found"],
    [
      "sources mix not_found, transport_error and timeout",
      [["not_found", "transport_error"], ["timeout"]],
      "unreachable",
    ],
    ["one source only timed out", [["not_found"], ["timeout"]], "unreachable"],
  ])(
    "names the header when %s",
    async (_label, [firstSource, secondSource], availability) => {
      const failure = await fetchFrom([
        scriptedSource("public-a", [firstSource!]),
        scriptedSource("public-b", [secondSource!]),
      ]).catch((error: unknown) => error);
      expect(failure).toBeInstanceOf(TransitionTraceChallengerError);
      expect(failure).toMatchObject({
        code: "fetchFailed",
        reason: "payloadUnavailable",
        headerHash: HEADER_HASH,
        // Only an answer from every source says the payload is not held.
        availability,
      });
      expect(isRetainedDaPayloadUnavailableError(failure)).toBe(true);
    },
  );

  it.each<RetainedDaFetchAttemptStatus>([
    "rejected",
    "conflict",
    "invalid_content",
    "failed_verification",
  ])(
    "reports the payload unavailable even when a source answered %s",
    async (status) => {
      const failure = await fetchFrom([
        scriptedSource("public-a", [["not_found"]]),
        scriptedSource("public-b", [["timeout", status]]),
      ]).catch((error: unknown) => error);
      // A bad answer is a failed attempt for its peer, never the outcome.
      expect(isRetainedDaPayloadUnavailableError(failure)).toBe(true);
      expect(failure).toMatchObject({
        code: "fetchFailed",
        reason: "payloadUnavailable",
        headerHash: HEADER_HASH,
        availability: "unreachable",
      });
      expect((failure as Error).message).toContain(`public-b/peer-a`);
      expect((failure as Error).message).toContain(status);
    },
  );

  it("keeps a plain fetchFailed only when no source reported any attempt", async () => {
    const failure = await fetchFrom([scriptedSource("public-a", [[]])]).catch(
      (error: unknown) => error,
    );
    expect(failure).toMatchObject({ code: "fetchFailed" });
    expect(isRetainedDaPayloadUnavailableError(failure)).toBe(false);
  });

  it("returns the payload a source serves on its retry", async () => {
    const fixture = await buildPayloadFixture({});
    const source = scriptedSource(
      "public-a",
      [["not_found"], "ok"],
      fixture.payloadEnvelopeCbor,
    );
    const fetched = await fetchRetainedDaPayloadByHeaderHash({
      headerHash: fixture.headerHash,
      sources: [source],
      retries: 1,
    });
    expect(source.calls()).toBe(2);
    expect(
      fetched.payloadEnvelopeCbor.equals(fixture.payloadEnvelopeCbor),
    ).toBe(true);
    expect(fetched.attempts.map(({ status }) => status)).toEqual(["not_found"]);
  });

  it.each<RetainedDaFetchAttemptStatus>([
    "rejected",
    "conflict",
    "invalid_content",
  ])("does not ask a source again after it answered %s", async (status) => {
    const source = scriptedSource("public-a", [[status]]);
    await fetchFrom([source]).catch(() => undefined);
    expect(source.calls()).toBe(1);
  });

  it("matches the discriminator by value, not by class identity", () => {
    const foreign = Object.assign(new Error("bundled elsewhere"), {
      code: "fetchFailed",
      reason: "payloadUnavailable",
      headerHash: HEADER_HASH,
    });
    expect(isRetainedDaPayloadUnavailableError(foreign)).toBe(true);
    for (const value of [
      { ...foreign },
      Object.assign(new Error("x"), { ...foreign, code: "fetchFailed2" }),
      Object.assign(new Error("x"), { code: "fetchFailed", headerHash: "" }),
      Object.assign(new Error("x"), {
        code: "fetchFailed",
        reason: "payloadUnavailable",
      }),
    ])
      expect(isRetainedDaPayloadUnavailableError(value)).toBe(false);
  });
});
