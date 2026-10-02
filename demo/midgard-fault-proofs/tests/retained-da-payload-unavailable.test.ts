import { DaRequestResponseProtocol } from "@al-ft/midgard-core/da-transport";
import { describe, expect, it } from "vitest";

import {
  fetchRetainedDaPayloadByHeaderHash,
  isRetainedDaPayloadUnavailableError,
  retainedDaAttemptsOnlyUnavailable,
  type RetainedDaFetchAttemptStatus,
  type RetainedDaPayloadSource,
  TransitionTraceChallengerError,
} from "../src/transition-trace/index.js";

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
          payloadEnvelopeCbor: Buffer.from("d87980", "hex"),
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
  ])(
    "keeps a plain fetchFailed when any source answered %s",
    async (status) => {
      const failure = await fetchFrom([
        scriptedSource("public-a", [["not_found"]]),
        scriptedSource("public-b", [["timeout", status]]),
      ]).catch((error: unknown) => error);
      expect(failure).toMatchObject({ code: "fetchFailed" });
      expect((failure as { reason?: unknown }).reason).toBeUndefined();
      expect(isRetainedDaPayloadUnavailableError(failure)).toBe(false);
    },
  );

  it("keeps a plain fetchFailed when no source reported any attempt", async () => {
    const failure = await fetchFrom([scriptedSource("public-a", [[]])]).catch(
      (error: unknown) => error,
    );
    expect(failure).toMatchObject({ code: "fetchFailed" });
    expect(isRetainedDaPayloadUnavailableError(failure)).toBe(false);
  });

  it("returns the payload a source serves on its retry", async () => {
    const source = scriptedSource("public-a", [["not_found"], "ok"]);
    const fetched = await fetchFrom([source]);
    expect(source.calls()).toBe(2);
    expect(fetched.payloadEnvelopeCbor.toString("hex")).toBe("d87980");
    expect(fetched.attempts.map(({ status }) => status)).toEqual(["not_found"]);
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

  it("classifies attempt lists with one shared status rule", () => {
    expect(
      retainedDaAttemptsOnlyUnavailable([
        attempt("a", "not_found"),
        attempt("b", "transport_error"),
        attempt("c", "timeout"),
      ]),
    ).toBe(true);
    for (const status of ["rejected", "conflict", "invalid_content"] as const)
      expect(
        retainedDaAttemptsOnlyUnavailable([
          attempt("a", "not_found"),
          attempt("b", status),
        ]),
      ).toBe(false);
  });
});
