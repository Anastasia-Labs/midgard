import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { runStartupProviderStepWithRetry } from "../src/commands/listen.retained-payload-server-thread.js";
import type {
  DaEnvelopeCapabilityPeerResult,
  DaProducerPublicationManifest,
} from "../src/da/libp2p-producer.js";
import {
  assertDaEnvelopeCapabilityQuorumOnStartup,
  classifyDaEnvelopeCapabilityQuorum,
  DaCapabilityMismatchError,
  DaCapabilityQuorumPendingError,
  daProviderAssertionsWaitReason,
} from "../src/da/startup.js";
import { isRetryableProviderError } from "../src/provider-retry.js";
import {
  DA_CAPABILITY_QUORUM_PENDING,
  StartupStepFailedError,
  StartupWaitingReporter,
} from "../src/services/startup-waiting.js";

const manifest = { threshold: 2 } as DaProducerPublicationManifest;

// The quorum verdict a failed startup step carries: the step's last failure
// (a `DatabaseInitializationError`) wraps it.
const verdictOf = (error: StartupStepFailedError | undefined): unknown =>
  (error?.cause as { readonly cause?: unknown } | undefined)?.cause;

// A peer that answered: `capable` or rejecting with `error`.
const answered = (
  signerIndex: number,
  error?: string,
): DaEnvelopeCapabilityPeerResult => ({
  peerId: `peer-${signerIndex.toString()}`,
  signerIndex,
  capable: error === undefined,
  capabilities: {} as NonNullable<
    DaEnvelopeCapabilityPeerResult["capabilities"]
  >,
  ...(error === undefined ? {} : { error }),
});

// A peer the probe could not reach or decode.
const unanswered = (
  signerIndex: number,
  error = "connect ECONNREFUSED 10.0.0.1:4001 (dial timeout)",
): DaEnvelopeCapabilityPeerResult => ({
  peerId: `peer-${signerIndex.toString()}`,
  signerIndex,
  capable: false,
  error,
});

// A peer whose libp2p host is up before its capability handler: nothing in
// the text reads as a transport failure, only the verdict says "not yet".
const notYetServing = (signerIndex: number) =>
  unanswered(
    signerIndex,
    "Protocol selection failed - could not negotiate /midgard/da/capabilities",
  );

describe("classifyDaEnvelopeCapabilityQuorum", () => {
  it.each([
    ["a met quorum", [answered(0), answered(1), unanswered(2)], undefined],
    [
      "peers still starting",
      [answered(0), unanswered(1), unanswered(2)],
      DaCapabilityQuorumPendingError,
    ],
    [
      "one rejection while the quorum can still form",
      [
        answered(0),
        answered(1, "max_chunk_bytes=1 does not match"),
        unanswered(2),
      ],
      DaCapabilityQuorumPendingError,
    ],
    [
      "a committee that rejects",
      [
        answered(0),
        answered(1, "deployment fingerprint mismatch"),
        answered(2, "deployment fingerprint mismatch"),
      ],
      DaCapabilityMismatchError,
    ],
    [
      "a signer with one rejecting and one unreachable peer",
      [
        answered(0),
        answered(1, "deployment fingerprint mismatch"),
        unanswered(1),
        answered(2, "deployment fingerprint mismatch"),
      ],
      DaCapabilityQuorumPendingError,
    ],
  ] as const)("judges %s", (_name, results, expected) => {
    const verdict = classifyDaEnvelopeCapabilityQuorum(
      manifest,
      "zstd",
      results,
    );
    if (expected === undefined) {
      expect(verdict).toBeUndefined();
    } else {
      expect(verdict).toBeInstanceOf(expected);
    }
  });
});

describe("the startup DA capability quorum", () => {
  // The startup's provider budget: `STARTUP_PROTOCOL_STATUS_QUERY_MAX_ATTEMPTS`
  // probes, here with no wait between them.
  const retry = {
    maxAttempts: 200,
    retryDelayMs: 0,
    reason: daProviderAssertionsWaitReason,
  } as const;
  const scripted = (rounds: readonly DaEnvelopeCapabilityPeerResult[][]) => {
    let calls = 0;
    return {
      calls: () => calls,
      probe: async () => {
        const round = rounds[Math.min(calls, rounds.length - 1)]!;
        calls += 1;
        return round;
      },
    };
  };
  const startup = (
    probe: ReturnType<typeof scripted>,
    quorumManifest: DaProducerPublicationManifest = manifest,
    maxAttempts: number = retry.maxAttempts,
  ) => {
    let proceeded = 0;
    const reported: [string, readonly string[]][] = [];
    const effect = runStartupProviderStepWithRetry(
      "da_provider_assertions",
      assertDaEnvelopeCapabilityQuorumOnStartup(
        quorumManifest,
        "zstd",
        probe.probe,
      ),
      { ...retry, maxAttempts },
    ).pipe(
      Effect.tap(() => Effect.sync(() => (proceeded += 1))),
      Effect.locally(StartupWaitingReporter, (key, reasons) =>
        Effect.sync(() => {
          reported.push([key, reasons]);
        }),
      ),
    );
    return {
      run: () => Effect.runPromise(Effect.either(effect)),
      proceeded: () => proceeded,
      reported: () => reported,
    };
  };

  it("waits while peers are below threshold, then proceeds exactly once", async () => {
    const shortfall = [answered(0), notYetServing(1), notYetServing(2)];
    const probe = scripted([
      shortfall,
      shortfall,
      shortfall,
      [answered(0), answered(1), unanswered(2)],
    ]);
    const run = startup(probe);
    const result = await run.run();

    expect(result._tag).toBe("Right");
    expect(probe.calls()).toBe(4);
    expect(run.proceeded()).toBe(1);
    expect(run.reported()).toEqual([
      ["da_provider_assertions", [DA_CAPABILITY_QUORUM_PENDING]],
      ["da_provider_assertions", [DA_CAPABILITY_QUORUM_PENDING]],
      ["da_provider_assertions", [DA_CAPABILITY_QUORUM_PENDING]],
      ["da_provider_assertions", []],
    ]);
  });

  it("keeps waiting while the quorum is still forming within the budget", async () => {
    const shortfall = [answered(0), notYetServing(1), notYetServing(2)];
    const probe = scripted([
      ...Array.from({ length: 150 }, () => shortfall),
      [answered(0), answered(1), unanswered(2)],
    ]);
    const run = startup(probe);
    const result = await run.run();

    expect(result._tag).toBe("Right");
    expect(probe.calls()).toBe(151);
    expect(run.proceeded()).toBe(1);
  });

  it("fails the startup under da_capability_quorum_pending once a quorum still short outlives the budget", async () => {
    const shortfall = [answered(0), notYetServing(1), notYetServing(2)];
    const probe = scripted([shortfall]);
    const run = startup(probe, manifest, 5);
    const result = await run.run();

    expect(result._tag).toBe("Left");
    expect(probe.calls()).toBe(5);
    expect(run.proceeded()).toBe(0);
    const error = result._tag === "Left" ? result.left : undefined;
    expect(error).toBeInstanceOf(StartupStepFailedError);
    expect(error).toMatchObject({
      step: "da_provider_assertions",
      reason: DA_CAPABILITY_QUORUM_PENDING,
      exhausted: true,
      attempts: 5,
    });
    expect(verdictOf(error)).toBeInstanceOf(DaCapabilityQuorumPendingError);
    expect(run.reported().at(-1)).toEqual(["da_provider_assertions", []]);
  });

  it("refuses at once when the committee answers and rejects", async () => {
    const rejecting = [
      answered(0),
      answered(1, "deployment fingerprint mismatch"),
      answered(2, "envelope content encoding is not supported"),
    ];
    const probe = scripted([
      rejecting,
      [answered(0), answered(1), answered(2)],
    ]);
    const run = startup(probe);
    const result = await run.run();

    expect(result._tag).toBe("Left");
    expect(probe.calls()).toBe(1);
    expect(run.proceeded()).toBe(0);
    const error = result._tag === "Left" ? result.left : undefined;
    expect(verdictOf(error)).toBeInstanceOf(DaCapabilityMismatchError);
    expect(error).toMatchObject({
      step: "da_provider_assertions",
      exhausted: false,
    });
    expect(isRetryableProviderError(verdictOf(error))).toBe(false);
  });

  it("keeps a limit mismatch terminal although its name reads like a transient", async () => {
    // capabilityMismatch names the disagreeing limit; "request_timeout_ms"
    // must not reach the provider-retry message fallback as "timeout".
    const mismatch = "request_timeout_ms=5000 does not match manifest 10000";
    const probe = scripted([
      [answered(0), answered(1, mismatch), answered(2, mismatch)],
      [answered(0), answered(1), answered(2)],
    ]);
    const run = startup(probe);
    const result = await run.run();

    expect(result._tag).toBe("Left");
    expect(probe.calls()).toBe(1);
    expect(run.proceeded()).toBe(0);
    const error = result._tag === "Left" ? result.left : undefined;
    expect(verdictOf(error)).toBeInstanceOf(DaCapabilityMismatchError);
    expect(String(verdictOf(error))).toMatch(/request_timeout_ms/u);
  });

  it("keeps a refusal terminal even beside an unreachable peer's transport error", async () => {
    const probe = scripted([
      [
        answered(0),
        answered(1, "deployment fingerprint mismatch"),
        answered(2, "deployment fingerprint mismatch"),
        unanswered(3),
      ],
    ]);
    // Threshold 3 of four signers, two rejecting: no quorum can form, and
    // the unreachable fourth peer's ECONNREFUSED text must not mask that.
    const result = await startup(probe, {
      threshold: 3,
    } as DaProducerPublicationManifest).run();
    expect(result._tag).toBe("Left");
    expect(probe.calls()).toBe(1);
    const error = result._tag === "Left" ? result.left : undefined;
    expect(verdictOf(error)).toBeInstanceOf(DaCapabilityMismatchError);
    expect(String(verdictOf(error))).not.toMatch(/ECONNREFUSED/u);
  });
});
