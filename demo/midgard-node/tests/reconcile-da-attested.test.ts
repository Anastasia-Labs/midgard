import { readFile } from "node:fs/promises";

import { describe, expect, it } from "vitest";

import {
  type CanonicalDaAttestationObservation,
  classifyCanonicalDaAttestation,
} from "@/commands/reconcile.js";

const HEADER_HASH = "11".repeat(28);
const DA_ATTESTATION_POLICY_ID = "22".repeat(28);

const observation = (
  override: Partial<CanonicalDaAttestationObservation> = {},
): CanonicalDaAttestationObservation => ({
  datumHeaderHash: HEADER_HASH,
  computedHeaderHash: HEADER_HASH,
  daAttestation: DA_ATTESTATION_POLICY_ID,
  outRef: `${"33".repeat(32)}#0`,
  ...override,
});

const classify = ({
  localPayloadPresent = true,
  observations = [observation()],
}: {
  readonly localPayloadPresent?: boolean;
  readonly observations?: readonly CanonicalDaAttestationObservation[];
} = {}) =>
  classifyCanonicalDaAttestation({
    headerHash: HEADER_HASH,
    expectedDaAttestationPolicyId: DA_ATTESTATION_POLICY_ID,
    localPayloadPresent,
    observations,
  });

describe("DA-attested reconciliation", () => {
  it("accepts the exact canonical state-queue marker without watcher trust", () => {
    expect(
      classify({
        localPayloadPresent: false,
      }),
    ).toEqual({
      status: "satisfied",
      reason: "attestation_applied",
      nextAction: null,
    });
  });

  it("reports a canonical unattested header with its payload as pending", () => {
    expect(
      classify({
        observations: [observation({ daAttestation: "" })],
      }),
    ).toMatchObject({
      status: "pending",
      reason: "attestation_pending",
    });
  });

  it("blocks an unattested header when the exact payload is unavailable", () => {
    expect(
      classify({
        localPayloadPresent: false,
        observations: [observation({ daAttestation: "" })],
      }),
    ).toMatchObject({
      status: "blocked",
      reason: "local_payload_missing",
    });
  });

  it("fails closed on a foreign DA-attestation policy marker", () => {
    expect(
      classify({
        observations: [
          observation({
            daAttestation: "44".repeat(28),
          }),
        ],
      }),
    ).toMatchObject({
      status: "blocked",
      reason: "unexpected_attestation_marker",
    });
  });

  it("fails closed when the datum key differs from the recomputed header", () => {
    expect(
      classify({
        observations: [
          observation({
            computedHeaderHash: "55".repeat(28),
          }),
        ],
      }),
    ).toMatchObject({
      status: "blocked",
      reason: "header_hash_mismatch",
    });
  });

  it("does not substitute evidence for a different canonical header", () => {
    expect(
      classify({
        observations: [
          observation({
            datumHeaderHash: "66".repeat(28),
            computedHeaderHash: "66".repeat(28),
          }),
        ],
      }),
    ).toMatchObject({
      status: "ambiguous",
      reason: "canonical_header_absent",
    });
  });

  it("fails closed on duplicate canonical observations", () => {
    expect(
      classify({
        observations: [
          observation(),
          observation({ outRef: `${"77".repeat(32)}#1` }),
        ],
      }),
    ).toMatchObject({
      status: "blocked",
      reason: "canonical_header_not_unique",
    });
  });

  it("contains no watcher per-header HTTP reconciliation route", async () => {
    const source = await readFile(
      new URL("../src/commands/reconcile.ts", import.meta.url),
      "utf8",
    );
    expect(source).not.toContain("/v1/deployments/");
    expect(source).not.toContain("watcher_header_status");
    expect(source).not.toMatch(/\bfetch\s*\(/u);
  });
});
