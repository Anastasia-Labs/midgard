import { describe, expect, it, vi } from "vitest";

import {
  type PreparedClaimRegistryMutationV1,
  requirePreparedClaimRegistryMutationV1,
} from "../src/claim-registry-transaction-v1.js";

const CLAIM_ID = "ab".repeat(32);

const mutation = (
  overrides: Partial<PreparedClaimRegistryMutationV1> = {},
): PreparedClaimRegistryMutationV1 =>
  ({
    claimId: CLAIM_ID,
    kind: "close",
    predecessorOutRef: `${"11".repeat(32)}#0`,
    predecessorDatum: {
      claims_root: "22".repeat(32),
      computation_thread_policy_id: "33".repeat(28),
    },
    registryUtxo: {} as PreparedClaimRegistryMutationV1["registryUtxo"],
    registryScript: { type: "PlutusV3", script: "00" },
    referenceInputs: [],
    referenceScriptUtxo:
      {} as PreparedClaimRegistryMutationV1["referenceScriptUtxo"],
    outputDatum: "d87980",
    apply: vi.fn((tx) => tx),
    ...overrides,
  }) as PreparedClaimRegistryMutationV1;

describe("Q33 terminal claim-registry Close boundary", () => {
  it("rejects omission", () => {
    expect(() =>
      requirePreparedClaimRegistryMutationV1({
        mutation: undefined,
        kind: "close",
        claimId: CLAIM_ID,
        label: "Q33 terminal",
      }),
    ).toThrow(/close mutation is required/i);
  });

  it("rejects the wrong mutation action and claim id", () => {
    expect(() =>
      requirePreparedClaimRegistryMutationV1({
        mutation: mutation({ kind: "cancel" }),
        kind: "close",
        claimId: CLAIM_ID,
        label: "Q33 terminal",
      }),
    ).toThrow(/changed its close/i);
    expect(() =>
      requirePreparedClaimRegistryMutationV1({
        mutation: mutation({ claimId: "cd".repeat(32) }),
        kind: "close",
        claimId: CLAIM_ID,
        label: "Q33 terminal",
      }),
    ).toThrow(/changed its close/i);
  });

  it("returns only the exact opaque Close mutation", () => {
    const exact = mutation();
    expect(
      requirePreparedClaimRegistryMutationV1({
        mutation: exact,
        kind: "close",
        claimId: CLAIM_ID,
        label: "Q33 terminal",
      }),
    ).toBe(exact);
  });
});
