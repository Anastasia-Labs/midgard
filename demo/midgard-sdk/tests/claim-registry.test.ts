import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  buildClaimRegistryMutationTransition,
  CLAIM_REGISTRY_CLOSED_VALUE,
  CLAIM_REGISTRY_LIVE_VALUE,
  claimIdFromCategoryAndHeader,
  ClaimRegistryDatum,
  ClaimRegistryRedeemer,
  EMPTY_MERKLE_TREE_ROOT,
} from "../src/index.js";

const claimId = claimIdFromCategoryAndHeader("0000001f", "ab".repeat(28));
const computationThreadPolicyId = "22".repeat(28);
const initialDatum = {
  claims_root: EMPTY_MERKLE_TREE_ROOT,
  computation_thread_policy_id: computationThreadPolicyId,
};

describe("claim-registry SDK boundary", () => {
  it("derives open, permanent close, and cancel roots without caller roots", () => {
    const opened = buildClaimRegistryMutationTransition({
      currentDatum: initialDatum,
      kind: "open",
      claimId,
      proof: [],
      carriage: { kind: "redeemer-carried" },
    });
    expect(opened.datum.claims_root).not.toBe(EMPTY_MERKLE_TREE_ROOT);

    const cancelled = buildClaimRegistryMutationTransition({
      currentDatum: opened.datum,
      kind: "cancel",
      claimId,
      proof: [],
      carriage: {
        kind: "published-chunks",
        orderedChunkReferenceInputIndices: [],
      },
    });
    expect(cancelled.datum).toEqual(initialDatum);

    const closed = buildClaimRegistryMutationTransition({
      currentDatum: opened.datum,
      kind: "close",
      claimId,
      proof: [],
      carriage: { kind: "redeemer-carried" },
    });
    expect(closed.datum.claims_root).not.toBe(opened.datum.claims_root);
    expect(closed.datum.claims_root).not.toBe(EMPTY_MERKLE_TREE_ROOT);
    expect(CLAIM_REGISTRY_LIVE_VALUE).not.toBe(CLAIM_REGISTRY_CLOSED_VALUE);

    expect(
      Data.from(Data.to(opened.datum, ClaimRegistryDatum), ClaimRegistryDatum),
    ).toEqual(opened.datum);
    expect(
      Data.from(
        Data.to(closed.redeemer, ClaimRegistryRedeemer),
        ClaimRegistryRedeemer,
      ),
    ).toEqual(closed.redeemer);
  });

  it("rejects a mutation proof that is not rooted at the current singleton", () => {
    expect(() =>
      buildClaimRegistryMutationTransition({
        currentDatum: {
          ...initialDatum,
          claims_root: "44".repeat(32),
        },
        kind: "open",
        claimId,
        proof: [],
        carriage: { kind: "redeemer-carried" },
      }),
    ).toThrow(/does not match current root/u);
  });

  it("builds only the exact four-byte category plus 28-byte header identity", () => {
    expect(claimId).toHaveLength(64);
    expect(() => claimIdFromCategoryAndHeader("1f", "ab".repeat(28))).toThrow(
      /four lowercase hex bytes/u,
    );
    expect(() =>
      claimIdFromCategoryAndHeader("0000001f", "AB".repeat(28)),
    ).toThrow(/28 lowercase hex bytes/u);
  });
});
