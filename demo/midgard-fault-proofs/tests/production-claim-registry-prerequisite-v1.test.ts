import {
  buildClaimRegistryMutationTransition,
  CLAIM_REGISTRY_CLOSED_VALUE,
  claimIdFromCategoryAndHeader,
  EMPTY_MERKLE_TREE_ROOT,
  Proof,
  type Proof as ProofData,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  PRODUCTION_CLAIM_REGISTRY_PUBLIC_PROOF_V1,
  verifyProductionClaimRegistryPublicProofV1,
} from "../src/workflow/production-claim-registry-prerequisite-v1.js";

const categoryId = "0000001f";
const headerHash = "11".repeat(28);
const claimId = claimIdFromCategoryAndHeader(categoryId, headerHash);
const predecessorOutRef = `${"22".repeat(32)}#0`;
const policyId = "33".repeat(28);
const proof: ProofData = [];
const proofCbor = Data.to(proof, Proof);

const material = ({
  kind,
  predecessorRoot,
}: {
  readonly kind: "open" | "close" | "cancel";
  readonly predecessorRoot: string;
}) => ({
  schemaVersion: PRODUCTION_CLAIM_REGISTRY_PUBLIC_PROOF_V1,
  claimId,
  kind,
  predecessorOutRef,
  predecessorRoot,
  proofCbor,
});

describe("production claim-registry prerequisite V1", () => {
  it("independently admits Open and re-derived Close against their live roots", () => {
    const empty = {
      claims_root: EMPTY_MERKLE_TREE_ROOT,
      computation_thread_policy_id: policyId,
    };
    const opened = verifyProductionClaimRegistryPublicProofV1({
      value: material({ kind: "open", predecessorRoot: empty.claims_root }),
      claimId,
      kind: "open",
      predecessorOutRef,
      predecessorDatum: empty,
    });
    const liveDatum = buildClaimRegistryMutationTransition({
      currentDatum: empty,
      kind: "open",
      claimId,
      proof: opened.proof,
      carriage: { kind: "redeemer-carried" },
    }).datum;
    const closed = verifyProductionClaimRegistryPublicProofV1({
      value: material({
        kind: "close",
        predecessorRoot: liveDatum.claims_root,
      }),
      claimId,
      kind: "close",
      predecessorOutRef,
      predecessorDatum: liveDatum,
    });
    const closedDatum = buildClaimRegistryMutationTransition({
      currentDatum: liveDatum,
      kind: "close",
      claimId,
      proof: closed.proof,
      carriage: { kind: "redeemer-carried" },
    }).datum;
    expect(closedDatum.claims_root).not.toBe(liveDatum.claims_root);
    expect(CLAIM_REGISTRY_CLOSED_VALUE).not.toBe("");
  });

  it("rejects stale roots, action substitution, and non-exact public material", () => {
    const predecessorDatum = {
      claims_root: EMPTY_MERKLE_TREE_ROOT,
      computation_thread_policy_id: policyId,
    };
    expect(() =>
      verifyProductionClaimRegistryPublicProofV1({
        value: material({ kind: "open", predecessorRoot: "44".repeat(32) }),
        claimId,
        kind: "open",
        predecessorOutRef,
        predecessorDatum,
      }),
    ).toThrow("changed its live action/root identity");
    expect(() =>
      verifyProductionClaimRegistryPublicProofV1({
        value: material({
          kind: "close",
          predecessorRoot: predecessorDatum.claims_root,
        }),
        claimId,
        kind: "open",
        predecessorOutRef,
        predecessorDatum,
      }),
    ).toThrow("changed its live action/root identity");
    expect(() =>
      verifyProductionClaimRegistryPublicProofV1({
        value: {
          ...material({
            kind: "open",
            predecessorRoot: predecessorDatum.claims_root,
          }),
          callerAuthoredSuccessorRoot: "55".repeat(32),
        },
        claimId,
        kind: "open",
        predecessorOutRef,
        predecessorDatum,
      }),
    ).toThrow("missing or unknown fields");
  });

  it("rejects a locally well-formed proof that does not fold to the authenticated root", () => {
    const unrelatedRoot = "66".repeat(32);
    expect(() =>
      verifyProductionClaimRegistryPublicProofV1({
        value: material({ kind: "open", predecessorRoot: unrelatedRoot }),
        claimId,
        kind: "open",
        predecessorOutRef,
        predecessorDatum: {
          claims_root: unrelatedRoot,
          computation_thread_policy_id: policyId,
        },
      }),
    ).toThrow("does not match current root");
  });
});
