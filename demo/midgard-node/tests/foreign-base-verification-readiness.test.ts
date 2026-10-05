import { describe, expect, it } from "vitest";

import {
  applyForeignBaseVerificationOutcome,
  beginForeignBaseVerification,
  foreignBaseVerificationForAuthority,
  type ForeignBaseVerificationScope,
  type ForeignBaseVerificationState,
} from "../src/services/foreign-base-verification.js";

const scope: ForeignBaseVerificationScope = {
  deploymentIdentity: "deployment",
  ownerToken: "owner",
  generation: "4",
  baseHeaderHash: "header",
};
const checking = (): ForeignBaseVerificationState =>
  beginForeignBaseVerification({ status: "unobserved" }, scope);
const held = () =>
  applyForeignBaseVerificationOutcome(checking(), scope, {
    status: "missing",
    foreignHeaderHash: "header",
    reason: "retained_da_unavailable",
  });

describe("foreign base readiness evidence", () => {
  it("holds a cold base until the exact authenticated header verifies", () => {
    expect(checking()).toMatchObject({
      status: "checking",
      foreignHeaderHash: "header",
    });
    expect(held()).toMatchObject({
      status: "missing",
      reason: "retained_da_unavailable",
    });
    expect(
      applyForeignBaseVerificationOutcome(held(), scope, {
        status: "verified",
        foreignHeaderHash: "header",
      }).status,
    ).toBe("verified");
  });

  it("retains refused evidence across retries and unrelated successful workers", () => {
    const refused = applyForeignBaseVerificationOutcome(checking(), scope, {
      status: "refused",
      foreignHeaderHash: "header",
      reason: "event_window_census_mismatch",
    });
    expect(beginForeignBaseVerification(refused, scope)).toEqual(refused);
    expect(
      applyForeignBaseVerificationOutcome(refused, scope, {
        status: "verified",
        foreignHeaderHash: "other",
      }),
    ).toEqual(refused);
    expect(
      applyForeignBaseVerificationOutcome(refused, scope, {
        status: "not_required",
        baseHeaderHash: "other",
      }),
    ).toEqual(refused);
  });

  it("does not discharge an older awaited foreign event on an unrelated local base", () => {
    const waiting = applyForeignBaseVerificationOutcome(checking(), scope, {
      status: "missing",
      foreignHeaderHash: "earlier-header",
      reason: "retained_da_unavailable",
    });
    expect(
      applyForeignBaseVerificationOutcome(waiting, scope, {
        status: "verified",
        foreignHeaderHash: "earlier-header",
      }),
    ).toEqual(waiting);
    expect(
      applyForeignBaseVerificationOutcome(waiting, scope, {
        status: "not_required",
        baseHeaderHash: scope.baseHeaderHash,
      }),
    ).toEqual(waiting);
  });

  it("discharges an older hold only when the exact current-base verification explicitly verified its prefix header", () => {
    const waiting = applyForeignBaseVerificationOutcome(checking(), scope, {
      status: "missing",
      foreignHeaderHash: "earlier-header",
      reason: "retained_da_unavailable",
    });
    expect(
      applyForeignBaseVerificationOutcome(waiting, scope, {
        status: "verified",
        foreignHeaderHash: "header",
        verifiedHeaderHashes: ["header"],
      }),
    ).toEqual(waiting);
    expect(
      applyForeignBaseVerificationOutcome(waiting, scope, {
        status: "verified",
        foreignHeaderHash: "header",
        verifiedHeaderHashes: ["earlier-header", "header"],
      }).status,
    ).toBe("verified");
    expect(
      applyForeignBaseVerificationOutcome(waiting, scope, {
        status: "verified",
        foreignHeaderHash: "other",
        verifiedHeaderHashes: ["earlier-header"],
      }),
    ).toEqual(waiting);
  });

  it.each(["generation", "deploymentIdentity", "ownerToken"] as const)(
    "rejects stale %s outcomes and readiness evidence",
    (field) => {
      const replacement = { ...scope, [field]: "replacement" };
      const next = beginForeignBaseVerification(held(), replacement);
      expect(next.status).toBe("checking");
      expect(
        applyForeignBaseVerificationOutcome(next, scope, {
          status: "verified",
          foreignHeaderHash: "header",
        }),
      ).toEqual(next);
      expect(foreignBaseVerificationForAuthority(held(), replacement)).toEqual({
        status: "unobserved",
      });
    },
  );

  it("retires the old hold on canonical replacement but verifies the replacement before readiness", () => {
    const replacement = { ...scope, baseHeaderHash: "replacement" };
    const next = beginForeignBaseVerification(held(), replacement);
    expect(next).toMatchObject({
      status: "checking",
      foreignHeaderHash: "replacement",
    });
    expect(
      applyForeignBaseVerificationOutcome(next, scope, {
        status: "verified",
        foreignHeaderHash: "header",
      }),
    ).toEqual(next);
    expect(
      applyForeignBaseVerificationOutcome(next, replacement, {
        status: "not_required",
        baseHeaderHash: "replacement",
      }).status,
    ).toBe("verified");
  });

  it("rederives readiness when the process or its authority has restarted", () => {
    expect(
      foreignBaseVerificationForAuthority({ status: "unobserved" }, scope),
    ).toEqual({ status: "unobserved" });
    expect(foreignBaseVerificationForAuthority(held(), undefined)).toEqual({
      status: "unobserved",
    });
  });
});
