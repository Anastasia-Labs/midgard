import type { Token } from "../database/eventHistoryAuthority.js";

export type ForeignBaseVerificationScope = Token &
  Readonly<{
    baseHeaderHash: string | null;
  }>;

/** Parent-owned evidence for the canonical base selected in this history generation. */
export type ForeignBaseVerificationState =
  | Readonly<{ status: "unobserved" }>
  | Readonly<{
      status: "checking" | "verified" | "missing" | "refused";
      scope: ForeignBaseVerificationScope;
      foreignHeaderHash: string | null;
      reason: string | null;
    }>;

export type ForeignBaseVerificationOutcome =
  | Readonly<{
      status: "verified";
      foreignHeaderHash: string;
      verifiedHeaderHashes?: readonly string[];
    }>
  | Readonly<{
      status: "missing" | "refused";
      foreignHeaderHash: string;
      reason: string;
    }>
  | Readonly<{
      status: "not_required";
      baseHeaderHash: string | null;
      verifiedHeaderHashes?: readonly string[];
    }>;

const sameToken = (left: Token, right: Token): boolean =>
  left.deploymentIdentity === right.deploymentIdentity &&
  left.ownerToken === right.ownerToken &&
  left.generation === right.generation;

const sameScope = (
  left: ForeignBaseVerificationScope,
  right: ForeignBaseVerificationScope,
): boolean =>
  sameToken(left, right) && left.baseHeaderHash === right.baseHeaderHash;

export const beginForeignBaseVerification = (
  current: ForeignBaseVerificationState,
  scope: ForeignBaseVerificationScope,
): ForeignBaseVerificationState =>
  current.status !== "unobserved" && sameScope(current.scope, scope)
    ? current
    : {
        status: "checking",
        scope,
        foreignHeaderHash: scope.baseHeaderHash,
        reason: null,
      };

/** An unrelated success cannot discharge a held foreign header. */
export const applyForeignBaseVerificationOutcome = (
  current: ForeignBaseVerificationState,
  scope: ForeignBaseVerificationScope,
  outcome: ForeignBaseVerificationOutcome,
): ForeignBaseVerificationState => {
  if (current.status === "unobserved" || !sameScope(current.scope, scope))
    return current;
  if (
    outcome.status !== "verified" &&
    outcome.status !== "not_required" &&
    outcome.status !== "missing" &&
    outcome.status !== "refused"
  )
    return current;
  if (outcome.status === "missing" || outcome.status === "refused")
    return {
      status: outcome.status,
      scope,
      foreignHeaderHash: outcome.foreignHeaderHash,
      reason: outcome.reason,
    };
  if (outcome.status === "not_required") {
    if (
      outcome.baseHeaderHash !== scope.baseHeaderHash ||
      ((current.status === "missing" || current.status === "refused") &&
        current.foreignHeaderHash !== outcome.baseHeaderHash &&
        (current.foreignHeaderHash === null ||
          !outcome.verifiedHeaderHashes?.includes(current.foreignHeaderHash)))
    )
      return current;
  } else if (
    outcome.status === "verified" &&
    (outcome.foreignHeaderHash !== scope.baseHeaderHash ||
      ((current.status === "missing" || current.status === "refused") &&
        outcome.foreignHeaderHash !== current.foreignHeaderHash &&
        (current.foreignHeaderHash === null ||
          !outcome.verifiedHeaderHashes?.includes(current.foreignHeaderHash))))
  )
    return current;
  return {
    status: "verified",
    scope,
    foreignHeaderHash: scope.baseHeaderHash,
    reason: null,
  };
};

/** Recovery invalidates readiness evidence even before the next commitment tick. */
export const foreignBaseVerificationForAuthority = (
  current: ForeignBaseVerificationState,
  authority: Token | undefined,
): ForeignBaseVerificationState =>
  current.status !== "unobserved" &&
  authority !== undefined &&
  sameToken(current.scope, authority)
    ? current
    : { status: "unobserved" };
