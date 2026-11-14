/** Serialized contract between the commitment parent and its compiled worker.
 * The node process commits as `node-commit:` so a restarted node can retire
 * its dead predecessor's lease by prefix; the offline `reconcile` command
 * commits as `commit:` and is never retired that way. */
const COMMIT_OWNER =
  /^(commit|node-commit):([0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12})$/u;

export type CommitLeaseOwnerKind = "commit" | "node-commit";

export const parseCommitLeaseOwner = (
  value: unknown,
): { readonly kind: CommitLeaseOwnerKind; readonly id: string } | undefined => {
  if (typeof value !== "string") return undefined;
  const match = COMMIT_OWNER.exec(value);
  return match
    ? { kind: match[1] as CommitLeaseOwnerKind, id: match[2] }
    : undefined;
};

const leaseOwner = (kind: CommitLeaseOwnerKind, uuid: string): string => {
  const owner = `${kind}:${uuid}`;
  if (!parseCommitLeaseOwner(owner)) {
    throw new Error("Commit lease owner requires a lowercase UUID v4");
  }
  return owner;
};

export const commitLeaseOwner = (uuid: string): string =>
  leaseOwner("commit", uuid);

export const nodeProcessCommitLeaseOwner = (uuid: string): string =>
  leaseOwner("node-commit", uuid);
