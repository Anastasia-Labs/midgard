/** Serialized contract between the commitment parent and its compiled worker. */
const COMMIT_OWNER =
  /^commit:([0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12})$/u;

export const parseCommitLeaseOwner = (
  value: unknown,
): { readonly kind: "commit"; readonly id: string } | undefined => {
  if (typeof value !== "string") return undefined;
  const match = COMMIT_OWNER.exec(value);
  return match ? { kind: "commit", id: match[1] } : undefined;
};

export const commitLeaseOwner = (uuid: string): string => {
  const owner = `commit:${uuid}`;
  if (!parseCommitLeaseOwner(owner)) {
    throw new Error("Commit lease owner requires a lowercase UUID v4");
  }
  return owner;
};
