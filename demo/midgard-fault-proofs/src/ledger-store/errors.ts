/** Typed refusals of the ledger store. */

/** `at` named a root the store does not retain. */
export class LedgerNotRetained extends Error {
  constructor(readonly root: string) {
    super(`ledger root ${root} is not retained`);
    this.name = "LedgerNotRetained";
  }
}

export type LedgerMutationRefusalReason =
  /** A delete of a key the ledger does not hold. */
  | "absent"
  /** An insert over a key the ledger already holds. */
  | "present";

/** `apply` refused one mutation; the ledger it was called on is unchanged. */
export class LedgerMutationRefused extends Error {
  constructor(
    readonly reason: LedgerMutationRefusalReason,
    readonly outRef: Buffer,
    /** The refused mutation's index in the `apply` call. */
    readonly index: number,
  ) {
    super(
      reason === "absent"
        ? `mutation ${index}: cannot delete absent out-ref ${outRef.toString("hex")}`
        : `mutation ${index}: cannot insert over present out-ref ${outRef.toString("hex")}`,
    );
    this.name = "LedgerMutationRefused";
  }
}

/** `admit` refused a snapshot that lists `outRef` more than once. */
export class LedgerSnapshotRefused extends Error {
  constructor(readonly outRef: Buffer) {
    super(`ledger snapshot lists out-ref ${outRef.toString("hex")} twice`);
    this.name = "LedgerSnapshotRefused";
  }
}

/** `apply` or `admit` refused an entry with no trie leaf: neither the
 * canonical descriptor nor the undecodable-script fallback derives from its
 * output. Never a fault of the store's records. */
export class LedgerEntryNotCanonical extends Error {
  constructor(
    readonly outRef: Buffer,
    cause: unknown,
  ) {
    super(`out-ref ${outRef.toString("hex")} is not a canonical ledger entry`, {
      cause,
    });
    this.name = "LedgerEntryNotCanonical";
  }
}

/** A trie node a ledger needs is not held: a storage fault, or a ledger read
 * from a witness that did not record it. */
export class LedgerNodeMissing extends Error {
  constructor(readonly hash: string) {
    super(`ledger trie node ${hash} is not held`);
    this.name = "LedgerNodeMissing";
  }
}

/** A present entry's output is not held. */
export class LedgerOutputMissing extends Error {
  constructor(readonly outRef: string) {
    super(`ledger output for out-ref ${outRef} is not held`);
    this.name = "LedgerOutputMissing";
  }
}

/** `ledgerFromWitness` refused a witness that does not open its root. */
export class LedgerWitnessRefused extends Error {
  constructor(readonly detail: string) {
    super(`ledger witness refused: ${detail}`);
    this.name = "LedgerWitnessRefused";
  }
}
