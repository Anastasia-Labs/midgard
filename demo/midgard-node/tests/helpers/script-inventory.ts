/**
 * Every compiled script reachable from a `MidgardValidators` tree.
 *
 * The inventory is walked rather than transcribed so that a validator added to
 * the SDK type is covered the day it lands: a hand-listed set of roles is what
 * let the always-succeeds stand-in hide behind `not.toEqual(placeholder)`
 * checks that only ever named a fraction of the tree.
 */
import type { Script } from "@lucid-evolution/lucid";

export type ScriptInventoryEntry = {
  /** Dotted path from the validator tree root, e.g. `stateQueue`. */
  readonly path: string;
  /** Which of the three script slots on that node this entry is. */
  readonly kind: "spending" | "minting" | "withdrawal";
  /** The applied script CBOR hex. */
  readonly cbor: string;
  /** The script as the SDK hands it to the ledger. */
  readonly script: Script;
  /** The hash (or policy id) the SDK declares alongside the script. */
  readonly declaredHash: string;
};

/** `<path>:<kind>`, the stable identity used in assertions and diffs. */
export const scriptInventoryId = (entry: ScriptInventoryEntry): string =>
  `${entry.path}:${entry.kind}`;

export const collectScriptInventory = (
  root: unknown,
): readonly ScriptInventoryEntry[] => {
  const entries: ScriptInventoryEntry[] = [];
  const visited = new Set<unknown>();
  const walk = (node: unknown, path: string): void => {
    if (node === null || typeof node !== "object" || visited.has(node)) {
      return;
    }
    visited.add(node);
    const record = node as Record<string, unknown>;
    for (const kind of ["spending", "minting", "withdrawal"] as const) {
      const cbor = record[`${kind}ScriptCBOR`];
      const script = record[`${kind}Script`];
      const declaredHash =
        record[kind === "minting" ? "policyId" : `${kind}ScriptHash`];
      if (
        typeof cbor === "string" &&
        typeof declaredHash === "string" &&
        script !== undefined
      ) {
        entries.push({
          path,
          kind,
          cbor,
          script: script as Script,
          declaredHash,
        });
      }
    }
    for (const [key, value] of Object.entries(record)) {
      walk(value, path === "" ? key : `${path}.${key}`);
    }
  };
  walk(root, "");
  return entries;
};
