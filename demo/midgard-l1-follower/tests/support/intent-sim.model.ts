/**
 * The fork simulator's intent oracle: the §8.2 statuses a naive model
 * expects, and the observed status reduced to the same shape.
 */
import { outRefKey } from "../../src/codec.js";
import type {
  BlockSummary,
  IntentHead,
  IntentStatus,
} from "../../src/index.js";

const hex = (bytes: Buffer): string => bytes.toString("hex");

/** What the model expects of one intent: the status kind and its flags. */
export type Expected = Readonly<{
  kind: IntentStatus["kind"];
  inputsAvailable?: boolean;
  ownSpender?: boolean;
  /** The conflicting spender: the earliest spend by slot (ties: lowest hash). */
  spender?: string;
}>;

export const expectedText = (e: Expected): string => JSON.stringify(e);

export const observedOf = (status: IntentStatus): Expected =>
  status.kind === "live"
    ? { kind: "live", inputsAvailable: status.inputsAvailable }
    : status.kind === "conflicted"
      ? {
          kind: "conflicted",
          ownSpender: status.ownSpender,
          spender: hex(status.spender),
        }
      : { kind: status.kind };

/**
 * The §8.2 statuses computed naively from the model's whole current chain
 * (every block since the origin, never pruned) and every intent ever
 * journaled: the oracle the follower's derivation is checked against.
 */
export const modelStatuses = (
  blocks: readonly BlockSummary[],
  tipSlot: number,
  intents: readonly IntentHead[],
  abandoned: ReadonlySet<string>,
): Map<string, Expected> => {
  const landed = new Map<string, boolean>();
  const spentBy = new Map<string, Readonly<{ by: string; slot: number }>>();
  const created = new Set<string>();
  for (const block of blocks)
    for (const tx of block.txs) {
      const hash = hex(tx.hash);
      landed.set(hash, tx.isValid);
      for (const outRef of tx.isValid ? tx.inputs : tx.collaterals)
        spentBy.set(outRefKey(outRef), { by: hash, slot: block.point.slot });
      if (tx.isValid)
        tx.outputs.forEach((_, index) =>
          created.add(outRefKey({ txHash: tx.hash, index })),
        );
      else if (tx.collateralReturn !== null)
        created.add(outRefKey({ txHash: tx.hash, index: tx.outputs.length }));
    }
  const byHash = new Map(intents.map((i) => [hex(i.txHash), i]));
  const memo = new Map<string, Expected>();
  const dead = (e: Expected): boolean =>
    e.kind !== "live" && e.kind !== "landed";
  const expect = (intent: IntentHead): Expected => {
    const key = hex(intent.txHash);
    const cached = memo.get(key);
    if (cached !== undefined) return cached;
    const result = derive(intent);
    memo.set(key, result);
    return result;
  };
  const derive = (intent: IntentHead): Expected => {
    const key = hex(intent.txHash);
    const landing = landed.get(key);
    if (landing !== undefined)
      return { kind: landing ? "landed" : "failed_landed" };
    const spends = [
      ...intent.inputs,
      ...intent.referenceInputs,
      ...intent.collaterals,
    ];
    // The earliest conflicting spend by slot, whatever the input order.
    const conflict = spends
      .map((o) => spentBy.get(outRefKey(o)))
      .filter((spend) => spend !== undefined && spend.by !== key)
      .sort((a, b) =>
        a!.slot !== b!.slot ? a!.slot - b!.slot : a!.by < b!.by ? -1 : 1,
      )[0];
    if (conflict !== undefined)
      return {
        kind: "conflicted",
        ownSpender: byHash.has(conflict.by),
        spender: conflict.by,
      };
    if (intent.validToSlot !== null && tipSlot >= intent.validToSlot)
      return { kind: "expired" };
    const parents = [
      ...new Set(spends.map((o) => hex(o.txHash)).filter((h) => byHash.has(h))),
    ];
    for (const parent of parents) {
      if (landed.get(parent) === true) continue;
      // A failed parent's collateral return exists: spending only it
      // depends on nothing dead.
      const fromParent = spends.filter((o) => hex(o.txHash) === parent);
      if (
        landed.get(parent) === false &&
        fromParent.every((o) => created.has(outRefKey(o)))
      )
        continue;
      if (dead(expect(byHash.get(parent)!))) return { kind: "dependency_dead" };
    }
    if (abandoned.has(key)) return { kind: "abandoned" };
    return {
      kind: "live",
      inputsAvailable: spends.every(
        (o) => created.has(outRefKey(o)) && !spentBy.has(outRefKey(o)),
      ),
    };
  };
  return new Map(intents.map((i) => [hex(i.txHash), expect(i)]));
};
