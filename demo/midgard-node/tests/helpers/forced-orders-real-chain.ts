/**
 * A follower chain of real signed transactions (the Lucid emulator's), for
 * tests that feed what a real ledger accepted to the follower store.
 */
import {
  applyChainSyncEvent,
  blake2b256,
  decodeTransaction,
  type FactStore,
  type Point,
  transportPoint,
} from "@al-ft/midgard-l1-follower";
import { cbor as c, SIM_ORIGIN } from "@al-ft/midgard-l1-follower/testing";

/**
 * The follower's chain: blocks of real signed transactions under a minimal
 * header (`[[height, slot, prev hash, branch], signature]`), applied as
 * chain-sync roll-forwards.
 */
export const realChain = (store: FactStore) => {
  let tip: Point = SIM_ORIGIN.point;
  let height = SIM_ORIGIN.height;
  let seq = 0n;
  const split = (tx: Buffer): { body: Buffer; witnesses: Buffer } => {
    // [body, witnesses, true, null]: a valid tx without auxiliary data.
    if (tx[0] !== 0x84 || !tx.subarray(-2).equals(Buffer.from([0xf5, 0xf6])))
      throw new Error("expected [body, witnesses, true, null]");
    const body = decodeTransaction(tx).bodyCbor;
    return { body, witnesses: tx.subarray(1 + body.length, tx.length - 2) };
  };
  return {
    forward: async (txs: readonly Buffer[]): Promise<void> => {
      const parts = txs.map(split);
      height += 1;
      const slot = tip.slot + 1;
      const header = c.array(
        c.array(c.uint(height), c.uint(slot), c.bytes(tip.hash), c.uint(0)),
        c.bytes(Buffer.alloc(8)),
      );
      const point = { slot, hash: blake2b256(header) };
      const step = await applyChainSyncEvent(store, {
        kind: "roll_forward",
        seq: (seq += 1n),
        point: transportPoint(point),
        blockNo: BigInt(height),
        blockType: 7,
        prevHash: tip.hash.toString("hex"),
        tip: { point: transportPoint(point), blockNo: BigInt(height) },
        block: c.array(
          header,
          c.array(...parts.map((p) => p.body)),
          c.array(...parts.map((p) => p.witnesses)),
          c.map(),
          c.array(),
        ),
      });
      if (step.result.kind !== "applied")
        throw new Error(`apply: ${step.result.kind}`);
      tip = point;
    },
  };
};
