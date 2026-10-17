/**
 * The follower's view of a captured emulator chain
 * (`helpers/emulator-chain-capture.ts`) for `replayJournaledOnFollower`:
 * raw blocks of the exact transaction bytes, read by the follower's own
 * block decoder, and the genesis outputs as a ledger-state answer.
 */
import {
  type BlockSummary,
  decodeBlock,
  decodeTransaction,
} from "@al-ft/midgard-l1-follower";
import { cbor as c } from "@al-ft/midgard-l1-follower/testing";
import { CML, getAddressDetails, type UTxO } from "@lucid-evolution/lucid";

export const addressBytes = (bech32: string): Buffer =>
  Buffer.from(getAddressDetails(bech32).address.hex, "hex");

/** A genesis output as the ledger-state answer carries it. */
export const ledgerOutput = (utxo: UTxO) => {
  const assets = new Map<string, Map<string, bigint>>();
  for (const [unit, quantity] of Object.entries(utxo.assets))
    if (unit !== "lovelace") {
      const policy = unit.slice(0, 56);
      const names = assets.get(policy) ?? new Map<string, bigint>();
      names.set(unit.slice(56), quantity);
      assets.set(policy, names);
    }
  return {
    outRef: {
      txHash: Buffer.from(utxo.txHash, "hex"),
      index: utxo.outputIndex,
    },
    output: {
      address: addressBytes(utxo.address),
      lovelace: utxo.assets.lovelace ?? 0n,
      ...(assets.size === 0 ? {} : { assets }),
      ...(utxo.datum == null ? {} : { datum: Buffer.from(utxo.datum, "hex") }),
    },
  };
};

/** One raw block of the exact transaction bytes, read by the follower's own decoder. */
export const blockOf = (
  height: number,
  slot: number,
  parentHash: Buffer,
  txs: readonly Buffer[],
): BlockSummary => {
  const parts = txs.map((bytes) => {
    const tx = CML.Transaction.from_cbor_bytes(bytes);
    const aux = tx.auxiliary_data();
    return {
      body: decodeTransaction(bytes).bodyCbor,
      witness: Buffer.from(tx.witness_set().to_cbor_bytes()),
      aux: aux === undefined ? null : Buffer.from(aux.to_cbor_bytes()),
      isValid: tx.is_valid(),
    };
  });
  const header = c.array(
    c.array(c.uint(height), c.uint(slot), c.bytes(parentHash), c.uint(0)),
    c.bytes(Buffer.alloc(8)),
  );
  return decodeBlock(
    c.array(
      header,
      c.array(...parts.map((part) => part.body)),
      c.array(...parts.map((part) => part.witness)),
      c.map(
        ...parts.flatMap((part, index): [Buffer, Buffer][] =>
          part.aux === null ? [] : [[c.uint(index), part.aux]],
        ),
      ),
      c.array(
        ...parts.flatMap((part, index) =>
          part.isValid ? [] : [c.uint(index)],
        ),
      ),
    ),
  );
};
