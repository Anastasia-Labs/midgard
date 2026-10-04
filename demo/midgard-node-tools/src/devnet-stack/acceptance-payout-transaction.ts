import { CML, coreToTxOutput } from "@lucid-evolution/lucid";

import {
  acceptanceOutRefKey,
  type AcceptanceTransaction,
  requireAcceptance,
} from "./acceptance-payout-types.js";

/** Decode actual selected-chain bytes, never synthesize witnesses from journal columns. */
export const decodeAcceptanceTransaction = (
  cbor: string | undefined,
  txHash: string,
  maxBytes: number,
): AcceptanceTransaction => {
  requireAcceptance(
    Number.isSafeInteger(maxBytes) && maxBytes > 0,
    "invalid transaction byte bound",
  );
  requireAcceptance(
    typeof cbor === "string" &&
      /^(?:[0-9a-f]{2})+$/u.test(cbor) &&
      cbor.length / 2 <= maxBytes,
    "missing, malformed or oversized canonical transaction bytes",
  );
  requireAcceptance(/^[0-9a-f]{64}$/u.test(txHash), "invalid transaction id");
  const allocated: { free(): void }[] = [];
  const own = <T extends { free(): void }>(value: T): T => {
    allocated.push(value);
    return value;
  };
  try {
    const tx = own(CML.Transaction.from_cbor_hex(cbor!));
    requireAcceptance(tx.is_valid(), "invalid canonical transaction");
    const body = own(tx.body());
    requireAcceptance(
      own(CML.hash_transaction(body)).to_hex() === txHash,
      "canonical bytes do not hash to observed transaction",
    );
    const rawInputs = own(body.inputs());
    const inputs = Array.from({ length: rawInputs.len() }, (_, index) => {
      const input = own(rawInputs.get(index));
      const outputIndex = Number(input.index());
      requireAcceptance(
        Number.isSafeInteger(outputIndex) && outputIndex >= 0,
        "invalid input index",
      );
      return { txHash: own(input.transaction_id()).to_hex(), outputIndex };
    }).sort((a, b) =>
      a.txHash < b.txHash
        ? -1
        : a.txHash > b.txHash
          ? 1
          : a.outputIndex - b.outputIndex,
    );
    requireAcceptance(
      new Set(inputs.map(acceptanceOutRefKey)).size === inputs.length,
      "duplicate canonical input",
    );
    const rawOutputs = own(body.outputs());
    const outputs = Array.from({ length: rawOutputs.len() }, (_, index) =>
      coreToTxOutput(own(rawOutputs.get(index))),
    );
    const mint: Record<string, bigint> = {};
    const rawMint = body.mint();
    if (rawMint !== undefined) {
      own(rawMint);
      const policies = own(rawMint.keys());
      for (let i = 0; i < policies.len(); i++) {
        const policy = own(policies.get(i));
        const assets = own(rawMint.get_assets(policy)!);
        const names = own(assets.keys());
        for (let j = 0; j < names.len(); j++) {
          const name = own(names.get(j));
          mint[policy.to_hex() + name.to_hex()] = assets.get(name)!;
        }
      }
    }
    const redeemers = new Map<string, string>();
    const add = (tag: CML.RedeemerTag, index: bigint, data: CML.PlutusData) => {
      const key = `${tag}:${index}`;
      requireAcceptance(
        !redeemers.has(key),
        "duplicate redeemer purpose/index",
      );
      redeemers.set(key, own(data).to_cbor_hex());
    };
    const witnesses = own(tx.witness_set());
    const rawRedeemers = witnesses.redeemers();
    if (rawRedeemers !== undefined) {
      own(rawRedeemers);
      const list = rawRedeemers.as_arr_legacy_redeemer();
      if (list !== undefined) {
        own(list);
        for (let i = 0; i < list.len(); i++) {
          const item = own(list.get(i));
          add(item.tag(), item.index(), item.data());
        }
      }
      const map = rawRedeemers.as_map_redeemer_key_to_redeemer_val();
      if (map !== undefined) {
        own(map);
        const keys = own(map.keys());
        for (let i = 0; i < keys.len(); i++) {
          const key = own(keys.get(i));
          const value = own(map.get(key)!);
          add(key.tag(), key.index(), value.data());
        }
      }
    }
    return {
      txHash,
      inputs,
      outputs,
      mint,
      policies: [
        ...new Set(Object.keys(mint).map((unit) => unit.slice(0, 56))),
      ].sort(),
      redeemers,
    };
  } finally {
    for (const value of allocated.reverse()) value.free();
  }
};
