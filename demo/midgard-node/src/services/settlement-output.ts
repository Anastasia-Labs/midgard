import { CML, coreToTxOutput, datumToHash } from "@lucid-evolution/lucid";
import { Schema } from "effect";
import JSONbig from "json-bigint";

import type { SettlementAttempt } from "../database/settlement.js";

const quantity = Schema.Union(
  Schema.String.pipe(Schema.pattern(/^\d+$/u)),
  Schema.Number.pipe(Schema.filter(Number.isSafeInteger)),
);
const matchesSchema = Schema.Array(
  Schema.Struct({
    transaction_id: Schema.String,
    output_index: Schema.Number,
    address: Schema.String,
    value: Schema.Struct({
      coins: quantity,
      assets: Schema.Record({ key: Schema.String, value: quantity }),
    }),
    datum_hash: Schema.NullOr(Schema.String),
    script_hash: Schema.NullOr(Schema.String),
    created_at: Schema.Struct({
      slot_no: Schema.Number,
      header_hash: Schema.String,
    }),
  }),
);

/** Kupo's historical matches include spent outputs. Their absence from the
 * current UTxO set is not evidence that a confirmed payout failed. */
export const settlementOutputsMatch = (
  attempt: SettlementAttempt,
  blockHash: string,
  raw: unknown,
): boolean => {
  const matches = Schema.decodeUnknownSync(matchesSchema)(raw);
  const outputs = CML.Transaction.from_cbor_hex(attempt.signed_cbor)
    .body()
    .outputs();
  return attempt.required_outputs.every((index) => {
    const expected = coreToTxOutput(outputs.get(index));
    const found = matches.filter(
      (row) =>
        row.transaction_id === attempt.tx_hash && row.output_index === index,
    );
    if (found.length !== 1) return false;
    const observed = found[0]!;
    const assets: Record<string, bigint> = {
      lovelace: BigInt(observed.value.coins),
    };
    for (const [unit, amount] of Object.entries(observed.value.assets))
      assets[unit.replace(".", "")] = BigInt(amount);
    const expectedDatumHash =
      expected.datumHash ??
      (expected.datum == null ? null : datumToHash(expected.datum));
    return (
      observed.created_at.header_hash === blockHash &&
      observed.address === expected.address &&
      observed.datum_hash === expectedDatumHash &&
      observed.script_hash === null &&
      Object.keys(assets).length === Object.keys(expected.assets).length &&
      Object.entries(expected.assets).every(
        ([unit, amount]) => assets[unit] === amount,
      )
    );
  });
};

export const readSettlementOutputEvidence = async (
  kupoUrl: string,
  attempt: SettlementAttempt,
  blockHash: string,
) => {
  const response = await fetch(`${kupoUrl}/matches/*@${attempt.tx_hash}`, {
    signal: AbortSignal.timeout(10_000),
  });
  if (!response.ok)
    throw new Error(`Settlement output query failed (${response.status})`);
  const raw: unknown = JSONbig({ storeAsString: true }).parse(
    await response.text(),
  );
  return settlementOutputsMatch(attempt, blockHash, raw);
};
