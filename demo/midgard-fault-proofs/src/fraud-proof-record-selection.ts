/**
 * Which fraud-proof record of a header is ours. Every conviction of a case on
 * the header mints a record of the same name, whoever proved it, and a record
 * is never spent (validators/fraud-proof.ak), so one header can carry several.
 * Another prover's record is no part of our workflow. Among ours, the one a
 * removal of the header referenced comes first; otherwise the lowest out-ref,
 * which stays first while it lives. The raw L1 derivation, the removal
 * builder and the case reads all select through here, so they name one record.
 */
import { outRefLabel, parseOutRefLabel } from "@al-ft/midgard-core/out-ref";
import { FraudProofTokenDatum } from "@al-ft/midgard-sdk";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

/** The prover an inline datum of `schema`, a fault-proof step's or a
 * record's, names; undefined when it does not decode. */
export const datumProver = (
  datum: string | null | undefined,
  schema: Parameters<typeof Data.from>[1],
): string | undefined => {
  if (typeof datum !== "string") return undefined;
  try {
    const prover = (
      Data.from(datum, schema) as { readonly fraud_prover?: unknown }
    ).fraud_prover;
    return typeof prover === "string" ? prover : undefined;
  } catch {
    return undefined;
  }
};

/** The prover a fraud-proof record's inline datum names. */
export const fraudProofRecordProver = (
  datum: string | null | undefined,
): string | undefined => datumProver(datum, FraudProofTokenDatum);

export const selectOwnFraudProofRecord = <Record>({
  records,
  outRefOf,
  datumOf,
  prover,
  referenced,
}: {
  readonly records: readonly Record[];
  readonly outRefOf: (record: Record) => string;
  readonly datumOf: (record: Record) => string | null | undefined;
  readonly prover: string;
  /** The out-refs the header's removal referenced, read only when we hold
   * more than one record. */
  readonly referenced?: (() => ReadonlySet<string>) | undefined;
}): Record | undefined => {
  const own = records
    .filter((record) => fraudProofRecordProver(datumOf(record)) === prover)
    .map((record) => ({ record, outRef: outRefOf(record) }))
    .sort((left, right) =>
      left.outRef < right.outRef ? -1 : left.outRef > right.outRef ? 1 : 0,
    );
  if (own.length < 2 || referenced === undefined) return own[0]?.record;
  const removalReferenced = referenced();
  return (own.find(({ outRef }) => removalReferenced.has(outRef)) ?? own[0]!)
    .record;
};

/**
 * The record a removal cites, with the prover it names. By `outRef`, as the
 * workflow derived it: a record at `address` holding exactly one of `unit`,
 * whose datum decodes, read by that out-ref and never by its unit, which any
 * conviction on the block shares. Without one (the manual CLI): the
 * signer's own record by the rule above, or else the one record there is.
 */
export const requireFraudProofRecord = async ({
  lucid,
  address,
  unit,
  outRef,
  prover,
}: {
  readonly lucid: LucidEvolution;
  readonly address: string;
  readonly unit: string;
  readonly outRef: string | undefined;
  readonly prover: string;
}): Promise<{ readonly utxo: UTxO; readonly prover: string }> => {
  const named = (utxo: UTxO | undefined) => {
    const recordProver = fraudProofRecordProver(utxo?.datum);
    return utxo !== undefined &&
      utxo.address === address &&
      (utxo.assets[unit] ?? 0n) === 1n &&
      recordProver !== undefined
      ? { utxo, prover: recordProver }
      : undefined;
  };
  if (outRef !== undefined) {
    const [utxo] = await lucid.utxosByOutRef([parseOutRefLabel(outRef)]);
    const record = named(utxo);
    if (record === undefined)
      throw new Error(
        `${outRef} is not a fraud-proof record of ${unit} with a decodable datum`,
      );
    return record;
  }
  const records = (await lucid.utxosAtWithUnit(address, unit)).filter(
    (utxo) => named(utxo) !== undefined,
  );
  const record = named(
    selectOwnFraudProofRecord({
      records,
      outRefOf: outRefLabel,
      datumOf: (utxo) => utxo.datum,
      prover,
    }) ?? (records.length === 1 ? records[0] : undefined),
  );
  if (record === undefined)
    throw new Error(
      `Expected the signer's fraud-proof record, or exactly one record, of ${unit}; found ${records.length.toString()}.`,
    );
  return record;
};
