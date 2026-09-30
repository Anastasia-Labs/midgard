import {
  DepositDatumSchema,
  DepositEventSchema,
  outputReferenceToPlutusDataCbor,
  TxOrderDatumSchema,
  TxOrderEventSchema,
  WithdrawalEventSchema,
  WithdrawalOrderDatumSchema,
} from "@al-ft/midgard-sdk";
import { CML, Data } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";

import { type WatcherNormalizedL1Block } from "../../l1/l1-adapter.js";
import { dataRoundTrip } from "./decode.forced-order-material-field-count.js";
import { isHex28, sha256Bytes } from "./policy.js";
import { type EventSchema, type WatcherUserEventKind } from "./types.js";

export const outputPolicies = (
  output: CML.TransactionOutput,
): readonly string[] => {
  const value = output.amount();
  if (!value.has_multiassets()) {
    return [];
  }
  const keys = value.multi_asset().keys();
  const result: string[] = [];
  for (let index = 0; index < keys.len(); index += 1) {
    result.push(keys.get(index).to_hex());
  }
  return result;
};

export const canonicalDatumForOutput = (
  transaction: WatcherNormalizedL1Block["transactions"][number],
  outputIndex: number,
  output: CML.TransactionOutput,
): Readonly<{ cborHex: string; digest: string }> | null => {
  const datum = output.datum()?.as_datum();
  if (datum === undefined || output.script_ref() !== undefined) {
    return null;
  }
  const cborHex = datum.to_cbor_hex();
  const normalizedDatum = datum.to_canonical_cbor_hex();
  const l1Utxo = transaction.utxos.find(
    (candidate) => candidate.outputIndex === outputIndex.toString(),
  );
  if (
    l1Utxo === undefined ||
    l1Utxo.output.bytesHex !== output.to_canonical_cbor_hex() ||
    l1Utxo.datum === null ||
    l1Utxo.datum.bytes.bytesHex !== normalizedDatum ||
    l1Utxo.datum.datumHash !==
      CML.hash_plutus_data(
        CML.PlutusData.from_cbor_hex(normalizedDatum),
      ).to_hex()
  ) {
    return null;
  }
  // The adapter descriptors are normalized; the event retains the original
  // datum from the authenticated transaction body, together with its own digest.
  return Object.freeze({
    cborHex,
    digest: sha256Bytes(Buffer.from(cborHex, "hex")),
  });
};

export const nonceAssetName = (input: CML.TransactionInput): string => {
  const cbor = outputReferenceToPlutusDataCbor({
    txHash: input.transaction_id().to_hex(),
    outputIndex: Number(input.index()),
  });
  return Buffer.from(blake2b(Buffer.from(cbor, "hex"), { dkLen: 32 })).toString(
    "hex",
  );
};

export const eventSchemas = (
  kind: WatcherUserEventKind,
): Readonly<{ datum: EventSchema; event: EventSchema }> =>
  kind === "deposit"
    ? { datum: DepositDatumSchema, event: DepositEventSchema }
    : kind === "withdrawal"
      ? { datum: WithdrawalOrderDatumSchema, event: WithdrawalEventSchema }
      : { datum: TxOrderDatumSchema, event: TxOrderEventSchema };

export const parseEventDatum = (
  kind: WatcherUserEventKind,
  cborHex: string,
): Readonly<{
  event: unknown;
  eventCborHex: string;
  inclusionTime: bigint;
  witness: string;
}> | null => {
  const schemas = eventSchemas(kind);
  const datum = dataRoundTrip<{
    event: unknown;
    inclusion_time: bigint;
    witness: string;
  }>(cborHex, schemas.datum);
  if (
    datum === null ||
    typeof datum.inclusion_time !== "bigint" ||
    !isHex28(datum.witness)
  ) {
    return null;
  }
  try {
    return Object.freeze({
      event: datum.event,
      eventCborHex: Data.to(datum.event as never, schemas.event),
      inclusionTime: datum.inclusion_time,
      witness: datum.witness,
    });
  } catch {
    return null;
  }
};

export const eventIdMatchesNonce = (
  kind: WatcherUserEventKind,
  event: unknown,
  input: CML.TransactionInput,
): boolean => {
  const record = event as {
    id?: { transactionId?: unknown; outputIndex?: unknown };
  };
  return (
    (kind === "forced_order" || kind === "deposit" || kind === "withdrawal") &&
    record.id?.transactionId === input.transaction_id().to_hex() &&
    record.id.outputIndex === input.index()
  );
};

export const addressMatchesData = (
  address: CML.Address,
  value: unknown,
): boolean => {
  const candidate = value as {
    paymentCredential?:
      | { ScriptCredential?: [unknown] }
      | { PublicKeyCredential?: [unknown] };
    stakeCredential?: unknown;
  };
  const payment = address.payment_cred();
  const expectedScript =
    "ScriptCredential" in (candidate.paymentCredential ?? {})
      ? (
          candidate.paymentCredential as {
            ScriptCredential: [unknown];
          }
        ).ScriptCredential[0]
      : null;
  const expectedKey =
    "PublicKeyCredential" in (candidate.paymentCredential ?? {})
      ? (
          candidate.paymentCredential as {
            PublicKeyCredential: [unknown];
          }
        ).PublicKeyCredential[0]
      : null;
  return (
    candidate.stakeCredential === null &&
    ((isHex28(expectedScript) &&
      payment?.as_script()?.to_hex() === expectedScript) ||
      (isHex28(expectedKey) && payment?.as_pub_key()?.to_hex() === expectedKey))
  );
};
