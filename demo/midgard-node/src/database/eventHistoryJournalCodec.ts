import { Schema } from "effect";

import type { HistoryIncarnation } from "../l1-event-history-provenance.js";
import type { LedgerSnapshotOutput } from "../l1-ledger-snapshot.js";

const digest = Schema.String.pipe(Schema.pattern(/^[0-9a-f]{64}$/u));
const bytes = Schema.String.pipe(Schema.pattern(/^(?:[0-9a-f]{2})+$/u));
const natural = Schema.Number.pipe(
  Schema.int(),
  Schema.between(0, Number.MAX_SAFE_INTEGER),
);
const quantity = Schema.String.pipe(
  Schema.pattern(/^(?:0|[1-9][0-9]*)$/u),
  Schema.compose(Schema.BigInt),
);
const outRef = Schema.Struct({ txHash: digest, outputIndex: natural });
const placement = Schema.Struct({
  blockHash: digest,
  slot: natural,
  height: natural,
  transactionHash: digest,
  transactionIndex: natural,
});
const incarnation = Schema.Struct({
  id: digest,
  bindingDigest: digest,
  kind: Schema.Literal("deposit", "withdrawal"),
  event: Schema.Struct({
    key: digest,
    idCbor: bytes,
    inclusionTime: quantity,
    factsCbor: bytes,
    payloadCbor: bytes,
    originalAssetsCbor: bytes,
    outRef,
  }),
  placement: Schema.NullOr(
    Schema.Struct({
      admission: placement,
      current: Schema.NullOr(Schema.Struct({ outRef, at: placement })),
      retirement: Schema.NullOr(
        Schema.Struct({
          at: placement,
          outRef,
          reason: Schema.Literal("absorbed", "payout_initialized", "refunded"),
          observerRedeemerIndex: natural,
          witnessCbor: bytes,
        }),
      ),
    }),
  ),
}).pipe(
  Schema.filter(
    ({ placement }) =>
      placement === null ||
      (placement.current === null) !== (placement.retirement === null),
    {
      message: () => "Canonical incarnation must be live or explicitly retired",
    },
  ),
);
const output = Schema.Struct({
  txHash: digest,
  outputIndex: natural,
  address: Schema.NonEmptyString,
  assets: Schema.Record({
    key: Schema.String.pipe(
      Schema.pattern(/^(?:lovelace|[0-9a-f]{56}(?:[0-9a-f]{2}){0,32})$/u),
    ),
    value: quantity,
  }),
  datum: Schema.optional(bytes),
  datumHash: Schema.optional(digest),
  hasReferenceScript: Schema.Boolean,
}).pipe(
  Schema.filter(
    ({ datum, datumHash }) => datum === undefined || datumHash === undefined,
    {
      message: () =>
        "Stored output cannot contain both inline datum and datum hash",
    },
  ),
);

const freezeRecord = <T>(value: T): T => {
  if (typeof value === "object" && value !== null) {
    for (const member of Object.values(value)) freezeRecord(member);
    Object.freeze(value);
  }
  return value;
};

/** Explicit decimal-string quantities preserve integers above JS's exact range.
 * These codecs validate local storage shape, not ledger/source authenticity. */
export const encodeJournalIncarnation = (value: HistoryIncarnation): string =>
  JSON.stringify(Schema.encodeSync(incarnation)(value));
export const decodeJournalIncarnation = (value: string): HistoryIncarnation =>
  freezeRecord(
    Schema.decodeUnknownSync(incarnation)(JSON.parse(value), {
      onExcessProperty: "error",
    }),
  );
export const encodeJournalOutput = (value: LedgerSnapshotOutput): string =>
  JSON.stringify(
    Schema.encodeSync(output)({
      ...value,
      assets: Object.fromEntries(
        Object.entries(value.assets).sort(([a], [b]) => a.localeCompare(b)),
      ),
    }),
  );
export const decodeJournalOutput = (value: string): LedgerSnapshotOutput =>
  freezeRecord(
    Schema.decodeUnknownSync(output)(JSON.parse(value), {
      onExcessProperty: "error",
    }),
  );
