import {
  decodeMidgardNativeTxProofFieldLengths,
  encodeMidgardNativeTxProofFieldLengths,
} from "@al-ft/midgard-core/codec";
import { decodeSingleCbor, encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/forced";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { buildCountedRoot } from "../src/transition-trace/phas.js";
import {
  buildPayloadFixture,
  forcedEventKey,
} from "./transition-trace-challenger.build-payload-fixture.js";
import {
  encodedEntry,
  eventToStepEntry,
  forcedTx,
  nativeMaterial,
  outRef,
} from "./transition-trace-challenger.native-material.js";

export const fieldLengthForcedFixture = async (multiple = false) => {
  const entries = [81, ...(multiple ? [82] : [])].map((byte) => ({
    key: outRef(byte),
    value: forcedTx(
      byte,
      multiple
        ? {
            ForcedTxInvalid: {
              reason: { FieldPreimageLengthMismatch: { field_index: 0n } },
            },
          }
        : "ForcedTxValid",
    ),
  }));
  return await buildPayloadFixture({
    forcedTransactions: entries.map(({ key, value }) =>
      encodedEntry({
        key,
        keySchema: SDK.OutputReference as never,
        value,
        valueSchema: SDK.ForcedInclusionTxV1Schema,
      }),
    ),
    steps: entries.map(({ key }, index) => ({
      schema_version: 1n,
      step_index: BigInt(index),
      event_key: forcedEventKey(key),
      phase: "ForcedTransaction",
      pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
      post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
    })),
    eventToStep: entries.map(({ key }, index) =>
      eventToStepEntry(forcedEventKey(key), {
        step_index: BigInt(index),
        phase: "ForcedTransaction",
      }),
    ),
  });
};

/** Commits a forged vector to L1 while preserving its actual canonical preimage. */
export const forcedLengthMismatchFixture = async () => {
  const fixture = await fieldLengthForcedFixture();
  const [key, leafCbor] = fixture.payload.block_body.forced_transactions[0]!;
  const leaf = Data.from(
    leafCbor,
    SDK.ForcedInclusionTxV1,
  ) as SDK.ForcedInclusionTxV1;
  const lengths = [
    ...decodeMidgardNativeTxProofFieldLengths(
      Buffer.from(leaf.submitted_source.field_preimage_lengths_cbor, "hex"),
    ),
  ];
  lengths[0] = lengths[0]! + 1;
  const changed: SDK.ForcedInclusionTxV1 = {
    ...leaf,
    submitted_source: {
      ...leaf.submitted_source,
      field_preimage_lengths_cbor:
        encodeMidgardNativeTxProofFieldLengths(lengths).toString("hex"),
    },
  };
  const sources: SDK.DaPayloadEntry[] = [
    [key, Data.to(changed, SDK.ForcedInclusionTxV1)],
  ];
  const root = await buildCountedRoot(
    SDK.ROOT_DOMAINS.forcedTransactionsV1,
    sources.map(([key, value]) => ({
      key: Buffer.from(key, "hex"),
      value: Buffer.from(value, "hex"),
    })),
  );
  const header = { ...fixture.header, forcedTransactionsRoot: root.root };
  const headerHash = await Effect.runPromise(SDK.hashBlockHeader(header));
  const payload: SDK.DaPayload = {
    ...fixture.payload,
    block_body: {
      ...fixture.payload.block_body,
      header,
      header_hash: headerHash,
      forced_transactions: sources,
    },
  };
  return {
    ...fixture,
    header,
    headerHash,
    payload,
    payloadEnvelopeCbor: await wrapDaPayload(SDK.encodeDaPayload(payload), {
      mode: "identity",
    }),
  };
};

/** Keep the full outer envelope canonical, while retaining opaque inner field bytes. */
export const opaqueForcedLengthMismatchFixture = async (preimage: Buffer) => {
  const fixture = await fieldLengthForcedFixture();
  const [key, canonicalCbor] =
    fixture.payload.block_body.forced_transaction_preimages[0]!;
  const envelope = decodeSingleCbor(
    Buffer.from(canonicalCbor, "hex"),
  ) as unknown[];
  const body = envelope[1] as unknown[];
  body[0] = preimage;
  const changedCbor = encodeCbor(envelope);
  const material = deriveMidgardForcedTxFaultEvidenceMaterial(changedCbor);
  const leaf = Data.from(
    fixture.payload.block_body.forced_transactions[0]![1],
    SDK.ForcedInclusionTxV1,
  ) as SDK.ForcedInclusionTxV1;
  const lengths = [
    ...decodeMidgardNativeTxProofFieldLengths(
      material.proofSource.fieldPreimageLengthsCbor,
    ),
  ];
  lengths[0] = preimage.length + 1;
  const changed: SDK.ForcedInclusionTxV1 = {
    ...leaf,
    tx_id: material.transactionId.toString("hex"),
    submitted_source: {
      compact_cbor: material.proofSource.compactCbor.toString("hex"),
      witness_set_compact_cbor:
        material.proofSource.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        encodeMidgardNativeTxProofFieldLengths(lengths).toString("hex"),
    },
  };
  return recommitFieldLengthFixture(fixture, {
    ...fixture.payload.block_body,
    forced_transactions: [[key, Data.to(changed, SDK.ForcedInclusionTxV1)]],
    forced_transaction_preimages: [[key, changedCbor.toString("hex")]],
  });
};

const recommitFieldLengthFixture = async (
  fixture: Awaited<ReturnType<typeof fieldLengthForcedFixture>>,
  body: SDK.DaPayload["block_body"],
) => {
  const root = async (
    domain: SDK.RootDomain,
    entries: readonly SDK.DaPayloadEntry[],
  ) =>
    (
      await buildCountedRoot(
        domain,
        entries.map(([key, value]) => ({
          key: Buffer.from(key, "hex"),
          value: Buffer.from(value, "hex"),
        })),
      )
    ).root;
  const header = {
    ...fixture.header,
    forcedTransactionsRoot: await root(
      SDK.ROOT_DOMAINS.forcedTransactionsV1,
      body.forced_transactions,
    ),
    transactionsRoot: await root(
      SDK.ROOT_DOMAINS.transactionsV1,
      body.transactions,
    ),
  };
  const headerHash = await Effect.runPromise(SDK.hashBlockHeader(header));
  const payload = {
    ...fixture.payload,
    block_body: { ...body, header, header_hash: headerHash },
  };
  return {
    ...fixture,
    header,
    headerHash,
    payload,
    payloadEnvelopeCbor: await wrapDaPayload(SDK.encodeDaPayload(payload), {
      mode: "identity",
    }),
  };
};

/** Both frontiers are L1 committed; only the normal frontier has a length proof. */
export const normalMismatchWithMalformedForcedFixture = async (
  normalMismatch = true,
) => {
  const native = nativeMaterial(93);
  const key = outRef(81);
  const forced = forcedTx(81, "ForcedTxValid");
  const fixture = await buildPayloadFixture({
    forcedTransactions: [
      encodedEntry({
        key,
        keySchema: SDK.OutputReference as never,
        value: forced,
        valueSchema: SDK.ForcedInclusionTxV1Schema,
      }),
    ],
    transactions: [
      [
        native.txId,
        Data.to(
          { tx_id: native.txId, source: native.source },
          SDK.L2TransactionSource,
        ),
      ],
    ],
    transactionPreimages: [[native.txId, native.canonicalCbor.toString("hex")]],
  });
  const lengths = [
    ...decodeMidgardNativeTxProofFieldLengths(
      Buffer.from(native.source.field_preimage_lengths_cbor, "hex"),
    ),
  ];
  if (normalMismatch) lengths[0] = lengths[0]! + 1;
  return recommitFieldLengthFixture(fixture, {
    ...fixture.payload.block_body,
    forced_transactions: [
      [
        fixture.payload.block_body.forced_transactions[0]![0],
        Data.to(
          {
            ...forced,
            submitted_source: {
              ...forced.submitted_source,
              field_preimage_lengths_cbor: "00",
            },
          },
          SDK.ForcedInclusionTxV1,
        ),
      ],
    ],
    transactions: [
      [
        native.txId,
        Data.to(
          {
            tx_id: native.txId,
            source: {
              ...native.source,
              field_preimage_lengths_cbor:
                encodeMidgardNativeTxProofFieldLengths(lengths).toString("hex"),
            },
          },
          SDK.L2TransactionSource,
        ),
      ],
    ],
  });
};
