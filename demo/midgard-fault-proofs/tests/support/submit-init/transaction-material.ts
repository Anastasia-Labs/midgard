/** Canonical transaction and trace material shared by emulator fixtures. Deployment and emulator resource ownership live in their scenario modules. */
import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  computeMidgardNativeTxId,
  decodeMidgardNativeByteListPreimage,
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core";
import { commitCountedRootProgram, ROOT_DOMAINS } from "@al-ft/midgard-sdk";
import { CML, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { nativeTxFromCoreCompact } from "../legacy-submit-emulator.js";
import { h32 } from "../submit-init-emulator-shared.js";

// Commit a raw transactions MPF root the way the node does: wrap it with the
// counted-root hash under the transactions domain. Fault-proof inclusion then
// authenticates the raw root against this committed value.
export const countedTransactionsRoot = (
  rawRoot: string,
  count: bigint,
): Promise<string> =>
  Effect.runPromise(
    commitCountedRootProgram({
      domain: ROOT_DOMAINS.transactionsV1,
      phasRoot: rawRoot,
      count,
    }),
  );

export const transitionTraceRawEntry = (
  key: string,
  value: string,
): [string, string] => [key, value];

export const sortedDaEntries = (
  entries: readonly [string, string][],
): [string, string][] =>
  [...entries].sort(([left], [right]) =>
    left < right ? -1 : left > right ? 1 : 0,
  );

export const positiveNonAdaAssets = (utxo: UTxO) =>
  Object.entries(utxo.assets).filter(
    ([unit, amount]) => unit !== "lovelace" && amount > 0n,
  );

export type TestOutputReference = {
  readonly transactionId: string;
  readonly outputIndex: bigint;
};

export type TransactionInclusionEntry = {
  readonly inclusion: unknown;
  readonly nativeTx: ReturnType<typeof nativeTxFromCoreCompact>;
  readonly nativeTxId: string;
  readonly spendInputCbors: readonly string[];
};

export const tx1InputsPreimage: readonly TestOutputReference[] = [
  { transactionId: h32("a1"), outputIndex: 0n },
  { transactionId: h32("a2"), outputIndex: 1n },
];

export const tx2InputsPreimage: readonly TestOutputReference[] = [
  { transactionId: h32("b1"), outputIndex: 0n },
  tx1InputsPreimage[1]!,
];

/**
 * Distinct filler spend inputs, used to drive the spend-input preimage
 * cardinality axis (finding Q1X-F6, issue #549). A filler is never the input a
 * family selects — it is what the step's authenticated preimage must
 * nevertheless re-hash, item by item, before it can select anything.
 */
export const spendInputFiller = (
  domain: number,
  index: number,
): TestOutputReference => {
  const transactionId = Buffer.alloc(32, 0x00);
  transactionId.writeUInt32BE(domain >>> 0, 0);
  transactionId.writeUInt32BE(index >>> 0, 4);
  return { transactionId: transactionId.toString("hex"), outputIndex: 0n };
};

/**
 * `selected` at the LAST position of a `cardinality`-long preimage, which is
 * the worst case for both costs this axis has: the whole collection is
 * re-hashed either way, and the selection walk is longest at the last index.
 */
export const spendInputsOfCardinality = ({
  selected,
  cardinality,
  domain,
}: {
  readonly selected: TestOutputReference;
  readonly cardinality: number;
  readonly domain: number;
}): readonly TestOutputReference[] => {
  if (!Number.isInteger(cardinality) || cardinality < 1) {
    throw new Error(
      `Spend-input cardinality must be a positive integer, got ${String(cardinality)}.`,
    );
  }
  return [
    ...Array.from({ length: cardinality - 1 }, (_unused, index) =>
      spendInputFiller(domain, index),
    ),
    selected,
  ];
};

export const outputReferenceCbor = (outRef: TestOutputReference): Buffer =>
  encodeMidgardSpendInputItem({
    txId: Buffer.from(outRef.transactionId, "hex"),
    outputIndex: Number(outRef.outputIndex),
  });

export const largeFittingOutputCbor = (
  inlineDatumPayloadBytes: number = 13_600,
): Buffer =>
  encodeMidgardTxOutput({
    address: Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 0x55)]),
    value: { lovelace: 100_000_000n, assets: new Map() },
    datum: {
      kind: "inline",
      cbor: Buffer.from(
        aikenSerialisedPlutusDataCborPreservingMapOrder(
          CML.PlutusData.new_bytes(
            Buffer.alloc(inlineDatumPayloadBytes, 0xa5),
          ).to_cbor_hex(),
        ),
        "hex",
      ),
    },
  });

export const midgardTxInput = (outRef: TestOutputReference) => ({
  tx_id: outRef.transactionId,
  output_index: outRef.outputIndex,
});

export const compactTxEntry = (
  nativeTx: MidgardNativeTxFull,
): Omit<TransactionInclusionEntry, "inclusion"> => ({
  nativeTx: nativeTxFromCoreCompact(nativeTx.compact),
  nativeTxId: computeMidgardNativeTxId(nativeTx).toString("hex"),
  spendInputCbors: decodeSpendInputCbors(nativeTx),
});

export const decodeSpendInputCbors = (
  nativeTx: MidgardNativeTxFull,
): readonly string[] =>
  decodeMidgardNativeByteListPreimage(
    nativeTx.body.spendInputsPreimageCbor,
    "test.spend_inputs",
  ).map((bytes) => Buffer.from(bytes).toString("hex"));
