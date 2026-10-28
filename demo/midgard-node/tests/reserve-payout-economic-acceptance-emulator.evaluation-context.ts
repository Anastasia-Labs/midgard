import {
  type Assets,
  coreToTxOutput,
  type LucidEvolution,
  SLOT_CONFIG_NETWORK,
  utxoToTransactionInput,
  utxoToTransactionOutput,
} from "@lucid-evolution/lucid";
import { eval_phase_two_raw } from "@lucid-evolution/uplc";
import { expect } from "vitest";

import { CML } from "./deposit-flow-emulator-shared.js";

export const outputValue = (txCbor: string): Assets => {
  const outputs = CML.Transaction.from_cbor_hex(txCbor).body().outputs();
  const total: Assets = {};
  for (let i = 0; i < outputs.len(); i++)
    for (const [unit, quantity] of Object.entries(
      coreToTxOutput(outputs.get(i)).assets,
    ))
      total[unit] = (total[unit] ?? 0n) + quantity;
  return total;
};

/**
 * A CBOR item head in any well-formed spelling: a completed transaction may
 * carry indefinite-length Data and non-minimal heads, which the canonical
 * readers of `@al-ft/midgard-core/codec/cbor` refuse.
 */
const cborHead = (bytes: Uint8Array, at: number) => {
  const initial = bytes[at];
  if (initial === undefined) throw new Error("CBOR ends inside an item");
  const major = initial >> 5;
  const info = initial & 0x1f;
  if (info === 31) {
    if (major < 2 || major > 5) throw new Error("Invalid CBOR break");
    return { major, value: 0n, indefinite: true, next: at + 1 };
  }
  if (info < 24)
    return { major, value: BigInt(info), indefinite: false, next: at + 1 };
  if (info > 27) throw new Error("Reserved CBOR additional information");
  const width = 1 << (info - 24);
  if (at + 1 + width > bytes.length) throw new Error("CBOR ends inside a head");
  let value = 0n;
  for (let i = 1; i <= width; i++)
    value = (value << 8n) | BigInt(bytes[at + i]!);
  return { major, value, indefinite: false, next: at + 1 + width };
};

/** The end offset of the CBOR item that starts at `at`. */
const cborItemEnd = (bytes: Uint8Array, at: number): number => {
  // Items each open container still owes; indefinite ones end at a break.
  const owed: number[] = [];
  let cursor = at;
  for (;;) {
    let complete = true;
    if (bytes[cursor] === 0xff && owed.at(-1) === Infinity) {
      owed.pop();
      cursor += 1;
    } else {
      const head = cborHead(bytes, cursor);
      cursor = head.next;
      const count = head.indefinite
        ? Infinity
        : head.major === 4
          ? Number(head.value)
          : head.major === 5
            ? 2 * Number(head.value)
            : head.major === 6
              ? 1
              : 0;
      if (!head.indefinite && (head.major === 2 || head.major === 3))
        cursor += Number(head.value);
      if (cursor > bytes.length) throw new Error("CBOR ends inside an item");
      if (count > 0) {
        owed.push(count);
        complete = false;
      }
    }
    while (complete) {
      if (owed.length === 0) return cursor;
      owed[owed.length - 1]! -= 1;
      if (owed.at(-1) === 0) owed.pop();
      else complete = false;
    }
  }
};

type CborSpan = { readonly start: number; readonly end: number };

/** The spans of the items of the array, or the keys and values of the map, at `at`. */
const cborEntries = (bytes: Uint8Array, at: number, major: 4 | 5) => {
  const head = cborHead(bytes, at);
  if (head.major !== major) return undefined;
  const total = head.indefinite
    ? Infinity
    : Number(head.value) * (major === 5 ? 2 : 1);
  const spans: CborSpan[] = [];
  let cursor = head.next;
  while (spans.length < total && !(head.indefinite && bytes[cursor] === 0xff)) {
    const end = cborItemEnd(bytes, cursor);
    spans.push({ start: cursor, end });
    cursor = end;
  }
  return spans;
};

const cborArray = (bytes: Uint8Array, span: CborSpan | undefined) =>
  span === undefined ? undefined : cborEntries(bytes, span.start, 4);

const cborMap = (bytes: Uint8Array, span: CborSpan | undefined) =>
  span === undefined ? undefined : cborEntries(bytes, span.start, 5);

/** The unsigned integer spanned by `span`, or undefined if it is not one. */
const cborUnsigned = (bytes: Uint8Array, span: CborSpan | undefined) => {
  if (span === undefined) return undefined;
  const head = cborHead(bytes, span.start);
  return head.major === 0 && head.next === span.end ? head.value : undefined;
};

/** The value span of `key` in map entries `entries`. */
const cborMapValue = (
  bytes: Uint8Array,
  entries: readonly CborSpan[],
  key: bigint,
): CborSpan | undefined => {
  for (let i = 0; i < entries.length; i += 2)
    if (cborUnsigned(bytes, entries[i]) === key) return entries[i + 1];
  return undefined;
};

/** `bytes` with each span replaced by its CBOR hex, all else byte for byte. */
const spliced = (
  bytes: Uint8Array,
  replacements: readonly { readonly span: CborSpan; readonly hex: string }[],
): string => {
  const parts: Buffer[] = [];
  let cursor = 0;
  for (const { span, hex } of [...replacements].sort(
    (left, right) => left.span.start - right.span.start,
  )) {
    parts.push(Buffer.from(bytes.subarray(cursor, span.start)));
    parts.push(Buffer.from(hex, "hex"));
    cursor = span.end;
  }
  parts.push(Buffer.from(bytes.subarray(cursor)));
  return Buffer.concat(parts).toString("hex");
};

// Splicing keeps every byte outside the replaced items, so map pairs,
// including the destination's duplicate keys, and every other transaction
// field are retained exactly.
export const withOutputs = (
  signedCbor: string,
  replace: ReadonlyMap<number, CML.TransactionOutput>,
): string => {
  const tx = Buffer.from(signedCbor, "hex");
  const body = cborMap(tx, cborEntries(tx, 0, 4)?.[0]);
  if (body === undefined)
    throw new Error("Expected a complete Cardano transaction");
  const outputs = cborArray(tx, cborMapValue(tx, body, 1n));
  if (outputs === undefined) throw new Error("Missing transaction outputs");
  return spliced(
    tx,
    [...replace].map(([index, output]) => {
      const span = outputs[index];
      if (span === undefined) throw new Error("Invalid output mutation index");
      return { span, hex: output.to_cbor_hex() };
    }),
  );
};

export const withRedeemer = (
  signedCbor: string,
  inputIndex: bigint,
  data: string,
): string => {
  const tx = Buffer.from(signedCbor, "hex");
  const witnesses = cborMap(tx, cborEntries(tx, 0, 4)?.[1]);
  if (witnesses === undefined)
    throw new Error("Expected complete transaction witnesses");
  const redeemersSpan = cborMapValue(tx, witnesses, 5n);
  const targeted = (tag: CborSpan | undefined, index: CborSpan | undefined) =>
    cborUnsigned(tx, tag) === BigInt(CML.RedeemerTag.Spend) &&
    cborUnsigned(tx, index) === inputIndex;
  const replacements: CborSpan[] = [];
  // Both ledger encodings are valid. Replace only the data item, retaining
  // the original map/list representation, order, indices and execution units.
  const asMap = cborMap(tx, redeemersSpan);
  const asList = cborArray(tx, redeemersSpan);
  if (asMap !== undefined) {
    for (let i = 0; i < asMap.length; i += 2) {
      const key = cborArray(tx, asMap[i]);
      const value = cborArray(tx, asMap[i + 1]);
      if (key?.length !== 2 || value?.length !== 2)
        throw new Error("Invalid completed redeemer map entry");
      if (targeted(key[0], key[1])) replacements.push(value[0]!);
    }
  } else if (asList !== undefined) {
    for (const entrySpan of asList) {
      const entry = cborArray(tx, entrySpan);
      if (entry?.length !== 4)
        throw new Error("Invalid completed legacy redeemer entry");
      if (targeted(entry[0], entry[1])) replacements.push(entry[2]!);
    }
  } else {
    throw new Error("Expected completed redeemer map or list");
  }
  if (replacements.length !== 1)
    throw new Error("Missing targeted payout spend redeemer");
  const mutated = spliced(
    tx,
    replacements.map((span) => ({ span, hex: data })),
  );
  expect(CML.Transaction.from_cbor_hex(mutated).body().to_cbor_hex()).toBe(
    CML.Transaction.from_cbor_hex(signedCbor).body().to_cbor_hex(),
  );
  return mutated;
};

export const evaluationContext = async (
  lucid: LucidEvolution,
  signedCbor: string,
) => {
  const body = CML.Transaction.from_cbor_hex(signedCbor).body();
  const refs = [body.inputs(), body.reference_inputs()].flatMap((inputs) =>
    Array.from({ length: inputs?.len() ?? 0 }, (_, i) => {
      const input = inputs!.get(i);
      return {
        txHash: input.transaction_id().to_hex(),
        outputIndex: Number(input.index()),
      };
    }),
  );
  const resolved = await lucid.utxosByOutRef(refs);
  expect(resolved).toHaveLength(refs.length);
  const utxos = refs.map((ref) => {
    const found = resolved.find(
      (utxo) =>
        utxo.txHash === ref.txHash && utxo.outputIndex === ref.outputIndex,
    );
    if (found === undefined) throw new Error("Missing actual evaluation input");
    return found;
  });
  const config = lucid.config();
  const slots = config.slotConfig ?? SLOT_CONFIG_NETWORK[config.network!];
  if (
    slots === undefined ||
    config.costModels === undefined ||
    config.protocolParameters === undefined
  )
    throw new Error("Missing actual evaluator parameters");
  const inputCbor = utxos.map((utxo) =>
    utxoToTransactionInput(utxo).to_cbor_hex(),
  );
  const outputCbor = utxos.map((utxo) =>
    utxoToTransactionOutput(utxo).to_cbor_hex(),
  );
  const costModelsCbor = config.costModels.to_cbor_hex();
  const parameters = config.protocolParameters;
  const evaluate = (cbor: string) =>
    eval_phase_two_raw(
      Buffer.from(cbor, "hex"),
      inputCbor.map((value) => Buffer.from(value, "hex")),
      outputCbor.map((value) => Buffer.from(value, "hex")),
      Buffer.from(costModelsCbor, "hex"),
      parameters.maxTxExSteps,
      parameters.maxTxExMem,
      BigInt(slots.zeroTime),
      BigInt(slots.zeroSlot),
      slots.slotLength,
    ).map((bytes) => Buffer.from(bytes).toString("hex"));
  const positive = evaluate(signedCbor);
  const redeemers = CML.Transaction.from_cbor_hex(signedCbor)
    .witness_set()
    .redeemers()!
    .to_flat_format();
  expect(positive).toHaveLength(redeemers.len());
  expect(signedCbor.length / 2).toBeLessThanOrEqual(parameters.maxTxSize);
  let memory = 0n;
  let steps = 0n;
  for (let i = 0; i < redeemers.len(); i++) {
    memory += redeemers.get(i).ex_units().mem();
    steps += redeemers.get(i).ex_units().steps();
  }
  expect(memory).toBeLessThanOrEqual(parameters.maxTxExMem);
  expect(steps).toBeLessThanOrEqual(parameters.maxTxExSteps);
  // Establish that the mutation serializer itself preserves the usable script
  // context, including ordered duplicate datum pairs, before testing changes.
  expect(evaluate(withOutputs(signedCbor, new Map()))).toEqual(positive);
  return {
    evaluate,
    evidence: {
      signedCbor,
      inputCbor,
      outputCbor,
      costModelsCbor,
      parameters,
      slots,
      positive,
    },
  };
};
