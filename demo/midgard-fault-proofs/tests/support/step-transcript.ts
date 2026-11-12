/**
 * The byte transcript of a fault-proof lifecycle: every transaction the
 * emulator accepts while it runs, kept as two digests.
 *
 * - The full digest is the transaction hash and the SHA-256 of its signed
 *   CBOR. It pins every byte.
 * - The step digest pins what the step itself states and leaves out what its
 *   funding chose: wallet inputs, collateral, the fee, the wallet's change
 *   output, the signatures, and the execution units and script data hash that
 *   follow from them. An input is named by the lifecycle transaction that
 *   produced it and its output index, not by a hash funding can move, input
 *   and reference lists are ordered by those names, and a redeemer is keyed
 *   by the input or policy it points at, not by its sorted position. An index
 *   a redeemer carries in its data still counts ledger order, so a funding
 *   change that moves an earlier transaction's hash can show up here as a
 *   moved reference index.
 *
 * Wallet seeds and the emulator's start time are the only inputs a run does
 * not choose, so a recording draws randomness from a stream seeded by its name
 * and freezes `Date` at a fixed instant. Two runs of the same code then accept
 * the same bytes.
 */
import { createHash } from "node:crypto";

import { layerMethod } from "@al-ft/midgard-test-support/method-layer";
import { CML, Emulator, getAddressDetails } from "@lucid-evolution/lucid";
import { expect, onTestFinished, vi } from "vitest";

/** The instant every recording's clock is frozen at, a whole second. */
const TRANSCRIPT_EPOCH_MS = Date.UTC(2026, 10, 1);

export const sha256 = (text: string) =>
  createHash("sha256").update(text).digest();

/** `crypto.getRandomValues` over a SHA-256 counter stream seeded by `seed`. */
const seededRandomValues = (
  seed: string,
): typeof globalThis.crypto.getRandomValues => {
  let draws = 0;
  return <T extends Exclude<NodeJS.TypedArray, Float32Array | Float64Array>>(
    array: T,
  ): T => {
    const bytes = new Uint8Array(
      array.buffer,
      array.byteOffset,
      array.byteLength,
    );
    for (let offset = 0; offset < bytes.length; offset += 32)
      bytes.set(
        sha256(`${seed}:${(draws++).toString()}`).subarray(
          0,
          bytes.length - offset,
        ),
        offset,
      );
    return array;
  };
};

/** One accepted transaction: its hash, its CBOR digest, its step digest. */
export type StepSubmission = Readonly<{
  txHash: string;
  cbor: string;
  step: string;
}>;

type Json = { readonly [key: string]: Json } | readonly Json[] | string | null;
type TxInputJson = Readonly<{ transaction_id: string; index: string }>;
type TxOutputJson = Readonly<Record<string, Readonly<{ address: string }>>>;

/** Parses CML JSON keeping every number as its source text: coins and mint
 * quantities exceed a double's integer range. */
const parseJson = JSON.parse as (
  text: string,
  reviver: (
    key: string,
    value: unknown,
    context: { source: string },
  ) => unknown,
) => Json;
const exactJson = (text: string) =>
  parseJson(text, (_key, value, context) =>
    typeof value === "number" ? context.source : value,
  ) as { readonly [key: string]: Json };

const outRef = ({ transaction_id, index }: TxInputJson) =>
  `${transaction_id}#${index}`;

const ledgerOrder = (left: TxInputJson, right: TxInputJson) =>
  left.transaction_id === right.transaction_id
    ? Number(left.index) - Number(right.index)
    : left.transaction_id < right.transaction_id
      ? -1
      : 1;

const outputAddress = (output: TxOutputJson) =>
  Object.values(output)[0]!.address;

/** Body fields funding chooses, which the step digest leaves out. */
const FUNDING_BODY_FIELDS = new Set([
  "inputs",
  "outputs",
  "reference_inputs",
  "fee",
  "collateral_inputs",
  "collateral_return",
  "total_collateral",
  "script_data_hash",
]);

const without = (
  record: { readonly [key: string]: Json },
  omitted: ReadonlySet<string>,
) =>
  Object.fromEntries(
    Object.entries(record).filter(([key]) => !omitted.has(key)),
  );

/**
 * The step digest of `cbor`. `addressOf` resolves a spent input's address and
 * `ordinal` names the lifecycle transaction that produced an output.
 */
export const stepDigest = (
  cbor: string,
  addressOf: (outRef: string) => string,
  ordinal: (txHash: string) => number | undefined,
): string => {
  const tx = CML.Transaction.from_cbor_hex(cbor);
  const json = exactJson(tx.to_json());
  const body = json["body"] as { readonly [key: string]: Json };
  const name = (input: TxInputJson) => {
    const index = ordinal(input.transaction_id);
    return index === undefined
      ? outRef(input)
      : `${index.toString()}#${input.index}`;
  };
  const inputs = [...(body["inputs"] as readonly TxInputJson[])].sort(
    ledgerOrder,
  );
  const walletAddresses = new Set(
    inputs
      .map((input) => addressOf(outRef(input)))
      .filter(
        (address) =>
          getAddressDetails(address).paymentCredential?.type === "Key",
      ),
  );
  const isWallet = (input: TxInputJson) =>
    walletAddresses.has(addressOf(outRef(input)));
  const outputs = body["outputs"] as readonly TxOutputJson[];
  const change = outputs.at(-1);
  const stated =
    change !== undefined && walletAddresses.has(outputAddress(change))
      ? outputs.slice(0, -1)
      : outputs;
  const policies = Array.from(
    { length: tx.body().mint()?.keys().len() ?? 0 },
    (_, i) => tx.body().mint()!.keys().get(i).to_hex(),
  ).sort();
  const flat = tx.witness_set().redeemers()?.to_flat_format();
  const redeemers = Array.from({ length: flat?.len() ?? 0 }, (_, i) => {
    const redeemer = flat!.get(i);
    const index = Number(redeemer.index());
    const tag = redeemer.tag();
    const target =
      tag === CML.RedeemerTag.Spend
        ? name(inputs[index]!)
        : tag === CML.RedeemerTag.Mint
          ? policies[index]!
          : `${tag.toString()}:${index.toString()}`;
    return `${target} ${tag.toString()} ${redeemer.data().to_cbor_hex()}`;
  }).sort();
  const witnessSet = json["witness_set"] as { readonly [key: string]: Json };
  return sha256(
    JSON.stringify({
      inputs: inputs
        .filter((input) => !isWallet(input))
        .map(name)
        .sort(),
      outputs: stated,
      reference_inputs: [
        ...((body["reference_inputs"] as readonly TxInputJson[] | null) ?? []),
      ]
        .map(name)
        .sort(),
      body: without(body, FUNDING_BODY_FIELDS),
      witnesses: without(
        witnessSet,
        new Set(["vkeywitnesses", "bootstrap_witnesses", "redeemers"]),
      ),
      redeemers,
      is_valid: json["is_valid"],
      auxiliary_data: json["auxiliary_data"],
    }),
  ).toString("hex");
};

export type StepRecording = Readonly<{
  /** Every transaction accepted so far, in order. */
  submitted: readonly StepSubmission[];
  restore: () => void;
}>;

/**
 * Seeds randomness with `seed`, freezes `Date`, and records every transaction
 * any emulator accepts until `restore`.
 */
export const recordStepSubmissions = (seed: string): StepRecording => {
  const submitted: StepSubmission[] = [];
  const ordinals = new Map<string, number>();
  const random = layerMethod(globalThis.crypto, "getRandomValues", () =>
    seededRandomValues(seed),
  );
  const submissions = layerMethod(
    Emulator.prototype,
    "submitTx",
    (next) =>
      async function (this: Emulator, cbor: string) {
        const addressOf = (ref: string) => {
          const [hash, index] = ref.split("#");
          const key = hash! + index!;
          const entry = this.ledger[key] ?? this.mempool[key];
          if (entry === undefined)
            throw new Error(`spent input ${ref} is not in the emulator ledger`);
          return entry.utxo.address;
        };
        const step = stepDigest(cbor, addressOf, (hash) => ordinals.get(hash));
        const txHash = await next.call(this, cbor);
        ordinals.set(txHash, submitted.length);
        submitted.push({ txHash, cbor: sha256(cbor).toString("hex"), step });
        return txHash;
      },
  );
  vi.useFakeTimers({ toFake: ["Date"], now: TRANSCRIPT_EPOCH_MS });
  return {
    submitted,
    restore: () => {
      vi.useRealTimers();
      submissions.restore();
      random.restore();
    },
  };
};

/**
 * Records the current test's transactions, seeded by its full name, and once
 * it passes matches them against its snapshot: one line per transaction with
 * its lifecycle ordinal, full digest and step digest.
 */
export const pinStepTranscript = (): void => {
  const seed = expect.getState().currentTestName!;
  const recording = recordStepSubmissions(seed);
  onTestFinished(({ task, expect }) => {
    recording.restore();
    if (task.result?.state === "fail") return;
    expect(
      recording.submitted
        .map(
          ({ txHash, cbor, step }, index) =>
            `${index.toString()} ${txHash} ${cbor} ${step}`,
        )
        .join("\n"),
    ).toMatchSnapshot();
  });
};
