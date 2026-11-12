/** Seeded committed blocks for the event-ledger differential test: normal
 * transactions with one drawn fault, and committed withdrawals for the
 * input-existence chronologies. */
import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { canonicalCommittedWithdrawalTransitionEffect } from "@al-ft/midgard-validation";
import { makeOutput } from "@al-ft/midgard-validation/tests/validation-fixtures";
import { Data } from "@lucid-evolution/lucid";

import { ledgerEntriesRoot, type LedgerEntry } from "../../src/ledger-at.js";
import {
  buildFixtureTransaction,
  type FixtureTransaction,
  outRefCbor,
} from "./canonical-block-evidence-fixture.js";
import { committedL2BlockFixture } from "./committed-l2-block-fixture.js";
import type { CommittedChronologyWithdrawal } from "./input-existence-chronology-fixture.js";
import { CHRONOLOGY_OWNER_KEY_HASH } from "./input-existence-chronology-seed.js";

/** Mulberry32: a fixed seed gives the same draws on every run. */
export const seeded = (seed: number) => {
  let state = seed >>> 0;
  return (bound: number) => {
    state = (state + 0x6d2b79f5) >>> 0;
    let t = state;
    t = Math.imul(t ^ (t >>> 15), t | 1);
    t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
    return ((t ^ (t >>> 14)) >>> 0) % bound >>> 0;
  };
};
export type Draw = ReturnType<typeof seeded>;

export const bytes = (draw: Draw, length: number) =>
  Buffer.from(Array.from({ length }, () => draw(256)));
export const shuffled = <T>(draw: Draw, items: readonly T[]): T[] => {
  const out = [...items];
  for (let index = out.length - 1; index > 0; index -= 1) {
    const other = draw(index + 1);
    [out[index], out[other]] = [out[other]!, out[index]!];
  }
  return out;
};
export const outRef = (txId: Buffer | string, outputIndex = 0) =>
  encodeMidgardSpendInputItem({
    txId: typeof txId === "string" ? Buffer.from(txId, "hex") : txId,
    outputIndex,
  });
export const hex = (buffer: Uint8Array) => Buffer.from(buffer).toString("hex");

const plainOutput = Buffer.from(
  "a200581d70aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a0",
  "hex",
);
/** The plain output with a native script_ref whose body does not decode: a
 * canonical entry under its committed undecodable-native descriptor. */
export const undecodableNativeOutput = Buffer.concat([
  Buffer.from("a3", "hex"),
  plainOutput.subarray(1),
  Buffer.from("03", "hex"),
  Buffer.from("8200428109", "hex"),
]);
const OUTPUTS = [
  makeOutput(2_000_000n),
  makeOutput(3_000_000n),
  plainOutput,
  undecodableNativeOutput,
];
/** An output that is not a canonical ledger entry. */
const NON_CANONICAL_OUTPUT = Buffer.from("a0", "hex");

export const FAULTS = [
  "none",
  "absent_input",
  "repeated_input",
  "rejected",
  "output_present",
  "foreign_post_root",
  "foreign_pre_root",
  "non_canonical_output",
] as const;
export type Fault = (typeof FAULTS)[number];

/**
 * A committed block of 1-7 normal transactions over 0-10 predecessor
 * entries, spending what the ledger holds and paying outputs from `OUTPUTS`,
 * with `fault` at a drawn step. The operator's roots are its lenient replay's:
 * a spend of an absent input and a rejected transaction change nothing, an
 * output overwrites, and a ledger with no canonical root gets a drawn one.
 */
export const seededBlock = async (seed: number, fault: Fault) => {
  const draw = seeded(seed);
  let predecessorEntries: LedgerEntry[] = Array.from(
    { length: draw(11) },
    () => ({
      outRef: outRef(bytes(draw, 32), draw(3)),
      output: OUTPUTS[draw(4)]!,
    }),
  );
  const count = 1 + draw(7);
  const faultStep =
    fault === "foreign_pre_root"
      ? 1 + draw(Math.max(count - 1, 1))
      : draw(count);
  const fresh = () => outRef(bytes(draw, 32));
  const live = new Set(predecessorEntries.map(({ outRef: key }) => hex(key)));
  const spent: string[] = [];
  const planned: {
    tx: FixtureTransaction;
    inputs: Buffer[];
    outputs: Buffer[];
    valid: boolean;
  }[] = [];
  for (let index = 0; index < count; index += 1) {
    const atFault = index === faultStep;
    const pool = shuffled(draw, [...live]);
    const inputs = pool.slice(0, Math.min(pool.length, 1 + draw(2)));
    if (atFault && fault === "absent_input") inputs.push(hex(fresh()));
    if (atFault && fault === "repeated_input")
      inputs.push(spent.length > 0 ? spent[draw(spent.length)]! : hex(fresh()));
    if (inputs.length === 0) inputs.push(hex(fresh()));
    const outputs = Array.from({ length: draw(3) }, () => OUTPUTS[draw(4)]!);
    if (
      atFault &&
      (fault === "output_present" || fault === "non_canonical_output") &&
      outputs.length === 0
    )
      outputs.push(OUTPUTS[draw(4)]!);
    if (atFault && fault === "non_canonical_output")
      outputs[draw(outputs.length)] = NON_CANONICAL_OUTPUT;
    const valid = !(atFault && fault === "rejected");
    const spendInputs = inputs.map((key) => Buffer.from(key, "hex"));
    const tx = buildFixtureTransaction({
      ...(valid ? {} : { validity: "TxIsInvalid" as const }),
      spendInputs,
      outputs,
      fee: BigInt(seed * 16 + index + 1),
    });
    planned.push({ tx, inputs: spendInputs, outputs, valid });
    if (!valid) continue;
    for (const key of inputs) if (live.delete(key)) spent.push(key);
    for (const outputIndex of outputs.keys())
      live.add(hex(outRef(tx.txId, outputIndex)));
  }
  if (fault === "output_present")
    predecessorEntries = [
      ...predecessorEntries,
      {
        outRef: outRef(planned[faultStep]!.tx.txId),
        output: OUTPUTS[draw(4)]!,
      },
    ];
  // The operator's lenient replay, and the roots it commits.
  const ledger = new Map(
    predecessorEntries.map((entry) => [hex(entry.outRef), entry]),
  );
  const roots = [await ledgerEntriesRoot(predecessorEntries)];
  for (const { tx, inputs, outputs, valid } of planned) {
    if (valid) {
      for (const key of inputs) ledger.delete(hex(key));
      for (const [outputIndex, output] of outputs.entries()) {
        const key = outRef(tx.txId, outputIndex);
        ledger.set(hex(key), { outRef: key, output });
      }
    }
    roots.push(
      await ledgerEntriesRoot([...ledger.values()]).catch(() =>
        hex(bytes(draw, 32)),
      ),
    );
  }
  const steps = planned.map((_, index) => ({
    pre: roots[index]!,
    post: roots[index + 1]!,
  }));
  const foreign = hex(bytes(draw, 32));
  if (fault === "foreign_post_root") {
    // The next step links from the foreign root, so the step fails its own
    // post-state check rather than the next one's link.
    steps[faultStep] = { ...steps[faultStep]!, post: foreign };
    if (faultStep + 1 < steps.length)
      steps[faultStep + 1] = { ...steps[faultStep + 1]!, pre: foreign };
  }
  if (fault === "foreign_pre_root" && faultStep < steps.length)
    steps[faultStep] = { ...steps[faultStep]!, pre: foreign };
  const txs = planned.map(({ tx }) => tx);
  const block = await committedL2BlockFixture({
    predecessorEntries,
    txs,
    steps,
  });
  const probes = [
    ...predecessorEntries.map(({ outRef: key }) => key),
    ...planned.flatMap(({ tx, inputs, outputs }) => [
      ...inputs,
      ...outputs.map((_, outputIndex) => outRef(tx.txId, outputIndex)),
    ]),
    fresh(),
  ];
  return {
    ...block,
    probes: [...new Map(probes.map((key) => [hex(key), key])).values()],
  };
};

/** A committed withdrawal of `byte…byte#0`: valid, so it spends that
 * output, or committed with a rejected signature, so it changes nothing.
 * The walk reads only its committed verdict and out-ref. */
export const withdrawalOf = (byte: number, valid: boolean) => {
  const id: SDK.OutputReference = {
    transactionId: "d5".repeat(32),
    outputIndex: 0n,
  };
  const info: SDK.WithdrawalInfo = {
    body: {
      l2_outref: {
        transactionId: byte.toString(16).repeat(32),
        outputIndex: 0n,
      },
      l2_owner: CHRONOLOGY_OWNER_KEY_HASH,
      l2_value: new Map([["", new Map([["", 2_000_000n]])]]),
      l1_address: {
        paymentCredential: { PublicKeyCredential: ["d7".repeat(28)] },
        stakeCredential: null,
      },
      l1_datum: "NoDatum",
    },
    signature: ["d8".repeat(32), "d9".repeat(64)],
    validity: valid ? "WithdrawalIsValid" : "IncorrectWithdrawalSignature",
  };
  const committed: CommittedChronologyWithdrawal = {
    eventKey: { WithdrawalEventKey: { withdrawal_id: id } },
    rootKey: Buffer.from(Data.to(id, SDK.OutputReference), "hex"),
    rootValue: Buffer.from(SDK.committedWithdrawalValueBytes(info), "hex"),
    effect: canonicalCommittedWithdrawalTransitionEffect({
      committedValid: valid,
      outRefCbor: outRefCbor(byte, 0n),
    }),
  };
  return { seed: byte * 2 + (valid ? 0 : 1), committed };
};
