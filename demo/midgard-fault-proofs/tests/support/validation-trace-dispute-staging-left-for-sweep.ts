/** Our staging outputs coming back to our wallet, what follows once those
 * spend backs roll back after the workflow's ending, and outputs left live
 * for a later proof-materials sweep. The ending waits on no spend back's
 * finality. A spend back a rollback drops leaves its outputs live on the
 * ledger, and the next runs spend them back again from what the ledger
 * shows: the same signed bytes when the build comes out the same, or one
 * new spend back. */
import { parseOutRefLabel } from "@al-ft/midgard-core/out-ref";
import { CML, coreToTxOutput, type UTxO } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import {
  createProofMaterialsView,
  lucidProofMaterialsReader,
} from "../../src/workflow/proof-materials-view.js";
import { landedSpenderOf } from "./validation-trace-dispute-one-step-staged-drive.js";
import {
  onLedger,
  rollBackSpend,
} from "./validation-trace-dispute-one-step-staging-ledger.js";
import type { Scene } from "./validation-trace-dispute-open-scene.js";

const outRefOf = ({ txHash, outputIndex }: UTxO) =>
  `${txHash}#${outputIndex.toString()}`;

/** The out-refs the signed transaction `cbor` spends. */
const inputsOf = (cbor: string): readonly string[] => {
  const inputs = CML.Transaction.from_cbor_hex(cbor).body().inputs();
  return Array.from({ length: inputs.len() }, (_, index) => {
    const input = inputs.get(index);
    return `${input.transaction_id().to_hex()}#${input.index().toString()}`;
  });
};

/** The outputs of `staged` the ledger holds unspent, sorted. */
export const liveStaged = (
  scene: Pick<Scene, "h">,
  staged: readonly string[],
): readonly string[] =>
  staged.filter((outRef) => onLedger(scene, parseOutRefLabel(outRef))).sort();

/**
 * Expects each output of `staged` spent by a landed transaction of ours
 * that pays our wallet alone and spends nothing but our staging outputs;
 * those transactions.
 */
export const expectBackInOurWallet = async (
  scene: Pick<Scene, "h" | "l1">,
  staged: readonly string[],
): Promise<ReadonlySet<string>> => {
  const spenders = new Set<string>();
  for (const outRef of staged) {
    const spender = await landedSpenderOf(
      scene.l1.signedCbors,
      scene.h.emulator,
      outRef,
    );
    expect(spender, outRef).toBeDefined();
    spenders.add(spender!);
  }
  for (const spender of spenders) {
    const cbor = scene.l1.signedCbors.get(spender)!;
    expect(inputsOf(cbor).every((outRef) => staged.includes(outRef))).toBe(
      true,
    );
    const outputs = CML.Transaction.from_cbor_hex(cbor).body().outputs();
    expect(outputs.len()).toBe(1);
    expect(coreToTxOutput(outputs.get(0)).address).toBe(
      scene.h.proverSigner.address,
    );
  }
  return spenders;
};

/**
 * Expects a proof-materials view over our proof-materials address to select
 * each output of `staged` for a sweep.
 */
export const expectSelectedForSweep = async (
  scene: Pick<Scene, "h">,
  staged: readonly string[],
): Promise<void> => {
  const { proofMaterialsAddress } = scene.h.proverSigner;
  const selected = await createProofMaterialsView({
    address: proofMaterialsAddress,
    reader: lucidProofMaterialsReader(
      scene.h.proverLucid,
      proofMaterialsAddress,
    ),
  }).sweepable(100);
  expect(selected.map(outRefOf)).toEqual(expect.arrayContaining([...staged]));
};

/**
 * After the workflow's ending, with each of `outputs` back in our wallet:
 * rolls back each spend back and expects each output live again; then runs
 * `runOnce` at least `runs` times, and on until each output is spent again
 * and a run `ended` (at most 12 runs). Expects each output spent again by a
 * landed transaction of ours paying our wallet alone: its original spend
 * back, or the one new spend back signed for it. The last run `ended`.
 */
export const expectSpentBackAgainAfterRollback = async <Result>({
  scene,
  outputs,
  runOnce,
  describe,
  ended,
  runs = 3,
}: {
  readonly scene: Pick<Scene, "h" | "l1">;
  /** The staging outputs as they stood before their spend backs. */
  readonly outputs: readonly UTxO[];
  readonly runOnce: () => Promise<Result>;
  readonly describe: (result: Result) => string;
  /** Whether `result` ends the workflow as it ended before the rollback. */
  readonly ended: (result: Result) => boolean;
  readonly runs?: number;
}): Promise<void> => {
  const staged = outputs.map(outRefOf);
  const original = new Map<string, string>();
  for (const spender of await expectBackInOurWallet(scene, staged)) {
    const cbor = scene.l1.signedCbors.get(spender)!;
    const spent = inputsOf(cbor);
    for (const outRef of spent) original.set(outRef, spender);
    rollBackSpend(
      scene.h.emulator,
      spender,
      outputs.filter((utxo) => spent.includes(outRefOf(utxo))),
      CML.Transaction.from_cbor_hex(cbor).body().outputs().len(),
    );
  }
  expect(liveStaged(scene, staged)).toStrictEqual([...staged].sort());
  const signed = new Set(scene.l1.signedCbors.keys());
  const after: string[] = [];
  let last: Result | undefined;
  for (let run = 0; run < 12; run += 1) {
    if (run >= runs && ended(last!) && liveStaged(scene, staged).length === 0)
      break;
    const before = new Set(scene.l1.signedCbors.keys());
    last = await runOnce();
    const sent = [...scene.l1.signedCbors]
      .filter(([hash]) => !before.has(hash))
      .filter(([, cbor]) =>
        inputsOf(cbor).some((outRef) => staged.includes(outRef)),
      )
      .map(([hash, cbor]) => `${hash} spending ${inputsOf(cbor).join(", ")}`);
    after.push(
      [describe(last), ...sent.map((line) => `  signed ${line}`)].join("\n"),
    );
  }
  const history = after.join("\n");
  expect(liveStaged(scene, staged), history).toStrictEqual([]);
  await expectBackInOurWallet(scene, staged);
  for (const outRef of staged) {
    const spender = await landedSpenderOf(
      scene.l1.signedCbors,
      scene.h.emulator,
      outRef,
    );
    const fresh = [...scene.l1.signedCbors]
      .filter(([hash]) => !signed.has(hash))
      .filter(([, cbor]) => inputsOf(cbor).includes(outRef))
      .map(([hash]) => hash);
    expect(
      fresh.length === 0
        ? spender === original.get(outRef)
        : fresh.length === 1 && fresh[0] === spender,
      `${outRef} first spent back by ${String(original.get(outRef))}, now by ${String(spender)}, signed anew ${fresh.join(", ")}\n${history}`,
    ).toBe(true);
  }
  expect(ended(last!), history).toBe(true);
};
