/** Our staging outputs coming back to our wallet, what follows once those
 * spend backs roll back after the workflow's ending, and outputs left live
 * for a later proof-materials sweep. The ending waits on no spend back's
 * finality, and the process follows each spend back it sent until it sees
 * it land final: one a rollback drops is sent again, the same signed bytes,
 * and the process signs nothing new for its outputs; one it has seen land
 * final is no longer followed, and the next runs spend its outputs back
 * once more. */
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

/** Rolls back the landed spend back of each output of `outputs`, and
 * expects each live again. */
const rollBackSpendBacks = async (
  scene: Pick<Scene, "h" | "l1">,
  outputs: readonly UTxO[],
): Promise<readonly string[]> => {
  const staged = outputs.map(outRefOf);
  for (const spender of await expectBackInOurWallet(scene, staged)) {
    const cbor = scene.l1.signedCbors.get(spender)!;
    rollBackSpend(
      scene.h.emulator,
      spender,
      outputs.filter((utxo) => inputsOf(cbor).includes(outRefOf(utxo))),
      CML.Transaction.from_cbor_hex(cbor).body().outputs().len(),
    );
  }
  expect(liveStaged(scene, staged)).toStrictEqual([...staged].sort());
  return staged;
};

/**
 * After the ending of a workflow under reconciliation-only authority, with
 * each of `outputs` back in our wallet: rolls back each spend back, expects
 * each output live again, then runs `runOnce` `runs` times and expects
 * nothing new signed that spends any of them, each still live and selected
 * by a proof-materials view for a sweep. That authority broadcasts no
 * transaction again, and each spend back holds its coins while it may still
 * land, so the sweep each run takes up waits on them (`held`).
 */
export const expectLeftForTheSweepAfterRollback = async <Result>({
  scene,
  outputs,
  runOnce,
  describe,
  held,
  runs = 3,
}: {
  readonly scene: Pick<Scene, "h" | "l1">;
  /** The staging outputs as they stood before their spend backs. */
  readonly outputs: readonly UTxO[];
  readonly runOnce: () => Promise<Result>;
  readonly describe: (result: Result) => string;
  /** Whether `result` waits on a spend back of ours that may still land. */
  readonly held: (result: Result) => boolean;
  readonly runs?: number;
}): Promise<void> => {
  const staged = await rollBackSpendBacks(scene, outputs);
  const signed = new Set(scene.l1.signedCbors.keys());
  const after: Result[] = [];
  for (let run = 0; run < runs; run += 1) after.push(await runOnce());
  const history = after.map(describe).join("\n");
  const spending = [...scene.l1.signedCbors]
    .filter(([hash]) => !signed.has(hash))
    .filter(([, cbor]) =>
      inputsOf(cbor).some((outRef) => staged.includes(outRef)),
    )
    .map(([hash]) => hash);
  expect(spending, history).toStrictEqual([]);
  expect(liveStaged(scene, staged), history).toStrictEqual([...staged].sort());
  await expectSelectedForSweep(scene, staged);
  for (const result of after) expect(held(result), history).toBe(true);
};

/**
 * After the workflow's ending, with each of `outputs` back in our wallet:
 * rolls back each spend back and expects each output live again; then runs
 * `runOnce` at least `runs` times, and on until each output is spent again
 * and a run `ended` (at most 12 runs). Expects each output spent again by a
 * landed transaction of ours paying our wallet alone: its original spend
 * back, the same signed bytes the process still follows, with nothing new
 * signed for it; or, for an output of `renewed`, or one whose spend back
 * stood more than `releaseDepth` blocks under the tip when it rolled back,
 * which the process had seen land final, that spend back or the one new
 * spend back signed for it. The last run `ended`.
 */
export const expectSpentBackAgainAfterRollback = async <Result>({
  scene,
  outputs,
  runOnce,
  describe,
  ended,
  renewed = [],
  releaseDepth,
  runs = 3,
}: {
  readonly scene: Pick<Scene, "h" | "l1">;
  /** The staging outputs as they stood before their spend backs. */
  readonly outputs: readonly UTxO[];
  readonly runOnce: () => Promise<Result>;
  readonly describe: (result: Result) => string;
  /** Whether `result` ends the workflow as it ended before the rollback. */
  readonly ended: (result: Result) => boolean;
  /** The outputs whose spend back the process had seen land final. */
  readonly renewed?: readonly string[];
  /** How many blocks under the tip the process holds a transaction final;
   * a spend back that stood deeper when it rolled back was final to it.
   * By default no depth makes a spend back final. */
  readonly releaseDepth?: number;
  readonly runs?: number;
}): Promise<void> => {
  const staged = outputs.map(outRefOf);
  const original = new Map<string, string>();
  /** How many blocks under the tip each spend back stood when it rolled
   * back. */
  const depth = new Map<string, number>();
  for (const spender of await expectBackInOurWallet(scene, staged)) {
    const cbor = scene.l1.signedCbors.get(spender)!;
    const spent = inputsOf(cbor);
    for (const outRef of spent) original.set(outRef, spender);
    const status = await scene.h.emulator.getTransactionStatus(spender);
    if (status.status === "confirmed")
      depth.set(
        spender,
        scene.h.emulator.blockHeight - (status.confirmation.blockHeight ?? 0),
      );
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
    const first = original.get(outRef);
    const finalBefore =
      releaseDepth !== undefined &&
      first !== undefined &&
      (depth.get(first) ?? 0) > releaseDepth;
    if (renewed.includes(outRef) || finalBefore)
      expect(
        fresh.length === 0 || (fresh.length === 1 && fresh[0] === spender),
        `${outRef} spent back by ${String(spender)}, signed anew ${fresh.join(", ")}\n${history}`,
      ).toBe(true);
    else {
      const about = `${outRef} first spent back by ${String(original.get(outRef))} (${String(depth.get(original.get(outRef)!))} blocks under the tip when rolled back), now by ${String(spender)}\n${history}`;
      expect(fresh, about).toStrictEqual([]);
      expect(spender, about).toBe(original.get(outRef));
    }
  }
  expect(ended(last!), history).toBe(true);
};
