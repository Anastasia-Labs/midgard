/** The validation-trace dispute runner's hold on the staging outputs of a
 * staged builtin step (arm 31) while a thread that read them still needs
 * them, on the emulator (support/validation-trace-dispute-one-step-staged-drive.ts),
 * with each move final only `RELEASE_DEPTH` blocks under the tip: both
 * threads dispute the same step and read the same staging. While no thread
 * move builds, the runner waits and may spend back a staging output no
 * thread holds; a thread holds every output its moves read until its move
 * to the settle, which read them, is final at release depth. */
import { CML, coreToTxOutput } from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import { validationTraceOneStepHolder } from "../src/validation-trace-dispute/one-step-holders.js";
import { withLedgerParameters } from "./support/validation-trace-dispute-one-step-play.js";
import {
  STAGED_BYTES_ARMS,
  stagedDrive,
} from "./support/validation-trace-dispute-one-step-staged-drive.js";

/** How many blocks under the tip a move is final. */
const RELEASE_DEPTH = 6;

/** A transaction size limit under which no thread move builds (the
 * smallest, an arm's, is about 1,600 bytes) and a spend back of one staging
 * output (about 230 bytes) does. */
const NO_THREAD_MOVE = 1_000;

const drive = stagedDrive({ releaseDepth: RELEASE_DEPTH });

/** The holders past the PrepareSelected move and short of the settle. */
const MID_CHAIN: readonly string[] = ["core", ...STAGED_BYTES_ARMS];

/** The landed move that paid the thread `unit` to the settle, if any. */
const settleMoveOf = async (unit: string) => {
  const { scene, deployment, prepared } = drive.dispute();
  const settle = validationTraceOneStepHolder(deployment, "settle").address;
  for (const { stage, hash, cbor } of prepared) {
    if (stage !== "resolve") continue;
    const status = await scene.h.emulator.getTransactionStatus(hash);
    if (status.status !== "confirmed") continue;
    const outputs = CML.Transaction.from_cbor_hex(cbor).body().outputs();
    for (let index = 0; index < outputs.len(); index += 1) {
      const { address, assets } = coreToTxOutput(outputs.get(index));
      if (address === settle && assets[unit] === 1n) return hash;
    }
  }
  return undefined;
};

/** The staging outputs the ledger shows staged. */
const staged = async () =>
  (await drive.stagedOnL1()).map(({ outRef }) => outRef);

/** Plays `runs` runs while no thread move builds. */
const holdFor = async (runs: number) => {
  const { scene } = drive.dispute();
  let played = 0;
  await withLedgerParameters(
    scene,
    { maxTxSize: NO_THREAD_MOVE },
    async () => await drive.play(async () => (played += 1) >= runs, runs),
  );
};

describe("validation-trace dispute runner: holding a staged step's staging outputs while a thread that read them needs them", () => {
  beforeAll(drive.setUp, 1_800_000);

  it("keeps every staging output a thread held mid-chain read, past release depth, while no thread move builds", async () => {
    await drive.play(async () =>
      MID_CHAIN.includes((await drive.placeOfB())?.holder ?? ""),
    );
    const outputs = await staged();
    expect(outputs.length).toBeGreaterThan(1);
    const place = await drive.placeOfB();
    await holdFor(RELEASE_DEPTH + 2);
    for (const outRef of outputs)
      expect(await drive.live(outRef), outRef).toBeDefined();
    expect((await drive.placeOfB())?.utxo).toEqual(place?.utxo);
  }, 1_200_000);

  it("keeps them while the threads' moves to the settle are short of release depth, and spends each back once they are final", async () => {
    const { a, b } = drive.dispute();
    await drive.play(
      async () =>
        (await settleMoveOf(a.unit)) !== undefined &&
        (await settleMoveOf(b.unit)) !== undefined,
      60,
    );
    const outputs = await staged();
    const live = [];
    for (const outRef of outputs)
      if ((await drive.live(outRef)) !== undefined) live.push(outRef);
    expect(live.length).toBeGreaterThan(1);
    await holdFor(2);
    for (const outRef of live)
      expect(await drive.live(outRef), outRef).toBeDefined();

    await holdFor(RELEASE_DEPTH + 2);
    const settled = await Promise.all(
      [a.unit, b.unit].map(
        async (unit) => await drive.blockOf((await settleMoveOf(unit))!),
      ),
    );
    for (const outRef of live) {
      expect(await drive.live(outRef), outRef).toBeUndefined();
      const reclaim = await drive.spenderOf(outRef);
      expect(reclaim, outRef).toBeDefined();
      const landed = await drive.blockOf(reclaim!);
      for (const block of settled)
        expect(landed - block).toBeGreaterThanOrEqual(RELEASE_DEPTH);
    }
  }, 1_200_000);
});
