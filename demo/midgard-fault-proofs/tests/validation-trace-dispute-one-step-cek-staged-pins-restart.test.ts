/** The validation-trace dispute runner's hold on the staging outputs of a
 * staged builtin step (arm 31) across a restart, on
 * the emulator (support/validation-trace-dispute-one-step-staged-drive.ts):
 * both threads dispute the same step and read the same staging. A fresh
 * process started with a proof-materials view of its own knows nothing of the staging until it observes; its first observation, which
 * takes nothing (support/validation-trace-dispute-restart-probe.ts), holds
 * every live staging output a thread of ours needs from the chain and our
 * trace alone, so no proof-materials sweep selects one. While the threads
 * stand at the PrepareSelected move's holder, the outputs are held by the
 * runs their plans stage; once a thread stands mid-chain, by the out-refs
 * its move read. That no sweep opens before that first observation is the
 * view's own (proof-materials-view.test.ts). */
import type { UTxO } from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import {
  createProofMaterialsView,
  lucidProofMaterialsReader,
} from "../src/workflow/proof-materials-view.js";
import { emulatorSignedTransactionRecovery } from "./support/emulator-signed-recovery.js";
import {
  STAGED_BYTES_ARMS,
  stagedDrive,
} from "./support/validation-trace-dispute-one-step-staged-drive.js";
import { probeRestart } from "./support/validation-trace-dispute-restart-probe.js";
import { makeRunner } from "./support/validation-trace-dispute-runner.js";

/** How many blocks under the tip a move is final. */
const RELEASE_DEPTH = 6;

/** More than every output our proof-materials address holds here. */
const PAGE = 200;

const drive = stagedDrive({ releaseDepth: RELEASE_DEPTH });

/** The holders past the PrepareSelected move and short of the settle. */
const MID_CHAIN: readonly string[] = ["core", ...STAGED_BYTES_ARMS];

const labelOf = ({ txHash, outputIndex }: UTxO) =>
  `${txHash}#${outputIndex.toString()}`;

/** The staging outputs the ledger shows staged that are live. */
const liveStaged = async () => {
  const live: string[] = [];
  for (const { outRef } of await drive.stagedOnL1())
    if ((await drive.live(outRef)) !== undefined) live.push(outRef);
  return live;
};

/**
 * Starts a fresh process with a proof-materials view of its own, and expects each of `held` sweepable before its first observation
 * and held from it on; what that observation asks for.
 */
const expectHeldFromTheFirstObservation = async (held: readonly string[]) => {
  const { scene, deployment } = drive.dispute();
  const { h } = scene;
  const signer = {
    ...h.proverSigner,
    proofMaterials: createProofMaterialsView({
      address: h.proverSigner.proofMaterialsAddress,
      reader: lucidProofMaterialsReader(
        h.proverLucid,
        h.proverSigner.proofMaterialsAddress,
      ),
    }),
  };
  const sweepable = async () =>
    (await signer.proofMaterials.sweepable(PAGE)).map(labelOf);
  expect(await sweepable()).toEqual(expect.arrayContaining([...held]));
  const asked = await probeRestart(
    (wrap) =>
      makeRunner(scene, {
        deployment,
        wrap,
        prover: { lucid: h.proverLucid, signer },
        signedRecovery: emulatorSignedTransactionRecovery(
          h.emulator,
          RELEASE_DEPTH,
        ),
      }),
    scene.failing,
  );
  const after = await sweepable();
  for (const outRef of held) expect(after, outRef).not.toContain(outRef);
  return asked;
};

describe("validation-trace dispute runner: holding a staged step's staging outputs from the first observation after a restart", () => {
  beforeAll(drive.setUp, 1_800_000);

  it("holds the runs the plans of threads at the PrepareSelected move's holder stage, which no move has read yet", async () => {
    const runs = await drive.runs();
    await drive.play(
      async () => (await drive.stagings()).length >= runs.length,
    );
    expect((await drive.placeOfB())?.holder).toBe("prepare");
    const staged = await liveStaged();
    expect(staged.length).toBeGreaterThanOrEqual(runs.length);
    await expectHeldFromTheFirstObservation(staged);
    expect((await drive.placeOfB())?.holder).toBe("prepare");
  }, 1_200_000);

  it("holds the staging outputs a thread mid-chain read", async () => {
    await drive.play(async () =>
      MID_CHAIN.includes((await drive.placeOfB())?.holder ?? ""),
    );
    const place = await drive.placeOfB();
    const staged = await liveStaged();
    expect(staged.length).toBeGreaterThan(1);
    await expectHeldFromTheFirstObservation(staged);
    expect((await drive.placeOfB())?.utxo).toEqual(place?.utxo);
  }, 1_200_000);
});
