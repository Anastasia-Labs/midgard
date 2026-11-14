/** The spend back of our staging outputs (arm 31) under the watcher's prover
 * funding once the challenged header has rolled back, over the scene of
 * support/validation-trace-dispute-funded-one-step-staging.ts: our workflow
 * staged each run of the step and played its thread to the award, and each
 * staging output is still live when the header rolls back. Every later turn
 * runs under the reconciliation-only permit the watcher derives from the
 * workflow it recorded.
 *
 * Under that permit the funding permit signs only a spend back of a staging
 * output this reservation's journal shows staged, paying our wallet alone.
 * It refuses a spend back that also spends our wallet's coin or another
 * party's, pays an output besides our wallet, mints, or carries another
 * funding authority beside its own, and a spend back of an output at our
 * staging address that no staging of this reservation left, ours or
 * another party's; each before any byte reaches the ledger. The genuine
 * spend backs then return each staging output of ours to our wallet, and
 * the outputs no staging of ours left stay where they are. */
import { CML, coreToTxOutput, type UTxO } from "@lucid-evolution/lucid";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import { VALIDATION_TRACE_STAGING_RECLAIM } from "../src/validation-trace-dispute/one-step-staging-journal.js";
import { grantValidationDisputeMoveAuthority } from "../src/workflow/funding-reservation-permit.assert-validation-dispute-move.js";
import { topUpProver } from "./support/installed-prover-wallet.js";
import { watchLedger } from "./support/own-outputs-installed.js";
import {
  fundedStagingScene,
  runs,
} from "./support/validation-trace-dispute-funded-one-step-staging.js";
import { outRefOf } from "./support/validation-trace-dispute-funded-one-step-turns.js";
import type { AdapterWrap } from "./support/validation-trace-dispute-funded-runner.js";
import {
  ADA,
  TIMEOUT,
} from "./support/validation-trace-dispute-funded-verify-source.js";
import {
  confirmedOneStepMoves,
  playThreads,
} from "./support/validation-trace-dispute-one-step-play.js";
import {
  entriesOf,
  landedSpenderOf,
} from "./support/validation-trace-dispute-one-step-staged-drive.js";
import {
  minting,
  payingAlso,
  presentReclaim,
  reclaimInstead,
  type ReclaimPresentation,
  spendingAlso,
} from "./support/validation-trace-dispute-tampered-reclaim.js";

const { at, turns, open, stagingAddress, stagedOutputs, depositOf } =
  fundedStagingScene();
/** The staging outputs the funded play staged, one per run. */
const stagedOutRefs: string[] = [];
/** The outputs at our staging address no staging of ours left. */
const unstagedOutRefs: string[] = [];

const FAILED =
  "funding reservation failed for validation-trace-reclaim-staging:";
const UNRESERVED = "signed transaction consumed an unreserved wallet input";
const BESIDE_STAGING =
  "validation dispute staging spend back spends an input besides its staging output";
const ELSEWHERE =
  "validation dispute staging pays an output besides its staging output and our wallet";
const MINTS = "validation dispute staging mints under the staging authority";
const BESIDE_AUTHORITY =
  "funding transaction carries a validation dispute staging authority beside another funding authority";
const LINEAGE =
  "validation dispute staging spend back lacks the lineage of a staging of ours";

/** Holds each spend back our workflow would take, as a wait. */
const holdingSpendBacks: AdapterWrap = (adapter) => ({
  ...adapter,
  observe: async (context) => {
    const observed = await adapter.observe(context);
    return observed.kind === "action_required" &&
      observed.action.input.schemaVersion === VALIDATION_TRACE_STAGING_RECLAIM
      ? { kind: "pending", reason: "spend backs held by the test" }
      : observed;
  },
});

/** A coin of `lovelace` the funder pays to `address`, with the inline datum
 * `datum` when given. */
const funderPays = async (
  address: string,
  lovelace: bigint,
  datum?: string,
): Promise<UTxO> => {
  const lucid = at.scene.h.funderLucid;
  const signed = await (
    await (
      datum === undefined
        ? lucid.newTx().pay.ToAddress(address, { lovelace })
        : lucid.newTx().pay.ToContract(
            address,
            { kind: "inline", value: datum },
            {
              lovelace,
            },
          )
    ).complete()
  ).sign
    .withWallet()
    .complete();
  const hash = await signed.submit();
  await lucid.awaitTx(hash);
  const [output] = await lucid.utxosByOutRef([
    { txHash: hash, outputIndex: 0 },
  ]);
  if (output?.address !== address)
    throw new Error("the funder's coin is not at output 0");
  return output;
};

/** An output at our staging address holding the first run's datum and
 * deposit, paid by our own key from a coin no reservation holds. */
const ownUnstagedOutput = async (): Promise<UTxO> => {
  const { h } = at.scene;
  const coin = await funderPays(
    h.proverSigner.address,
    depositOf(runs[0]!.datumCbor) + 5n * ADA,
  );
  const signed = await (
    await h.proverLucid
      .newTx()
      .collectFrom([coin])
      .pay.ToContract(
        stagingAddress(),
        { kind: "inline", value: runs[0]!.datumCbor },
        { lovelace: depositOf(runs[0]!.datumCbor) },
      )
      .complete({ coinSelection: false })
  ).sign
    .withWallet()
    .complete();
  const hash = await signed.submit();
  await h.proverLucid.awaitTx(hash);
  const [output] = await h.proverLucid.utxosByOutRef([
    { txHash: hash, outputIndex: 0 },
  ]);
  if (output?.address !== stagingAddress())
    throw new Error("the staging output is not at output 0");
  return output;
};

/**
 * One turn under `wrap`, which the funding permit must refuse with
 * `refusal` before any byte reaches the ledger: the staging outputs stay
 * live and nothing of ours lands.
 */
const expectRefused = async (wrap: AdapterWrap, refusal: string) => {
  const before = (await stagedOutputs()).map(outRefOf).sort();
  const watch = watchLedger(at.scene.h.emulator);
  let result;
  try {
    result = await turns(wrap).runOnce();
  } finally {
    watch.restore();
  }
  expect(
    result.kind,
    "reason" in result ? `${result.kind}: ${result.reason}` : result.kind,
  ).toBe("stalled");
  if (result.kind !== "stalled") return;
  expect(result.reason.startsWith(FAILED), result.reason).toBe(true);
  expect(result.reason.endsWith(`: ${refusal}`), result.reason).toBe(true);
  expect(watch.ours()).toHaveLength(0);
  expect((await stagedOutputs()).map(outRefOf).sort()).toStrictEqual(before);
};

/** Our deployment and the challenged header, as the staging builder grants
 * its authority under them. */
const terms = () => ({
  deploymentFingerprint: at.dispute.deployment.deploymentFingerprint,
  headerHash: at.scene.failing.evidence.headerHash,
});

describe("validation-trace dispute spend back of our staging outputs under reconciliation-only funding after a header rollback", () => {
  beforeAll(open, 1_800_000);

  afterAll(async () => {
    await at.dispute?.close();
  });

  it(
    "plays the thread through the staged chain to the award with each staging output live, then the header rolls back",
    async () => {
      await topUpProver(at.scene.h as never, 400n * ADA);
      const { result, history } = await playThreads({
        scene: at.scene,
        runner: turns(holdingSpendBacks),
        sides: [at.side],
        until: async (run) =>
          confirmedOneStepMoves(entriesOf(run)).some(
            ({ stage }) => stage === "close_awarded",
          ),
        limit: 60,
      });
      for (const { event } of entriesOf(result))
        if (
          event.kind === "submission_intent" &&
          event.actionInput.stage === "stage_operands"
        ) {
          const outputs = CML.Transaction.from_cbor_hex(
            at.scene.l1.signedCbors.get(event.txHash)!,
          )
            .body()
            .outputs();
          for (let index = 0; index < outputs.len(); index += 1)
            if (coreToTxOutput(outputs.get(index)).address === stagingAddress())
              stagedOutRefs.push(
                outRefOf({ txHash: event.txHash, outputIndex: index }),
              );
        }
      expect(stagedOutRefs, history.join("\n")).toHaveLength(runs.length);
      expect((await stagedOutputs()).map(outRefOf).sort()).toStrictEqual(
        [...stagedOutRefs].sort(),
      );
      await at.dispute.rollBackHeader();
    },
    TIMEOUT,
  );

  it.each([
    {
      name: "also spends a coin of our wallet",
      refusal: UNRESERVED,
      present: async (): Promise<ReclaimPresentation> => {
        const [coin] = await at.scene.h.emulator.getUtxos(
          at.scene.h.proverSigner.address,
        );
        return { edit: spendingAlso(coin!) };
      },
    },
    {
      name: "also spends another party's coin",
      refusal: BESIDE_STAGING,
      present: async (): Promise<ReclaimPresentation> => ({
        edit: spendingAlso(
          await funderPays(
            await at.scene.h.funderLucid.wallet().address(),
            5n * ADA,
          ),
        ),
      }),
    },
    {
      name: "pays an output at a governed custody contract besides our wallet",
      refusal: ELSEWHERE,
      present: async (): Promise<ReclaimPresentation> => ({
        edit: payingAlso(
          at.scene.chain.disputeBond.spendingScriptAddress,
          BigInt(at.scene.h.emulator.protocolParameters.coinsPerUtxoByte),
        ),
      }),
    },
    {
      name: "pays an output at another party's wallet besides our wallet",
      refusal: ELSEWHERE,
      present: async (): Promise<ReclaimPresentation> => ({
        edit: payingAlso(
          await at.scene.h.funderLucid.wallet().address(),
          BigInt(at.scene.h.emulator.protocolParameters.coinsPerUtxoByte),
        ),
      }),
    },
    {
      name: "mints under our staging script",
      refusal: MINTS,
      present: async (): Promise<ReclaimPresentation> => ({
        edit: minting(at.scene.h.proverSigner.paymentKeyHash),
      }),
    },
    {
      name: "carries a thread move authority beside its own",
      refusal: BESIDE_AUTHORITY,
      present: async (): Promise<ReclaimPresentation> => ({
        beside: (signed, staging) =>
          grantValidationDisputeMoveAuthority({
            signed,
            kind: "reveal",
            ...terms(),
            caseOpenerOutRef: outRefOf(staging),
            defenderVkh: at.scene.h.proverSigner.paymentKeyHash,
            thread: staging,
          }),
      }),
    },
  ])(
    "refuses, under the reconciliation-only permit, a spend back of ours that $name, before any byte reaches the ledger",
    async ({ refusal, present }) => {
      await expectRefused(presentReclaim(at.scene, await present()), refusal);
    },
    TIMEOUT,
  );

  it.each([
    {
      name: "another party paid",
      output: async () =>
        await funderPays(
          stagingAddress(),
          depositOf(runs[0]!.datumCbor),
          runs[0]!.datumCbor,
        ),
    },
    {
      name: "our key paid outside this reservation",
      output: ownUnstagedOutput,
    },
  ])(
    "refuses, under the reconciliation-only permit, a spend back of an output at our staging address that $name, no staging of this reservation left, before any byte reaches the ledger",
    async ({ output }) => {
      const unstaged = await output();
      unstagedOutRefs.push(outRefOf(unstaged));
      await expectRefused(reclaimInstead(at.scene, unstaged, terms()), LINEAGE);
      expect(
        await at.scene.h.emulator.getUtxosByOutRef([unstaged]),
      ).toHaveLength(1);
    },
    TIMEOUT,
  );

  it(
    "spends each staging output of ours back to our wallet under the reconciliation-only permit, and no output at our staging address that no staging of ours left",
    async () => {
      const live = async () => (await stagedOutputs()).map(outRefOf).sort();
      const history: string[] = [];
      for (let turn = 0; turn < 30; turn += 1) {
        const result = await turns().runOnce();
        history.push(
          "reason" in result ? `${result.kind}: ${result.reason}` : result.kind,
        );
        if ((await live()).every((outRef) => !stagedOutRefs.includes(outRef)))
          break;
      }
      expect(await live(), history.join("\n")).toStrictEqual(
        [...unstagedOutRefs].sort(),
      );
      const reclaims = await Promise.all(
        stagedOutRefs.map(
          async (outRef) =>
            await landedSpenderOf(
              at.scene.l1.signedCbors,
              at.scene.h.emulator,
              outRef,
            ),
        ),
      );
      expect(reclaims.every((txHash) => txHash !== undefined)).toBe(true);
      for (const txHash of reclaims as string[]) {
        const body = CML.Transaction.from_cbor_hex(
          at.scene.l1.signedCbors.get(txHash)!,
        ).body();
        expect(body.inputs().len()).toBe(1);
        expect(body.outputs().len()).toBe(1);
        expect(body.mint()).toBeUndefined();
        expect(coreToTxOutput(body.outputs().get(0)).address).toBe(
          at.scene.h.proverSigner.address,
        );
        console.info(`reconciliation spend back fee ${body.fee().toString()}`);
      }
    },
    TIMEOUT,
  );
});
