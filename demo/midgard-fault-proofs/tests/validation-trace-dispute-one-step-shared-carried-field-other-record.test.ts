/** The validation-trace dispute runner with two threads of our case at the
 * same script-sources redeemer step, both reading our one publication of its
 * field (validation-trace-dispute-one-step-shared-carried-field.test.ts),
 * each transaction final only `RELEASE_DEPTH` blocks under the tip. Another
 * prover opens a case of its own on the block and leaves it undefended. Both
 * threads of ours play to their award and close while the test holds our
 * spend back of the publication, so it is still live; past both cases'
 * deadlines that prover removes the block against its own record, and the
 * removal goes past rollback recovery before our run is let on. Our run then
 * reads the block as removed against another record: it spends the
 * publication back by key, and ends resolved by that record only once the
 * spend back is final at release depth (carried-field.ts). */
import * as SDK from "@al-ft/midgard-sdk";
import { Data, generateEmulatorAccount, Lucid } from "@lucid-evolution/lucid";
import { afterAll, beforeAll, describe, expect, it, vi } from "vitest";

import { resolveProverSigner } from "../src/index.js";
import { confirmedValidationTraceStages } from "../src/validation-trace-dispute/actions.js";
import { VALIDATION_TRACE_CASE_OPEN } from "../src/validation-trace-dispute/adapter.js";
import {
  VALIDATION_TRACE_COPY_OUTSTANDING,
  VALIDATION_TRACE_COPY_RECLAIM,
} from "../src/validation-trace-dispute/carried-field.js";
import { VALIDATION_TRACE_SCRIPT_SOURCES_REDEEMER_INLINE_MAX_PREIMAGE_BYTES } from "../src/validation-trace-dispute/one-step-phase-evidence.js";
import { validationTraceDefenseDeadlineMs } from "../src/validation-trace-dispute/waiting.js";
import type { FraudProofWorkflowRunResult } from "../src/workflow/orchestrator.fraud-proof-workflow-run-result.js";
import type { FraudProofFamilyWorkflowAdapter } from "../src/workflow/orchestrator.js";
import { parseOwnOutRef } from "../src/workflow/own-outputs.js";
import { network } from "./support/submit-init-emulator-shared.js";
import {
  blockOf,
  copiesOf,
  expectReclaimedByKey,
  readersOf,
  spenderOf,
} from "./support/validation-trace-dispute-observe-copies.js";
import {
  awaitLastSlotAtOrBefore,
  defendTwoThreads,
  liveCase,
  threadClosed,
  type TwoThreadCase,
} from "./support/validation-trace-dispute-one-step-case.js";
import { playThreads } from "./support/validation-trace-dispute-one-step-play.js";
import { redeemerDataForFieldPreimage } from "./support/validation-trace-dispute-redeemer-field.js";
import { recordedRemoval } from "./support/validation-trace-dispute-removal-l1.js";
import {
  makeRunner,
  type Runner,
  runUntil,
} from "./support/validation-trace-dispute-runner.js";
import { RELEASE_FINALITY_POLICY } from "./workflow.make-adapter.js";

const BYTES =
  VALIDATION_TRACE_SCRIPT_SOURCES_REDEEMER_INLINE_MAX_PREIMAGE_BYTES + 1;

/** The state both threads' traces agree with ours up to. */
const STEP = 48;

/** How many blocks under the tip a transaction of ours is final. */
const RELEASE_DEPTH = 4;

/** How deep a removal is past rollback recovery. */
const FINAL_DEPTH = RELEASE_FINALITY_POLICY.automaticRecoveryMaxDepth + 2;

const SPEND_BACK_HELD = "the publication's spend back is held by the test";

let dispute: TwoThreadCase;
let theirs: Runner;
let theirCase: string;
let copy: string;
/** Whether the test lets our spend back of the publication go. */
let released = false;
let clock: { readonly mockRestore: () => void } | undefined;

const describeRun = (result: FraudProofWorkflowRunResult) =>
  "reason" in result ? `${result.kind}: ${result.reason}` : result.kind;

/** Holds the spend back of a publication of ours, as a wait, until the test
 * lets it go. */
const holding = (
  adapter: FraudProofFamilyWorkflowAdapter,
): FraudProofFamilyWorkflowAdapter => ({
  ...adapter,
  observe: async (context) => {
    const observed = await adapter.observe(context);
    return !released &&
      observed.kind === "action_required" &&
      observed.action.input.schemaVersion === VALIDATION_TRACE_COPY_RECLAIM
      ? { kind: "pending", reason: SPEND_BACK_HELD }
      : observed;
  },
});

/** A runner of another prover, funded from ours, with its own wallet and
 * key. */
const anotherProver = async (): Promise<Runner> => {
  const { scene, recorder } = dispute;
  const { h } = scene;
  const signer = resolveProverSigner({
    network,
    walletSeedPhrase: generateEmulatorAccount({ lovelace: 0n }).seedPhrase,
  });
  const funding = await h.proverLucid
    .newTx()
    .pay.ToAddress(signer.address, { lovelace: 15_000_000_000n })
    .pay.ToAddress(signer.address, { lovelace: 5_000_000_000n })
    .complete({ localUPLCEval: true });
  await h.proverLucid.awaitTx(
    await (await funding.sign.withWallet().complete()).submit(),
  );
  const lucid = await Lucid(h.emulator, "Custom");
  signer.selectWallet(lucid);
  const theirRemoval = await recordedRemoval(
    scene,
    recorder!,
    signer.paymentKeyHash,
  );
  return makeRunner(scene, {
    prover: { lucid, signer },
    removal: theirRemoval.removal,
    terminalVerifier: theirRemoval.terminalVerifier(
      scene.failing.evidence.headerHash,
    ),
  });
};

/** Past both cases' deadlines, the other prover convicts its case and
 * removes the block against its record, and the removal goes past rollback
 * recovery; that record. */
const removeByTheirs = async (): Promise<string> => {
  const { h, failing } = dispute.scene;
  const [their] = await h.proverLucid.utxosByOutRef([
    parseOwnOutRef(theirCase),
  ]);
  const ours = await liveCase(dispute).catch(() => undefined);
  const deadline = Math.max(
    ...[ours?.datum, their!.datum].flatMap((datum) =>
      datum == null
        ? []
        : [validationTraceDefenseDeadlineMs(Data.from(datum, SDK.Case))],
    ),
  );
  if (h.emulator.now() <= deadline)
    awaitLastSlotAtOrBefore(dispute.scene, deadline + 1_000);
  // The removal dates its validity window from the application clock, which
  // on the emulator is the emulator's.
  clock ??= vi.spyOn(Date, "now").mockImplementation(() => h.emulator.now());
  const history: string[] = [];
  for (let attempt = 0; attempt < 16; attempt += 1) {
    const outcome = await theirs.runOnce(failing);
    history.push(describeRun(outcome));
    if (outcome.kind === "terminal_included" && "terminal" in outcome) {
      h.emulator.awaitBlock(FINAL_DEPTH);
      return outcome.terminal.correction.referencedProofTokenOutRef;
    }
  }
  throw new Error(`their removal did not land:\n${history.join("\n")}`);
};

describe("validation-trace dispute runner: a publication two threads read, once another prover removes the block against its own record", () => {
  beforeAll(async () => {
    dispute = await defendTwoThreads({
      withRemoval: true,
      phases: true,
      operatorFrom: STEP,
      successorFrom: STEP,
      failingRedeemerData: redeemerDataForFieldPreimage(BYTES),
      releaseDepth: RELEASE_DEPTH,
      wrap: () => holding,
    });
    theirs = await anotherProver();
    const { result } = await runUntil(
      theirs,
      dispute.scene.failing,
      VALIDATION_TRACE_CASE_OPEN,
    );
    if (!("entries" in result)) throw new Error("no workflow journal");
    theirCase = confirmedValidationTraceStages(result.entries).find(
      ({ action }) => action.stage === "verify_source",
    )!.outputs[0]!.outRef;
  }, 1_800_000);

  afterAll(() => {
    clock?.mockRestore();
    dispute?.recorder?.restore();
  });

  it("plays both threads through the reader to their award and close over one publication, its spend back held", async () => {
    const { scene, runner, a, b } = dispute;
    await playThreads({
      scene,
      runner,
      sides: [a, b],
      until: async () =>
        (await threadClosed(dispute, a.unit)) &&
        (await threadClosed(dispute, b.unit)),
      limit: 160,
    });
    const copies = copiesOf(scene, BYTES);
    expect(copies).toHaveLength(1);
    copy = copies[0]!;
    expect(readersOf(scene, copy)).toHaveLength(2);
    expect(spenderOf(scene, copy)).toBeUndefined();
  }, 1_800_000);

  it("spends the publication back by key and ends resolved by their record only once the spend back is final at release depth", async () => {
    const theirRecord = await removeByTheirs();
    released = true;
    const { scene, runner } = dispute;
    const runs: { result: FraudProofWorkflowRunResult; observedAt: number }[] =
      [];
    let resolved: FraudProofWorkflowRunResult | undefined;
    while (resolved === undefined) {
      expect(
        runs.length,
        runs.map(({ result }) => describeRun(result)).join("\n"),
      ).toBeLessThan(60);
      const observedAt = scene.h.emulator.blockHeight;
      const result = await runner.runOnce(scene.failing);
      runs.push({ result, observedAt });
      if (result.kind === "resolved_by_another_record") resolved = result;
    }
    if (resolved.kind !== "resolved_by_another_record")
      throw new Error("not resolved");
    expect(resolved.resolution.referencedProofTokenOutRef).toBe(theirRecord);
    const spender = spenderOf(scene, copy);
    expect(spender, "the publication is spent back").toBeDefined();
    expectReclaimedByKey(scene, spender!);
    const { observedAt } = runs.at(-1)!;
    expect(
      observedAt - (await blockOf(scene, spender!.hash)),
    ).toBeGreaterThanOrEqual(RELEASE_DEPTH);
    // Our run waited on the spend back before it let the resolution go.
    expect(
      runs.some(
        ({ result }) =>
          result.kind === "pending" &&
          result.reason.startsWith(VALIDATION_TRACE_COPY_OUTSTANDING),
      ),
    ).toBe(true);
  }, 1_800_000);
});
