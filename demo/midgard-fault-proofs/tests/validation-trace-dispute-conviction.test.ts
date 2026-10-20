/** The validation-trace dispute runner convicting an undefended case, on the
 * long-maturity emulator profile so the challenged block is still in the
 * queue when the case's defense deadline passes: the runner opens its case,
 * waits out the deadline, and closes the case Convicted, so the fraud-proof
 * record names us, the bond and the case's remaining lovelace come back to
 * us and the case tokens are locked beside the record. That record then
 * removes the block and its descendants through the shared removal, driven
 * here directly: the runner's hand-off to it is not installed. A close whose lower bound is
 * not yet past the deadline is refused by the case validator. */
import * as SDK from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  Data,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import {
  type ConfirmedValidationTraceStage,
  confirmedValidationTraceStages,
} from "../src/validation-trace-dispute/actions.js";
import {
  VALIDATION_TRACE_CASE_CONVICTED,
  VALIDATION_TRACE_CASE_OPEN,
} from "../src/validation-trace-dispute/adapter.js";
import {
  parseValidationTraceDisputeArtifact,
  type ValidationTraceDisputeArtifact,
} from "../src/validation-trace-dispute/artifact.js";
import {
  buildValidationTraceClose,
  validationTraceFraudProofUnit,
} from "../src/validation-trace-dispute/close.js";
import { validationTraceDefenseDeadlineMs } from "../src/validation-trace-dispute/waiting.js";
import type { JournalJsonObject } from "../src/workflow/journal.js";
import { parseOwnOutRef } from "../src/workflow/own-outputs.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.js";
import { expectStateQueueHeaderOrder } from "./support/submit-init-emulator-fixtures.js";
import {
  network,
  publishRemovalReferenceScripts,
} from "./support/submit-init-emulator-shared.js";
import {
  BOND_LOVELACE,
  type Scene,
  setUpScene,
} from "./support/validation-trace-dispute-open-scene.js";
import {
  makeRunner,
  type Runner,
  runnerDeployment,
  runUntil,
} from "./support/validation-trace-dispute-runner.js";
import { resolvingRemovalAs } from "./support/validation-trace-removal.js";

let scene: Scene;
let runner: Runner;
let artifact: ValidationTraceDisputeArtifact;
let verified: ConfirmedValidationTraceStage;

const outRefOf = (utxo: UTxO) =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

const liveAt = async (outRef: string): Promise<UTxO | undefined> =>
  (await scene.h.proverLucid.utxosByOutRef([parseOwnOutRef(outRef)]))[0];

/** The confirmed close, once the runner has closed the case. */
let closed: ConfirmedValidationTraceStage;

describe("validation-trace dispute runner: conviction of an undefended case", () => {
  beforeAll(async () => {
    scene = await setUpScene();
    runner = makeRunner(scene);
    const { result } = await runUntil(
      runner,
      scene.failing,
      VALIDATION_TRACE_CASE_OPEN,
    );
    if (!("entries" in result)) throw new Error("no workflow journal");
    const prepared = result.entries.find(
      ({ event }) => event.kind === "prepared",
    );
    if (prepared?.event.kind !== "prepared") throw new Error("no artifact");
    // The journal keeps the family artifact inside the workflow envelope.
    artifact = parseValidationTraceDisputeArtifact(
      prepared.event.artifact.familyArtifact as JournalJsonObject,
    );
    verified = confirmedValidationTraceStages(result.entries).find(
      ({ action }) => action.stage === "verify_source",
    )!;
  }, 1_800_000);

  it("refuses a close whose lower bound is not past the defense deadline, at case_can_close", async () => {
    const { h } = scene;
    const lucid = h.proverLucid;
    const caseUtxo = (await liveAt(verified.outputs[0]!.outRef))!;
    const deadline = validationTraceDefenseDeadlineMs(
      Data.from(caseUtxo.datum!, SDK.Case),
    );
    // The last slot whose time is not past the deadline.
    h.emulator.awaitSlot(lucid.unixTimeToSlot(deadline) - lucid.currentSlot());
    const validFrom = lucid.slotToUnixTime(lucid.currentSlot());
    expect(validFrom).toBeLessThanOrEqual(deadline);
    expect(deadline - validFrom).toBeLessThan(1_000);
    const { tx } = await buildValidationTraceClose({
      lucid,
      signer: h.proverSigner,
      deployment: runnerDeployment(scene),
      artifact,
      caseOutRef: verified.outputs[0]!.outRef,
      bondOutRef: verified.outputs[1]!.outRef,
      validFrom,
    });
    await expectOnchainRefusal(() => tx.complete({ localUPLCEval: true }), {
      refusedBy: "fraud_proofs/validation_trace/case_v1",
      check: /case_can_close/u,
    });
    expect(await liveAt(verified.outputs[0]!.outRef)).toBeDefined();
  }, 300_000);

  it("closes the case Convicted once the deadline has passed: the record names us, and the bond and the case's lovelace come back", async () => {
    const { h } = scene;
    const caseUtxo = (await liveAt(verified.outputs[0]!.outRef))!;
    const bond = (await liveAt(verified.outputs[1]!.outRef))!;
    h.emulator.awaitSlot(1);
    const { result } = await runUntil(
      runner,
      scene.failing,
      VALIDATION_TRACE_CASE_CONVICTED,
    );
    const deployment = runnerDeployment(scene);
    const proofUnit = validationTraceFraudProofUnit(
      deployment,
      artifact.headerHash,
    );
    if (!("entries" in result)) throw new Error("no workflow journal");
    const close = confirmedValidationTraceStages(result.entries).find(
      ({ action }) => action.stage === "close",
    )!;
    closed = close;
    expect(close.action.consumes).toBe(outRefOf(caseUtxo));
    const record = close.outputs[0]!;
    if (!("reason" in result)) throw new Error("no reason");
    expect(result.reason).toContain(`${proofUnit} at ${record.outRef}`);

    const [recorded] = await h.proverLucid.utxosByOutRef([
      parseOwnOutRef(record.outRef),
    ]);
    expect(recorded!.address).toBe(
      h.contracts.fraudProof.spendingScriptAddress,
    );
    expect(recorded!.assets).toEqual({
      lovelace: 2_000_000n,
      [proofUnit]: 1n,
    });
    expect(
      Data.from(recorded!.datum!, SDK.FraudProofTokenDatum).fraud_prover,
    ).toBe(h.proverSigner.paymentKeyHash);

    // The bond and the case are spent; the bond comes back in full at
    // output 0, the case's lovelace less the record and the lock at 1.
    expect(await liveAt(outRefOf(caseUtxo))).toBeUndefined();
    expect(await liveAt(outRefOf(bond))).toBeUndefined();
    const ours = credentialToAddress(network, {
      type: "Key",
      hash: h.proverSigner.paymentKeyHash,
    });
    const [refund, remainder] = await h.proverLucid.utxosByOutRef([
      { txHash: close.txHash, outputIndex: 0 },
      { txHash: close.txHash, outputIndex: 1 },
    ]);
    expect(refund!.address).toBe(ours);
    expect(refund!.assets).toEqual({ lovelace: BOND_LOVELACE });
    expect(remainder!.address).toBe(ours);

    // The case NFT and the bond marker are locked beside the record; the
    // thread token is burned.
    const caseUnit = verified.outputs[0]!.unit;
    const markerUnit = verified.outputs[1]!.unit;
    const [locked] = await h.proverLucid.utxosAtWithUnit(
      h.contracts.fraudProof.spendingScriptAddress,
      caseUnit,
    );
    expect(locked!.assets[markerUnit]).toBe(1n);
    const threadUnit = toUnit(
      h.contracts.computationThread.policyId,
      deployment.init.category.categoryId + artifact.headerHash,
    );
    expect(
      await h.proverLucid.utxosAtWithUnit(
        h.contracts.fraudProof.spendingScriptAddress,
        threadUnit,
      ),
    ).toHaveLength(0);
  }, 600_000);

  it("removes the convicted block and its descendants through the shared removal, keeping the record", async () => {
    const { h } = scene;
    const removal = await publishRemovalReferenceScripts({
      lucid: h.proverLucid,
      contracts: h.contracts,
    });
    const now = BigInt(h.emulator.now());
    // The removal resolves its contracts from a deployment manifest this
    // scene does not have; it needs only the category id, as the fraud-proof
    // policy and address are shared by every category.
    await resolvingRemovalAs("validationTraceDispute", {}, () =>
      submitRemoveFraudulentBlock({
        lucid: h.proverLucid,
        blueprint: h.realBlueprint,
        deploymentInfo: buildRemovalDeploymentInfo(h.contracts, h.catalogue, {
          removalReferenceScripts: removal.published,
        }),
        network,
        signer: h.proverSigner,
        fraudCategory: "validationTraceDispute",
        fraudulentHeaderHash: artifact.headerHash,
        requireReferenceScripts: true,
        validFrom: now - 120_000n,
        validTo: now + 300_000n,
      }),
    );
    await expectStateQueueHeaderOrder({
      lucid: h.proverLucid,
      contracts: h.contracts,
      expectedHeaderHashes: [scene.failing.evidence.header.prevHeaderHash],
    });
    expect(await liveAt(closed.outputs[0]!.outRef)).toBeDefined();
  }, 600_000);
});
