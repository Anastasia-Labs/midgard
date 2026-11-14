/**
 * The maximum Plutus witness of `witnessScriptDecoding` (item kind 0), Init →
 * block → watcher → record → conviction on the registered chain: one PlutusV3
 * item that fills the 32,768-byte field-6 bound with the densest Flat there
 * is, version 1.1.0 and then `delay` after `delay` until the bytes run out, so
 * Cardano refuses it only at its last bit.
 *
 * The subject spends a funded output and is otherwise valid, so the
 * producer's first fault is that item, at the witness-script phase (5). The
 * operator accepts it anyway. The record walks every node through the
 * script-decode validator and its class stakers, and step 04 convicts the
 * acceptance by that record; the block is removed.
 *
 * The record outlasts the emulator profile's block maturity on its own, so it
 * is taken first, from the script alone, as a watcher can once the
 * transaction is visible; the harness keeps the reference-script auth policy
 * open for it. Every transaction is checked against the ledger's limits and
 * reported with its bytes and execution units.
 */
import { scriptDecodeRecordOfScript } from "@al-ft/midgard-validation";
import { describe, expect, it } from "vitest";

import {
  plutusItemScriptDecode,
  recordScriptDecode,
} from "../src/script-decode/index.js";
import { scriptDecodeDeploymentOf } from "./support/script-decode-emulator.js";
import {
  createTransactionJournal,
  MAX_RECORD_AUTH_TIMELOCK_MS,
  MAXIMUM_FIELD_BYTES,
  maximumDelayChainWitnessScript,
  plutusItem,
  producerVerdict,
  WALK_CEILING,
  witnessSubject,
} from "./support/script-decode-plutus-max-shape.js";
import { scriptWitnessField } from "./support/witness-script-decoding-raw.js";
import { makeHarness } from "./witness-script-decoding-lifecycle.make-harness.js";
import {
  acceptedEvidence,
  expectReplayDetection,
  shapeOfNativeTx,
} from "./witness-script-decoding-lifecycle.scan-honest-accepted-to-close.js";

const PLUTUS_V3 = 3;
const WITNESS_SCRIPT_PHASE = 5;

describe("witnessScriptDecoding Plutus maximum shape", () => {
  it("convicts the accepted undecodable Plutus witness that fills the field bound, the producer's phase-5 first fault, through removal", async () => {
    const script = maximumDelayChainWitnessScript();
    const subject = witnessSubject([plutusItem(script)]);
    expect(scriptWitnessField(subject.items)).toHaveLength(MAXIMUM_FIELD_BYTES);
    const decoded = plutusItemScriptDecode(subject.items[0]!);
    if (decoded?.record.k !== "refused")
      throw new Error("the maximum script must be refused");
    // Refused at its very last bit: every node is walked.
    expect(decoded.record.bitOffset).toBe(decoded.scriptBytes.length * 8);

    // The producer's first fault is the item, at the witness-script phase.
    expect(await producerVerdict(subject)).toEqual({
      verdict: "rejected",
      reason: {
        PlutusScriptUndecodable: {
          item_kind: 0n,
          item_index: 0n,
          class: BigInt(decoded.record.class),
        },
      },
      phase: WITNESS_SCRIPT_PHASE,
    });

    const h = await makeHarness({
      referenceScriptAuthTimelockMs: MAX_RECORD_AUTH_TIMELOCK_MS,
    });
    const { emulator } = h.harness;
    const journal = createTransactionJournal(emulator, h.lucid);
    const recordedFrom = emulator.now();
    const recorded = await journal.stage(
      "record",
      () =>
        recordScriptDecode({
          lucid: h.lucid,
          deployment: scriptDecodeDeploymentOf(emulator),
          language: PLUTUS_V3,
          scriptBytes: script,
        }),
      { summarize: true },
    );
    expect(recorded.record).toEqual(
      scriptDecodeRecordOfScript(PLUTUS_V3, script),
    );
    expect(recorded.record).toEqual(decoded.record);
    expect(recorded.walks.length).toBeGreaterThan(100);
    expect(recorded.walks.length).toBeLessThanOrEqual(WALK_CEILING);
    const recordVirtualMs = emulator.now() - recordedFrom;

    // The operator accepts it; the watcher's replay of the committed block
    // finds the undecodable item.
    const shape = shapeOfNativeTx(
      `${MAXIMUM_FIELD_BYTES.toString()}-byte field 6; [3, delay chain]: refused at its last bit (Certified)`,
      subject.items,
      subject.nativeTx,
    );
    expect(shape.certified).toBe(true);
    const { block, setup, inclusionOf } = await h.acceptedBlock(shape);
    const evidence = acceptedEvidence(shape);
    expect(evidence.resultClass).toBe(100 + decoded.record.class);
    expectReplayDetection(block, [shape], evidence, [
      "witness-plutus-script-undecodable",
    ]);

    const openedAt = emulator.now();
    const published = await journal.stage("field carriage", () =>
      h.publishField(shape, null),
    );
    const init = await journal.stage("init", () =>
      h.init(setup.fraudulentBlockOutRef, "plutus-maximum-init"),
    );
    const bound = await journal.stage("step 01", () =>
      h.step01Accepted(
        init,
        inclusionOf(shape),
        setup.fraudulentBlockOutRef,
        0n,
        "plutus-maximum-step01",
      ),
    );
    const opened = await journal.stage("step 02", () =>
      h.step02(
        bound,
        shape,
        evidence,
        "plutus-maximum-step02",
        published.carriageUtxos,
        published.certificateUtxo,
      ),
    );
    expect(opened.scanState.result_class).toBe(BigInt(evidence.resultClass));
    const closed = await journal.stage(
      "step 03",
      () => h.scanToClose(opened.nextThreadOutRef, evidence, "plutus-maximum"),
      { summarize: true },
    );
    await journal.stage("step 04 (proof mint)", () =>
      h.step04(
        closed.threadOutRef,
        evidence,
        "plutus-maximum-step04-proof-mint",
      ),
    );
    await h.expectThreadsGone(setup.headerHash);
    await journal.stage("removal", () =>
      h.remove(setup.headerHash, "plutus-maximum-remove"),
    );
    journal.report("kind 0 conviction");
    console.info(
      `[script-decode-plutus-max-shape] kind 0 ${JSON.stringify({
        scriptBytes: script.length,
        walks: recorded.walks.length,
        ticks: recorded.walks.reduce((sum, walk) => sum + walk.fuel, 0),
        recordVirtualMs,
        convictionVirtualMs: emulator.now() - openedAt,
      })}`,
    );
  }, 7_200_000);
});
