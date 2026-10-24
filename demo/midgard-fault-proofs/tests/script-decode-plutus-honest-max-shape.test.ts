/**
 * Honest Plutus at the maximum shape, Init → block → watcher → every decode
 * family's attempt. One transaction spends an output locked by a PlutusV3
 * script carried in field 6 (item kind 0), whose item fills the 32,768-byte
 * field bound, into an output of exactly the 16,384-byte bound whose
 * reference script is another PlutusV3 script (item kind 1). Both scripts
 * ignore their argument and a padding constant and return unit; neither
 * branches on anything.
 *
 * The producer accepts the transaction, and no decode family's watcher
 * replay of the block finds a fault. Each Plutus decode family's attempt
 * against the acceptance runs to its terminal step and is refused there, at
 * the rule that convicts only a class that is a fault: `witnessScriptDecoding`
 * at step 04, `outputReferenceScriptDecoding` at step 06.
 */
import {
  acceptedVerdictSubject,
  type EvidenceProvenance,
} from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/index.js";
import {
  OutputReferenceScriptResultClasses,
  prepareOutputReferenceScriptDecodingEvidence,
} from "../src/output-reference-script-decoding/index.js";
import { detectOutputReferenceScriptDecodingCanonicalViolations } from "../src/output-reference-script-decoding/output-reference-script-decoding.canonical-findings.js";
import { recordScriptDecode } from "../src/script-decode/index.js";
import {
  detectWitnessScriptDecodingCompleteReplay,
  WitnessScriptDecodingResultClasses,
} from "../src/witness-script-decoding/index.js";
import {
  CANONICAL_DECODABILITY_COMPLETE_CANONICAL_REPLAY,
  NATIVE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
  OUTPUT_REFERENCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
  WITNESS_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  fixtureTransactionFromCanonicalCbor,
} from "./helpers/canonical-block-evidence-fixture.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { nativeDecodingFixture } from "./support/native-script-decoding-retained.js";
import {
  commitAcceptedBlock,
  createMeasurementRecorder,
  makeOutputReferenceHarness,
  makeOutputReferenceStages,
  OUTPUT_REFERENCE_MAX_OUTPUT_BYTES,
  publishFamilyReferences,
} from "./support/output-reference-script-decoding-emulator.js";
import { scriptDecodeDeploymentOf } from "./support/script-decode-emulator.js";
import {
  createTransactionJournal,
  honestSpendSubject,
  MAX_RECORD_AUTH_TIMELOCK_MS,
  MAXIMUM_FIELD_BYTES,
  maximumHonestReferenceScript,
  maximumHonestWitnessScript,
  plutusItem,
  plutusReferenceOutput,
  producerVerdict,
} from "./support/script-decode-plutus-max-shape.js";
import { scriptWitnessField } from "./support/witness-script-decoding-raw.js";
import { makeHarness } from "./witness-script-decoding-lifecycle.make-harness.js";
import {
  acceptedEvidence,
  shapeOfNativeTx,
} from "./witness-script-decoding-lifecycle.scan-honest-accepted-to-close.js";

const PLUTUS_V3 = 3;

const DA_PROVENANCE: EvidenceProvenance = {
  trustClass: "public_or_permissionless_da",
  sourceId: "libp2p/peer-a",
  grade: "security",
};

const witnessScript = maximumHonestWitnessScript();
const referenceScript = maximumHonestReferenceScript();
const subject = honestSpendSubject(witnessScript, referenceScript);
const txIdHex = subject.txId.toString("hex");

/** The committed block's transactions as a family's replay reads them. */
const committedReplay = (headerHash: string, reconstruction: unknown) =>
  ({
    headerHash,
    transactions: [
      { nodeTxId: txIdHex, txCbor: subject.txCbor.toString("hex") },
    ],
    reconstruction,
  }) as never;

/** Records `script` on the harness's script-decode deployment. */
const record = (
  harness: Awaited<ReturnType<typeof makeHarness>>["harness"],
  script: Buffer,
) =>
  recordScriptDecode({
    lucid: harness.proverLucid,
    deployment: scriptDecodeDeploymentOf(harness.emulator),
    language: PLUTUS_V3,
    scriptBytes: script,
  });

describe("honest Plutus at the maximum shape", () => {
  it("the producer accepts the spend and the reference script, and no decode family's watcher replay finds a fault", async () => {
    expect(scriptWitnessField(subject.items)).toHaveLength(MAXIMUM_FIELD_BYTES);
    expect(plutusReferenceOutput(referenceScript)).toHaveLength(
      OUTPUT_REFERENCE_MAX_OUTPUT_BYTES,
    );
    expect(await producerVerdict(subject)).toEqual({ verdict: "accepted" });

    const fixture = await buildCanonicalBlockFixture({
      transactions: [
        fixtureTransactionFromCanonicalCbor({
          txId: txIdHex,
          canonicalCbor: subject.txCbor,
        }),
      ],
      utxos: subject.state.map(({ outRef, output }) => ({
        key: outRef,
        value: output,
      })),
    });
    const canonical = await canonicalBlockEvidenceFromVerifiedPayload({
      observation: authenticatedHeaderObservation(fixture),
      payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
      daProvenance: DA_PROVENANCE,
    });
    for (const family of [
      WITNESS_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
      OUTPUT_REFERENCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
      CANONICAL_DECODABILITY_COMPLETE_CANONICAL_REPLAY,
    ])
      expect(
        (await family.replay(canonical)).detections,
        family.launchScope.join(","),
      ).toEqual([]);
    // `nativeScriptDecoding` reads the script of an output a transaction
    // resolves, against the authenticated predecessor ledger: the honest
    // reference script's output, spent and referenced by an accepted
    // transaction.
    for (const sourceKind of [0, 1] as const) {
      const { evidence, context } = await nativeDecodingFixture({
        direction: 0,
        sourceKind,
        item: plutusItem(referenceScript),
      });
      expect(
        (
          await NATIVE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY.replay(
            evidence,
            context,
          )
        ).detections,
        `resolved by input kind ${sourceKind.toString()}`,
      ).toEqual([]);
    }
  }, 300_000);

  it("witnessScriptDecoding: the attempt on the honest witness is refused at step 04", async () => {
    const h = await makeHarness({
      referenceScriptAuthTimelockMs: MAX_RECORD_AUTH_TIMELOCK_MS,
    });
    const journal = createTransactionJournal(h.harness.emulator, h.lucid);
    const recorded = await journal.stage(
      "record",
      () => record(h.harness, witnessScript),
      { summarize: true },
    );
    expect(recorded.record?.k).toBe("decoded");

    const shape = shapeOfNativeTx(
      `${MAXIMUM_FIELD_BYTES.toString()}-byte field 6; [3, honest program] (Certified)`,
      subject.items,
      subject.nativeTx,
    );
    const { block, setup, inclusionOf } = await h.acceptedBlock(shape);
    expect(
      detectWitnessScriptDecodingCompleteReplay(
        committedReplay(block.headerHash, block.reconstruction),
      ),
    ).toEqual([]);
    const evidence = acceptedEvidence(shape);
    expect(evidence.resultClass).toBe(
      WitnessScriptDecodingResultClasses.NoFault,
    );

    const published = await journal.stage("field carriage", () =>
      h.publishField(shape, null),
    );
    const init = await journal.stage("init", () =>
      h.init(setup.fraudulentBlockOutRef, null),
    );
    const bound = await journal.stage("step 01", () =>
      h.step01Accepted(
        init,
        inclusionOf(shape),
        setup.fraudulentBlockOutRef,
        0n,
      ),
    );
    const opened = await journal.stage("step 02", () =>
      h.step02(
        bound,
        shape,
        evidence,
        null,
        published.carriageUtxos,
        published.certificateUtxo,
      ),
    );
    expect(opened.scanState.result_class).toBe(
      BigInt(WitnessScriptDecodingResultClasses.NoFault),
    );
    const closed = await journal.stage(
      "step 03",
      () => h.scanToClose(opened.nextThreadOutRef, evidence, null),
      { summarize: true },
    );
    expect((await h.scanStateAt(closed.threadOutRef, 3)).result_class).toBe(
      BigInt(WitnessScriptDecodingResultClasses.NoFault),
    );
    // The terminal rule convicts only a class that is a fault.
    await expectOnchainRefusal(() => h.step04Raw(closed.threadOutRef), {
      refusedBy: "fraud_proofs/witness_script_decoding/step_04",
      check: /^Validator returned false$/u,
    });
    await journal.stage("cancel", () => h.cancel(closed.threadOutRef, 3));
    journal.report("honest kind 0 attempt");
  }, 3_600_000);

  it("outputReferenceScriptDecoding: the attempt on the honest reference script is refused at step 06", async () => {
    const context = await makeOutputReferenceHarness({
      referenceScriptAuthTimelockMs: MAX_RECORD_AUTH_TIMELOCK_MS,
    });
    const { harness } = context;
    const journal = createTransactionJournal(
      harness.emulator,
      harness.proverLucid,
    );
    const recorded = await journal.stage(
      "record",
      () => record(harness, referenceScript),
      { summarize: true },
    );
    expect(recorded.record?.k).toBe("decoded");

    const block = await commitAcceptedBlock(context, [subject.nativeTx]);
    const [accepted] = block.subjects;
    if (accepted === undefined) throw new Error("accepted subject absent");
    expect(
      detectOutputReferenceScriptDecodingCanonicalViolations(
        committedReplay(block.setup.headerHash, { forcedTransactions: [] }),
      ),
    ).toEqual([]);
    const stages = makeOutputReferenceStages({
      context,
      fraudulentBlockOutRef: block.setup.fraudulentBlockOutRef,
      ...(await publishFamilyReferences(
        context,
        createMeasurementRecorder(),
        "output-reference-plutus-honest-maximum",
      )),
    });
    const prepare = (admitHonestVerdict: boolean) =>
      prepareOutputReferenceScriptDecodingEvidence({
        subject: acceptedVerdictSubject(accepted.nativeTxId),
        outputIndex: 0,
        canonicalTransactionCbor: accepted.canonicalCbor,
        admitHonestVerdict,
      });
    expect(() => prepare(false)).toThrow(/agrees with operator verdict/u);
    const evidence = prepare(true);
    expect(evidence.outputLength).toBe(OUTPUT_REFERENCE_MAX_OUTPUT_BYTES);
    expect(evidence.resultClass).toBe(
      OutputReferenceScriptResultClasses.NoFault,
    );

    const initialized = await journal.stage("init", () => stages.init());
    const s1 = await journal.stage("step 01", () =>
      stages.step01Accepted(initialized.result, evidence, accepted.txInclusion),
    );
    const s2 = await journal.stage("step 02", () =>
      stages.step02(s1.result.nextThreadOutRef, evidence, accepted),
    );
    const s3 = await journal.stage(
      "step 03",
      () => stages.step03Loop(s2.result.nextThreadOutRef, evidence),
      { summarize: true },
    );
    const s4 = await journal.stage("step 04", () =>
      stages.step04(
        s3.threadOutRef,
        evidence,
        accepted.compactCborHex,
        s2.result,
        recorded.recordUtxo,
      ),
    );
    const s5 = await journal.stage("step 05", () =>
      stages.step05(s4.result.nextThreadOutRef, evidence),
    );
    expect(s5.result.closed).toBe(true);
    const closed = s5.result.nextThreadOutRef;
    // The terminal rule convicts only a class that is a fault.
    await expectOnchainRefusal(() => stages.step06Raw(closed), {
      refusedBy: "fraud_proofs/output_reference_script_decoding/step_06",
      check: /^Validator returned false$/u,
    });
    await journal.stage("cancel", () => stages.cancel(closed, 5));
    journal.report("honest kind 1 attempt");
  }, 3_600_000);
});
