/**
 * The maximum Plutus shapes of `outputReferenceScriptDecoding`, Init → record
 * → conviction on the registered chain, each in an output of exactly the
 * 16,384-byte consensus bound.
 *
 * - The densest Flat there is: version 1.1.0 and then `delay` after `delay`
 *   (four bits each) until the bytes run out, so Cardano refuses it only at
 *   its last bit. Recording it walks every node, and step 04 convicts its
 *   acceptance.
 * - The shape whose record takes the most Walk transactions: a constant list
 *   of a unit pair chain over `list unit`. It decodes, and step 04 convicts a
 *   rejection that called it undecodable, reading the `Decoded` record.
 *
 * The delay chain is recorded after Init, while the thread is open. The
 * Walk-heaviest record outlasts the emulator profile's block maturity on its
 * own, so it is recorded first, from the script alone, as a watcher can once
 * the forced transaction is visible, and the case then runs inside the window.
 * It ends at the conviction: the harness's reference-script auth policy
 * expires four hours after the harness starts, so the removal references can
 * no longer be published, and removal itself is the delay chain's.
 */
import {
  acceptedVerdictSubject,
  forcedVerdictSubject,
} from "@al-ft/midgard-sdk";
import { scriptDecodeRecordOfScript } from "@al-ft/midgard-validation";
import { describe, expect, it } from "vitest";

import {
  OutputReferenceScriptResultClasses,
  prepareOutputReferenceScriptDecodingEvidence,
} from "../src/output-reference-script-decoding/index.js";
import {
  plutusItemScriptDecode,
  recordScriptDecode,
  type ScriptDecodeWalkReport,
} from "../src/script-decode/index.js";
import {
  commitAcceptedBlock,
  commitForcedBlock,
  createMeasurementRecorder,
  makeOutputReferenceHarness,
  makeOutputReferenceStages,
  OUTPUT_REFERENCE_CATEGORY_ID,
  OUTPUT_REFERENCE_MAX_OUTPUT_BYTES,
  outputWithPlutusScript,
  publishFamilyReferences,
  subjectTransaction,
} from "./support/output-reference-script-decoding-emulator.js";
import { scriptDecodeDeploymentOf } from "./support/script-decode-emulator.js";
import { WALK_SHAPES } from "./support/script-decode-walk-shapes.js";

/** Flat 1.1.0 followed by `flatBytes - 3` bytes of `delay delay`. */
const delayChainScript = (flatBytes: number): Buffer => {
  const flat = Buffer.concat([
    Buffer.from("010100", "hex"),
    Buffer.alloc(flatBytes - 3, 0x11),
  ]);
  return Buffer.concat([
    Buffer.from([0x59, flat.length >> 8, flat.length & 0xff]),
    flat,
  ]);
};

/** The delay-chain output of exactly `length` canonical bytes. */
const delayChainOutputOfLength = (length: number) => {
  for (let flatBytes = length; flatBytes > length - 128; flatBytes -= 1) {
    const output = outputWithPlutusScript(delayChainScript(flatBytes));
    if (output.length === length) return output;
  }
  throw new Error(`no delay-chain output of ${length.toString()} bytes`);
};

/** The most Walk-heavy shape (see `script-decode-walk-gate.test.ts`). */
const MAX_WALK_SHAPE = WALK_SHAPES["list of a unit pair chain over list unit"];

/** The max-walk-shape output of exactly `length` canonical bytes, and its
 * script. */
const maxWalkOutputOfLength = (length: number) => {
  const outputOf = (count: number) =>
    outputWithPlutusScript(MAX_WALK_SHAPE(count));
  let low = 1;
  let high = 140_000;
  while (low < high) {
    const middle = Math.floor((low + high + 1) / 2);
    if (outputOf(middle).length <= length) low = middle;
    else high = middle - 1;
  }
  const script = MAX_WALK_SHAPE(low);
  const output = outputWithPlutusScript(script);
  if (output.length !== length)
    throw new Error(`no max-walk output of ${length.toString()} bytes`);
  return { output, script };
};

const MAX_TX_BYTES = 16_384;

/** D_case / 2 over the public profile's transaction interval. */
const PUBLIC_WALK_GATE = 877;

const PLUTUS_V3 = 3;

/** Every Walk fits the ledger's transaction limits. */
const expectWalksFit = (
  walks: readonly ScriptDecodeWalkReport[],
  limits: { readonly maxTxExMem: bigint; readonly maxTxExSteps: bigint },
) => {
  for (const walk of walks) {
    expect(walk.exUnits.mem).toBeLessThanOrEqual(limits.maxTxExMem);
    expect(walk.exUnits.steps).toBeLessThanOrEqual(limits.maxTxExSteps);
    expect(walk.txBytes).toBeLessThanOrEqual(MAX_TX_BYTES);
  }
};

const walkSummary = (walks: readonly ScriptDecodeWalkReport[]) => ({
  walks: walks.length,
  ticks: walks.reduce((sum, walk) => sum + walk.fuel, 0),
  maxFuel: Math.max(...walks.map((walk) => walk.fuel)),
  maxMem: Math.max(...walks.map((walk) => Number(walk.exUnits.mem))),
  maxSteps: Math.max(...walks.map((walk) => Number(walk.exUnits.steps))),
  maxTxBytes: Math.max(...walks.map((walk) => walk.txBytes)),
});

describe("outputReferenceScriptDecoding Plutus maximum shape", () => {
  it("records the densest Plutus reference script of a 16,384-byte output after Init and convicts its acceptance through removal", async () => {
    const context = await makeOutputReferenceHarness();
    const { emulator, proverLucid } = context.harness;
    const maximum = delayChainOutputOfLength(OUTPUT_REFERENCE_MAX_OUTPUT_BYTES);
    const block = await commitAcceptedBlock(context, [
      subjectTransaction([maximum], 7n),
    ]);
    const [subject] = block.subjects;
    if (subject === undefined) throw new Error("accepted subject absent");
    const stages = makeOutputReferenceStages({
      context,
      fraudulentBlockOutRef: block.setup.fraudulentBlockOutRef,
      ...(await publishFamilyReferences(
        context,
        createMeasurementRecorder(),
        "output-reference-plutus-maximum",
      )),
    });
    const evidence = prepareOutputReferenceScriptDecodingEvidence({
      subject: acceptedVerdictSubject(subject.nativeTxId),
      outputIndex: 0,
      canonicalTransactionCbor: subject.canonicalCbor,
    });
    expect(evidence.outputLength).toBe(OUTPUT_REFERENCE_MAX_OUTPUT_BYTES);
    expect(evidence.carriage).toBe("Certified");
    const decoded = plutusItemScriptDecode(
      Buffer.from(evidence.referenceScriptItemHex, "hex"),
    );
    if (decoded?.record.k !== "refused")
      throw new Error("the maximum script must be refused");
    // Refused at its very last bit: every node is walked.
    expect(decoded.record.bitOffset).toBe(decoded.scriptBytes.length * 8);
    expect(evidence.resultClass).toBe(100 + decoded.record.class);

    const openedAt = emulator.now();
    const s1 = await stages.step01Accepted(
      (await stages.init()).result,
      evidence,
      subject.txInclusion,
    );
    const s2 = await stages.step02(
      s1.result.nextThreadOutRef,
      evidence,
      subject,
    );
    const opened = {
      carriageUtxos: s2.result.carriageUtxos,
      certificateUtxo: s2.result.certificateUtxo,
    };
    const s3 = await stages.step03Loop(s2.result.nextThreadOutRef, evidence);

    const recordedFrom = emulator.now();
    const recorded = await recordScriptDecode({
      lucid: proverLucid,
      deployment: scriptDecodeDeploymentOf(emulator),
      language: decoded.language,
      scriptBytes: decoded.scriptBytes,
    });
    const recordedAt = emulator.now();
    expect(recorded.record).toEqual(decoded.record);
    expectWalksFit(recorded.walks, proverLucid.config().protocolParameters!);
    console.info(
      `[script-decode-max-shape] ${JSON.stringify({
        shape: "delay chain",
        scriptBytes: decoded.scriptBytes.length,
        ...walkSummary(recorded.walks),
        recordVirtualMs: recordedAt - recordedFrom,
      })}`,
    );
    expect(recorded.walks.length).toBeGreaterThan(100);

    const s4 = await stages.step04(
      s3.threadOutRef,
      evidence,
      subject.compactCborHex,
      opened,
      recorded.recordUtxo,
    );
    const s5 = await stages.step05(s4.result.nextThreadOutRef, evidence);
    expect(s5.result.closed).toBe(true);
    const s6 = await stages.step06(s5.result.nextThreadOutRef, evidence);
    expect(s6.result.fraudProofUnit).toBeTruthy();
    const removal = await stages.remove(block.setup.headerHash);
    expect(removal.result.fraudCategoryId).toBe(OUTPUT_REFERENCE_CATEGORY_ID);
    console.info(
      `[script-decode-max-shape] delay chain case virtual ms ${(emulator.now() - openedAt).toString()}`,
    );
  }, 7_200_000);

  it("records the most Walk-heavy decodable script of a 16,384-byte output and convicts its undecodable rejection by the Decoded record", async () => {
    const context = await makeOutputReferenceHarness();
    const { emulator, proverLucid } = context.harness;
    const { output: maximum, script } = maxWalkOutputOfLength(
      OUTPUT_REFERENCE_MAX_OUTPUT_BYTES,
    );

    const recordedFrom = emulator.now();
    const recorded = await recordScriptDecode({
      lucid: proverLucid,
      deployment: scriptDecodeDeploymentOf(emulator),
      language: PLUTUS_V3,
      scriptBytes: script,
    });
    const recordedAt = emulator.now();
    expect(recorded.record).toEqual(
      scriptDecodeRecordOfScript(PLUTUS_V3, script),
    );
    expect(recorded.record?.k).toBe("decoded");
    expectWalksFit(recorded.walks, proverLucid.config().protocolParameters!);
    console.info(
      `[script-decode-max-shape] ${JSON.stringify({
        shape: "unit pair chain over list unit",
        scriptBytes: script.length,
        ...walkSummary(recorded.walks),
        recordVirtualMs: recordedAt - recordedFrom,
      })}`,
    );
    expect(recorded.walks.length).toBeGreaterThan(PUBLIC_WALK_GATE / 2);
    expect(recorded.walks.length).toBeLessThanOrEqual(PUBLIC_WALK_GATE);

    const committedFrom = emulator.now();
    const block = await commitForcedBlock(context, [
      {
        nativeTx: subjectTransaction([maximum], 7n),
        reason: {
          PlutusScriptUndecodable: { item_kind: 1n, item_index: 0n, class: 3n },
        } as never,
      },
    ]);
    const [leaf] = block.leaves;
    if (leaf === undefined) throw new Error("forced leaf absent");
    const stages = makeOutputReferenceStages({
      context,
      fraudulentBlockOutRef: block.setup.fraudulentBlockOutRef,
      ...(await publishFamilyReferences(
        context,
        createMeasurementRecorder(),
        "output-reference-plutus-max-walk",
      )),
    });
    const evidence = prepareOutputReferenceScriptDecodingEvidence({
      subject: forcedVerdictSubject({
        transactionId: leaf.transaction.tx_id,
        sourceKey: leaf.membership.key,
        rejectionReason: leaf.reason,
      }),
      outputIndex: 0,
      canonicalTransactionCbor: leaf.canonicalCbor,
    });
    expect(evidence.outputLength).toBe(OUTPUT_REFERENCE_MAX_OUTPUT_BYTES);
    expect(evidence.resultClass).toBe(
      OutputReferenceScriptResultClasses.NoFault,
    );
    const decoded = plutusItemScriptDecode(
      Buffer.from(evidence.referenceScriptItemHex, "hex"),
    );
    if (decoded === undefined) throw new Error("the item must be Plutus");
    expect(decoded.language).toBe(PLUTUS_V3);
    expect(Buffer.from(decoded.scriptBytes)).toEqual(script);
    expect(decoded.record).toEqual(recorded.record);

    const openedAt = emulator.now();
    const s1 = await stages.step01Forced(
      stages.threadOf((await stages.init()).result),
      evidence,
      leaf,
      block.header,
    );
    const s2 = await stages.step02(s1.result.nextThreadOutRef, evidence, {
      compactCborHex: leaf.transaction.submitted_source.compact_cbor,
      witnessSetCompactCborHex:
        leaf.transaction.submitted_source.witness_set_compact_cbor,
    });
    const s3 = await stages.step03Loop(s2.result.nextThreadOutRef, evidence);
    const s4 = await stages.step04(
      s3.threadOutRef,
      evidence,
      leaf.transaction.submitted_source.compact_cbor,
      s2.result,
      recorded.recordUtxo,
    );
    const s5 = await stages.step05(s4.result.nextThreadOutRef, evidence);
    expect(s5.result.closed).toBe(true);
    const s6 = await stages.step06(s5.result.nextThreadOutRef, evidence);
    expect(s6.result.fraudProofUnit).toBeTruthy();
    console.info(
      `[script-decode-max-shape] max-walk case virtual ms ${(emulator.now() - openedAt).toString()}, from the block commit ${(emulator.now() - committedFrom).toString()}`,
    );
  }, 7_200_000);
});
