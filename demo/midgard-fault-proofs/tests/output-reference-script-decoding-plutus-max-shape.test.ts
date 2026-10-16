/**
 * The maximum Plutus shape of `outputReferenceScriptDecoding`, Init → record
 * → conviction on the registered chain.
 *
 * The output is exactly the 16,384-byte consensus bound and its PlutusV3
 * reference script is the densest Flat there is: version 1.1.0 and then
 * `delay` after `delay` (four bits each) until the bytes run out, so Cardano
 * refuses it only at its last bit. Recording it walks every node, over the
 * most Walk transactions any reference script can take. The record is made
 * after Init, while the thread is open, and step 04 closes on it.
 */
import { acceptedVerdictSubject } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { prepareOutputReferenceScriptDecodingEvidence } from "../src/output-reference-script-decoding/index.js";
import {
  plutusItemScriptDecode,
  recordScriptDecode,
} from "../src/script-decode/index.js";
import {
  commitAcceptedBlock,
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

const MAX_TX_BYTES = 16_384;

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
    const { maxTxExMem, maxTxExSteps } =
      proverLucid.config().protocolParameters!;
    for (const walk of recorded.walks) {
      expect(walk.exUnits.mem).toBeLessThanOrEqual(maxTxExMem);
      expect(walk.exUnits.steps).toBeLessThanOrEqual(maxTxExSteps);
      expect(walk.txBytes).toBeLessThanOrEqual(MAX_TX_BYTES);
    }
    const ticks = recorded.walks.reduce((sum, walk) => sum + walk.fuel, 0);
    console.info(
      `[script-decode-max-shape] ${JSON.stringify({
        scriptBytes: decoded.scriptBytes.length,
        walks: recorded.walks.length,
        ticks,
        maxFuel: Math.max(...recorded.walks.map((walk) => walk.fuel)),
        maxMem: Math.max(
          ...recorded.walks.map((walk) => Number(walk.exUnits.mem)),
        ),
        maxSteps: Math.max(
          ...recorded.walks.map((walk) => Number(walk.exUnits.steps)),
        ),
        maxTxBytes: Math.max(...recorded.walks.map((walk) => walk.txBytes)),
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
      `[script-decode-max-shape] case virtual ms ${(emulator.now() - openedAt).toString()}`,
    );
  }, 7_200_000);
});
