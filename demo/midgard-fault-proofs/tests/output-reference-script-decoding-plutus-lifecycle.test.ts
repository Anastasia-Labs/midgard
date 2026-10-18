/**
 * `outputReferenceScriptDecoding` Plutus reference scripts on the registered
 * chain: step 04 closes a PlutusV3 item by the decode record of its script,
 * read as a reference input. `4401020304` wraps four bytes that are not a
 * program (refused with class 3, result 103); `450101004981` is the flat
 * program 1.1.0 `(con unit ())`, which decodes.
 */
import {
  acceptedVerdictSubject,
  forcedVerdictSubject,
} from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  type OutputReferenceScriptDecodingEvidence,
  OutputReferenceScriptResultClasses,
  prepareOutputReferenceScriptDecodingEvidence,
  readOutputReferenceScriptScanState,
} from "../src/output-reference-script-decoding/index.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import {
  commitAcceptedBlock,
  commitForcedBlock,
  createMeasurementRecorder,
  type ForcedLeaf,
  makeOutputReferenceHarness,
  makeOutputReferenceStages,
  type OutputReferenceContext,
  outputWithPlutusScript,
  publishFamilyReferences,
  subjectTransaction,
} from "./support/output-reference-script-decoding-emulator.js";
import { recordPlutusItem } from "./support/script-decode-emulator.js";

const UNDECODABLE = Buffer.from("4401020304", "hex");
const DECODABLE = Buffer.from("450101004981", "hex");
/** `[3, h'450101004981']`, the decodable script as a PlutusV3 item. */
const DECODABLE_ITEM = Buffer.from("820346450101004981", "hex");
const REFUSED_CLASS_3 = 103;

const undecodableReason = (cls: bigint) =>
  ({
    PlutusScriptUndecodable: { item_kind: 1n, item_index: 0n, class: cls },
  }) as never;
const forcedEvidence = (leaf: ForcedLeaf, admitHonestVerdict = false) =>
  prepareOutputReferenceScriptDecodingEvidence({
    subject: forcedVerdictSubject({
      transactionId: leaf.transaction.tx_id,
      sourceKey: leaf.membership.key,
      rejectionReason: leaf.reason,
    }),
    outputIndex: 0,
    canonicalTransactionCbor: leaf.canonicalCbor,
    admitHonestVerdict,
  });
const forcedSourceOf = (leaf: ForcedLeaf) => ({
  compactCborHex: leaf.transaction.submitted_source.compact_cbor,
  witnessSetCompactCborHex:
    leaf.transaction.submitted_source.witness_set_compact_cbor,
});
const resultClassOf = async (
  context: OutputReferenceContext,
  threadOutRef: string,
) =>
  (
    await readOutputReferenceScriptScanState({
      lucid: context.harness.proverLucid,
      contracts: context.contracts,
      categoryId: context.category.categoryId,
      signer: context.harness.proverSigner,
      threadOutRef,
    })
  ).state.result_class;
const itemOf = (evidence: OutputReferenceScriptDecodingEvidence) =>
  Buffer.from(evidence.referenceScriptItemHex, "hex");

describe("outputReferenceScriptDecoding Plutus reference scripts", () => {
  it("convicts an accepted undecodable Plutus reference script by its record, refusing a missing or foreign record first", async () => {
    const context = await makeOutputReferenceHarness();
    const block = await commitAcceptedBlock(context, [
      subjectTransaction([outputWithPlutusScript(UNDECODABLE)], 7n),
    ]);
    const [subject] = block.subjects;
    if (subject === undefined) throw new Error("accepted subject absent");
    const stages = makeOutputReferenceStages({
      context,
      fraudulentBlockOutRef: block.setup.fraudulentBlockOutRef,
      ...(await publishFamilyReferences(
        context,
        createMeasurementRecorder(),
        "output-reference-plutus",
      )),
    });
    const evidence = prepareOutputReferenceScriptDecodingEvidence({
      subject: acceptedVerdictSubject(subject.nativeTxId),
      outputIndex: 0,
      canonicalTransactionCbor: subject.canonicalCbor,
    });
    expect(evidence.resultClass).toBe(REFUSED_CLASS_3);
    const { emulator, proverLucid } = context.harness;
    const record = await recordPlutusItem(
      emulator,
      proverLucid,
      itemOf(evidence),
    );
    const foreign = await recordPlutusItem(
      emulator,
      proverLucid,
      DECODABLE_ITEM,
    );
    if (record === undefined || foreign === undefined)
      throw new Error("Plutus items take a record");

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
    const s3 = await stages.step03Loop(s2.result.nextThreadOutRef, evidence);
    for (const wrong of [null, foreign])
      await expectOnchainRefusal(() =>
        stages.step04(
          s3.threadOutRef,
          evidence,
          subject.compactCborHex,
          s2.result,
          wrong,
        ),
      );
    const s4 = await stages.step04(
      s3.threadOutRef,
      evidence,
      subject.compactCborHex,
      s2.result,
      record,
    );
    expect(await resultClassOf(context, s4.result.nextThreadOutRef)).toBe(
      BigInt(REFUSED_CLASS_3),
    );
    const s5 = await stages.step05(s4.result.nextThreadOutRef, evidence);
    expect(s5.result.closed).toBe(true);
    const s6 = await stages.step06(s5.result.nextThreadOutRef, evidence);
    expect(s6.result.fraudProofUnit).toBeTruthy();
  }, 900_000);

  it("contradicts a wrongful undecodable rejection of a decodable Plutus reference script, and refuses to contradict an honest one", async () => {
    const context = await makeOutputReferenceHarness();
    const block = await commitForcedBlock(context, [
      {
        nativeTx: subjectTransaction([outputWithPlutusScript(DECODABLE)], 7n),
        reason: undecodableReason(3n),
      },
      {
        nativeTx: subjectTransaction([outputWithPlutusScript(UNDECODABLE)], 8n),
        reason: undecodableReason(3n),
      },
    ]);
    const [wrongful, honest] = block.leaves;
    if (wrongful === undefined || honest === undefined)
      throw new Error("forced leaves absent");
    const stages = makeOutputReferenceStages({
      context,
      fraudulentBlockOutRef: block.setup.fraudulentBlockOutRef,
      ...(await publishFamilyReferences(
        context,
        createMeasurementRecorder(),
        "output-reference-plutus-forced",
      )),
    });
    const toStep05 = async (
      leaf: ForcedLeaf,
      evidence: OutputReferenceScriptDecodingEvidence,
    ) => {
      const s1 = await stages.step01Forced(
        stages.threadOf((await stages.init()).result),
        evidence,
        leaf,
        block.header,
      );
      const s2 = await stages.step02(
        s1.result.nextThreadOutRef,
        evidence,
        forcedSourceOf(leaf),
      );
      const s3 = await stages.step03Loop(s2.result.nextThreadOutRef, evidence);
      const s4 = await stages.step04(
        s3.threadOutRef,
        evidence,
        leaf.transaction.submitted_source.compact_cbor,
        s2.result,
      );
      expect(await resultClassOf(context, s4.result.nextThreadOutRef)).toBe(
        BigInt(evidence.resultClass),
      );
      const s5 = await stages.step05(s4.result.nextThreadOutRef, evidence);
      expect(s5.result.closed).toBe(true);
      return s5.result.nextThreadOutRef;
    };

    const contradicted = forcedEvidence(wrongful);
    expect(contradicted.resultClass).toBe(
      OutputReferenceScriptResultClasses.NoFault,
    );
    const s6 = await stages.step06(
      await toStep05(wrongful, contradicted),
      contradicted,
    );
    expect(s6.result.fraudProofUnit).toBeTruthy();

    expect(() => forcedEvidence(honest)).toThrow(
      /agrees with operator verdict/u,
    );
    const agreeing = forcedEvidence(honest, true);
    expect(agreeing.resultClass).toBe(REFUSED_CLASS_3);
    const closed = await toStep05(honest, agreeing);
    await expectOnchainRefusal(() => stages.step06Raw(closed));
    await stages.cancel(closed, 5);
  }, 900_000);
});
