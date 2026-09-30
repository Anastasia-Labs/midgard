import "node:crypto";
import "node:fs";
import "node:url";
import "@al-ft/midgard-core/codec";
import "@al-ft/midgard-core/codec/forced";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "effect";
import "vitest";
import "../src/field-opening.js";
import "../src/proof-fit/van-rossem-fit-ledger.js";
import "../src/remove-fraudulent-block.js";
import "../src/testing/complete-lifecycle.js";
import "../src/transition-trace/witnesses.js";
import "../src/witness-script-decoding/index.js";
import "./support/emulator/blueprints.js";
import "./support/emulator/emulator-context.js";
import "./support/emulator/expect-onchain-refusal.js";
import "./support/emulator/measurement.js";
import "./support/emulator/registered-chain.js";
import "./support/lifecycle-coverage.js";
import "./support/native-script-decoding-emulator.js";
import "./support/submit-init-emulator-shared.js";
import "./support/witness-script-decoding-raw.js";
import "./witness-script-decoding-lifecycle.authentication-seams.js";
import "./witness-script-decoding-lifecycle.make-harness.js";
import "./witness-script-decoding-lifecycle.scan-honest-accepted-to-close.js";

import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";

import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  buildVanRossemFitLedger,
  writeVanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import { assertCompleteLifecycleCoverage } from "../src/testing/complete-lifecycle.js";
import {
  planWitnessScriptDecodingStep03Transition,
  WitnessScriptDecodingResultClasses,
} from "../src/witness-script-decoding/index.js";
import { realBlueprintPath } from "./support/emulator/blueprints.js";
import { runEmulatorLifecycleStage } from "./support/emulator/emulator-context.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import {
  DEEP_MAXIMUM_DEPTH,
  MAXIMUM_FIELD_BYTES,
  mutateWitnessCertifiedCarriage,
  mutateWitnessCompactSource,
  mutateWitnessRawUtxoCarriage,
  mutateWitnessSet,
  scriptWitnessField,
  submitWitnessScriptDecodingStep01ForcedRaw,
} from "./support/witness-script-decoding-raw.js";
import {
  AUTHENTICATION_SEAMS,
  CANCELLABLE_STEPS,
  CATEGORY_ID,
  coverage,
  DEEP_DEPTH,
  ledgerPath,
  measurements,
  REASON_ARMS,
  WRITE_LEDGER,
} from "./witness-script-decoding-lifecycle.authentication-seams.js";
import { makeHarness } from "./witness-script-decoding-lifecycle.make-harness.js";
import {
  acceptedEvidence,
  deepShape,
  emptyPayloadShape,
  expectReplayDetection,
  forcedEvidence,
  headerAdjacentShape,
  headerMaximumShape,
  headerSmallShape,
  nativeMaximumShape,
  plutusShape,
  reasonOf,
  scanHonestAcceptedToClose,
  smallCanonicalShape,
  wideMaximumShape,
} from "./witness-script-decoding-lifecycle.scan-honest-accepted-to-close.js";

// ---------------------------------------------------------------------------
// Scenarios
// ---------------------------------------------------------------------------

describe("witnessScriptDecoding registered-chain lifecycle", () => {
  it("convicts the accepted undecodable wrapper at the maximum item, cancels every step, refuses the membership, coordinate and honest accepted polarities, then mints and removes", async () => {
    const h = await makeHarness();
    const shape = headerMaximumShape();
    const honestNative = smallCanonicalShape();
    const honestPlutus = plutusShape();
    const adjacent = headerAdjacentShape();
    const { block, setup, inclusionOf } = await h.acceptedBlock(shape, [
      honestNative,
      honestPlutus,
      adjacent,
    ]);
    const evidence = acceptedEvidence(shape);
    expect(scriptWitnessField([shape.item])).toHaveLength(MAXIMUM_FIELD_BYTES);
    expectReplayDetection(
      block,
      [shape, honestNative, honestPlutus],
      evidence,
      ["witness-script-header-malformed"],
    );
    const published = await h.publishField(shape, "accepted-header");

    // The consensus bound, one byte over. Off chain the field view refuses
    // the preimage before any evidence exists; on chain the certificate
    // policy refuses the length of the real chunk carriage, so the exact
    // 32,768-byte shape above is the last one any door can open.
    expect(() => acceptedEvidence(adjacent)).toThrow(/aggregate bound/u);
    await h.certifyOverBoundField(adjacent);

    // Cancel from every physical step.
    await h.cancel(
      await h.init(setup.fraudulentBlockOutRef, null, shape.label),
      0,
      "accepted-header-cancel-step01",
      shape.label,
    );
    const atStep02 = await h.step01Accepted(
      await h.init(setup.fraudulentBlockOutRef, null, shape.label),
      inclusionOf(shape),
      setup.fraudulentBlockOutRef,
      0n,
    );
    await h.cancel(atStep02, 1, "accepted-header-cancel-step02", shape.label);
    const atStep03 = (
      await h.step02(
        await h.step01Accepted(
          await h.init(setup.fraudulentBlockOutRef, null, shape.label),
          inclusionOf(shape),
          setup.fraudulentBlockOutRef,
          0n,
        ),
        shape,
        evidence,
        null,
        published.carriageUtxos,
        published.certificateUtxo,
      )
    ).nextThreadOutRef;
    await h.cancel(atStep03, 2, "accepted-header-cancel-step03", shape.label);
    const atStep04 = (
      await h.scanToClose(
        (
          await h.step02(
            await h.step01Accepted(
              await h.init(setup.fraudulentBlockOutRef, null, shape.label),
              inclusionOf(shape),
              setup.fraudulentBlockOutRef,
              0n,
            ),
            shape,
            evidence,
            null,
            published.carriageUtxos,
            published.certificateUtxo,
          )
        ).nextThreadOutRef,
        evidence,
        null,
        shape.label,
      )
    ).threadOutRef;
    await h.cancel(atStep04, 3, "accepted-header-cancel-step04", shape.label);

    // Step-01 seam: the transaction's membership in the committed block.
    const membershipThread = await h.init(
      setup.fraudulentBlockOutRef,
      null,
      shape.label,
    );
    await expectOnchainRefusal(() =>
      h.step01Accepted(
        membershipThread,
        { ...inclusionOf(shape), transactionsPhasRoot: "ff".repeat(32) },
        setup.fraudulentBlockOutRef,
        0n,
      ),
    );
    coverage.seamMutated("tx_membership");
    await h.cancel(membershipThread, 0);

    // Step-02 seam: a coordinate past the field's item count. Step 01 binds
    // any non-negative ordinal; the field door refuses the opening.
    const outOfRange = await h.step01Accepted(
      await h.init(setup.fraudulentBlockOutRef, null, shape.label),
      inclusionOf(shape),
      setup.fraudulentBlockOutRef,
      1n,
    );
    const outOfRangeState = h.exactScanState(evidence);
    await expectOnchainRefusal(() =>
      h.step02Raw(outOfRange, shape, evidence, published, {
        nextState: {
          ...outOfRangeState,
          bound: { ...outOfRangeState.bound, script_index: 1n },
        },
      }),
    );
    coverage.seamMutated("script_coordinate");
    coverage.scenario("reason_or_subject_coordinate_mutation");
    await h.cancel(outOfRange, 1);

    // Honest accepted polarity, native: the canonical script reaches the exact
    // terminal through the resumable scan and step 04 refuses the no-fault
    // close.
    const honestEvidence = acceptedEvidence(honestNative);
    const honestTwin = forcedEvidence(
      honestNative,
      { transactionId: "ee".repeat(32), outputIndex: 0n },
      reasonOf("WitnessNativeScriptNodeLimit", 0n),
    );
    const honestBound = (
      await h.step02(
        await h.step01Accepted(
          await h.init(setup.fraudulentBlockOutRef, null, honestNative.label),
          inclusionOf(honestNative),
          setup.fraudulentBlockOutRef,
          0n,
        ),
        honestNative,
        honestEvidence,
      )
    ).nextThreadOutRef;
    const honestClosed = await scanHonestAcceptedToClose(
      h,
      honestBound,
      honestEvidence,
      honestTwin,
    );
    expect((await h.scanStateAt(honestClosed, 3)).result_class).toBe(
      BigInt(WitnessScriptDecodingResultClasses.NoFault),
    );
    await expectOnchainRefusal(() => h.step04Raw(honestClosed));
    coverage.scenario("honest_accepted_block_refusal");
    await h.cancel(honestClosed, 3);

    // Honest accepted polarity, non-native: a decodable Plutus wrapper is a
    // successful decoder result and closes at step 02.
    const plutusEvidence = acceptedEvidence(honestPlutus);
    const plutusClosed = (
      await h.scanToClose(
        (
          await h.step02(
            await h.step01Accepted(
              await h.init(
                setup.fraudulentBlockOutRef,
                null,
                honestPlutus.label,
              ),
              inclusionOf(honestPlutus),
              setup.fraudulentBlockOutRef,
              0n,
            ),
            honestPlutus,
            plutusEvidence,
          )
        ).nextThreadOutRef,
        plutusEvidence,
        null,
        honestPlutus.label,
      )
    ).threadOutRef;
    await expectOnchainRefusal(() => h.step04Raw(plutusClosed));
    await h.cancel(plutusClosed, 3);

    // The real lifecycle: Init, bind, open the maximum certified field, close
    // the header class through step 03, mint the proof, remove the block.
    const init = await h.init(
      setup.fraudulentBlockOutRef,
      "accepted-header-init",
      shape.label,
    );
    const bound = await h.step01Accepted(
      init,
      inclusionOf(shape),
      setup.fraudulentBlockOutRef,
      0n,
      "accepted-header-step01",
      shape.label,
    );
    const opened = await h.step02(
      bound,
      shape,
      evidence,
      "accepted-header-step02",
      published.carriageUtxos,
      published.certificateUtxo,
    );
    expect(opened.scanState.result_class).toBe(
      BigInt(WitnessScriptDecodingResultClasses.HeaderMalformed),
    );
    const closed = await h.scanToClose(
      opened.nextThreadOutRef,
      evidence,
      "accepted-header",
      shape.label,
    );
    expect(closed.resumes).toBe(0);
    await h.step04(
      closed.threadOutRef,
      evidence,
      "accepted-header-step04-proof-mint",
      shape.label,
    );
    await h.expectThreadsGone(setup.headerHash);
    coverage.reason("WitnessScriptHeaderMalformed", "accepted_invalid");
    coverage.scenario("wrongful_acceptance_success");
    coverage.scenario("maximum_supported_evidence");
    await h.remove(setup.headerHash, "accepted-header-remove", shape.label);
  }, 900_000);

  it("convicts the accepted malformed payload at the maximum item through the resumable scan, refusing every field-opening and scan seam first", async () => {
    const h = await makeHarness();
    const shape = nativeMaximumShape();
    const { block, setup, inclusionOf } = await h.acceptedBlock(shape);
    const evidence = acceptedEvidence(shape);
    expect(scriptWitnessField([shape.item])).toHaveLength(MAXIMUM_FIELD_BYTES);
    expect(evidence.chunkProofCount).toBe(9);
    expectReplayDetection(block, [shape], evidence, [
      "witness-native-script-malformed",
    ]);
    const published = await h.publishField(shape, "accepted-native");
    const other = plutusShape(1_100n);

    // Step-02 seams on one bound thread.
    const bound = await h.step01Accepted(
      await h.init(setup.fraudulentBlockOutRef, null, shape.label),
      inclusionOf(shape),
      setup.fraudulentBlockOutRef,
      0n,
    );
    await expectOnchainRefusal(() =>
      h.step02Raw(bound, shape, evidence, published, {
        mutateOpening: (opening) =>
          mutateWitnessCertifiedCarriage(opening, (carriage) => ({
            ...carriage,
            cert_ref_input_index: carriage.chunk_ref_input_indices[0]!,
          })),
      }),
    );
    coverage.seamMutated("field_certificate");
    await expectOnchainRefusal(() =>
      h.step02Raw(bound, shape, evidence, published, {
        mutateOpening: (opening) =>
          mutateWitnessCertifiedCarriage(opening, (carriage) => ({
            ...carriage,
            chunk_ref_input_indices: [
              carriage.chunk_ref_input_indices[1]!,
              carriage.chunk_ref_input_indices[0]!,
              ...carriage.chunk_ref_input_indices.slice(2),
            ],
          })),
      }),
    );
    coverage.seamMutated("field_chunks");
    await expectOnchainRefusal(() =>
      h.step02Raw(bound, shape, evidence, published, {
        mutateOpening: (opening) =>
          mutateWitnessCompactSource(opening, other.carriage.compactCbor),
      }),
    );
    coverage.seamMutated("native_tx_source");
    await expectOnchainRefusal(() =>
      h.step02Raw(bound, shape, evidence, published, {
        mutateOpening: (opening) =>
          mutateWitnessSet(opening, other.carriage.witnessSet),
      }),
    );
    coverage.seamMutated("witness_set");
    const exact = h.exactScanState(evidence);
    await expectOnchainRefusal(() =>
      h.step02Raw(bound, shape, evidence, published, {
        nextState: { ...exact, item_commitment: "ab".repeat(32) },
      }),
    );
    coverage.seamMutated("item_commitment");
    await expectOnchainRefusal(() =>
      h.step02Raw(bound, shape, evidence, published, { nextStepIndex: 3 }),
    );
    coverage.seamMutated("successor_script");
    const opened = await h.step02(
      bound,
      shape,
      evidence,
      null,
      published.carriageUtxos,
      published.certificateUtxo,
    );
    expect(opened.scanState.result_class).toBe(
      BigInt(WitnessScriptDecodingResultClasses.Pending),
    );

    // Step-03 seams on the pending scan state. The first segment of this
    // shape consumes one frame witness (container, leaf, frame pop) and stops
    // before the refusing token.
    const state = await h.scanStateAt(opened.nextThreadOutRef, 2);
    const transition = planWitnessScriptDecodingStep03Transition({
      state,
      evidence,
      contracts: h.contracts,
    });
    expect(transition.route).toBe("segment");
    expect(transition.args.frames).toHaveLength(1);
    expect(transition.args.next_chunk_proof).not.toBeNull();
    await expectOnchainRefusal(() =>
      h.step03Raw(opened.nextThreadOutRef, transition, {
        args: {
          ...transition.args,
          chunk_proof: transition.args.next_chunk_proof,
          next_chunk_proof: transition.args.chunk_proof,
        },
      }),
    );
    coverage.seamMutated("scan_chunk");
    await expectOnchainRefusal(() =>
      h.step03Raw(opened.nextThreadOutRef, transition, {
        args: {
          ...transition.args,
          control_cbor: transition.nextState.control_cbor,
        },
      }),
    );
    coverage.seamMutated("scan_control");
    await expectOnchainRefusal(() =>
      h.step03Raw(opened.nextThreadOutRef, transition, {
        args: {
          ...transition.args,
          frames: transition.args.frames.map((frame) => ({
            ...frame,
            remaining: frame.remaining + 1n,
          })),
        },
      }),
    );
    coverage.seamMutated("scan_frame");
    await expectOnchainRefusal(() =>
      h.step03Raw(opened.nextThreadOutRef, transition, {
        args: { ...transition.args, step_budget: 0n },
      }),
    );
    coverage.seamMutated("scan_budget");
    await expectOnchainRefusal(() =>
      h.step03Raw(opened.nextThreadOutRef, transition, {
        nextState: {
          ...transition.nextState,
          checkpoint_hash: "cd".repeat(32),
        },
      }),
    );
    coverage.seamMutated("scan_checkpoint");
    await expectOnchainRefusal(() =>
      h.step03Raw(opened.nextThreadOutRef, transition, { nextStepIndex: 3 }),
    );
    await h.cancel(opened.nextThreadOutRef, 2);

    // The real lifecycle on a fresh thread.
    const init = await h.init(
      setup.fraudulentBlockOutRef,
      "accepted-native-init",
      shape.label,
    );
    const rebound = await h.step01Accepted(
      init,
      inclusionOf(shape),
      setup.fraudulentBlockOutRef,
      0n,
      "accepted-native-step01",
      shape.label,
    );
    const reopened = await h.step02(
      rebound,
      shape,
      evidence,
      "accepted-native-step02",
      published.carriageUtxos,
      published.certificateUtxo,
    );
    const closed = await h.scanToClose(
      reopened.nextThreadOutRef,
      evidence,
      "accepted-native",
      shape.label,
    );
    expect(closed.resumes).toBeGreaterThan(0);
    expect((await h.scanStateAt(closed.threadOutRef, 3)).result_class).toBe(
      BigInt(WitnessScriptDecodingResultClasses.NativeMalformed),
    );
    await h.step04(
      closed.threadOutRef,
      evidence,
      "accepted-native-step04-proof-mint",
      shape.label,
    );
    await h.expectThreadsGone(setup.headerHash);
    coverage.reason("WitnessNativeScriptMalformed", "accepted_invalid");
    coverage.scenario("wrongful_acceptance_success");
    coverage.scenario("maximum_supported_evidence");
    await h.remove(setup.headerHash, "accepted-native-remove", shape.label);
  }, 900_000);

  it("convicts the accepted empty tag-0 payload, which step 02 closes as the structural class", async () => {
    const h = await makeHarness();
    const shape = emptyPayloadShape();
    const { block, setup, inclusionOf } = await h.acceptedBlock(shape);
    const evidence = acceptedEvidence(shape);
    expect(evidence.initialResultClass).toBe(
      WitnessScriptDecodingResultClasses.NativeMalformed,
    );
    expectReplayDetection(block, [shape], evidence, [
      "witness-native-script-malformed",
    ]);
    const init = await h.init(
      setup.fraudulentBlockOutRef,
      "accepted-empty-init",
      shape.label,
    );
    const bound = await h.step01Accepted(
      init,
      inclusionOf(shape),
      setup.fraudulentBlockOutRef,
      0n,
      "accepted-empty-step01",
      shape.label,
    );
    const opened = await h.step02(
      bound,
      shape,
      evidence,
      "accepted-empty-step02",
    );
    expect(opened.scanState.result_class).toBe(
      BigInt(WitnessScriptDecodingResultClasses.NativeMalformed),
    );
    expect(opened.scanState.control_cbor).toBe("");
    const closed = await h.scanToClose(
      opened.nextThreadOutRef,
      evidence,
      "accepted-empty",
      shape.label,
    );
    expect(closed.resumes).toBe(0);
    await h.step04(
      closed.threadOutRef,
      evidence,
      "accepted-empty-step04-proof-mint",
      shape.label,
    );
    await h.expectThreadsGone(setup.headerHash);
    coverage.reason("WitnessNativeScriptMalformed", "accepted_invalid");
    coverage.scenario("wrongful_acceptance_success");
    await h.remove(setup.headerHash, "accepted-empty-remove", shape.label);
  }, 600_000);

  it("contradicts a wrongful header-malformed rejection of a decodable script, refusing every forced-door seam first", async () => {
    const h = await makeHarness();
    const shape = smallCanonicalShape();
    const reason = reasonOf("WitnessScriptHeaderMalformed", 0n);
    const forced = await h.forcedBlock(shape, reason);
    const evidence = forcedEvidence(shape, forced.orderKey, reason);
    expectReplayDetection(forced.block, [], evidence, [
      "witness-script-header-malformed",
    ]);

    const seams: readonly [
      (typeof AUTHENTICATION_SEAMS)[number],
      Partial<Parameters<typeof submitWitnessScriptDecodingStep01ForcedRaw>[0]>,
    ][] = [
      [
        "forced_leaf_header",
        {
          header: {
            ...forced.block.header,
            forcedTransactionsRoot: "ff".repeat(32),
          },
        },
      ],
      [
        "forced_leaf_membership",
        {
          membership: {
            ...forced.membership,
            key: {
              ...forced.membership.key,
              outputIndex: forced.membership.key.outputIndex + 1n,
            },
          },
        },
      ],
      ["forced_direction", { direction: 0n }],
      ["script_coordinate", { scriptIndex: 1n }],
      ["bound_witness_set_hash", { witnessSetHash: "ab".repeat(32) }],
      ["bound_accused_class", { accusedClass: 1n }],
      ["successor_script", { nextStepIndex: 2 }],
    ];
    for (const [seam, patch] of seams) {
      const thread = await h.init(
        forced.setup.fraudulentBlockOutRef,
        null,
        shape.label,
      );
      await expectOnchainRefusal(() =>
        h.step01ForcedRaw(thread, forced, evidence, patch),
      );
      coverage.seamMutated(seam);
      await h.cancel(thread, 0);
    }
    coverage.scenario("reason_or_subject_coordinate_mutation");

    // Step-02 seam on the published (RawUtxo) tier: the same small field,
    // published whole, opened against a reference input that is not its
    // publication. The door refuses the carriage on chain.
    const published = await h.publishField(shape, null);
    expect(published.certificateUtxo).toBeUndefined();
    const rawThread = await h.step01Forced(
      await h.init(forced.setup.fraudulentBlockOutRef, null, shape.label),
      forced,
      evidence.finding.witnessSetHash,
      0n,
    );
    await expectOnchainRefusal(() =>
      h.step02Raw(rawThread, shape, evidence, published, {
        mutateOpening: (opening) => mutateWitnessRawUtxoCarriage(opening, 1n),
      }),
    );
    coverage.seamMutated("field_raw_utxo");
    await h.cancel(rawThread, 1);

    const init = await h.init(
      forced.setup.fraudulentBlockOutRef,
      "forced-header-init",
      shape.label,
    );
    const bound = await h.step01Forced(
      init,
      forced,
      evidence.finding.witnessSetHash,
      0n,
      "forced-header-step01",
      shape.label,
    );
    const opened = await h.step02(
      bound,
      shape,
      evidence,
      "forced-header-step02",
    );
    const closed = await h.scanToClose(
      opened.nextThreadOutRef,
      evidence,
      "forced-header",
      shape.label,
    );
    expect((await h.scanStateAt(closed.threadOutRef, 3)).result_class).toBe(
      BigInt(WitnessScriptDecodingResultClasses.NoFault),
    );
    await h.step04(
      closed.threadOutRef,
      evidence,
      "forced-header-step04-proof-mint",
      shape.label,
    );
    await h.expectThreadsGone(forced.setup.headerHash);
    coverage.reason("WitnessScriptHeaderMalformed", "forced_rejection_wrong");
    coverage.scenario("wrongful_forced_rejection_success");
    await h.remove(
      forced.setup.headerHash,
      "forced-header-remove",
      shape.label,
    );
  }, 600_000);

  it("contradicts a wrongful native-malformed rejection of a non-native script, which closes at step 02", async () => {
    const h = await makeHarness();
    const shape = plutusShape();
    const reason = reasonOf("WitnessNativeScriptMalformed", 0n);
    const forced = await h.forcedBlock(shape, reason);
    const evidence = forcedEvidence(shape, forced.orderKey, reason);
    const init = await h.init(
      forced.setup.fraudulentBlockOutRef,
      "forced-native-init",
      shape.label,
    );
    const bound = await h.step01Forced(
      init,
      forced,
      evidence.finding.witnessSetHash,
      0n,
      "forced-native-step01",
      shape.label,
    );
    const opened = await h.step02(
      bound,
      shape,
      evidence,
      "forced-native-step02",
    );
    expect(opened.scanState.result_class).toBe(
      BigInt(WitnessScriptDecodingResultClasses.NoFault),
    );
    const closed = await h.scanToClose(
      opened.nextThreadOutRef,
      evidence,
      "forced-native",
      shape.label,
    );
    await h.step04(
      closed.threadOutRef,
      evidence,
      "forced-native-step04-proof-mint",
      shape.label,
    );
    await h.expectThreadsGone(forced.setup.headerHash);
    coverage.reason("WitnessNativeScriptMalformed", "forced_rejection_wrong");
    coverage.scenario("wrongful_forced_rejection_success");
    await h.remove(
      forced.setup.headerHash,
      "forced-native-remove",
      shape.label,
    );
  }, 600_000);

  it("contradicts a wrongful node-limit rejection of the widest canonical script the field bound admits", async () => {
    const h = await makeHarness();
    const shape = wideMaximumShape();
    expect(scriptWitnessField([shape.item])).toHaveLength(MAXIMUM_FIELD_BYTES);
    const reason = reasonOf("WitnessNativeScriptNodeLimit", 0n);
    const forced = await h.forcedBlock(shape, reason);
    const evidence = forcedEvidence(shape, forced.orderKey, reason);
    expect(evidence.resultClass).toBe(
      WitnessScriptDecodingResultClasses.NoFault,
    );
    const published = await h.publishField(shape, "forced-node-wide");
    const init = await h.init(
      forced.setup.fraudulentBlockOutRef,
      "forced-node-wide-init",
      shape.label,
    );
    const bound = await h.step01Forced(
      init,
      forced,
      evidence.finding.witnessSetHash,
      0n,
      "forced-node-wide-step01",
      shape.label,
    );
    const opened = await h.step02(
      bound,
      shape,
      evidence,
      "forced-node-wide-step02",
      published.carriageUtxos,
      published.certificateUtxo,
    );
    const closed = await h.scanToClose(
      opened.nextThreadOutRef,
      evidence,
      "forced-node-wide",
      shape.label,
    );
    // 2,119 primitive steps in sixteen-step segments, plus one segment cut
    // at a bounded-item chunk window: 134 scan transactions, 133 of them
    // resumes, most opening on a frame step with their window supplied.
    expect(closed.resumes).toBe(133);
    await h.step04(
      closed.threadOutRef,
      evidence,
      "forced-node-wide-step04-proof-mint",
      shape.label,
    );
    await h.expectThreadsGone(forced.setup.headerHash);
    coverage.reason("WitnessNativeScriptNodeLimit", "forced_rejection_wrong");
    coverage.scenario("wrongful_forced_rejection_success");
    coverage.scenario("maximum_supported_evidence");
    await h.remove(
      forced.setup.headerHash,
      "forced-node-wide-remove",
      shape.label,
    );
  }, 900_000);

  it("contradicts a wrongful depth-limit rejection of the deepest canonical script the field bound admits", async () => {
    const h = await makeHarness();
    const shape = deepShape();
    if (DEEP_DEPTH === DEEP_MAXIMUM_DEPTH)
      expect(scriptWitnessField([shape.item])).toHaveLength(
        MAXIMUM_FIELD_BYTES,
      );
    const reason = reasonOf("WitnessNativeScriptDepthLimit", 0n);
    const forced = await h.forcedBlock(shape, reason);
    const evidence = forcedEvidence(shape, forced.orderKey, reason);
    expect(evidence.resultClass).toBe(
      WitnessScriptDecodingResultClasses.NoFault,
    );
    const published = await h.publishField(shape, "forced-depth-deep");
    const init = await h.init(
      forced.setup.fraudulentBlockOutRef,
      "forced-depth-deep-init",
      shape.label,
    );
    const bound = await h.step01Forced(
      init,
      forced,
      evidence.finding.witnessSetHash,
      0n,
      "forced-depth-deep-step01",
      shape.label,
    );
    const opened = await h.step02(
      bound,
      shape,
      evidence,
      "forced-depth-deep-step02",
      published.carriageUtxos,
      published.certificateUtxo,
    );
    const closed = await h.scanToClose(
      opened.nextThreadOutRef,
      evidence,
      "forced-depth-deep",
      shape.label,
    );
    // `2·depth + 2` primitive steps in sixteen-step segments, plus the cuts
    // the planner makes at the eight bounded-item chunk windows of the
    // maximum item: 1,369 scan transactions at the maximum depth, 1,368 of
    // them resumes through a real checkpoint.
    expect(closed.resumes).toBeGreaterThanOrEqual(
      Math.ceil((2 * DEEP_DEPTH + 2) / 16) - 1,
    );
    if (DEEP_DEPTH === DEEP_MAXIMUM_DEPTH) expect(closed.resumes).toBe(1_368);
    await h.step04(
      closed.threadOutRef,
      evidence,
      "forced-depth-deep-step04-proof-mint",
      shape.label,
    );
    await h.expectThreadsGone(forced.setup.headerHash);
    coverage.reason("WitnessNativeScriptDepthLimit", "forced_rejection_wrong");
    coverage.scenario("wrongful_forced_rejection_success");
    if (DEEP_DEPTH === DEEP_MAXIMUM_DEPTH)
      coverage.scenario("maximum_supported_evidence");
    await runEmulatorLifecycleStage("witness-depth.remove", () =>
      h.remove(
        forced.setup.headerHash,
        "forced-depth-deep-remove",
        shape.label,
      ),
    );
  }, 1_800_000);

  it("refuses to contradict honest forced rejections: the undecodable wrapper and the empty payload", async () => {
    for (const [shape, arm] of [
      [headerSmallShape(), "WitnessScriptHeaderMalformed"],
      [emptyPayloadShape(), "WitnessNativeScriptMalformed"],
    ] as const) {
      const h = await makeHarness();
      const reason = reasonOf(arm, 0n);
      const forced = await h.forcedBlock(shape, reason);
      const evidence = forcedEvidence(shape, forced.orderKey, reason);
      expect(evidence.resultClass).toBe(evidence.finding.accusedClass);
      const bound = await h.step01Forced(
        await h.init(forced.setup.fraudulentBlockOutRef, null, shape.label),
        forced,
        evidence.finding.witnessSetHash,
        0n,
      );
      const opened = await h.step02(bound, shape, evidence);
      const closed = await h.scanToClose(
        opened.nextThreadOutRef,
        evidence,
        null,
        shape.label,
      );
      await expectOnchainRefusal(() => h.step04Raw(closed.threadOutRef));
      coverage.reason(arm);
      coverage.scenario("honest_forced_rejection_refusal");
      await h.cancel(closed.threadOutRef, 3);
    }
  }, 600_000);

  it("refuses to bind a forced rejection whose authenticated leaf carries a sibling typed reason", async () => {
    const h = await makeHarness();
    const shape = smallCanonicalShape();
    const forced = await h.forcedBlock(shape, "NetworkIdMismatch");
    const thread = await h.init(
      forced.setup.fraudulentBlockOutRef,
      null,
      shape.label,
    );
    await expectOnchainRefusal(() =>
      submitWitnessScriptDecodingStep01ForcedRaw({
        lucid: h.lucid,
        contracts: h.contracts,
        categoryId: h.categoryId,
        signer: h.signer,
        threadOutRef: thread,
        header: forced.block.header,
        membership: forced.membership,
        direction: 1n,
        subject: SDK.forcedVerdictSubject({
          transactionId: shape.txId,
          sourceKey: forced.orderKey,
          rejectionReason: "NetworkIdMismatch",
        }),
        witnessSetHash: shape.carriage.witnessSetHash,
        scriptIndex: 0n,
        accusedClass: 0n,
        referenceScriptUtxo: h.ref(0),
      }),
    );
    coverage.seamMutated("forced_leaf_reason");
    coverage.scenario("reason_or_subject_coordinate_mutation");
    await h.cancel(thread, 0);
  }, 600_000);

  it("closes the coverage gate and the Van Rossem fit ledger", async () => {
    // A successful proof mint is not a completed journey. Refuse to write
    // partial evidence if any preceding lifecycle failed during removal.
    expect(
      measurements
        .map((entry) => entry.name)
        .filter((name) => name.endsWith("-remove"))
        .sort(),
    ).toEqual([
      "accepted-empty-remove",
      "accepted-header-remove",
      "accepted-native-remove",
      "forced-depth-deep-remove",
      "forced-header-remove",
      "forced-native-remove",
      "forced-node-wide-remove",
    ]);
    // Recorded honestly: every arm, seam, cancel, resume and the adjacent
    // refusal at the aggregate field bound are reached. The two omissions
    // the gate reports are the wrongful-acceptance directions of the
    // node-limit and depth-limit arms, which no canonical field-6 item can
    // realise: the aggregate preimage is bounded at 32,768 bytes and a node
    // costs at least three bytes, so 16,385 nodes (or depth 16,385) need at
    // least 49,155 bytes. The widest and deepest shapes above stop at 1,059
    // nodes and depth 10,909; the exact-bound and adjacent-over-bound
    // node/depth refusals are engine facts pinned by the family's Aiken and
    // TypeScript selectors, not lifecycles.
    expect(() =>
      assertCompleteLifecycleCoverage({
        coverage: coverage.snapshot(),
        expectedReasonArms: [...REASON_ARMS],
        authenticationSeams: [...AUTHENTICATION_SEAMS],
        cancellablePhysicalSteps: [...CANCELLABLE_STEPS],
        resumable: true,
        hasAdjacentConsensusBound: true,
      }),
    ).toThrow(
      "incomplete fault-proof lifecycle coverage: WitnessNativeScriptNodeLimit success directions: accepted_invalid; WitnessNativeScriptDepthLimit success directions: accepted_invalid",
    );
    const blueprintBytes = readFileSync(realBlueprintPath);
    const preamble = JSON.parse(blueprintBytes.toString("utf8")) as {
      readonly preamble?: { readonly compiler?: { readonly version?: string } };
    };
    const ledger = buildVanRossemFitLedger({
      category: `witnessScriptDecoding:${CATEGORY_ID}:testnet`,
      blueprintSha256: createHash("sha256")
        .update(blueprintBytes)
        .digest("hex"),
      compilerVersion: `aiken ${preamble.preamble?.compiler?.version ?? "unknown"}`,
      measurements,
    });
    for (const entry of ledger.entries) {
      expect(entry.signedByteMargin, entry.name).toBeGreaterThan(0);
      expect(BigInt(entry.memoryUnitMargin), entry.name).toBeGreaterThan(0n);
      expect(BigInt(entry.cpuUnitMargin), entry.name).toBeGreaterThan(0n);
      if (entry.kind === "publication")
        expect(
          entry.publicationReserveMargin,
          entry.name,
        ).toBeGreaterThanOrEqual(0);
    }
    if (WRITE_LEDGER) {
      if (DEEP_DEPTH !== DEEP_MAXIMUM_DEPTH)
        throw new Error("the ledger is written only at the maximum deep shape");
      await writeVanRossemFitLedger(ledgerPath, ledger);
      console.info(`[witness-script-decoding-fit-ledger] wrote ${ledgerPath}`);
    }
    console.info(
      `[witness-script-decoding-fit-ledger] ${JSON.stringify(ledger.entries)}`,
    );
  });
});
