/**
 * `native-script-decoding` abort, refusal and resume coverage (#635, #633;
 * offchain plan §8.2 suite 6).
 *
 * What this file pins:
 *
 * - `ct.Cancel` at every one of the six validators, each releasing the thread to
 *   its named prover and burning the thread NFT, so a cancelled header can be
 *   re-initialised from scratch.
 * - The two fail-closed refusals a well-formed thread can walk into: a Scan
 *   segment replaying a control the thread has already advanced past, and
 *   item bytes substituted under the frozen anchor.
 * - The §7.2 out-of-domain polarity: OpenSubject closes a genuinely
 *   out-of-domain direction-B accusation and routes an in-domain subject to
 *   BindDescriptor instead.
 * - §7.1 crash-resume: a thread abandoned mid-loop is located by its asset
 *   name and resumed from the `machine_state_hash` boundary to the mint.
 *
 * The adversarial-prover negatives — an honest commitment attacked from
 * outside — live in their own file (suite 7).
 *
 * Lives in its own file for the reason its siblings do: `@lucid-evolution/uplc`
 * never reclaims wasm linear memory and vitest isolates per FILE. See
 * tests/support/uplc-heap-guard.ts.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { Data, toUnit } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  buildNativeScriptDecodingScanPlan,
  type NativeScriptDecodingFinding,
  NativeScriptDecodingProvability,
  proveNativeScriptDecodingFault,
  submitNativeScriptDecodingCancel,
  submitNativeScriptDecodingInit,
  submitNativeScriptDecodingStep01BindNormal,
  submitNativeScriptDecodingStep01RecordForced,
  submitNativeScriptDecodingStep02,
  submitNativeScriptDecodingStep03AdvanceOrCloseClose,
  submitNativeScriptDecodingStep03AdvanceOrCloseSegment,
  submitNativeScriptDecodingStep03BindDescriptor,
  submitNativeScriptDecodingStep03OpenSubject,
} from "../src/native-script-decoding/index.js";
import {
  decodingCanonicalItem,
  decodingMalformedMultiChunkItem,
  decodingProverDeps,
  type DecodingScenario,
  makeDecodingEmulatorHarness,
  publishDecodingReferenceScripts,
  setupDecodingScenario,
  submitRawDecodingCancel,
} from "./support/native-script-decoding-emulator.js";
import {
  expectSingleUtxoWithUnit,
  network,
} from "./support/submit-init-emulator-shared.js";

const FORCED_ORDER_KEY: SDK.OutputReference = {
  transactionId: "cd".repeat(32),
  outputIndex: 0n,
};

/** A rejection accusing the named (source kind, ordinal) pair. */
const rejectionAccusing = (
  sourceKind: bigint,
  ordinal: bigint,
): SDK.OperatorVerdict => ({
  ForcedTxInvalid: {
    reason: {
      ResolvedReferenceScriptMalformed: {
        source_kind: sourceKind,
        input_index: ordinal,
      },
    },
  },
});

type Harness = Awaited<ReturnType<typeof makeDecodingEmulatorHarness>>;

const catalogueOf = (harness: Harness) => ({
  policyId: harness.contracts.fraudProofCatalogue.policyId,
  spendingScriptAddress:
    harness.contracts.fraudProofCatalogue.spendingScriptAddress,
  root: harness.catalogue.root,
});

const subjectOutpointKeyCbor = (scenario: DecodingScenario): string =>
  Buffer.from(
    SDK.encodeMidgardTxInputCanonical(scenario.subjectFieldInputs[0]!),
  ).toString("hex");

describe("native-script-decoding aborts, refusals and resume", () => {
  it("cancels a thread at each of the six validators and re-initialises after every abort", async () => {
    const harness = await makeDecodingEmulatorHarness();
    const { proverLucid, proverSigner, decoding, category, realBlueprint } =
      harness;
    const item = decodingMalformedMultiChunkItem();
    const scenario = await setupDecodingScenario({
      harness,
      referenceScriptItemBytes: item,
      source: { kind: "normal" },
    });
    const { block, ledger, setup } = scenario;
    if (block.txInclusion === null) {
      throw new Error("normal-source fixture carries no tx inclusion");
    }
    const plan = buildNativeScriptDecodingScanPlan({
      itemBytes: item,
      direction: 0,
    });
    const [
      step01Ref,
      step02Ref,
      step03OpenSubjectRef,
      step03BindDescriptorRef,
      step03AdvanceOrCloseRef,
      step04Ref,
    ] = await publishDecodingReferenceScripts({
      lucid: harness.funderLucid,
      contracts: decoding,
    });
    const stepRefs = [
      step01Ref,
      step02Ref,
      step03OpenSubjectRef,
      step03BindDescriptorRef,
      step03AdvanceOrCloseRef,
      step04Ref,
    ] as const;
    const threadUnit = toUnit(
      decoding.computationThread.policyId,
      `${category.categoryId}${setup.headerHash}`,
    );
    const shared = {
      lucid: proverLucid,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      contracts: decoding,
      categoryId: category.categoryId,
      signer: proverSigner,
    };

    const initThread = async () =>
      (
        await submitNativeScriptDecodingInit({
          lucid: proverLucid,
          witnessReferenceScripts: harness.witnessReferenceScripts,
          blueprint: realBlueprint,
          network,
          contracts: decoding,
          category,
          catalogue: catalogueOf(harness),
          signer: proverSigner,
          fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
        })
      ).nextThreadOutRef;

    // The thread is advanced one step further before each cancel, so every
    // step's `ct.Cancel` arm is exercised on a live thread.
    for (const cancelAt of [0, 1, 2, 3, 4, 5] as const) {
      let threadOutRef = await initThread();
      if (cancelAt >= 1) {
        threadOutRef = (
          await submitNativeScriptDecodingStep01BindNormal({
            ...shared,
            blueprint: realBlueprint,
            network,
            threadOutRef,
            stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
            txInclusion: block.txInclusion,
            referenceScriptUtxo: step01Ref,
          })
        ).nextThreadOutRef;
      }
      if (cancelAt >= 2) {
        threadOutRef = (
          await submitNativeScriptDecodingStep02({
            ...shared,
            threadOutRef,
            reconstruction: block.reconstruction,
            chosenOutpoint: {
              sourceKind: scenario.accusedSourceKind,
              cursor: 0n,
            },
            referenceScriptUtxo: step02Ref,
          })
        ).nextThreadOutRef;
      }
      if (cancelAt >= 3) {
        threadOutRef = (
          await submitNativeScriptDecodingStep03OpenSubject({
            ...shared,
            threadOutRef,
            nativeTxCompactCbor: block.nativeTxCompactCbor,
            subjectFieldInputs: scenario.subjectFieldInputs,
            referenceScriptUtxo: step03OpenSubjectRef,
          })
        ).nextThreadOutRef;
      }
      if (cancelAt >= 4) {
        threadOutRef = (
          await submitNativeScriptDecodingStep03BindDescriptor({
            ...shared,
            threadOutRef,
            outpointKeyCbor: subjectOutpointKeyCbor(scenario),
            descriptorCbor: ledger.descriptorCbor,
            ledgerTrie: ledger.trie,
            plan,
            referenceScriptItemBytes: item,
            referenceScriptUtxo: step03BindDescriptorRef,
          })
        ).nextThreadOutRef;
      }
      if (cancelAt === 5) {
        for (const segment of plan.segments) {
          threadOutRef = (
            await submitNativeScriptDecodingStep03AdvanceOrCloseSegment({
              ...shared,
              threadOutRef,
              segment,
              referenceScriptItemBytes: item,
              referenceScriptUtxo: step03AdvanceOrCloseRef,
            })
          ).nextThreadOutRef;
        }
        threadOutRef = (
          await submitNativeScriptDecodingStep03AdvanceOrCloseClose({
            ...shared,
            threadOutRef,
            verdict: plan.verdict,
            referenceScriptItemBytes: item,
            referenceScriptUtxo: step03AdvanceOrCloseRef,
          })
        ).nextThreadOutRef;
      }
      const cancelled = await submitNativeScriptDecodingCancel({
        ...shared,
        threadOutRef,
        referenceScriptUtxo: stepRefs[cancelAt],
      });
      expect(cancelled.cancelledStepIndex).toBe(cancelAt);
      expect(cancelled.fraudulentHeaderHash).toBe(setup.headerHash);
      // The thread NFT is burned: nothing of it survives at any step.
      for (const step of decoding.steps) {
        await expect(
          proverLucid.utxosAtWithUnit(step.spendingScriptAddress, threadUnit),
        ).resolves.toHaveLength(0);
      }
    }
    // The raw cancel builder the adversarial suite uses to reach the
    // validator's signature demand is proven LANDABLE here, signed by the
    // thread's own prover: its refusals over there are the validator's, not
    // an inability to assemble the transaction at all.
    const rawThreadOutRef = await initThread();
    const [rawTxHash, rawOutputIndex] = rawThreadOutRef.split("#");
    const [rawThreadUtxo] = await proverLucid.utxosByOutRef([
      { txHash: rawTxHash!, outputIndex: Number(rawOutputIndex) },
    ]);
    await submitRawDecodingCancel({
      lucid: proverLucid,
      contracts: decoding,
      signer: proverSigner,
      stepIndex: 0,
      threadUtxo: rawThreadUtxo!,
      threadUnit,
      threadAssetName: `${category.categoryId}${setup.headerHash}`,
      referenceScriptUtxo: step01Ref,
      computationThreadReferenceUtxo:
        harness.witnessReferenceScripts.computationThreadMint!,
    });
    for (const step of decoding.steps) {
      await expect(
        proverLucid.utxosAtWithUnit(step.spendingScriptAddress, threadUnit),
      ).resolves.toHaveLength(0);
    }

    // Nothing was ever adjudicated, so no fraud-proof token exists.
    await expect(
      proverLucid.utxosAtWithUnit(
        decoding.fraudProof.spendingScriptAddress,
        toUnit(
          decoding.fraudProof.policyId,
          `${category.categoryId}${setup.headerHash}`,
        ),
      ),
    ).resolves.toHaveLength(0);
  }, 600_000);

  it("refuses a Scan that replays a stale control and item bytes that miss the frozen anchor", async () => {
    const harness = await makeDecodingEmulatorHarness();
    const { proverLucid, proverSigner, decoding, category, realBlueprint } =
      harness;
    const item = decodingMalformedMultiChunkItem();
    const scenario = await setupDecodingScenario({
      harness,
      referenceScriptItemBytes: item,
      source: { kind: "normal" },
    });
    const { block, ledger, setup } = scenario;
    if (block.txInclusion === null) {
      throw new Error("normal-source fixture carries no tx inclusion");
    }
    const plan = buildNativeScriptDecodingScanPlan({
      itemBytes: item,
      direction: 0,
    });
    const [
      step01Ref,
      step02Ref,
      step03OpenSubjectRef,
      step03BindDescriptorRef,
      step03AdvanceOrCloseRef,
    ] = await publishDecodingReferenceScripts({
      lucid: harness.funderLucid,
      contracts: decoding,
    });
    const shared = {
      lucid: proverLucid,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      contracts: decoding,
      categoryId: category.categoryId,
      signer: proverSigner,
    };
    const init = await submitNativeScriptDecodingInit({
      lucid: proverLucid,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      blueprint: realBlueprint,
      network,
      contracts: decoding,
      category,
      catalogue: catalogueOf(harness),
      signer: proverSigner,
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
    });
    const step01 = await submitNativeScriptDecodingStep01BindNormal({
      ...shared,
      blueprint: realBlueprint,
      network,
      threadOutRef: init.nextThreadOutRef,
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      txInclusion: block.txInclusion,
      referenceScriptUtxo: step01Ref,
    });
    const step02 = await submitNativeScriptDecodingStep02({
      ...shared,
      threadOutRef: step01.nextThreadOutRef,
      reconstruction: block.reconstruction,
      chosenOutpoint: { sourceKind: scenario.accusedSourceKind, cursor: 0n },
      referenceScriptUtxo: step02Ref,
    });
    const opened = await submitNativeScriptDecodingStep03OpenSubject({
      ...shared,
      threadOutRef: step02.nextThreadOutRef,
      nativeTxCompactCbor: block.nativeTxCompactCbor,
      subjectFieldInputs: scenario.subjectFieldInputs,
      referenceScriptUtxo: step03OpenSubjectRef,
    });
    const bind = await submitNativeScriptDecodingStep03BindDescriptor({
      ...shared,
      threadOutRef: opened.nextThreadOutRef,
      outpointKeyCbor: subjectOutpointKeyCbor(scenario),
      descriptorCbor: ledger.descriptorCbor,
      ledgerTrie: ledger.trie,
      plan,
      referenceScriptItemBytes: item,
      referenceScriptUtxo: step03BindDescriptorRef,
    });

    // Substituted item bytes: the thread froze the anchor at bind, so bytes
    // that rebuild a different commitment can never open it.
    const substituted = Buffer.from(item);
    substituted[substituted.length - 1] ^= 0xff;
    await expect(
      submitNativeScriptDecodingStep03AdvanceOrCloseSegment({
        ...shared,
        threadOutRef: bind.nextThreadOutRef,
        segment: plan.segments[0]!,
        referenceScriptItemBytes: substituted,
        referenceScriptUtxo: step03AdvanceOrCloseRef,
      }),
    ).rejects.toThrow(/do not rebuild the frozen item commitment/u);

    const scan = await submitNativeScriptDecodingStep03AdvanceOrCloseSegment({
      ...shared,
      threadOutRef: bind.nextThreadOutRef,
      segment: plan.segments[0]!,
      referenceScriptItemBytes: item,
      referenceScriptUtxo: step03AdvanceOrCloseRef,
    });

    // Replaying the segment the thread has already advanced past: its
    // `controlBefore` is no longer the committed machine.
    await expect(
      submitNativeScriptDecodingStep03AdvanceOrCloseSegment({
        ...shared,
        threadOutRef: scan.nextThreadOutRef,
        segment: plan.segments[0]!,
        referenceScriptItemBytes: item,
        referenceScriptUtxo: step03AdvanceOrCloseRef,
      }),
    ).rejects.toThrow(/not the thread's committed machine/u);

    // The thread is untouched by either refusal and still closes honestly.
    const verdict = await submitNativeScriptDecodingStep03AdvanceOrCloseClose({
      ...shared,
      threadOutRef: scan.nextThreadOutRef,
      verdict: plan.verdict,
      referenceScriptItemBytes: item,
      referenceScriptUtxo: step03AdvanceOrCloseRef,
    });
    expect(verdict.scanState.refusal_class).toBe(0n);
  }, 600_000);

  it("closes a genuinely out-of-domain accusation through the SS7.2 arm", async () => {
    const harness = await makeDecodingEmulatorHarness();
    const item = decodingCanonicalItem();
    const scenario = await setupDecodingScenario({
      harness,
      referenceScriptItemBytes: item,
      // The committed rejection accuses reference-input ordinal 5, but the
      // transaction has exactly one reference input.
      source: { kind: "forced", verdict: rejectionAccusing(1n, 5n) },
    });
    expect(scenario.subjectFieldInputs).toHaveLength(1);
    const [
      step01,
      step02,
      step03OpenSubject,
      step03BindDescriptor,
      step03AdvanceOrClose,
      step04,
    ] = await publishDecodingReferenceScripts({
      lucid: harness.funderLucid,
      contracts: harness.decoding,
    });
    const finding: NativeScriptDecodingFinding = {
      direction: 1n,
      sourceKind: 1n,
      event: {
        kind: "forcedEvent",
        orderKeyCbor: Data.to(FORCED_ORDER_KEY, SDK.OutputReference),
      },
      headerHash: scenario.setup.headerHash,
      fraudulentBlockOutRef: scenario.setup.fraudulentBlockOutRef,
      accusedOutpointSourceKind: 1n,
      accusedOutpointCursor: 5n,
      scanReasonClass: 0n,
      provability: NativeScriptDecodingProvability.OutOfDomainAccusation,
      descriptor: null,
      estimatedThreadTxCount: 5,
    };
    const outcome = await Effect.runPromise(
      proveNativeScriptDecodingFault(
        finding,
        decodingProverDeps({
          harness,
          scenario,
          referenceScriptItemBytes: null,
          referenceScriptUtxos: {
            step01,
            step02,
            step03OpenSubject,
            step03BindDescriptor,
            step03AdvanceOrClose,
            step04,
          },
        }),
      ),
    );
    if (outcome.kind !== "proven") {
      throw new Error(
        `expected a proven outcome, got ${outcome.kind}: ${JSON.stringify(outcome)}`,
      );
    }
    // init, step-01, step-02, the closing bind, step-04.
    expect(outcome.txHashes).toHaveLength(5);
    const utxo = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      harness.decoding.fraudProof.spendingScriptAddress,
      outcome.fraudProofUnit,
    );
    expect(utxo.assets[outcome.fraudProofUnit]).toBe(1n);
  }, 600_000);

  it("routes an in-domain accusation through OpenSubject and BindDescriptor", async () => {
    const harness = await makeDecodingEmulatorHarness();
    const { proverLucid, proverSigner, decoding, category, realBlueprint } =
      harness;
    const item = decodingCanonicalItem();
    const scenario = await setupDecodingScenario({
      harness,
      referenceScriptItemBytes: item,
      // Ordinal 0 IS in the transaction's reference inputs.
      source: { kind: "forced", verdict: rejectionAccusing(1n, 0n) },
    });
    const { block, ledger, setup } = scenario;
    const [
      step01Ref,
      step02Ref,
      step03OpenSubjectRef,
      step03BindDescriptorRef,
    ] = await publishDecodingReferenceScripts({
      lucid: harness.funderLucid,
      contracts: decoding,
    });
    const shared = {
      lucid: proverLucid,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      contracts: decoding,
      categoryId: category.categoryId,
      signer: proverSigner,
    };
    const init = await submitNativeScriptDecodingInit({
      lucid: proverLucid,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      blueprint: realBlueprint,
      network,
      contracts: decoding,
      category,
      catalogue: catalogueOf(harness),
      signer: proverSigner,
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
    });
    const step01 = await submitNativeScriptDecodingStep01RecordForced({
      ...shared,
      threadOutRef: init.nextThreadOutRef,
      direction: 1n,
      referenceScriptUtxo: step01Ref,
    });
    const step02 = await submitNativeScriptDecodingStep02({
      ...shared,
      threadOutRef: step01.nextThreadOutRef,
      reconstruction: block.reconstruction,
      forcedOrderKey: FORCED_ORDER_KEY,
      referenceScriptUtxo: step02Ref,
    });
    expect(step02.scanState.outpoint_cursor).toBe(0n);

    const opened = await submitNativeScriptDecodingStep03OpenSubject({
      ...shared,
      threadOutRef: step02.nextThreadOutRef,
      nativeTxCompactCbor: block.nativeTxCompactCbor,
      subjectFieldInputs: scenario.subjectFieldInputs,
      referenceScriptUtxo: step03OpenSubjectRef,
    });
    expect(opened.destinationAddress).toBe(
      decoding.steps[3]!.spendingScriptAddress,
    );
    expect(opened.scanState.outpoint_key_hash).not.toBe("");

    // An in-domain subject never reaches step-04 from OpenSubject. The Aiken
    // selector pins the malicious opposite-polarity close directly on-chain.
    await expect(
      proverLucid.utxosAtWithUnit(
        decoding.steps[5]!.spendingScriptAddress,
        init.computationThreadUnit,
      ),
    ).resolves.toHaveLength(0);

    // The authenticated subject is then the only key BindDescriptor accepts.
    const plan = buildNativeScriptDecodingScanPlan({
      itemBytes: item,
      direction: 1,
    });
    const bind = await submitNativeScriptDecodingStep03BindDescriptor({
      ...shared,
      threadOutRef: opened.nextThreadOutRef,
      outpointKeyCbor: subjectOutpointKeyCbor(scenario),
      descriptorCbor: ledger.descriptorCbor,
      ledgerTrie: ledger.trie,
      plan,
      referenceScriptItemBytes: item,
      referenceScriptUtxo: step03BindDescriptorRef,
    });
    expect(bind.scanState.machine_state_hash).toBe(
      plan.segments[0]!.controlBefore.hashHex,
    );
  }, 600_000);

  it("resumes a thread abandoned mid-loop from its committed machine state", async () => {
    const harness = await makeDecodingEmulatorHarness();
    const { proverLucid, proverSigner, decoding, category, realBlueprint } =
      harness;
    const item = decodingMalformedMultiChunkItem();
    const scenario: DecodingScenario = await setupDecodingScenario({
      harness,
      referenceScriptItemBytes: item,
      source: { kind: "normal" },
    });
    const { block, ledger, setup } = scenario;
    if (block.txInclusion === null) {
      throw new Error("normal-source fixture carries no tx inclusion");
    }
    const plan = buildNativeScriptDecodingScanPlan({
      itemBytes: item,
      direction: 0,
    });
    const [
      step01Ref,
      step02Ref,
      step03OpenSubjectRef,
      step03BindDescriptorRef,
      step03AdvanceOrCloseRef,
      step04Ref,
    ] = await publishDecodingReferenceScripts({
      lucid: harness.funderLucid,
      contracts: decoding,
    });
    const shared = {
      lucid: proverLucid,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      contracts: decoding,
      categoryId: category.categoryId,
      signer: proverSigner,
    };
    const init = await submitNativeScriptDecodingInit({
      lucid: proverLucid,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      blueprint: realBlueprint,
      network,
      contracts: decoding,
      category,
      catalogue: catalogueOf(harness),
      signer: proverSigner,
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
    });
    const step01 = await submitNativeScriptDecodingStep01BindNormal({
      ...shared,
      blueprint: realBlueprint,
      network,
      threadOutRef: init.nextThreadOutRef,
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      txInclusion: block.txInclusion,
      referenceScriptUtxo: step01Ref,
    });
    const step02 = await submitNativeScriptDecodingStep02({
      ...shared,
      threadOutRef: step01.nextThreadOutRef,
      reconstruction: block.reconstruction,
      chosenOutpoint: { sourceKind: scenario.accusedSourceKind, cursor: 0n },
      referenceScriptUtxo: step02Ref,
    });
    const opened = await submitNativeScriptDecodingStep03OpenSubject({
      ...shared,
      threadOutRef: step02.nextThreadOutRef,
      nativeTxCompactCbor: block.nativeTxCompactCbor,
      subjectFieldInputs: scenario.subjectFieldInputs,
      referenceScriptUtxo: step03OpenSubjectRef,
    });
    const bind = await submitNativeScriptDecodingStep03BindDescriptor({
      ...shared,
      threadOutRef: opened.nextThreadOutRef,
      outpointKeyCbor: subjectOutpointKeyCbor(scenario),
      descriptorCbor: ledger.descriptorCbor,
      ledgerTrie: ledger.trie,
      plan,
      referenceScriptItemBytes: item,
      referenceScriptUtxo: step03BindDescriptorRef,
    });
    // The prover crashes here. Nothing but the thread NFT and its datum
    // survives; the resume must find both from the finding alone.
    const finding: NativeScriptDecodingFinding = {
      direction: 0n,
      sourceKind: 0n,
      event: { kind: "l2Transaction", txId: block.nativeTxId },
      headerHash: setup.headerHash,
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      accusedOutpointSourceKind: scenario.accusedSourceKind,
      accusedOutpointCursor: 0n,
      scanReasonClass: null,
      provability: NativeScriptDecodingProvability.MachineRoute,
      descriptor: {
        referenceScriptLanguage: 0,
        outputIndex: 0,
        totalLength: item.length,
      },
      estimatedThreadTxCount: 7 + plan.segments.length,
    };
    const journal: string[] = [];
    const outcome = await Effect.runPromise(
      proveNativeScriptDecodingFault(
        finding,
        decodingProverDeps({
          harness,
          scenario,
          referenceScriptItemBytes: item,
          referenceScriptUtxos: {
            step01: step01Ref,
            step02: step02Ref,
            step03OpenSubject: step03OpenSubjectRef,
            step03BindDescriptor: step03BindDescriptorRef,
            step03AdvanceOrClose: step03AdvanceOrCloseRef,
            step04: step04Ref,
          },
          journal: (event) => {
            journal.push(`${event.phase}:${event.message}`);
          },
        }),
      ),
    );
    if (outcome.kind !== "proven") {
      throw new Error(
        `expected a proven outcome, got ${outcome.kind}: ${JSON.stringify(outcome)}`,
      );
    }
    // The resume re-submits only what the thread still owes: the remaining
    // Scan segments, the verdict, and step-04. Init and the bind are not
    // repeated.
    expect(outcome.txHashes).toHaveLength(plan.segments.length + 2);
    expect(journal.some((line) => line.startsWith("init:"))).toBe(false);
    expect(bind.scanState.machine_state_hash).toBe(
      plan.segments[0]!.controlBefore.hashHex,
    );
    const utxo = await expectSingleUtxoWithUnit(
      proverLucid,
      decoding.fraudProof.spendingScriptAddress,
      outcome.fraudProofUnit,
    );
    expect(utxo.assets[outcome.fraudProofUnit]).toBe(1n);
  }, 600_000);
});
