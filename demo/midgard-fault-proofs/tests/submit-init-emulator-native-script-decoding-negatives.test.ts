/**
 * `native-script-decoding` abort, refusal and resume coverage (#635, #633;
 * offchain plan §8.2 suite 6).
 *
 * What this file pins:
 *
 * - `ct.Cancel` at every one of the four steps, each releasing the thread to
 *   its named prover and burning the thread NFT, so a cancelled header can be
 *   re-initialised from scratch.
 * - The two fail-closed refusals a well-formed thread can walk into: a Scan
 *   segment replaying a control the thread has already advanced past, and
 *   item bytes substituted under the frozen anchor.
 * - The §7.2 out-of-domain close in both polarities: it convicts on a
 *   genuinely out-of-domain accusation, and refuses an in-domain one, which
 *   belongs to `BindOutpoint`. The in-domain refusal is pinned in BOTH
 *   planes — the submitter's face classification, and the validator's own
 *   neutralisation check reached through a raw transaction.
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
  buildNativeScriptDecodingScanPlanV1,
  type NativeScriptDecodingFindingV1,
  NativeScriptDecodingProvabilityV1,
  proveNativeScriptDecodingFaultV1,
  submitNativeScriptDecodingCancel,
  submitNativeScriptDecodingInit,
  submitNativeScriptDecodingStep01BindNormal,
  submitNativeScriptDecodingStep01RecordForced,
  submitNativeScriptDecodingStep02,
  submitNativeScriptDecodingStep03BindOutOfDomain,
  submitNativeScriptDecodingStep03BindOutpoint,
  submitNativeScriptDecodingStep03Scan,
  submitNativeScriptDecodingStep03Verdict,
} from "../src/native-script-decoding/index.js";
import {
  decodingCanonicalItemV1,
  decodingMalformedMultiChunkItemV1,
  decodingProverDepsV1,
  type DecodingScenarioV1,
  expectOnchainRefusalV1,
  makeDecodingEmulatorHarnessV1,
  publishDecodingReferenceScriptsV1,
  setupDecodingScenarioV1,
  submitRawDecodingBindOutOfDomainV1,
  submitRawDecodingCancelV1,
} from "./support/native-script-decoding-emulator-v1.js";
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
): SDK.OperatorVerdictV1 => ({
  ForcedTxInvalid: {
    reason: {
      ResolvedReferenceScriptMalformed: {
        source_kind: sourceKind,
        input_index: ordinal,
      },
    },
  },
});

type Harness = Awaited<ReturnType<typeof makeDecodingEmulatorHarnessV1>>;

const catalogueOf = (harness: Harness) => ({
  policyId: harness.contracts.fraudProofCatalogue.policyId,
  spendingScriptAddress:
    harness.contracts.fraudProofCatalogue.spendingScriptAddress,
  root: harness.catalogue.root,
});

describe("native-script-decoding aborts, refusals and resume", () => {
  it("cancels a thread at each of the four steps and re-initialises after every abort", async () => {
    const harness = await makeDecodingEmulatorHarnessV1();
    const { proverLucid, proverSigner, decoding, category, realBlueprint } =
      harness;
    const item = decodingMalformedMultiChunkItemV1();
    const scenario = await setupDecodingScenarioV1({
      harness,
      referenceScriptItemBytes: item,
      source: { kind: "normal" },
    });
    const { block, ledger, setup } = scenario;
    if (block.txInclusion === null) {
      throw new Error("normal-source fixture carries no tx inclusion");
    }
    const plan = buildNativeScriptDecodingScanPlanV1({
      itemBytes: item,
      direction: 0,
    });
    const [step01Ref, step02Ref, step03Ref, step04Ref] =
      await publishDecodingReferenceScriptsV1({
        lucid: harness.funderLucid,
        contracts: decoding,
      });
    const stepRefs = [step01Ref, step02Ref, step03Ref, step04Ref] as const;
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
    for (const cancelAt of [0, 1, 2, 3] as const) {
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
      if (cancelAt === 3) {
        threadOutRef = (
          await submitNativeScriptDecodingStep03BindOutpoint({
            ...shared,
            threadOutRef,
            nativeTxCompactCbor: block.nativeTxCompactCbor,
            subjectFieldInputs: scenario.subjectFieldInputs,
            descriptorCbor: ledger.descriptorCbor,
            ledgerTrie: ledger.trie,
            plan,
            referenceScriptItemBytes: item,
            referenceScriptUtxo: step03Ref,
          })
        ).nextThreadOutRef;
        for (const segment of plan.segments) {
          threadOutRef = (
            await submitNativeScriptDecodingStep03Scan({
              ...shared,
              threadOutRef,
              segment,
              referenceScriptItemBytes: item,
              referenceScriptUtxo: step03Ref,
            })
          ).nextThreadOutRef;
        }
        threadOutRef = (
          await submitNativeScriptDecodingStep03Verdict({
            ...shared,
            threadOutRef,
            verdict: plan.verdict,
            referenceScriptItemBytes: item,
            referenceScriptUtxo: step03Ref,
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
    await submitRawDecodingCancelV1({
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
    const harness = await makeDecodingEmulatorHarnessV1();
    const { proverLucid, proverSigner, decoding, category, realBlueprint } =
      harness;
    const item = decodingMalformedMultiChunkItemV1();
    const scenario = await setupDecodingScenarioV1({
      harness,
      referenceScriptItemBytes: item,
      source: { kind: "normal" },
    });
    const { block, ledger, setup } = scenario;
    if (block.txInclusion === null) {
      throw new Error("normal-source fixture carries no tx inclusion");
    }
    const plan = buildNativeScriptDecodingScanPlanV1({
      itemBytes: item,
      direction: 0,
    });
    const [step01Ref, step02Ref, step03Ref] =
      await publishDecodingReferenceScriptsV1({
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
    const bind = await submitNativeScriptDecodingStep03BindOutpoint({
      ...shared,
      threadOutRef: step02.nextThreadOutRef,
      nativeTxCompactCbor: block.nativeTxCompactCbor,
      subjectFieldInputs: scenario.subjectFieldInputs,
      descriptorCbor: ledger.descriptorCbor,
      ledgerTrie: ledger.trie,
      plan,
      referenceScriptItemBytes: item,
      referenceScriptUtxo: step03Ref,
    });

    // Substituted item bytes: the thread froze the anchor at bind, so bytes
    // that rebuild a different commitment can never open it.
    const substituted = Buffer.from(item);
    substituted[substituted.length - 1] ^= 0xff;
    await expect(
      submitNativeScriptDecodingStep03Scan({
        ...shared,
        threadOutRef: bind.nextThreadOutRef,
        segment: plan.segments[0]!,
        referenceScriptItemBytes: substituted,
        referenceScriptUtxo: step03Ref,
      }),
    ).rejects.toThrow(/do not rebuild the frozen item commitment/u);

    const scan = await submitNativeScriptDecodingStep03Scan({
      ...shared,
      threadOutRef: bind.nextThreadOutRef,
      segment: plan.segments[0]!,
      referenceScriptItemBytes: item,
      referenceScriptUtxo: step03Ref,
    });

    // Replaying the segment the thread has already advanced past: its
    // `controlBefore` is no longer the committed machine.
    await expect(
      submitNativeScriptDecodingStep03Scan({
        ...shared,
        threadOutRef: scan.nextThreadOutRef,
        segment: plan.segments[0]!,
        referenceScriptItemBytes: item,
        referenceScriptUtxo: step03Ref,
      }),
    ).rejects.toThrow(/not the thread's committed machine/u);

    // The thread is untouched by either refusal and still closes honestly.
    const verdict = await submitNativeScriptDecodingStep03Verdict({
      ...shared,
      threadOutRef: scan.nextThreadOutRef,
      verdict: plan.verdict,
      referenceScriptItemBytes: item,
      referenceScriptUtxo: step03Ref,
    });
    expect(verdict.scanState.refusal_class).toBe(0n);
  }, 600_000);

  it("closes a genuinely out-of-domain accusation through the SS7.2 arm", async () => {
    const harness = await makeDecodingEmulatorHarnessV1();
    const item = decodingCanonicalItemV1();
    const scenario = await setupDecodingScenarioV1({
      harness,
      referenceScriptItemBytes: item,
      // The committed rejection accuses reference-input ordinal 5, but the
      // transaction has exactly one reference input.
      source: { kind: "forced", verdict: rejectionAccusing(1n, 5n) },
    });
    expect(scenario.subjectFieldInputs).toHaveLength(1);
    const [step01, step02, step03, step04] =
      await publishDecodingReferenceScriptsV1({
        lucid: harness.funderLucid,
        contracts: harness.decoding,
      });
    const finding: NativeScriptDecodingFindingV1 = {
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
      provability: NativeScriptDecodingProvabilityV1.OutOfDomainAccusation,
      descriptor: null,
      estimatedThreadTxCount: 5,
    };
    const outcome = await Effect.runPromise(
      proveNativeScriptDecodingFaultV1(
        finding,
        decodingProverDepsV1({
          harness,
          scenario,
          referenceScriptItemBytes: null,
          referenceScriptUtxos: { step01, step02, step03, step04 },
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

  it("refuses the SS7.2 arm for an in-domain accusation and binds it through BindOutpoint instead", async () => {
    const harness = await makeDecodingEmulatorHarnessV1();
    const { proverLucid, proverSigner, decoding, category, realBlueprint } =
      harness;
    const item = decodingCanonicalItemV1();
    const scenario = await setupDecodingScenarioV1({
      harness,
      referenceScriptItemBytes: item,
      // Ordinal 0 IS in the transaction's reference inputs.
      source: { kind: "forced", verdict: rejectionAccusing(1n, 0n) },
    });
    const { block, ledger, setup } = scenario;
    const [step01Ref, step02Ref, step03Ref] =
      await publishDecodingReferenceScriptsV1({
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

    // Offchain plane: the submitter classifies the face and refuses.
    await expect(
      submitNativeScriptDecodingStep03BindOutOfDomain({
        ...shared,
        threadOutRef: step02.nextThreadOutRef,
        nativeTxCompactCbor: block.nativeTxCompactCbor,
        subjectFieldInputs: scenario.subjectFieldInputs,
        referenceScriptUtxo: step03Ref,
      }),
    ).rejects.toThrow(/in-domain — bind it through BindOutpoint instead/u);

    // On-chain plane: the same close, built past that classification, has to
    // die at the validator's own neutralisation check — the arm the Aiken
    // selector `decoding_step_03_rejects_an_in_domain_ordinal_close` twins.
    // A prover who patched the guard out of their tooling must gain nothing.
    await expectOnchainRefusalV1(() =>
      submitRawDecodingBindOutOfDomainV1({
        harness,
        threadOutRef: step02.nextThreadOutRef,
        nativeTxCompactCbor: block.nativeTxCompactCbor,
        subjectFieldInputs: scenario.subjectFieldInputs,
        referenceScriptUtxo: step03Ref,
      }),
    );
    // Nothing reached step-04.
    await expect(
      proverLucid.utxosAtWithUnit(
        decoding.steps[3]!.spendingScriptAddress,
        init.computationThreadUnit,
      ),
    ).resolves.toHaveLength(0);

    // The thread is untouched, and the honest door still opens.
    const plan = buildNativeScriptDecodingScanPlanV1({
      itemBytes: item,
      direction: 1,
    });
    const bind = await submitNativeScriptDecodingStep03BindOutpoint({
      ...shared,
      threadOutRef: step02.nextThreadOutRef,
      nativeTxCompactCbor: block.nativeTxCompactCbor,
      subjectFieldInputs: scenario.subjectFieldInputs,
      descriptorCbor: ledger.descriptorCbor,
      ledgerTrie: ledger.trie,
      plan,
      referenceScriptItemBytes: item,
      referenceScriptUtxo: step03Ref,
    });
    expect(bind.scanState.machine_state_hash).toBe(
      plan.segments[0]!.controlBefore.hashHex,
    );
  }, 600_000);

  it("resumes a thread abandoned mid-loop from its committed machine state", async () => {
    const harness = await makeDecodingEmulatorHarnessV1();
    const { proverLucid, proverSigner, decoding, category, realBlueprint } =
      harness;
    const item = decodingMalformedMultiChunkItemV1();
    const scenario: DecodingScenarioV1 = await setupDecodingScenarioV1({
      harness,
      referenceScriptItemBytes: item,
      source: { kind: "normal" },
    });
    const { block, ledger, setup } = scenario;
    if (block.txInclusion === null) {
      throw new Error("normal-source fixture carries no tx inclusion");
    }
    const plan = buildNativeScriptDecodingScanPlanV1({
      itemBytes: item,
      direction: 0,
    });
    const [step01Ref, step02Ref, step03Ref, step04Ref] =
      await publishDecodingReferenceScriptsV1({
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
    const bind = await submitNativeScriptDecodingStep03BindOutpoint({
      ...shared,
      threadOutRef: step02.nextThreadOutRef,
      nativeTxCompactCbor: block.nativeTxCompactCbor,
      subjectFieldInputs: scenario.subjectFieldInputs,
      descriptorCbor: ledger.descriptorCbor,
      ledgerTrie: ledger.trie,
      plan,
      referenceScriptItemBytes: item,
      referenceScriptUtxo: step03Ref,
    });
    // The prover crashes here. Nothing but the thread NFT and its datum
    // survives; the resume must find both from the finding alone.
    const finding: NativeScriptDecodingFindingV1 = {
      direction: 0n,
      sourceKind: 0n,
      event: { kind: "l2Transaction", txId: block.nativeTxId },
      headerHash: setup.headerHash,
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      accusedOutpointSourceKind: scenario.accusedSourceKind,
      accusedOutpointCursor: 0n,
      scanReasonClass: null,
      provability: NativeScriptDecodingProvabilityV1.MachineRoute,
      descriptor: {
        referenceScriptLanguage: 0,
        outputIndex: 0,
        totalLength: item.length,
      },
      estimatedThreadTxCount: 6 + plan.segments.length,
    };
    const journal: string[] = [];
    const outcome = await Effect.runPromise(
      proveNativeScriptDecodingFaultV1(
        finding,
        decodingProverDepsV1({
          harness,
          scenario,
          referenceScriptItemBytes: item,
          referenceScriptUtxos: {
            step01: step01Ref,
            step02: step02Ref,
            step03: step03Ref,
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
