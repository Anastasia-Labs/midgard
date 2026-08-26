/**
 * `native-script-decoding` direction-A emulator lifecycle (#635, #633;
 * offchain plan §8.2 suite 4).
 *
 * Direction A is wrongful ACCEPTANCE: the operator committed a transaction
 * that resolves a reference-script item the canonical decoder refuses. The
 * disputed item here crosses exactly one 4,095-byte chunk boundary, so the
 * machine route runs bind → one Scan segment → Verdict with the mandatory
 * chunk-plus-next window on both the segment and the verdict — the smallest
 * payload that still exercises every windowed opening.
 *
 * Both step-01 carriages are driven (design §10 Q4): the redeemer-carried
 * inclusion proof, and the #545 published-chunk transport. One of the two
 * threads is driven end-to-end through the §4.3 proving core off a §3.4
 * finding record; the other drives the per-step submitters directly, so the
 * two planes are pinned against each other.
 *
 * Lives in its own file for the reason its siblings do: `@lucid-evolution/uplc`
 * never reclaims wasm linear memory and vitest isolates per FILE. See
 * tests/support/uplc-heap-guard.ts.
 */
import { outRefLabel } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, toUnit } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { publishProofChunksV1 } from "../src/index.js";
import {
  buildNativeScriptDecodingScanPlanV1,
  NATIVE_SCRIPT_DECODING_PROVER_POLICY_DEFAULTS_V1,
  type NativeScriptDecodingFindingV1,
  NativeScriptDecodingPlanRoutesV1,
  NativeScriptDecodingProvabilityV1,
  type NativeScriptDecodingProverDepsV1,
  proveNativeScriptDecodingFaultV1,
  submitNativeScriptDecodingInit,
  submitNativeScriptDecodingStep01BindNormal,
  submitNativeScriptDecodingStep02,
  submitNativeScriptDecodingStep03BindOutpoint,
  submitNativeScriptDecodingStep03Scan,
  submitNativeScriptDecodingStep03Verdict,
  submitNativeScriptDecodingStep04,
} from "../src/native-script-decoding/index.js";
import {
  decodingMalformedMultiChunkItemV1,
  makeDecodingEmulatorHarnessV1,
  publishDecodingReferenceScriptsV1,
  setupDecodingScenarioV1,
} from "./support/native-script-decoding-emulator-v1.js";
import {
  expectSingleUtxoWithUnit,
  network,
} from "./support/submit-init-emulator-shared.js";

/** The emulator has no L1 depth or maturity to observe; both gates are off. */
const EMULATOR_PROVER_POLICY_V1 = {
  ...NATIVE_SCRIPT_DECODING_PROVER_POLICY_DEFAULTS_V1,
  minSettlementDepth: 0n,
  maturityGuardFactor: 0,
  maxThreadBudgetLovelace: null,
};

describe("native-script-decoding direction-A emulator lifecycle", () => {
  it("proves a wrongful acceptance through the proving core and mints the permanent fraud-proof token", async () => {
    const harness = await makeDecodingEmulatorHarnessV1();
    const {
      realBlueprint,
      funderLucid,
      proverLucid,
      proverSigner,
      catalogue,
      decoding,
      category,
    } = harness;
    const item = decodingMalformedMultiChunkItemV1();
    const scenario = await setupDecodingScenarioV1({
      harness,
      referenceScriptItemBytes: item,
      source: { kind: "normal" },
    });
    const { block, ledger, setup } = scenario;

    // The plan the fixture pins: two chunks, one Scan segment, and a windowed
    // verdict — the direction-A machine route in its smallest honest shape.
    const plan = buildNativeScriptDecodingScanPlanV1({
      itemBytes: item,
      direction: 0,
    });
    expect(plan.route).toBe(NativeScriptDecodingPlanRoutesV1.Machine);
    expect(plan.segments).toHaveLength(1);
    expect(plan.verdict.window).toStrictEqual({
      chunkIndex: 0,
      needNext: true,
    });
    expect(plan.verdict.refusalClass).toBe(0);

    const [step01Ref, step02Ref, step03Ref, step04Ref] =
      await publishDecodingReferenceScriptsV1({
        lucid: funderLucid,
        contracts: decoding,
      });

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
      estimatedThreadTxCount: 6,
    };
    const journal: string[] = [];
    const deps: NativeScriptDecodingProverDepsV1 = {
      lucid: proverLucid,
      blueprint: realBlueprint,
      network,
      contracts: decoding,
      category,
      catalogue: {
        policyId: harness.contracts.fraudProofCatalogue.policyId,
        spendingScriptAddress:
          harness.contracts.fraudProofCatalogue.spendingScriptAddress,
        root: catalogue.root,
      },
      signer: proverSigner,
      evidence: {
        txInclusion: async () => {
          if (block.txInclusion === null) {
            throw new Error("normal-source fixture carries no tx inclusion");
          }
          return block.txInclusion;
        },
        reconstruction: async () => block.reconstruction,
        subjectTx: async () => ({
          nativeTxCompactCbor: block.nativeTxCompactCbor,
          subjectFieldInputs: scenario.subjectFieldInputs,
        }),
        descriptor: async () => ({
          descriptorCbor: ledger.descriptorCbor,
          referenceScriptItemBytes: item,
        }),
        ledgerTrie: async () => ledger.trie,
      },
      observations: {},
      journal: (event) => {
        journal.push(`${event.phase}:${event.message}`);
      },
      policy: EMULATOR_PROVER_POLICY_V1,
      referenceScriptUtxos: {
        step01: step01Ref,
        step02: step02Ref,
        step03: step03Ref,
        step04: step04Ref,
      },
    };

    const outcome = await Effect.runPromise(
      proveNativeScriptDecodingFaultV1(finding, deps),
    );
    if (outcome.kind !== "proven") {
      throw new Error(
        `expected a proven outcome, got ${outcome.kind}: ${JSON.stringify(outcome)}`,
      );
    }
    // init, step-01, step-02, bind, one Scan, verdict, step-04.
    expect(outcome.txHashes).toHaveLength(7);
    expect(journal).toContain("outcome:proven");

    // The permanent token is minted and the thread NFT is burned: no step
    // address still holds it.
    const threadUnit = toUnit(
      decoding.computationThread.policyId,
      `${category.categoryId}${setup.headerHash}`,
    );
    for (const step of decoding.steps) {
      await expect(
        proverLucid.utxosAtWithUnit(step.spendingScriptAddress, threadUnit),
      ).resolves.toHaveLength(0);
    }
    const fraudProofUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      decoding.fraudProof.spendingScriptAddress,
      outcome.fraudProofUnit,
    );
    expect(outRefLabel(fraudProofUtxo)).toBe(outcome.fraudProofOutRef);
    expect(fraudProofUtxo.assets[outcome.fraudProofUnit]).toBe(1n);
    expect(
      Data.from(fraudProofUtxo.datum!, SDK.FraudProofTokenDatum),
    ).toStrictEqual({ fraud_prover: proverSigner.paymentKeyHash });

    // A second invocation is idempotent, not a second thread.
    const again = await Effect.runPromise(
      proveNativeScriptDecodingFaultV1(finding, deps),
    );
    expect(again).toMatchObject({ kind: "refused", refusal: "alreadyProven" });
  }, 600_000);

  it("binds the same acceptance through the published-chunk step-01 carriage", async () => {
    const harness = await makeDecodingEmulatorHarnessV1();
    const {
      realBlueprint,
      funderLucid,
      proverLucid,
      proverSigner,
      catalogue,
      decoding,
      category,
    } = harness;
    const item = decodingMalformedMultiChunkItemV1();
    const scenario = await setupDecodingScenarioV1({
      harness,
      referenceScriptItemBytes: item,
      source: { kind: "normal" },
      // A single-leaf transactions trie proves in zero steps and has nothing
      // to publish; one decoy leaf is the smallest publishable proof.
      decoyTransactionCount: 1,
    });
    const { block, ledger, setup } = scenario;
    if (block.txInclusion === null) {
      throw new Error("normal-source fixture carries no tx inclusion");
    }
    const [step01Ref, step02Ref, step03Ref, step04Ref] =
      await publishDecodingReferenceScriptsV1({
        lucid: funderLucid,
        contracts: decoding,
      });
    const plan = buildNativeScriptDecodingScanPlanV1({
      itemBytes: item,
      direction: 0,
    });

    const initResult = await submitNativeScriptDecodingInit({
      lucid: proverLucid,
      blueprint: realBlueprint,
      network,
      contracts: decoding,
      category,
      catalogue: {
        policyId: harness.contracts.fraudProofCatalogue.policyId,
        spendingScriptAddress:
          harness.contracts.fraudProofCatalogue.spendingScriptAddress,
        root: catalogue.root,
      },
      signer: proverSigner,
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
    });
    expect(initResult.computationThreadAssetName).toBe(
      `${category.categoryId}${setup.headerHash}`,
    );

    // #545 carriage: the membership proof is published once, then referenced.
    const publication = await publishProofChunksV1({
      lucid: proverLucid,
      network,
      signer: proverSigner,
      proofCbor: block.txInclusion.txMembershipProofCbor,
      awaitConfirmation: true,
    });
    expect(publication.chunks.length).toBeGreaterThan(0);

    const step01 = await submitNativeScriptDecodingStep01BindNormal({
      lucid: proverLucid,
      blueprint: realBlueprint,
      contracts: decoding,
      categoryId: category.categoryId,
      network,
      signer: proverSigner,
      threadOutRef: initResult.nextThreadOutRef,
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      txInclusion: block.txInclusion,
      publishedProofChunks: publication.chunks,
      referenceScriptUtxo: step01Ref,
    });
    expect(step01.bindState).toStrictEqual({
      direction: 0n,
      source_kind: 0n,
      verified_tx_id: block.nativeTxId,
    });

    const step02 = await submitNativeScriptDecodingStep02({
      lucid: proverLucid,
      contracts: decoding,
      categoryId: category.categoryId,
      signer: proverSigner,
      threadOutRef: step01.nextThreadOutRef,
      reconstruction: block.reconstruction,
      chosenOutpoint: {
        sourceKind: scenario.accusedSourceKind,
        cursor: 0n,
      },
      referenceScriptUtxo: step02Ref,
    });
    expect(step02.scanState.prior_ledger_root).toBe(ledger.rootHex);
    expect(step02.scanState.machine_state_hash).toBe("");

    const bind = await submitNativeScriptDecodingStep03BindOutpoint({
      lucid: proverLucid,
      contracts: decoding,
      categoryId: category.categoryId,
      signer: proverSigner,
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

    const scan = await submitNativeScriptDecodingStep03Scan({
      lucid: proverLucid,
      contracts: decoding,
      categoryId: category.categoryId,
      signer: proverSigner,
      threadOutRef: bind.nextThreadOutRef,
      segment: plan.segments[0]!,
      referenceScriptItemBytes: item,
      referenceScriptUtxo: step03Ref,
    });
    expect(scan.scanState.machine_state_hash).toBe(
      plan.verdict.control!.hashHex,
    );

    const verdict = await submitNativeScriptDecodingStep03Verdict({
      lucid: proverLucid,
      contracts: decoding,
      categoryId: category.categoryId,
      signer: proverSigner,
      threadOutRef: scan.nextThreadOutRef,
      verdict: plan.verdict,
      referenceScriptItemBytes: item,
      referenceScriptUtxo: step03Ref,
    });
    expect(verdict.scanState.refusal_class).toBe(0n);

    const step04 = await submitNativeScriptDecodingStep04({
      lucid: proverLucid,
      contracts: decoding,
      categoryId: category.categoryId,
      signer: proverSigner,
      threadOutRef: verdict.nextThreadOutRef,
      referenceScriptUtxo: step04Ref,
    });
    expect(step04.fraudProofAssetName).toBe(
      initResult.computationThreadAssetName,
    );
    for (const step of decoding.steps) {
      await expect(
        proverLucid.utxosAtWithUnit(
          step.spendingScriptAddress,
          initResult.computationThreadUnit,
        ),
      ).resolves.toHaveLength(0);
    }
    const fraudProofUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step04.fraudProofAddress,
      step04.fraudProofUnit,
    );
    expect(fraudProofUtxo.assets[step04.fraudProofUnit]).toBe(1n);
  }, 600_000);
});
