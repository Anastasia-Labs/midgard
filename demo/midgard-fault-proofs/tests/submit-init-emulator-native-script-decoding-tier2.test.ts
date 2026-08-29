/**
 * `native-script-decoding` emulator tier-2 carriage: the §8.4 size partition
 * exercised with a genuinely large committed subject field.
 *
 * Step-03's `BindOutpoint` opens the accused §2.5 field through the §8.8
 * door, and §8.4 partitions the carriage tier on the preimage's size alone —
 * the tier is never a caller's argument. This journey commits a subject
 * transaction whose reference-input set is large enough (365 forty-byte
 * out-ref items, a 14,603-byte §5.1 preimage) that tier-1 inline carriage is
 * inadmissible: the preimage exceeds the 14,336-byte tier-1 redeemer bound,
 * so a real prover MUST publish it as a §8.2 `RawUtxo` bytes-only inline
 * datum and hand the door a reference input. Nothing forces the tier — the
 * data's size does. The accused outpoint is buried among 364 fabricated
 * decoys, the finding names its post-sort ordinal, and the proving core
 * still lands the direction-A conviction.
 *
 * Lives in its own file for the reason its siblings do: `@lucid-evolution/uplc`
 * never reclaims wasm linear memory and vitest isolates per FILE. See
 * tests/support/uplc-heap-guard.ts.
 */
import {
  encodeMidgardFieldPreimageV1,
  MIDGARD_CHUNK_BYTES_K_V1,
  MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1,
  outRefLabel,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, toUnit } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  buildNativeScriptDecodingScanPlanV1,
  NATIVE_SCRIPT_DECODING_PROVER_POLICY_DEFAULTS_V1,
  type NativeScriptDecodingFindingV1,
  NativeScriptDecodingPlanRoutesV1,
  NativeScriptDecodingProvabilityV1,
  type NativeScriptDecodingProverDepsV1,
  proveNativeScriptDecodingFaultV1,
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

/**
 * 364 decoys plus the accused outpoint (constant §5.3 stride of 40 bytes
 * each) make a 14,603-byte subject-field preimage: past §8.4's tier-1 bound,
 * inside the single-publication tier-2 window `(14,336, 15,148]` — the size
 * alone selects `RawUtxo`.
 */
const TIER2_DECOY_SUBJECT_INPUT_COUNT = 364;

describe("native-script-decoding emulator tier-2 carriage", () => {
  it("convicts the accused outpoint buried in a 14,603-byte subject field through a size-selected RawUtxo publication", async () => {
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
      decoySubjectInputCount: TIER2_DECOY_SUBJECT_INPUT_COUNT,
    });
    const { block, ledger, setup } = scenario;
    // The size, not any flag, is what selects tier 2: past the tier-1
    // redeemer bound, within one publication.
    const preimage = encodeMidgardFieldPreimageV1(
      scenario.subjectFieldInputs.map(SDK.encodeMidgardTxInputCanonicalV1),
    );
    expect(preimage.length).toBeGreaterThan(
      MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1,
    );
    expect(preimage.length).toBeLessThanOrEqual(MIDGARD_CHUNK_BYTES_K_V1);

    const plan = buildNativeScriptDecodingScanPlanV1({
      itemBytes: item,
      direction: 0,
    });
    expect(plan.route).toBe(NativeScriptDecodingPlanRoutesV1.Machine);

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
      accusedOutpointCursor: BigInt(scenario.accusedOrdinal),
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
      witnessReferenceScripts: harness.witnessReferenceScripts,
    };

    const outcome = await Effect.runPromise(
      proveNativeScriptDecodingFaultV1(finding, deps),
    );
    if (outcome.kind !== "proven") {
      throw new Error(
        `expected a proven outcome, got ${outcome.kind}: ${JSON.stringify(outcome)}`,
      );
    }
    expect(journal).toContain("outcome:proven");

    // The tier-2 publication really exists: the whole §5.1 preimage sits at
    // the prover's address as a bytes-only inline datum, referenced rather
    // than carried in the bind's own redeemer.
    const expectedDatum = SDK.fieldPreimagePublicationDatumCborV1(preimage);
    const publications = (
      await proverLucid.utxosAt(proverSigner.address)
    ).filter((utxo) => utxo.datum === expectedDatum);
    expect(publications).toHaveLength(1);

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
    expect(
      Data.from(fraudProofUtxo.datum!, SDK.FraudProofTokenDatum),
    ).toStrictEqual({ fraud_prover: proverSigner.paymentKeyHash });
  }, 600_000);
});
