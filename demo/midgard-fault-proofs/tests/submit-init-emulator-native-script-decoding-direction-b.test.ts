/**
 * `native-script-decoding` forced-source emulator lifecycles (#635, #633;
 * offchain plan §8.2 suite 5).
 *
 * Three forced-leaf threads, all driven through the §4.3 proving core off a
 * §3.4 finding record:
 *
 * 1. **Direction B, machine route.** The operator committed
 *    `ForcedTxInvalid { ResolvedReferenceScriptMalformed }` over a payload
 *    that decodes canonically. The thread scans to the EXACT canonical
 *    terminal and closes with a windowless verdict — direction B reads no
 *    chunk window, because its terminal is the whole item.
 * 2. **Direction B, descriptor contradiction.** The accused outpoint's
 *    descriptor names a non-native language, which contradicts a decoding
 *    rejection outright: the thread closes at bind with no chunk proof and
 *    never enters the machine.
 * 3. **Direction A over a forced source.** `ForcedTxValid` over a payload
 *    the canonical decoder refuses — wrongful acceptance reached through the
 *    forced door rather than the `transactions_root`.
 *
 * Lives in its own file for the reason its siblings do. The split was made
 * while `@lucid-evolution/uplc` (through 0.2.22) leaked wasm linear memory on
 * every script evaluation and vitest isolates per FILE; that leak is fixed
 * upstream, and the split is kept so each file runs in its own fresh process.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { Data, toUnit } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  buildNativeScriptDecodingScanPlan,
  type NativeScriptDecodingFinding,
  NativeScriptDecodingPlanRoutes,
  NativeScriptDecodingProvability,
  proveNativeScriptDecodingFault,
} from "../src/native-script-decoding/index.js";
import {
  decodingCanonicalItem,
  decodingMalformedMultiChunkItem,
  decodingPlutusItem,
  decodingProverDeps,
  type DecodingScenario,
  makeDecodingEmulatorHarness,
  publishDecodingReferenceScripts,
  setupDecodingScenario,
} from "./support/native-script-decoding-emulator.js";
import { expectSingleUtxoWithUnit } from "./support/submit-init-emulator-shared.js";

const FORCED_ORDER_KEY: SDK.OutputReference = {
  transactionId: "cd".repeat(32),
  outputIndex: 0n,
};

/** The §2.4.3(e) rejection this family disputes, accusing (reference, 0). */
const MALFORMED_REJECTION_VERDICT: SDK.OperatorVerdict = {
  ForcedTxInvalid: {
    reason: {
      ResolvedReferenceScriptMalformed: { source_kind: 1n, input_index: 0n },
    },
  },
};

/** Asserts the thread NFT is gone from all six validators and the token is live. */
const expectProvenAndBurned = async (
  harness: Awaited<ReturnType<typeof makeDecodingEmulatorHarness>>,
  scenario: DecodingScenario,
  fraudProofUnit: string,
) => {
  const { proverLucid, decoding, category } = harness;
  const threadUnit = toUnit(
    decoding.computationThread.policyId,
    `${category.categoryId}${scenario.setup.headerHash}`,
  );
  for (const step of decoding.steps) {
    await expect(
      proverLucid.utxosAtWithUnit(step.spendingScriptAddress, threadUnit),
    ).resolves.toHaveLength(0);
  }
  const utxo = await expectSingleUtxoWithUnit(
    proverLucid,
    decoding.fraudProof.spendingScriptAddress,
    fraudProofUnit,
  );
  expect(utxo.assets[fraudProofUnit]).toBe(1n);
};

describe("native-script-decoding forced-source emulator lifecycles", () => {
  it("proves a wrongful rejection at the exact canonical terminal", async () => {
    const harness = await makeDecodingEmulatorHarness();
    const item = decodingCanonicalItem();
    const scenario = await setupDecodingScenario({
      harness,
      referenceScriptItemBytes: item,
      source: { kind: "forced", verdict: MALFORMED_REJECTION_VERDICT },
    });
    const plan = buildNativeScriptDecodingScanPlan({
      itemBytes: item,
      direction: 1,
    });
    expect(plan.route).toBe(NativeScriptDecodingPlanRoutes.Machine);
    // Direction B's terminal is the whole item, so the verdict reads nothing.
    expect(plan.verdict.window).toBeNull();

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
      accusedOutpointCursor: 0n,
      scanReasonClass: 0n,
      provability: NativeScriptDecodingProvability.MachineRoute,
      descriptor: {
        referenceScriptLanguage: 0,
        outputIndex: 0,
        totalLength: item.length,
      },
      estimatedThreadTxCount: 6 + plan.segments.length,
    };
    const outcome = await Effect.runPromise(
      proveNativeScriptDecodingFault(
        finding,
        decodingProverDeps({
          harness,
          scenario,
          referenceScriptItemBytes: item,
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
    expect(outcome.txHashes).toHaveLength(6 + plan.segments.length);
    await expectProvenAndBurned(harness, scenario, outcome.fraudProofUnit);
  }, 600_000);

  it("closes a wrongful rejection at bind when the descriptor names a non-native language", async () => {
    const harness = await makeDecodingEmulatorHarness();
    const item = decodingPlutusItem();
    const scenario = await setupDecodingScenario({
      harness,
      referenceScriptItemBytes: item,
      referenceScriptLanguage: 3,
      source: { kind: "forced", verdict: MALFORMED_REJECTION_VERDICT },
    });
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
      accusedOutpointCursor: 0n,
      scanReasonClass: 0n,
      provability: NativeScriptDecodingProvability.DescriptorContradiction,
      descriptor: {
        referenceScriptLanguage: 3,
        outputIndex: 0,
        totalLength: item.length,
      },
      estimatedThreadTxCount: 6,
    };
    const outcome = await Effect.runPromise(
      proveNativeScriptDecodingFault(
        finding,
        decodingProverDeps({
          harness,
          scenario,
          // The contradiction is the descriptor's own; no item is scanned.
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
    // init, step-01, step-02, bind (closing), step-04 — the machine never runs.
    expect(outcome.txHashes).toHaveLength(6);
    await expectProvenAndBurned(harness, scenario, outcome.fraudProofUnit);
  }, 600_000);

  it("proves a wrongful acceptance recorded as an explicit forced verdict", async () => {
    const harness = await makeDecodingEmulatorHarness();
    const item = decodingMalformedMultiChunkItem();
    const scenario = await setupDecodingScenario({
      harness,
      referenceScriptItemBytes: item,
      source: { kind: "forced", verdict: "ForcedTxValid" },
    });
    const plan = buildNativeScriptDecodingScanPlan({
      itemBytes: item,
      direction: 0,
    });
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
      direction: 0n,
      sourceKind: 1n,
      event: {
        kind: "forcedEvent",
        orderKeyCbor: Data.to(FORCED_ORDER_KEY, SDK.OutputReference),
      },
      headerHash: scenario.setup.headerHash,
      fraudulentBlockOutRef: scenario.setup.fraudulentBlockOutRef,
      accusedOutpointSourceKind: 1n,
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
    const outcome = await Effect.runPromise(
      proveNativeScriptDecodingFault(
        finding,
        decodingProverDeps({
          harness,
          scenario,
          referenceScriptItemBytes: item,
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
    expect(outcome.txHashes).toHaveLength(7 + plan.segments.length);
    await expectProvenAndBurned(harness, scenario, outcome.fraudProofUnit);
  }, 600_000);
});
