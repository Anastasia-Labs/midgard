import "node:fs";
import "node:path";
import "@al-ft/midgard-core";
import "@al-ft/midgard-core/codec/native-tx-field-access";
import "@al-ft/midgard-core/consensus-profile";
import "@al-ft/midgard-core/narrowing";
import "@al-ft/midgard-fault-proofs";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "effect";
import "vitest";
import "../src/index.js";
import "./validation-fixtures.js";
import "./complete-item-proof-fit-emulator.build-trace-with-outputs.js";
import "./complete-item-proof-fit-emulator.build-canonical-decode-item-case.js";
import "./complete-item-proof-fit-emulator.submit-stage.js";
import "./complete-item-proof-fit-emulator.publish-proof-item-publication.js";

import { MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES } from "@al-ft/midgard-core/codec/native-tx-field-access";
import { MIDGARD_ENVELOPE_MEASUREMENTS } from "@al-ft/midgard-core/consensus-profile";
import { type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  buildCanonicalDecodeItemCase,
  loadContracts,
  sameDatumValue,
  setupEmulator,
} from "./complete-item-proof-fit-emulator.build-canonical-decode-item-case.js";
import {
  makeExactSizeOutputItem,
  MAX_L1_PROOF_TX_BYTES,
  RESERVED_CPU_UNITS,
  RESERVED_MEMORY_UNITS,
  TIER1_MAX_COMPLETE_ITEM_BYTES,
} from "./complete-item-proof-fit-emulator.build-trace-with-outputs.js";
import {
  measureObserveAt,
  measurePublicationFrontierAt,
  publishProofItem,
  publishRawProofItemForNegativeControl,
} from "./complete-item-proof-fit-emulator.publish-proof-item-publication.js";
import {
  type CompleteItemJourney,
  publishReferenceScript,
  runJourneyToObserve,
} from "./complete-item-proof-fit-emulator.submit-stage.js";

/**
 * **RESOLVED 2026-08-14: `5 passed (5)`.** The handoff below is the historical
 * record of the pre-#579 freeze it describes; it is no longer the suite's state.
 * #579 regenerated the blueprint, the applied §3.2 resolver hash was re-pinned
 * from the producer, and two further things that the freeze had been masking
 * came out with it:
 *
 * - The complete-item witness selector matched on `(phase, kind)` alone, so it
 *   silently measured **field 0**'s few-dozen-byte preimage instead of field 2's.
 *   The publication rows were measuring the wrong field, and the substitution row
 *   was writing its flipped byte past the end of a ~40-byte buffer — mutating
 *   nothing, and so rejecting nothing. Both are fixed and both now assert that
 *   what they claim to exercise is what they exercise.
 * - The row at the applied publication maximum MOVED to
 *   `complete-item-carriage-tiers-emulator.test.ts`, because 14,396 bytes is
 *   tier-2 under §8.4 and this harness is tier-1 only (owner ruling). See
 *   `TIER1_MAX_COMPLETE_ITEM_BYTES` for the 64-byte overhang that exposed, which
 *   **#580 owns**.
 *
 */
describe("complete-item proof fit V1 (emulator, applied validators)", () => {
  it("measures the applied observe door at the staged reliability boundary", async () => {
    // FLIPPED ONTO THE OBSERVE STAGE (#617 sign-off item 1). Through the
    // counted era and the #597 wiring era this row measured the AUTHENTICATE
    // stage, because that was the stage the §5.1 preimage rode: it double-
    // carried the item and was therefore the direct route's binder (owner-
    // signed reserve 12,810 / exact 13,294). Option B (#620) moved the
    // preimage to the observe stage's §8.8 door and left authenticate
    // item-size-independent, so the binder moved with it and the owner-signed
    // frontier rebound to the measured post-change pair (13,522 / 14,004,
    // #622 ruling (b)). This row now walks the real staged chain to that door
    // and measures the transaction that actually grows with the item.
    //
    // What is asserted is a RELATION, not a restatement: the production
    // journey's byte table is pinned in
    // `demo/midgard-fault-proofs/tests/submit-init-emulator-option-b-*.test.ts`,
    // and #622's caveat 1 says those numbers are framing-relative. This
    // harness has its own framing, so it asserts that the applied validators
    // carry the reserve-frontier item through the door inside the reliability
    // budget, that the door is the binder, and that the §3.3 execution
    // reserve holds — every one of which is falsifiable here.
    const reliableItemBytes =
      MIDGARD_ENVELOPE_MEASUREMENTS.maxReliableDirectCompleteItemBytes;
    const journey = await measureObserveAt(reliableItemBytes);
    const observe = journey.observe.measurement;

    // One spend redeemer; one reference input, and it is the parked observe
    // validator — the preimage rides inline, so nothing else is referenced.
    expect(observe.redeemerCount).toBe(1);
    expect(observe.referenceInputCount).toBe(1);
    expect(observe.completeSignedBytes).toBeLessThanOrEqual(
      MAX_L1_PROOF_TX_BYTES -
        MIDGARD_ENVELOPE_MEASUREMENTS.proofItemEnvelopeReliabilityReserveBytes,
    );
    expect(Number(observe.executionMemory)).toBeLessThanOrEqual(
      RESERVED_MEMORY_UNITS,
    );
    expect(Number(observe.executionSteps)).toBeLessThanOrEqual(
      RESERVED_CPU_UNITS,
    );

    // The door is the binder: it carries the item, the two stages before it do
    // not, and every stage of the chain fits the real L1 envelope.
    const authenticate = journey.authenticate.measurement;
    const source = journey.source.measurement;
    expect(observe.completeSignedBytes).toBeGreaterThan(
      authenticate.completeSignedBytes,
    );
    expect(observe.completeSignedBytes).toBeGreaterThan(
      source.completeSignedBytes,
    );
    expect(authenticate.completeSignedBytes).toBeLessThan(reliableItemBytes);
    for (const [stage, measurement] of [
      ["authenticate", authenticate],
      ["source", source],
      ["observe", observe],
    ] as const) {
      expect(measurement.completeSignedBytes, stage).toBeLessThanOrEqual(
        MAX_L1_PROOF_TX_BYTES,
      );
      expect(Number(measurement.executionMemory), stage).toBeLessThanOrEqual(
        RESERVED_MEMORY_UNITS,
      );
      expect(Number(measurement.executionSteps), stage).toBeLessThanOrEqual(
        RESERVED_CPU_UNITS,
      );
    }

    // The door wrote the observation the off-chain staging derived — the row
    // measures a journey that really completed, not one that merely balanced.
    expect(
      sameDatumValue(
        journey.observe.nextThreadUtxo.datum ?? "",
        journey.itemCase.observedDatum,
      ),
    ).toBe(true);

    if (process.env.MIDGARD_PRINT_PROOF_FIT === "1") {
      const contracts = await loadContracts();
      console.info(
        JSON.stringify(
          {
            completeItemDirectProofFitV1: {
              appliedSemanticScriptHash:
                contracts.validationTraceDispute.semanticResolvers[1]!
                  .spendingScriptHash,
              reliableDirectItemBytes: reliableItemBytes,
              authenticateTransaction: authenticate,
              sourceTransaction: source,
              observeTransaction: observe,
              reservedMemoryUnits: RESERVED_MEMORY_UNITS,
              reservedCpuUnits: RESERVED_CPU_UNITS,
            },
          },
          (_key, value: unknown) =>
            typeof value === "bigint" ? value.toString() : value,
          2,
        ),
      );
    }
  }, 600_000);

  it("measures the complete signed tier-1 step transaction at the 14,336-byte preimage cap", async () => {
    // #611 (#557 M2): #580 measured the at-cap ONE-STEP EVIDENCE — 15,848
    // bytes over the 14,336-byte preimage, 536 unspent inside the 16,383-byte
    // evidence envelope — but no suite built the complete SIGNED step
    // transaction around it, so M2 was narrowed rather than closed. This row
    // builds and submits that transaction.
    //
    // MOVED ONTO THE OBSERVE STAGE (#617 sign-off item 1). The at-cap inline
    // preimage used to ride the authenticate stage's redeemer; since Option B
    // (#620) it rides the observe stage's §8.8 door, so the worst-case inline
    // step transaction is the observe one and the M2 reading has to be taken
    // there. The production route still cannot produce it — build-time
    // routing (#621) demotes items far below the cap to publication, and the
    // pre-sign envelope gate refuses the rest — so this hand-driven journey
    // remains the only producer of the shape an adversarial prover is free to
    // attempt, which is exactly the shape the tier-1 bound must hold for.
    const atCap = await measureObserveAt(TIER1_MAX_COMPLETE_ITEM_BYTES);
    const byReference = atCap.observe.measurement;
    // The carried §5.1 preimage really is the whole tier-1 domain — cap
    // bytes, carried Inline (the case builder throws on any other carriage).
    expect(Buffer.from(atCap.itemCase.fieldPreimageHex, "hex").length).toBe(
      MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
    );
    expect(byReference.redeemerCount).toBe(1);
    expect(byReference.referenceInputCount).toBe(1);
    expect(Number(byReference.executionMemory)).toBeLessThanOrEqual(
      RESERVED_MEMORY_UNITS,
    );
    expect(Number(byReference.executionSteps)).toBeLessThanOrEqual(
      RESERVED_CPU_UNITS,
    );
    // #557 M2, MEASURED AND FALSIFIED (#611, 2026-08-17; re-measured on the
    // observe stage 2026-08-23): the complete signed step transaction at the
    // cap does NOT fit maxTxSize even on the deployed route. The
    // evidence-layer reading (15,848 bytes, 536 unspent in the 16,383-byte
    // evidence envelope) never included a stage's protocol framing — thread
    // input, continuation output and datum, required signer, reference input,
    // change — which the production shape cannot shed. This assertion pins the
    // measured overflow so the row flips of its own accord when the owner
    // reprices the tier-1 bound; it does NOT accept the state as correct —
    // repricing is parameter churn and rides the #611 escalation. It is also
    // the same reading #622 recorded from the production journey: the
    // contiguous inline frontier ends at 14,004, below the 14,336 cap, and
    // items past it auto-demote to publication.
    expect(byReference.completeSignedBytes).toBeGreaterThan(
      MAX_L1_PROOF_TX_BYTES,
    );

    // Embedded basis, measured for the record: a prover who attaches the
    // observe validator instead of referencing the published copy adds the
    // whole validator body on top — route waste on the prover's side, but it
    // pins that the published reference script is load-bearing for step
    // liveness anywhere near the cap.
    const embedded = await measureObserveAt(TIER1_MAX_COMPLETE_ITEM_BYTES, {
      embedObserveValidator: true,
    });
    expect(embedded.observe.measurement.referenceInputCount).toBe(0);
    expect(embedded.observe.measurement.completeSignedBytes).toBeGreaterThan(
      byReference.completeSignedBytes,
    );

    // The actual fitting frontier, bisected on the deployed route: the largest
    // complete item whose signed OBSERVE transaction fits maxTxSize.
    // `maxReliableDirectCompleteItemBytes` is a known fitting floor (the row
    // above measures it inside the reliability budget); the cap is the
    // measured overflow above. This is the number a repricing decision needs.
    // Not every exact item size is constructible — the datum filler's chunk
    // headers make the size ladder skip a byte or two at chunk boundaries —
    // so the bisect walks the nearest constructible size and the frontier is
    // exact at constructible-size resolution.
    const constructibleNear = (target: number): number | undefined => {
      for (let offset = 0; offset <= 4; offset += 1) {
        for (const candidate of offset === 0
          ? [target]
          : [target - offset, target + offset]) {
          try {
            makeExactSizeOutputItem(candidate);
            return candidate;
          } catch {
            // Not constructible; keep looking.
          }
        }
      }
      return undefined;
    };
    let fittingItemBytes: number =
      MIDGARD_ENVELOPE_MEASUREMENTS.maxReliableDirectCompleteItemBytes;
    let overflowingItemBytes: number = TIER1_MAX_COMPLETE_ITEM_BYTES;
    const probes: Record<string, number> = {
      [TIER1_MAX_COMPLETE_ITEM_BYTES.toString()]:
        byReference.completeSignedBytes,
    };
    while (overflowingItemBytes - fittingItemBytes > 1) {
      const midpoint = Math.floor(
        (fittingItemBytes + overflowingItemBytes) / 2,
      );
      const candidate = constructibleNear(midpoint);
      if (
        candidate === undefined ||
        candidate <= fittingItemBytes ||
        candidate >= overflowingItemBytes
      ) {
        break;
      }
      const probe = await measureObserveAt(candidate);
      probes[candidate.toString()] =
        probe.observe.measurement.completeSignedBytes;
      if (
        probe.observe.measurement.completeSignedBytes <= MAX_L1_PROOF_TX_BYTES
      ) {
        fittingItemBytes = candidate;
      } else {
        overflowingItemBytes = candidate;
      }
    }
    if (probes[fittingItemBytes.toString()] === undefined) {
      const floorProbe = await measureObserveAt(fittingItemBytes);
      probes[fittingItemBytes.toString()] =
        floorProbe.observe.measurement.completeSignedBytes;
    }
    // The frontier is tight to within the constructible-size ladder's gaps.
    expect(overflowingItemBytes - fittingItemBytes).toBeLessThanOrEqual(3);
    expect(probes[fittingItemBytes.toString()]).toBeLessThanOrEqual(
      MAX_L1_PROOF_TX_BYTES,
    );
    expect(probes[overflowingItemBytes.toString()]).toBeGreaterThan(
      MAX_L1_PROOF_TX_BYTES,
    );

    if (process.env.MIDGARD_PRINT_PROOF_FIT === "1") {
      console.info(
        JSON.stringify(
          {
            tier1CapSignedStepTransactionV1: {
              tier1PreimageCapBytes: MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
              itemBytes: TIER1_MAX_COMPLETE_ITEM_BYTES,
              evidenceBytes: atCap.itemCase.argument.evidenceCbor.length,
              byReferenceObserveTransaction: byReference,
              embeddedObserveTransaction: embedded.observe.measurement,
              fittingFrontierItemBytes: fittingItemBytes,
              fittingFrontierSignedBytes: probes[fittingItemBytes.toString()],
              overflowSignedBytes: probes[overflowingItemBytes.toString()],
              probes,
              maxL1ProofTxBytes: MAX_L1_PROOF_TX_BYTES,
            },
          },
          (_key, value: unknown) =>
            typeof value === "bigint" ? value.toString() : value,
          2,
        ),
      );
    }
  }, 900_000);

  it("pins the exact applied publication frontiers and reliability reserve", async () => {
    const reliable =
      MIDGARD_ENVELOPE_MEASUREMENTS.maxReliableCompleteItemPublicationBytes;
    const exact =
      MIDGARD_ENVELOPE_MEASUREMENTS.maxExactCompleteItemPublicationBytes;
    const [reliableFit, reliableOverflow, exactFit, exactOverflow] =
      await measurePublicationFrontierAt([
        reliable,
        reliable + 1,
        exact,
        exact + 1,
      ]);
    expect(reliableFit!.publication.measurement.completeSignedBytes).toBe(
      MAX_L1_PROOF_TX_BYTES -
        MIDGARD_ENVELOPE_MEASUREMENTS.proofItemEnvelopeReliabilityReserveBytes,
    );
    expect(
      reliableOverflow!.publication.measurement.completeSignedBytes,
    ).toBeGreaterThan(
      MAX_L1_PROOF_TX_BYTES -
        MIDGARD_ENVELOPE_MEASUREMENTS.proofItemEnvelopeReliabilityReserveBytes,
    );
    expect(exactFit!.publication.measurement.completeSignedBytes).toBe(
      MAX_L1_PROOF_TX_BYTES,
    );
    expect(
      exactOverflow!.publication.measurement.completeSignedBytes,
    ).toBeGreaterThan(MAX_L1_PROOF_TX_BYTES);

    if (process.env.MIDGARD_PRINT_PROOF_FIT === "1") {
      console.info(
        JSON.stringify(
          {
            completeItemPublicationFrontierV1: Object.fromEntries(
              [reliableFit, reliableOverflow, exactFit, exactOverflow].map(
                ({ itemBytes, publication }) => [
                  itemBytes.toString(),
                  {
                    ...publication.measurement,
                    datumBytes: publication.datumCbor.length / 2,
                    minAdaLovelace: publication.minAdaLovelace,
                  },
                ],
              ),
            ),
          },
          (_key, value: unknown) =>
            typeof value === "bigint" ? value.toString() : value,
          2,
        ),
      );
    }
  }, 600_000);

  // REMOVED 2026-08-14 (owner ruling): "measures inline-datum publication plus
  // reference-input consumption at the publication maximum" MOVED to
  // `complete-item-carriage-tiers-emulator.test.ts` as "carries one complete
  // item at the applied publication maximum through the tier-2 door".
  //
  // It cannot run here. The publication maximum is 14,396 bytes, whose field-2
  // preimage is 14,400 — tier-2 `RawUtxo` under §8.4, and this harness is
  // tier-1 `Inline` only. The row only appeared to run because the witness
  // selector matched field 0 instead of field 2, so it measured a ~40-byte
  // preimage and a 363-byte "publication". See TIER1_MAX_COMPLETE_ITEM_BYTES
  // for the 64-byte overhang this exposed, which #580 owns.

  it("reaches the identical observed state through inline and reference delivery of the same item", async () => {
    // Same complete item, both deliveries, one emulator: the applied door must
    // accept each and write the byte-identical observation. MOVED ONTO THE
    // OBSERVE DOOR (#617 sign-off item 1): before Option B the two deliveries
    // were two arms of the authenticate stage's `Verify`; #620 retired the
    // reference arm there and made the observe stage the sole content gate, so
    // route determinism is a property of that door now. Both legs park and
    // read the same reference scripts, the production basis since the #617
    // wiring.
    const itemBytes =
      MIDGARD_ENVELOPE_MEASUREMENTS.maxReliableDirectCompleteItemBytes;
    const itemCase = await buildCanonicalDecodeItemCase(itemBytes);
    const harness = await setupEmulator([
      itemCase.preparedThreadDatum,
      itemCase.preparedThreadDatum,
    ]);
    const [inlineThread, referenceThread] = harness.threadUtxos;
    const semanticScriptReference = await publishReferenceScript(
      harness,
      harness.semanticScript,
      "2f",
    );
    const observeScriptReference = await publishReferenceScript(
      harness,
      harness.stages.observe.spendingScript,
      "3f",
    );

    const inline = await runJourneyToObserve({
      harness,
      itemCase,
      threadUtxo: inlineThread!,
      delivery: { kind: "inline" },
      observeScriptReference,
      semanticScriptReference,
    });
    const publication = await publishProofItem({ harness, itemCase });
    const reference = await runJourneyToObserve({
      harness,
      itemCase,
      threadUtxo: referenceThread!,
      delivery: { kind: "reference", publication: publication.utxo },
      observeScriptReference,
      semanticScriptReference,
    });

    // Both doors wrote the same observation, and both transactions fit the
    // real L1 envelope.
    expect(
      sameDatumValue(
        inline.observe.nextThreadUtxo.datum ?? "",
        reference.observe.nextThreadUtxo.datum ?? "",
      ),
    ).toBe(true);
    expect(
      sameDatumValue(
        inline.observe.nextThreadUtxo.datum ?? "",
        itemCase.observedDatum,
      ),
    ).toBe(true);
    expect(inline.observe.measurement.completeSignedBytes).toBeLessThanOrEqual(
      MAX_L1_PROOF_TX_BYTES,
    );
    expect(
      reference.observe.measurement.completeSignedBytes,
    ).toBeLessThanOrEqual(MAX_L1_PROOF_TX_BYTES);

    // The whole point of the reference delivery: the item is named, not
    // serialized again, so its door transaction is far smaller than the inline
    // one at the same item size.
    expect(reference.observe.measurement.completeSignedBytes).toBeLessThan(
      inline.observe.measurement.completeSignedBytes - itemBytes / 2,
    );
    expect(reference.observe.measurement.referenceInputCount).toBe(2);

    const terminal = (
      await harness.lucid.utxosAt(harness.stages.proof.spendingScriptAddress)
    ).map((utxo) => utxo.datum ?? "");
    expect(terminal).toHaveLength(2);
    expect(new Set(terminal).size).toBe(1);
  }, 900_000);

  it("rejects substituted and trailing-byte items at the observe reference door, and accepts the honest one", async () => {
    // REWRITTEN ONTO THE OBSERVE DOOR (#617 sign-off item 1). This row used to
    // drive the authenticate stage's retired `VerifyReference` arm. On the
    // Option B wire that arm does not exist, so every submission it made —
    // hostile or honest — was refused for the wrong reason: the row was
    // VACUOUS, rejecting the honest publication too, which is exactly what an
    // unfalsifiable negative control looks like. The honest leg below is the
    // control that keeps it honest: the same machinery, the same door, the
    // unmutated publication, GREEN. A refusal only counts because that leg
    // passes.
    const itemBytes = 12_000;
    const itemCase = await buildCanonicalDecodeItemCase(itemBytes);
    const harness = await setupEmulator([
      itemCase.preparedThreadDatum,
      itemCase.preparedThreadDatum,
      itemCase.preparedThreadDatum,
    ]);
    const semanticScriptReference = await publishReferenceScript(
      harness,
      harness.semanticScript,
      "2f",
    );
    const observeScriptReference = await publishReferenceScript(
      harness,
      harness.stages.observe.spendingScript,
      "3f",
    );
    const observeWith = async (
      threadUtxo: UTxO,
      publication: UTxO,
    ): Promise<CompleteItemJourney> =>
      await runJourneyToObserve({
        harness,
        itemCase,
        threadUtxo,
        delivery: { kind: "reference", publication },
        observeScriptReference,
        semanticScriptReference,
      });

    // Substitution: same length, one flipped byte deep inside the published
    // field preimage. The door hashes the whole preimage against the committed
    // field commitment, so a single flipped byte anywhere inside it fails
    // closed.
    //
    // #579: the offset is taken from the preimage's OWN length. It used to be
    // `itemBytes - 100`, an index into a buffer that a selection defect had
    // made ~40 bytes long — the write landed past the end, `Buffer` swallowed
    // it, and the "substituted" preimage was byte-identical to the original.
    // The equality guard below is what keeps that from being silent again: a
    // test that mutates a buffer must show the mutation took before it can
    // claim the mutation was rejected.
    const original = Buffer.from(itemCase.fieldPreimageHex, "hex");
    const substituted = Buffer.from(original);
    const flipOffset = substituted.length - 100;
    expect(flipOffset).toBeGreaterThan(0);
    substituted[flipOffset] = substituted[flipOffset]! ^ 0x01;
    expect(substituted.length).toBe(original.length);
    expect(substituted.equals(original)).toBe(false);
    const substitutedPublication = await publishRawProofItemForNegativeControl({
      harness,
      itemCase,
      fieldPreimage: substituted.toString("hex"),
    });
    await expect(
      observeWith(harness.threadUtxos[0]!, substitutedPublication.utxo),
    ).rejects.toThrow(/canonical item observation local evaluation failed/u);

    // Trailing data: the exact item plus one extra byte.
    const trailingPublication = await publishRawProofItemForNegativeControl({
      harness,
      itemCase,
      fieldPreimage: `${itemCase.fieldPreimageHex}00`,
    });
    await expect(
      observeWith(harness.threadUtxos[1]!, trailingPublication.utxo),
    ).rejects.toThrow(/canonical item observation local evaluation failed/u);

    // The control: the honest publication, through the same door, on the same
    // blueprint. If this leg ever goes red the two refusals above stop meaning
    // anything, and this row says so instead of staying quietly green.
    const honestPublication = await publishProofItem({ harness, itemCase });
    const honest = await observeWith(
      harness.threadUtxos[2]!,
      honestPublication.utxo,
    );
    expect(
      sameDatumValue(
        honest.observe.nextThreadUtxo.datum ?? "",
        itemCase.observedDatum,
      ),
    ).toBe(true);
  }, 900_000);
});
