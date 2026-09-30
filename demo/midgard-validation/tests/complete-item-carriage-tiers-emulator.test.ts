import "node:fs";
import "node:path";
import "@al-ft/midgard-core";
import "@al-ft/midgard-core/codec/native-tx-carriage";
import "@al-ft/midgard-core/codec/native-tx-field-access";
import "@al-ft/midgard-core/consensus-profile";
import "@al-ft/midgard-fault-proofs";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "effect";
import "vitest";
import "../src/index.js";
import "./validation-fixtures.js";
import "./complete-item-carriage-tiers-emulator.outputs-for-field-two-preimage-bytes.js";
import "./complete-item-carriage-tiers-emulator.publish-carriage.js";
import "./complete-item-carriage-tiers-emulator.submit-stage.js";
import "./complete-item-carriage-tiers-emulator.run-tier-journey.js";
import "./complete-item-carriage-tiers-emulator.publication-maximum-item-bytes.js";

import {
  encodeMidgardFieldPreimage,
  MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
} from "@al-ft/midgard-core/codec/native-tx-field-access";
import { assertMidgardFieldCarriageResolvesAtDoor } from "@al-ft/midgard-sdk";
import {
  applyParamsToScript,
  type UTxO,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  CERTIFICATE_MINT_TITLE,
  certificatePolicy,
  compiledScript,
  loadContracts,
  makeExactSizeOutputItem,
  MAX_L1_TX_BYTES,
  NETWORK,
  OBSERVE_TITLE,
  OUTPUT_FIELD_INDEX,
  outputsForFieldTwoPreimageBytes,
} from "./complete-item-carriage-tiers-emulator.outputs-for-field-two-preimage-bytes.js";
import {
  PUBLICATION_MAXIMUM_ITEM_BYTES,
  PUBLICATION_MAXIMUM_PREIMAGE_BYTES,
  PUBLICATION_MAXIMUM_TIER1_OVERHANG_BYTES,
  TIER1_ADMISSIBLE_ITEM_BYTES,
  TIER2_PREIMAGE_BYTES,
  TIER3_PREIMAGE_BYTES,
} from "./complete-item-carriage-tiers-emulator.publication-maximum-item-bytes.js";
import { sameDatumValue } from "./complete-item-carriage-tiers-emulator.publish-carriage.js";
import { runTierJourney } from "./complete-item-carriage-tiers-emulator.run-tier-journey.js";

describe("complete-item §8 carriage tiers 2 and 3 (emulator, applied validators)", () => {
  it("pins the §8.6 certificate policy the observe door is parameterised by", async () => {
    // The door's tier-3 branch checks the named reference input for one unit of
    // *this* policy, so if the policy id the certification mints under is not
    // the one baked into the observe validator, tier 3 cannot pass — and would
    // look like a carriage defect. Rebuilding the validator from the computed
    // policy id and matching script hashes is what turns that into a stated
    // identity rather than an inference from a green row.
    const contracts = await loadContracts();
    const policy = certificatePolicy();
    const rebuilt = validatorToScriptHash({
      type: "PlutusV3",
      script: applyParamsToScript(compiledScript(OBSERVE_TITLE), [
        contracts.validationTraceDispute.canonicalDecodeItemStages.proof
          .spendingScriptHash,
        contracts.computationThread.policyId,
        contracts.validationTraceDispute.proofItem.spendingScriptHash,
        policy.policyId,
      ]),
    });
    expect(rebuilt).toBe(
      contracts.validationTraceDispute.canonicalDecodeItemStages.observe
        .spendingScriptHash,
    );
    // The certificate address is the policy's own script credential with no
    // stake part — the mint handler refuses anything else.
    expect(policy.address).toBe(
      validatorToAddress(NETWORK, {
        type: "PlutusV3",
        script: compiledScript(CERTIFICATE_MINT_TITLE),
      }),
    );
  }, 120_000);

  it("carries a tier-2 RawUtxo field preimage through the applied door", async () => {
    const journey = await runTierJourney(TIER2_PREIMAGE_BYTES);
    expect(journey.tier).toBe("RawUtxo");
    expect(journey.preimageBytes).toBeGreaterThan(
      MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
    );
    expect(journey.published.chunkUtxos).toHaveLength(1);
    expect(journey.published.certificateUtxo).toBeUndefined();

    // The committed index is not `0`: the published observe validator sorts
    // into the same list, so an index that agreed with itself trivially would
    // prove nothing about the resolution.
    if (journey.committedCarriage.carriage !== "RawUtxo") {
      throw new Error("tier-2 journey did not commit RawUtxo carriage");
    }
    expect(journey.committedCarriage.refInputIndex).toBeGreaterThanOrEqual(0);
    expect(journey.doorReferenceInputs).toHaveLength(2);

    // The auxiliary is indices, so it is O(1) in the output it stands for —
    // 14,778 bytes of preimage reach the door in a handful of redeemer bytes.
    expect(journey.auxiliaryBytes).toBeLessThan(128);

    // The door wrote the observation the off-chain staging predicted.
    expect(
      sameDatumValue(journey.observedOnLedger, journey.observedDatum),
    ).toBe(true);
    expect(journey.observation.itemCount).toBe(2n);
    expect(journey.observation.itemLength).toBe(
      BigInt(outputsForFieldTwoPreimageBytes(TIER2_PREIMAGE_BYTES)[0]!.length),
    );

    for (const [stage, bytes] of Object.entries(journey.stageBytes)) {
      expect(bytes, stage).toBeLessThanOrEqual(MAX_L1_TX_BYTES);
    }
    for (const bytes of journey.published.publicationBytes) {
      expect(bytes).toBeLessThanOrEqual(MAX_L1_TX_BYTES);
    }
  }, 900_000);

  it("carries one complete item at the applied publication maximum through the tier-2 door", async () => {
    // Moved here from `complete-item-proof-fit-emulator.test.ts` (owner
    // ruling, 2026-08-14). One item at the cap, not a byte count split across
    // two: the claim is about the largest single complete item the applied
    // policy admits for publication.
    const item = makeExactSizeOutputItem(PUBLICATION_MAXIMUM_ITEM_BYTES);
    expect(item.length).toBe(PUBLICATION_MAXIMUM_ITEM_BYTES);
    expect(encodeMidgardFieldPreimage([item]).length).toBe(
      PUBLICATION_MAXIMUM_PREIMAGE_BYTES,
    );

    // The overhang, asserted rather than described. #580 measured and
    // dispositioned it — the band selects tier 2, it is not a hole — and this
    // assertion stays as the anti-conflation guard: if a future change closes
    // the overhang or equates the two ceilings, this row fails and says so.
    expect(PUBLICATION_MAXIMUM_ITEM_BYTES - TIER1_ADMISSIBLE_ITEM_BYTES).toBe(
      PUBLICATION_MAXIMUM_TIER1_OVERHANG_BYTES,
    );
    expect(
      encodeMidgardFieldPreimage([
        makeExactSizeOutputItem(TIER1_ADMISSIBLE_ITEM_BYTES),
      ]).length,
    ).toBe(MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES);

    const journey = await runTierJourney(PUBLICATION_MAXIMUM_PREIMAGE_BYTES, {
      outputs: [item],
    });

    // The cap is past tier 1, which is the whole reason this row is in this
    // suite rather than the tier-1 proof-fit harness.
    expect(journey.tier).toBe("RawUtxo");
    expect(journey.preimageBytes).toBeGreaterThan(
      MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
    );
    expect(journey.observation.itemCount).toBe(1n);
    expect(journey.observation.itemLength).toBe(
      BigInt(PUBLICATION_MAXIMUM_ITEM_BYTES),
    );

    // "Inline-datum publication at the publication maximum" — the item is
    // published whole, in one publication, inside the L1 envelope.
    expect(journey.published.chunkUtxos).toHaveLength(1);
    expect(journey.published.certificateUtxo).toBeUndefined();
    for (const bytes of journey.published.publicationBytes) {
      expect(bytes).toBeLessThanOrEqual(MAX_L1_TX_BYTES);
    }

    // "...plus reference-input consumption": the door resolves the item from
    // the UTxO set instead of serializing it again. The old row asserted the
    // consuming transaction was under half the publication; indices are O(1) in
    // the item they stand for, which is strictly stronger.
    if (journey.committedCarriage.carriage !== "RawUtxo") {
      throw new Error("publication-maximum journey did not commit RawUtxo");
    }
    expect(journey.committedCarriage.refInputIndex).toBeGreaterThanOrEqual(0);
    expect(journey.auxiliaryBytes).toBeLessThan(128);
    expect(
      sameDatumValue(journey.observedOnLedger, journey.observedDatum),
    ).toBe(true);
    for (const [stage, bytes] of Object.entries(journey.stageBytes)) {
      expect(bytes, stage).toBeLessThanOrEqual(MAX_L1_TX_BYTES);
    }
  }, 900_000);

  it("carries a tier-3 Certified field preimage through the applied door", async () => {
    const journey = await runTierJourney(TIER3_PREIMAGE_BYTES);
    expect(journey.tier).toBe("Certified");
    expect(journey.plan.publications.length).toBeGreaterThan(1);
    expect(journey.published.certificateUtxo).toBeDefined();
    if (journey.committedCarriage.carriage !== "Certified") {
      throw new Error("tier-3 journey did not commit Certified carriage");
    }
    expect(journey.committedCarriage.chunkRefInputIndices).toHaveLength(
      journey.plan.publications.length,
    );
    // Every index is distinct and inside the door's reference-input list.
    const named = [
      journey.committedCarriage.certRefInputIndex,
      ...journey.committedCarriage.chunkRefInputIndices,
    ];
    expect(new Set(named).size).toBe(named.length);
    for (const index of named) {
      expect(index).toBeGreaterThanOrEqual(0);
      expect(index).toBeLessThan(journey.doorReferenceInputs.length);
    }

    expect(journey.auxiliaryBytes).toBeLessThan(128);
    expect(
      sameDatumValue(journey.observedOnLedger, journey.observedDatum),
    ).toBe(true);
    expect(journey.observation.itemCount).toBe(2n);
    expect(journey.observation.itemLength).toBe(
      BigInt(outputsForFieldTwoPreimageBytes(TIER3_PREIMAGE_BYTES)[0]!.length),
    );

    // Every transaction on the ladder — the full-`K` chunk publication
    // included — clears the real 16,384-byte envelope. That is §8.3 erratum
    // E1's repair, measured against applied validators rather than asserted.
    for (const [stage, bytes] of Object.entries(journey.stageBytes)) {
      expect(bytes, stage).toBeLessThanOrEqual(MAX_L1_TX_BYTES);
    }
    for (const bytes of journey.published.publicationBytes) {
      expect(bytes).toBeLessThanOrEqual(MAX_L1_TX_BYTES);
    }
    expect(journey.published.certificationBytes ?? 0).toBeLessThanOrEqual(
      MAX_L1_TX_BYTES,
    );
  }, 900_000);

  it("refuses a door whose reference-input set moved under the committed indices", async () => {
    // Ruling D3-A, over ledger-resolved material: the indices were resolved
    // against one list and the door would see another. Catching that off chain
    // is the guard's whole job, because the committed `evidence_hash` binds the
    // indices and a failed submission has already spent the staged evidence.
    const journey = await runTierJourney(TIER3_PREIMAGE_BYTES);

    // The perturbation is the cheapest realistic one and the only deterministic
    // one: an entry that sorts ahead of every emulator UTxO — no blake2b tx id
    // is all-zero — shifts the whole canonically-sorted list by one, which is
    // exactly "the door transaction acquired a reference input the indices did
    // not count".
    const acquiredAhead: UTxO = {
      txHash: "00".repeat(32),
      outputIndex: 0,
      address: journey.doorReferenceInputs[0]!.address,
      assets: { lovelace: 5_000_000n },
    };
    const movedDoor = [acquiredAhead, ...journey.doorReferenceInputs];
    expect(() =>
      assertMidgardFieldCarriageResolvesAtDoor({
        carriage: journey.committedCarriage,
        plan: journey.plan,
        doorReferenceInputs: movedDoor,
        certificatePolicyId: journey.published.certificatePolicyId,
        label: `field ${OUTPUT_FIELD_INDEX.toString()}`,
      }),
    ).toThrowError(
      /§8 carriage does not resolve at the door-running transaction/u,
    );
    // The disagreement is a real re-resolution rather than an artefact of the
    // comparison: every index moved by exactly one.
    const movedCarriage = journey.reobserve(movedDoor);
    expect(movedCarriage).not.toStrictEqual(journey.committedCarriage);
    if (
      movedCarriage.carriage !== "Certified" ||
      journey.committedCarriage.carriage !== "Certified"
    ) {
      throw new Error("tier-3 journey did not commit Certified carriage");
    }
    expect(movedCarriage.certRefInputIndex).toBe(
      journey.committedCarriage.certRefInputIndex + 1,
    );
    expect(movedCarriage.chunkRefInputIndices).toStrictEqual(
      journey.committedCarriage.chunkRefInputIndices.map((index) => index + 1),
    );
    // And the guard is not policing something the door would have shrugged off:
    // the same committed redeemer, submitted without the carriage the indices
    // name, is refused by the applied validator.
    await expect(
      runTierJourney(TIER3_PREIMAGE_BYTES, { withholdCarriageAtDoor: true }),
    ).rejects.toThrow(/canonical item observation local evaluation failed/u);
  }, 900_000);
});
