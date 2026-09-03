/**
 * `withdrawn-input` emulator tier-2 carriage: the §8.4 size partition
 * exercised with genuinely large committed data.
 *
 * The family's step-02 opens §2.5 field 0 through the §8.8 door, and §8.4
 * partitions the carriage tier on the preimage's size alone — the tier is
 * never a caller's argument. This journey commits a transaction whose
 * spend-input set is large enough (365 forty-byte out-ref items, a
 * 14,603-byte §5.1 preimage) that tier-1 inline carriage is inadmissible:
 * the preimage exceeds the 14,336-byte tier-1 redeemer bound, so a real
 * prover MUST publish it as a §8.2 `RawUtxo` bytes-only inline datum and
 * hand the door a reference input. Nothing forces the tier — the data's
 * size does. The withdrawn input is buried among 364 decoys that are not in
 * the withdrawals set, and the conviction still lands on it.
 *
 * Lives in its own file for the reason its siblings do: `@lucid-evolution/uplc`
 * never reclaims wasm linear memory and vitest isolates per FILE. See
 * tests/support/uplc-heap-guard.ts.
 */
import {
  encodeMidgardFieldPreimage,
  MIDGARD_CHUNK_BYTES_K,
  MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
} from "@al-ft/midgard-core";
import { encodeMidgardTxInputCanonical } from "@al-ft/midgard-sdk";
import { fieldPreimagePublicationDatumCbor } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { submitWithdrawnInputStep03 } from "../src/index.js";
import {
  advanceWithdrawnInputToStep03,
  makeWithdrawnInputEmulatorScenario,
} from "./support/withdrawn-input-emulator.js";

/**
 * 364 decoys plus the withdrawn input (constant §5.3 stride of 40 bytes
 * each) make a 14,603-byte field-0 preimage: past §8.4's tier-1 bound,
 * inside the single-publication tier-2 window `(14,336, 15,148]` — the size
 * alone selects `RawUtxo`.
 */
const TIER2_DECOY_SPEND_INPUT_COUNT = 364;

describe("withdrawn-input emulator tier-2 carriage", () => {
  it("convicts a withdrawn input buried in a 14,603-byte spend-input field through a size-selected RawUtxo publication", async () => {
    const scenario = await makeWithdrawnInputEmulatorScenario("fault", {
      decoySpendInputCount: TIER2_DECOY_SPEND_INPUT_COUNT,
    });
    // The size, not any flag, is what selects tier 2: past the tier-1
    // redeemer bound, within one publication.
    const preimage = encodeMidgardFieldPreimage(
      scenario.fixture.spendInputs.map(encodeMidgardTxInputCanonical),
    );
    expect(preimage.length).toBeGreaterThan(
      MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
    );
    expect(preimage.length).toBeLessThanOrEqual(MIDGARD_CHUNK_BYTES_K);

    const { step02 } = await advanceWithdrawnInputToStep03(scenario);
    expect(step02.carriageTier).toBe("RawUtxo");

    // The tier-2 publication really exists: the whole §5.1 preimage sits at
    // the prover's address as a bytes-only inline datum, referenced rather
    // than carried in the step's own redeemer.
    const expectedDatum = fieldPreimagePublicationDatumCbor(preimage);
    const publications = (
      await scenario.harness.proverLucid.utxosAt(
        scenario.harness.proverSigner.address,
      )
    ).filter((utxo) => utxo.datum === expectedDatum);
    expect(publications).toHaveLength(1);

    const final = await submitWithdrawnInputStep03({
      lucid: scenario.harness.proverLucid,
      contracts: scenario.contracts,
      categoryId: scenario.category.categoryId,
      signer: scenario.harness.proverSigner,
      threadOutRef: step02.nextThreadOutRef,
      withdrawalMembership: scenario.fixture.withdrawalMembership,
      referenceScriptUtxo: scenario.references[2],
      witnessReferenceScripts: scenario.harness.witnessReferenceScripts,
    });
    const [faultToken] = await scenario.harness.proverLucid.utxosAtWithUnit(
      final.fraudProofAddress,
      final.fraudProofUnit,
    );
    expect(faultToken).toBeDefined();
  }, 600_000);
});
