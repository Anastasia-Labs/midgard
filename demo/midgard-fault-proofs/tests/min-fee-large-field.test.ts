/**
 * The min-fee end to end at a large field: 761 committed outputs fill field 2
 * under tier 3. The fee formula reads each field's byte length only, so step
 * 02 authenticates the committed bytes and its cost no longer grows with the
 * field's item count.
 */
import {
  encodeCbor,
  encodeMidgardFieldPreimage,
  encodeMidgardNativeTxWitnessSetCompact,
  MIDGARD_CHUNK_BYTES_K,
  outRefLabel,
} from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import {
  certifyFaultProofFieldCarriage,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../src/field-opening.js";
import { submitMinFeeStep02 } from "../src/index.js";
import {
  VAN_ROSSEM_MAX_CPU_UNITS,
  VAN_ROSSEM_MAX_MEMORY_UNITS,
} from "../src/proof-fit/van-rossem.js";
import { createOwnOutputJournal } from "../src/workflow/own-outputs.js";
import {
  advanceStep01,
  initThread,
  makeHarness,
  setupScenario,
} from "./submit-init-emulator-min-fee.setup-scenario.js";
import {
  captureEmulatorSubmission,
  expectSingleUtxoWithUnit,
  network,
  publishPlainReferenceScriptUtxo,
} from "./support/submit-init-emulator-shared.js";

/** 761 outputs of 43 preimage bytes fill field 2 to the maximum. */
const OUTPUT_COUNT = 761;
const RESERVED_MEMORY = (VAN_ROSSEM_MAX_MEMORY_UNITS * 4n) / 5n;
const RESERVED_CPU = (VAN_ROSSEM_MAX_CPU_UNITS * 4n) / 5n;
const LABEL = "min-fee step-02 field 2";

/** One canonical output: enterprise pubkey address, lovelace only. */
const outputCbor = (index: number): Buffer =>
  Buffer.concat([
    Buffer.from([0xa2, 0x00, 0x58, 0x1d, 0x60]),
    Buffer.alloc(28, (0x40 + index) % 0x100),
    Buffer.from([0x01, 0x82]),
    encodeCbor(5_000_000n + BigInt(index)),
    Buffer.from([0xa0]),
  ]);

describe("min-fee step 02 at a large field", () => {
  it("convicts a fee below the minimum with 761 outputs certified in chunks", async () => {
    const harness = await makeHarness();
    const scenario = await setupScenario({
      harness,
      fee: 999n,
      headerMinimum: 1_000n,
      outputs: Array.from({ length: OUTPUT_COUNT }, (_, index) =>
        outputCbor(index),
      ),
    });
    const outputItems = scenario.fieldItemCbors[2];
    expect(outputItems).toHaveLength(OUTPUT_COUNT);
    expect(encodeMidgardFieldPreimage(outputItems).length).toBeGreaterThan(
      MIDGARD_CHUNK_BYTES_K,
    );

    const init = await initThread(harness, scenario);
    const bound = await advanceStep01(harness, scenario, init.nextThreadOutRef);

    // Tier 3: field 2's chunks and their §8.6 certificate exist first.
    const { witnessSet } = scenario.prepared.tx;
    const ownOutputs = createOwnOutputJournal();
    const planned = planFaultProofFieldOpening({
      anchorSourceKind: 0n,
      fieldIndex: 2,
      anchorTxId: scenario.prepared.tx.nodeTxId,
      nativeTxCompactCbor: scenario.prepared.tx.nativeTxCompactCbor,
      itemCbors: outputItems,
      owner: harness.proverSigner.paymentKeyHash,
      label: LABEL,
    });
    expect(planned.plan.tier).toBe("Certified");
    const chunkUtxos = await publishFaultProofFieldCarriage({
      ownOutputs,
      lucid: harness.proverLucid,
      signer: harness.proverSigner,
      planned,
      publisherAddress: harness.proverSigner.address,
      label: LABEL,
    });
    const preimageCertificate = harness.contracts.fieldPreimageCertificate;
    const { utxo: preimageCertificateReference } =
      await publishPlainReferenceScriptUtxo({
        lucid: harness.funderLucid,
        script: preimageCertificate.mintingScript,
        label: "field preimage certificate",
      });
    const { certificateUtxo } = await certifyFaultProofFieldCarriage({
      ownOutputs,
      lucid: harness.proverLucid,
      network,
      signer: harness.proverSigner,
      planned,
      certificatePolicyId: preimageCertificate.policyId,
      certificateMintingScript: preimageCertificate.mintingScript,
      certificateReferenceScriptUtxo: preimageCertificateReference,
      chunkUtxos,
      compactCbor: scenario.prepared.tx.nativeTxCompactCbor,
      witnessSetCompactCbor: encodeMidgardNativeTxWitnessSetCompact({
        addrTxWitsHash: Buffer.from(witnessSet.addr_tx_wits_hash, "hex"),
        scriptTxWitsHash: Buffer.from(witnessSet.script_tx_wits_hash, "hex"),
        redeemerTxWitsHash: Buffer.from(
          witnessSet.redeemer_tx_wits_hash,
          "hex",
        ),
      }).toString("hex"),
    });

    const convicted = await captureEmulatorSubmission(harness.emulator, () =>
      submitMinFeeStep02({
        ownOutputs,
        lucid: harness.proverLucid,
        contracts: harness.minFee,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: bound.nextThreadOutRef,
        nativeTxCompactCbor: scenario.prepared.tx.nativeTxCompactCbor,
        witnessSet: scenario.prepared.tx.witnessSet,
        fieldItemCbors: scenario.fieldItemCbors,
        referenceScriptUtxo: scenario.refs[1],
        witnessReferenceScripts: harness.witnessReferenceScripts,
        existingPublicationUtxos: chunkUtxos,
        certificateUtxos: [certificateUtxo],
        publishMissingCarriages: false,
      }),
    );
    expect(convicted.measurement.executionMemory).toBeLessThanOrEqual(
      RESERVED_MEMORY,
    );
    expect(convicted.measurement.executionSteps).toBeLessThanOrEqual(
      RESERVED_CPU,
    );
    const finalized = convicted.result;
    expect(finalized.fee).toBe(999n);
    expect(finalized.minimumFee).toBe(1_000n);
    expect(finalized.fieldCarriageTiers[2]).toBe("Certified");
    expect(finalized.fieldPreimageLengths[2]).toBe(
      encodeMidgardFieldPreimage(outputItems).length,
    );

    const proofUtxo = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      harness.minFee.fraudProof.spendingScriptAddress,
      finalized.fraudProofUnit,
    );
    expect(outRefLabel(proofUtxo)).toBe(finalized.fraudProofOutRef);
  }, 1_800_000);
});
