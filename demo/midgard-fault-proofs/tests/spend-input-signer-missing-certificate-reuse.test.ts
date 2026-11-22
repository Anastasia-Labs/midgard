/**
 * Step 02 and the shared signer-witness scan at Certified-tier openings, each
 * driven turn by turn, one transaction per build. Step 02
 * certifies the spend-input opening, the opening scan turn the
 * address-witness opening, and every resumed scan turn reuses the
 * address-witness certificate the opening turn left on chain, so each turn
 * after certification builds the scan itself and the thread holds exactly
 * one certificate per opening.
 */
import {
  encodeCbor,
  encodeMidgardNativeTxCanonical,
} from "@al-ft/midgard-core";
import { acceptedVerdictSubject } from "@al-ft/midgard-sdk/fraud-proof";
import { describe, expect, it } from "vitest";

import { buildSignerWitnessScan } from "../src/signer-witness-scan/index.js";
import { buildSpendInputSignerMissingStep02 } from "../src/spend-input-signer-missing/submit-step-02.js";
import {
  commitAcceptedBlock,
  publishReferences,
} from "./spend-input-signer-missing-lifecycle.commit-forced-block.js";
import { familyDriver } from "./spend-input-signer-missing-lifecycle.family-driver.js";
import {
  FAMILY,
  garbageWitness,
  MAXIMUM_WITNESSES,
  newHarness,
  prepareSpendInputSignerMissingEvidence,
  priorLedgerFor,
  registeredContracts,
} from "./spend-input-signer-missing-lifecycle.registered-contracts.js";
import {
  capturedOutputHolding,
  driveCapturedStep,
  fieldPreimageCertificateCount,
} from "./support/captured-step-turns.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";

/** Enough repeated spend inputs to carry the input field above RawUtxo. */
const SPEND_INPUT_COUNT = 800;

describe("spendInputSignerMissing step 02 and the scan built turn by turn", () => {
  it("reuses each field certificate already on chain, so every turn after certification is the step", async () => {
    const harness = await newHarness();
    const family = await registeredContracts(harness);
    const prior = await priorLedgerFor(
      Buffer.alloc(28, 0x51).toString("hex"),
      "ab".repeat(32),
    );
    const nativeTx = makeNativeTx({
      spendInputCbors: Array.from(
        { length: SPEND_INPUT_COUNT },
        () => prior.outRefBytes,
      ),
      fee: 7n,
      addrTxWitsPreimageCbor: encodeCbor(
        Array.from({ length: MAXIMUM_WITNESSES }, (_unused, index) =>
          garbageWitness(index),
        ),
      ),
    });
    const block = await commitAcceptedBlock(
      harness,
      family,
      nativeTx,
      prior.priorRoot,
    );
    const evidence = prepareSpendInputSignerMissingEvidence({
      subject: acceptedVerdictSubject(block.nativeTxId),
      inputIndex: 0n,
      canonicalTransactionCbor: encodeMidgardNativeTxCanonical(nativeTx),
      resolved: prior.resolved,
    });
    expect(evidence.inputCarriage).toBe("Certified");
    expect(evidence.witnessCarriage).toBe("Certified");
    const { references, certificateReference } = await publishReferences(
      harness,
      family,
      FAMILY,
      true,
    );
    const run = familyDriver(harness, family, references, certificateReference);
    const thread = await run.initThread(block.blockOutRef);
    const unit = thread.result.computationThreadUnit;
    const step01 = await run.step01Accepted(
      thread,
      evidence,
      block.blockOutRef,
      block.txInclusion,
    );
    const common = {
      lucid: harness.proverLucid,
      network: harness.proverLucid.config().network!,
      contracts: family.contracts,
      categoryId: family.category.categoryId,
      signer: harness.proverSigner,
      evidence,
      nativeTxCompactCbor: block.compactCbor,
      witnessSetCompactCbor: block.witnessSetCompactCbor,
      certificateReferenceScriptUtxo: certificateReference,
      // The claims each turn's carriage is read back by, as a CLI run keeps
      // them in memory.
    };
    const certificates = () =>
      fieldPreimageCertificateCount({
        lucid: harness.proverLucid,
        certificatePolicyId: family.contracts.fieldPreimageCertificatePolicyId,
      });

    const step02 = await driveCapturedStep({
      emulator: harness.emulator,
      threadOutRef: step01.nextThreadOutRef,
      maxTurns: 6,
      invoke: async () =>
        await buildSpendInputSignerMissingStep02({
          ...common,
          threadOutRef: step01.nextThreadOutRef,
          referenceScriptUtxo: references[1]!,
          membershipReferenceScriptUtxo:
            harness.witnessReferenceScripts.phasMembershipWithdraw!,
        }),
    });
    expect(step02.prerequisites.length).toBeGreaterThan(0);
    expect(await certificates()).toBe(1n);

    let scanThread = capturedOutputHolding(step02.step, unit);
    for (const scanTurn of ["opening", "resumed", "resumed again"]) {
      const threadOutRef = scanThread;
      const scan = await driveCapturedStep({
        emulator: harness.emulator,
        threadOutRef,
        maxTurns: scanTurn === "opening" ? 6 : 3,
        invoke: async () =>
          await buildSignerWitnessScan({
            ...common,
            family: FAMILY,
            threadOutRef,
            referenceScriptUtxo: references[2]!,
          }),
      });
      if (scanTurn === "opening") {
        expect(scan.prerequisites.length).toBeGreaterThan(0);
        expect(await certificates()).toBe(2n);
      } else
        expect({ scanTurn, prerequisites: scan.prerequisites.length }).toEqual({
          scanTurn,
          prerequisites: 0,
        });
      scanThread = capturedOutputHolding(scan.step, unit);
    }
    expect(await certificates()).toBe(2n);
  }, 1_800_000);
});
