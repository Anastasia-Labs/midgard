/**
 * Steps 02, 03 and 04 driven turn by turn through the workflow capture
 * boundary over a Certified-tier address-witness opening. Step 03 certifies
 * the opening and every step-04 scan turn reuses that certificate, so each
 * turn after certification captures the step itself and the opening keeps a
 * single certificate.
 */
import { acceptedVerdictSubject } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  PROTECTED_OUTPUT_SIGNER_MAX_WITNESSES,
  submitProtectedOutputSignerMissingStep02,
  submitProtectedOutputSignerMissingStep03,
  submitProtectedOutputSignerMissingStep04,
} from "../src/protected-output-signer-missing/index.js";
import { createOwnOutputJournal } from "../src/workflow/own-outputs.js";
import { makeScenario } from "./protected-output-signer-missing-lifecycle.make-scenario.js";
import {
  compactHex,
  decoyWitness,
  evidenceFor,
  nativeTxWith,
  witnessSetCompactHex,
} from "./protected-output-signer-missing-lifecycle.registered-contracts.js";
import {
  capturedOutputHolding,
  driveCapturedStep,
  fieldPreimageCertificateCount,
} from "./support/captured-step-turns.js";

describe("protectedOutputSignerMissing steps 02-04 under the workflow capture boundary", () => {
  it("reuses the field certificate already on chain, so every turn after certification is the step", async () => {
    const nativeTx = nativeTxWith({
      fee: 7n,
      witnesses: () =>
        Array.from(
          { length: PROTECTED_OUTPUT_SIGNER_MAX_WITNESSES },
          (_unused, index) => decoyWitness(index),
        ),
    });
    const s = await makeScenario({ nativeTx });
    const evidence = evidenceFor(
      acceptedVerdictSubject(s.block.nativeTxId),
      nativeTx,
    );
    expect(evidence.witnessCarriage).toBe("Certified");
    const bound = await s.accepted01(await s.init(), evidence);
    const [boundUtxo] = await s.harness.proverLucid.utxosByOutRef([
      {
        txHash: bound.split("#")[0]!,
        outputIndex: Number(bound.split("#")[1]),
      },
    ]);
    const unit = Object.keys(boundUtxo?.assets ?? {}).find((candidate) =>
      candidate.startsWith(s.contracts.computationThread.policyId),
    );
    if (unit === undefined) throw new Error("thread token absent");
    const common = {
      lucid: s.harness.proverLucid,
      contracts: s.contracts,
      categoryId: s.category.categoryId,
      signer: s.harness.proverSigner,
      evidence,
      nativeTxCompactCbor: compactHex(nativeTx),
      witnessSetCompactCbor: witnessSetCompactHex(nativeTx),
      certificateReferenceScriptUtxo: s.certificateRef,
      // The claims each turn's carriage is read back by, as a CLI run keeps
      // them in memory.
      ownOutputs: createOwnOutputJournal(),
    };
    const certificates = () =>
      fieldPreimageCertificateCount({
        lucid: s.harness.proverLucid,
        certificatePolicyId: s.contracts.fieldPreimageCertificatePolicyId,
      });

    const step02 = await driveCapturedStep({
      emulator: s.harness.emulator,
      threadOutRef: bound,
      maxTurns: 6,
      invoke: async (boundary) =>
        await submitProtectedOutputSignerMissingStep02({
          ...common,
          threadOutRef: bound,
          referenceScriptUtxo: s.refs[1]!,
          publicationBoundary: boundary,
          certificateBoundary: boundary,
          preSubmitBoundary: boundary,
        }),
    });

    const step03Thread = capturedOutputHolding(step02.step, unit);
    const step03 = await driveCapturedStep({
      emulator: s.harness.emulator,
      threadOutRef: step03Thread,
      maxTurns: 6,
      invoke: async (boundary) =>
        await submitProtectedOutputSignerMissingStep03({
          ...common,
          threadOutRef: step03Thread,
          referenceScriptUtxo: s.refs[2]!,
          publicationBoundary: boundary,
          certificateBoundary: boundary,
          preSubmitBoundary: boundary,
        }),
    });
    expect(step03.prerequisites.length).toBeGreaterThan(0);
    expect(await certificates()).toBe(1n);

    let scanThread = capturedOutputHolding(step03.step, unit);
    for (const scanTurn of ["initial", "resumed"]) {
      const threadOutRef = scanThread;
      const scan = await driveCapturedStep({
        emulator: s.harness.emulator,
        threadOutRef,
        maxTurns: 3,
        invoke: async (boundary) =>
          await submitProtectedOutputSignerMissingStep04({
            ...common,
            threadOutRef,
            referenceScriptUtxo: s.refs[3]!,
            publicationBoundary: boundary,
            certificateBoundary: boundary,
            preSubmitBoundary: boundary,
          }),
      });
      expect({ scanTurn, prerequisites: scan.prerequisites.length }).toEqual({
        scanTurn,
        prerequisites: 0,
      });
      scanThread = capturedOutputHolding(scan.step, unit);
    }
    expect(await certificates()).toBe(1n);
  }, 1_800_000);
});
