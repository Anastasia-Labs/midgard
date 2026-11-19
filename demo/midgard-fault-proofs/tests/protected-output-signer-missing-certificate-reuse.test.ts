/**
 * Step 02 and the shared signer-witness scan driven turn by turn, one
 * transaction per build, over a Certified-tier address-witness opening.
 * The opening scan turn certifies the field and every resumed turn reuses
 * that certificate, so each turn after certification builds the scan
 * itself and the opening keeps a single certificate.
 */
import { acceptedVerdictSubject } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { PROTECTED_OUTPUT_SIGNER_MAX_WITNESSES } from "../src/protected-output-signer-missing/protected-output-signer-missing.js";
import { buildProtectedOutputSignerMissingStep02 } from "../src/protected-output-signer-missing/submit-step-02.js";
import { buildSignerWitnessScan } from "../src/signer-witness-scan/index.js";
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

describe("protectedOutputSignerMissing step 02 and the scan built turn by turn", () => {
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
      invoke: async () =>
        await buildProtectedOutputSignerMissingStep02({
          ...common,
          threadOutRef: bound,
          referenceScriptUtxo: s.refs[1]!,
        }),
    });

    let scanThread = capturedOutputHolding(step02.step, unit);
    for (const scanTurn of ["opening", "resumed", "resumed again"]) {
      const threadOutRef = scanThread;
      const scan = await driveCapturedStep({
        emulator: s.harness.emulator,
        threadOutRef,
        maxTurns: scanTurn === "opening" ? 6 : 3,
        invoke: async () =>
          await buildSignerWitnessScan({
            ...common,
            family: "protected-output-signer-missing",
            threadOutRef,
            referenceScriptUtxo: s.refs[2]!,
          }),
      });
      if (scanTurn === "opening") {
        expect(scan.prerequisites.length).toBeGreaterThan(0);
        expect(await certificates()).toBe(1n);
      } else
        expect({ scanTurn, prerequisites: scan.prerequisites.length }).toEqual({
          scanTurn,
          prerequisites: 0,
        });
      scanThread = capturedOutputHolding(scan.step, unit);
    }
    expect(await certificates()).toBe(1n);
  }, 1_800_000);
});
