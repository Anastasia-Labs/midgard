/**
 * Publication fit for the shared ledger-output-proof family: every one of
 * the thirty LOP yields (twenty-three stage yields, the three attestation
 * yields, the four descriptor yields) and the four step/finalize dispatchers
 * publishes as a reference script under the real 16,384-byte L1 envelope
 * with margin. With `MIDGARD_WRITE_FIT_LEDGER=1` the measurements are pinned
 * to `docs/fault-proofs/size-plans/validation-trace-ledger-output-proof-publication-fit-ledger.json`.
 */
import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import { type ValidationTraceDisputeFaultProofContracts } from "@al-ft/midgard-sdk";
import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
  PROTOCOL_PARAMETERS_DEFAULT,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { validationSemanticResolverGlobalIndex } from "../src/index.js";
import {
  buildVanRossemFitLedger,
  type VanRossemFitMeasurement,
  writeVanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import {
  LEDGER_OUTPUT_DESCRIPTOR_YIELD_ROLES,
  LEDGER_OUTPUT_PROOF_ATTESTATION_YIELD_ROLES,
  LEDGER_OUTPUT_PROOF_STAGE_YIELD_ROLES,
} from "../src/validation-dispute/ledger-output-proof-yields.js";
import {
  alwaysSucceedsBlueprintPath,
  buildMinimalFaultProofContracts,
  EMULATOR_PROTOCOL_PARAMETERS,
  publishPlainReferenceScriptUtxo,
  readBlueprint,
  realBlueprintPath,
} from "./support/submit-init-emulator-shared.js";

/** The step/finalize dispatchers of both shared-LOP resolver pairs. */
const LOP_DISPATCHERS = [
  { name: "resolveInputsMembershipStepSemantic", resolver: 7, semantic: 3 },
  { name: "resolveInputsMembershipFinalizeSemantic", resolver: 7, semantic: 4 },
  { name: "scriptSourcesOutputProofStepSemantic", resolver: 8, semantic: 2 },
  {
    name: "scriptSourcesOutputProofFinalizeSemantic",
    resolver: 8,
    semantic: 3,
  },
] as const;

describe("ledger-output-proof publication", () => {
  it("publishes every LOP yield and dispatcher under the real L1 limit", async () => {
    const real = readBlueprint(realBlueprintPath);
    const publisher = generateEmulatorAccount({ lovelace: 40_000_000_000n });
    const emulator = new Emulator([publisher], {
      ...EMULATOR_PROTOCOL_PARAMETERS,
      maxTxSize: PROTOCOL_PARAMETERS_DEFAULT.maxTxSize,
    });
    const lucid = await Lucid(emulator, "Custom");
    lucid.selectWallet.fromSeed(publisher.seedPhrase);
    const nonce = (await lucid.wallet().getUtxos())[0]!;
    const contracts = await buildMinimalFaultProofContracts(
      real,
      readBlueprint(alwaysSucceedsBlueprintPath),
      nonce,
      { realValidationTraceDispute: true, alwaysFraudProofCatalogue: true },
    );
    const family = (
      contracts as typeof contracts & ValidationTraceDisputeFaultProofContracts
    ).validationTraceDispute;
    const yieldSpecs = [
      ...LEDGER_OUTPUT_PROOF_STAGE_YIELD_ROLES,
      LEDGER_OUTPUT_PROOF_ATTESTATION_YIELD_ROLES.scalarInteger,
      LEDGER_OUTPUT_PROOF_ATTESTATION_YIELD_ROLES.scalarBytes,
      ...LEDGER_OUTPUT_DESCRIPTOR_YIELD_ROLES,
    ];
    const rows: VanRossemFitMeasurement[] = [];
    const blueprintBytes = readFileSync(realBlueprintPath);
    const blueprintSha256 = createHash("sha256")
      .update(blueprintBytes)
      .digest("hex");
    for (const spec of yieldSpecs) {
      const contract = family.yields[spec.contract];
      const result = await publishPlainReferenceScriptUtxo({
        lucid,
        script: contract.withdrawalScript,
        label: spec.role,
      });
      const measurement = result.publicationMeasurement;
      rows.push({
        name: spec.deployment,
        maximumShape: "parameterized yield publication",
        kind: "publication",
        signedBytes: measurement.completeSignedBytes,
        memoryUnits: measurement.executionMemory,
        cpuUnits: measurement.executionSteps,
      });
      expect(measurement.l1ByteMargin, spec.deployment).toBeGreaterThanOrEqual(
        512,
      );
    }
    for (const dispatcher of LOP_DISPATCHERS) {
      const contract =
        family.semanticResolvers[
          validationSemanticResolverGlobalIndex(
            dispatcher.resolver,
            dispatcher.semantic,
          )
        ];
      if (contract === undefined) {
        throw new Error(`${dispatcher.name} is not in the resolver roster`);
      }
      const result = await publishPlainReferenceScriptUtxo({
        lucid,
        script: contract.spendingScript,
        label: dispatcher.name,
      });
      const measurement = result.publicationMeasurement;
      rows.push({
        name: dispatcher.name,
        maximumShape: "parameterized dispatcher publication",
        kind: "publication",
        signedBytes: measurement.completeSignedBytes,
        memoryUnits: measurement.executionMemory,
        cpuUnits: measurement.executionSteps,
      });
      expect(measurement.l1ByteMargin, dispatcher.name).toBeGreaterThanOrEqual(
        512,
      );
    }
    expect(rows).toHaveLength(34);
    if (process.env.MIDGARD_WRITE_FIT_LEDGER === "1")
      await writeVanRossemFitLedger(
        fileURLToPath(
          new URL(
            "../../../docs/fault-proofs/size-plans/validation-trace-ledger-output-proof-publication-fit-ledger.json",
            import.meta.url,
          ),
        ),
        buildVanRossemFitLedger({
          category: "validationTraceDispute/ledger-output-proof publication",
          blueprintSha256,
          compilerVersion: JSON.parse(blueprintBytes.toString()).preamble
            .compiler.version,
          measurements: rows,
        }),
      );
  }, 900_000);
});
