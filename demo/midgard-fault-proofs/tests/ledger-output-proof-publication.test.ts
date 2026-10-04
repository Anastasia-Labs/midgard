/**
 * Publication fit for the shared ledger-output-proof family: every one of
 * the thirty LOP yields (twenty-four stage yields, two attestation
 * yields, four descriptor yields), the four step/finalize dispatchers, and
 * the shared redeemer item normalizers, authenticator and full executor roster
 * publishes as a reference script under the real 16,384-byte L1 envelope
 * with margin. With `MIDGARD_WRITE_FIT_LEDGER=1` the measurements are pinned
 * to `docs/fault-proofs/size-plans/validation-trace-ledger-output-proof-publication-fit-ledger.json`.
 */
import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import {
  completeReferenceScriptPublicationTxProgram,
  createReferenceScriptAuthPolicy,
  type ValidationTraceDisputeFaultProofContracts,
  REDEEMER_ITEM_EXECUTOR_REFERENCES,
  sharedRedeemerItemReferenceScripts,
} from "@al-ft/midgard-sdk";
import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
  PROTOCOL_PARAMETERS_DEFAULT,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
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
  measureCompleteSignedTransaction,
  readBlueprint,
  realBlueprintPath,
} from "./support/submit-init-emulator-shared.js";

/** The step/finalize dispatchers of both shared-LOP resolver pairs. */
const LOP_DISPATCHERS = [
  {
    name: "resolveInputsMembershipStepSemantic",
    role: "V1 validation-trace resolve-inputs MembershipStep semantic",
    resolver: 7,
    semantic: 3,
  },
  {
    name: "resolveInputsMembershipFinalizeSemantic",
    role: "V1 validation-trace resolve-inputs MembershipFinalize semantic",
    resolver: 7,
    semantic: 4,
  },
  {
    name: "scriptSourcesOutputProofStepSemantic",
    role: "V1 validation-trace script-sources OutputProofStep semantic",
    resolver: 8,
    semantic: 2,
  },
  {
    name: "scriptSourcesOutputProofFinalizeSemantic",
    role: "V1 validation-trace script-sources OutputProofFinalize semantic",
    resolver: 8,
    semantic: 3,
  },
] as const;

describe("ledger-output-proof publication", () => {
  it("publishes every LOP yield, dispatcher and shared executor under the real L1 limit", async () => {
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
    const shared = family.scriptSourcesStageOneRedeemerStages;
    expect(shared.executors).toHaveLength(
      REDEEMER_ITEM_EXECUTOR_REFERENCES.length,
    );
    const sharedReferences = sharedRedeemerItemReferenceScripts(shared);
    expect(sharedReferences).toHaveLength(23);
    const authPolicy = await createReferenceScriptAuthPolicy(
      lucid,
      emulator.now(),
    );
    const publishAuthenticated = async (
      script: Parameters<
        typeof completeReferenceScriptPublicationTxProgram
      >[0]["missingTargets"][number]["script"],
      label: string,
    ) => {
      const walletAddress = await lucid.wallet().address();
      const built = await Effect.runPromise(
        completeReferenceScriptPublicationTxProgram({
          lucid,
          // The production publisher selects funding separately from existing
          // authenticated reference outputs; do not consume prior publications.
          selectedFundingInputs: (await lucid.wallet().getUtxos())
            .filter(
              (utxo) =>
                utxo.scriptRef === undefined &&
                Object.keys(utxo.assets).length === 1,
            )
            .sort((left, right) =>
              left.assets.lovelace > right.assets.lovelace ? -1 : 1,
            )
            .slice(0, 1),
          walletAddress,
          referenceScriptsAddress: walletAddress,
          missingTargets: [{ name: label, script }],
          authPolicy,
        }),
      );
      const signed = await built.tx.sign.withWallet().complete();
      const publicationMeasurement = measureCompleteSignedTransaction(
        signed.toCBOR(),
      );
      console.info(
        `${label}: authenticated signed publication ${publicationMeasurement.completeSignedBytes} bytes`,
      );
      expect(publicationMeasurement.nativeScriptCount).toBe(1);
      await lucid.awaitTx(await signed.submit());
      return { publicationMeasurement };
    };
    const rows: VanRossemFitMeasurement[] = [];
    const blueprintBytes = readFileSync(realBlueprintPath);
    const blueprintSha256 = createHash("sha256")
      .update(blueprintBytes)
      .digest("hex");
    for (const spec of yieldSpecs) {
      const contract = family.yields[spec.contract];
      const result = await publishAuthenticated(
        contract.withdrawalScript,
        spec.role,
      );
      const measurement = result.publicationMeasurement;
      rows.push({
        name: spec.deployment,
        maximumShape: "parameterized authenticated yield publication",
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
      const result = await publishAuthenticated(
        contract.spendingScript,
        dispatcher.role,
      );
      const measurement = result.publicationMeasurement;
      rows.push({
        name: dispatcher.name,
        maximumShape: "parameterized authenticated dispatcher publication",
        kind: "publication",
        signedBytes: measurement.completeSignedBytes,
        memoryUnits: measurement.executionMemory,
        cpuUnits: measurement.executionSteps,
      });
      expect(measurement.l1ByteMargin, dispatcher.name).toBeGreaterThanOrEqual(
        512,
      );
    }
    for (const spec of sharedReferences) {
      const result = await publishAuthenticated(
        spec.validator.spendingScript,
        spec.role,
      );
      const measurement = result.publicationMeasurement;
      rows.push({
        name: spec.deploymentEntry,
        maximumShape: "parameterized authenticated shared executor publication",
        kind: "publication",
        signedBytes: measurement.completeSignedBytes,
        memoryUnits: measurement.executionMemory,
        cpuUnits: measurement.executionSteps,
      });
      expect(
        measurement.l1ByteMargin,
        spec.deploymentEntry,
      ).toBeGreaterThanOrEqual(512);
    }
    expect(rows).toHaveLength(57);
    expect(new Set(rows.map(({ name }) => name)).size).toBe(rows.length);
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
