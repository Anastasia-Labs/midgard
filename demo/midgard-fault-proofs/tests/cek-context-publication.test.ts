import { createHash } from "node:crypto";
import { appendFileSync, readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import {
  cekContextReferenceScripts,
  type ValidationTraceDisputeFaultProofContracts,
} from "@al-ft/midgard-sdk";
import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
  PROTOCOL_PARAMETERS_DEFAULT,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  buildVanRossemFitLedger,
  type VanRossemFitMeasurement,
  writeVanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import {
  alwaysSucceedsBlueprintPath,
  buildMinimalFaultProofContracts,
  EMULATOR_PROTOCOL_PARAMETERS,
  publishPlainReferenceScriptUtxo,
  readBlueprint,
  realBlueprintPath,
} from "./support/submit-init-emulator-shared.js";

describe("CEK context tail publication", () => {
  it("publishes each parameterized continuation under the real L1 limit", async () => {
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
    const publications = cekContextReferenceScripts(
      family.cekContextStages,
      family.cekContextItemStages,
    );
    const rows: VanRossemFitMeasurement[] = [];
    const blueprintBytes = readFileSync(realBlueprintPath);
    const blueprintSha256 = createHash("sha256")
      .update(blueprintBytes)
      .digest("hex");
    for (const { deploymentEntry: key, validator: contract } of publications) {
      const result = await publishPlainReferenceScriptUtxo({
        lucid,
        script: contract.spendingScript,
        label: `CEK context ${key}`,
      });
      const measurement = result.publicationMeasurement;
      if (process.env.MIDGARD_WRITE_FIT_LEDGER === "1")
        appendFileSync(
          `/tmp/midgard-cek-context-publication-raw-${blueprintSha256}.jsonl`,
          JSON.stringify(
            { blueprintSha256, key, ...measurement },
            (_key, value) =>
              typeof value === "bigint" ? value.toString() : value,
          ) + "\n",
        );
      rows.push({
        name: key,
        maximumShape: "parameterized context publication",
        kind: "publication",
        signedBytes: measurement.completeSignedBytes,
        memoryUnits: measurement.executionMemory,
        cpuUnits: measurement.executionSteps,
      });
      expect(measurement.l1ByteMargin, key).toBeGreaterThanOrEqual(512);
    }
    if (process.env.MIDGARD_WRITE_FIT_LEDGER === "1")
      await writeVanRossemFitLedger(
        fileURLToPath(
          new URL(
            "../../../docs/fault-proofs/size-plans/validation-trace-cek-context-publication-fit-ledger.json",
            import.meta.url,
          ),
        ),
        buildVanRossemFitLedger({
          category: "validationTraceDispute/CEK context publication",
          blueprintSha256,
          compilerVersion: JSON.parse(blueprintBytes.toString()).preamble
            .compiler.version,
          measurements: rows,
        }),
      );
  }, 900_000);
});
