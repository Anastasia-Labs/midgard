import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import { createReferenceScriptAuthPolicy } from "@al-ft/midgard-sdk";
import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  buildVanRossemFitLedger,
  VAN_ROSSEM_PUBLICATION_TARGET_BYTES,
  type VanRossemFitMeasurement,
  writeVanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import {
  alwaysSucceedsBlueprintPath,
  readBlueprint,
  realBlueprintPath,
} from "./support/emulator/blueprints.js";
import { buildMinimalFaultProofContracts } from "./support/emulator/contracts.js";
import { EMULATOR_PROTOCOL_PARAMETERS } from "./support/emulator/protocol-parameters.js";
import {
  publishPlainReferenceScriptUtxo,
  publishStateQueueYieldReferenceScript,
} from "./support/emulator/reference-scripts.js";

const ledgerPath = fileURLToPath(
  new URL(
    "../../../docs/fault-proofs/size-plans/state-queue-publication-fit-ledger.json",
    import.meta.url,
  ),
);

describe("state-queue withdraw-zero publication admission V1", () => {
  it("publishes the mint policy and every arm-specific rewarding script under Van Rossem limits", async () => {
    const account = generateEmulatorAccount({ lovelace: 40_000_000_000n });
    const emulator = new Emulator([account], EMULATOR_PROTOCOL_PARAMETERS);
    const lucid = await Lucid(emulator, "Custom");
    lucid.selectWallet.fromSeed(account.seedPhrase);
    const nonceUtxo = (await lucid.wallet().getUtxos())[0];
    if (nonceUtxo === undefined) throw new Error("missing publication nonce");

    const referenceScriptAuth = createReferenceScriptAuthPolicy(
      lucid,
      emulator.now(),
    );
    const contracts = {
      ...(await buildMinimalFaultProofContracts(
        readBlueprint(realBlueprintPath),
        readBlueprint(alwaysSucceedsBlueprintPath),
        nonceUtxo,
        { referenceScriptAuthPolicyId: referenceScriptAuth.policyId },
      )),
      referenceScriptAuth,
    };

    const measurements: VanRossemFitMeasurement[] = [];
    const mint = await publishPlainReferenceScriptUtxo({
      lucid,
      script: contracts.stateQueue.mintingScript,
      label: "state-queue mint publication admission",
    });
    const record = (
      name: string,
      measurement: typeof mint.publicationMeasurement,
    ) => {
      expect(measurement.completeSignedBytes, name).toBeLessThanOrEqual(
        VAN_ROSSEM_PUBLICATION_TARGET_BYTES,
      );
      measurements.push({
        name,
        maximumShape: "fully applied reference-script publication",
        kind: "publication",
        signedBytes: measurement.completeSignedBytes,
        memoryUnits: measurement.executionMemory,
        cpuUnits: measurement.executionSteps,
      });
    };
    record("mint", mint.publicationMeasurement);
    for (const arm of [
      "commit",
      "unattestedTimeout",
      "unavailableTimeout",
      "fraudRemoval",
      "merge",
    ] as const) {
      const publication = await publishStateQueueYieldReferenceScript({
        lucid,
        contracts,
        arm,
      });
      record(arm, publication.publicationMeasurement);
    }
    // Every publication the admission claim covers was actually measured: the
    // mint policy plus one rewarding script per state-queue arm, no arm
    // silently absent from the loop above.
    expect(measurements.map((measurement) => measurement.name)).toEqual([
      "mint",
      "commit",
      "unattestedTimeout",
      "unavailableTimeout",
      "fraudRemoval",
      "merge",
    ]);
    const blueprintBytes = readFileSync(realBlueprintPath);
    const ledger = buildVanRossemFitLedger({
      category: "stateQueue/publication",
      blueprintSha256: createHash("sha256")
        .update(blueprintBytes)
        .digest("hex"),
      compilerVersion: JSON.parse(blueprintBytes.toString()).preamble.compiler
        .version,
      measurements,
    });
    if (process.env.MIDGARD_WRITE_FIT_LEDGER === "1") {
      await writeVanRossemFitLedger(ledgerPath, ledger);
    }
  });
});
