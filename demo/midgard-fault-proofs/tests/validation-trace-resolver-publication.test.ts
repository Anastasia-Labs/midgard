/**
 * Publication fit for **every** validation-trace semantic resolver.
 *
 * The resolver proof-fit sweep
 * (`resolver-proof-fit-sweep-generate.test.ts`) measures a resolver only when
 * an existing fixture drives a genuine one-step dispute all the way to it, so
 * 89 of the 91 semantic resolvers are honestly recorded there as `unfit[]`
 * rows with an "no harness-reachable fixture" reason. That is the right answer
 * for *lifecycle* fit — nothing can attest a step nobody can build — but it
 * left **publication** fit unmeasured for those 89 as well, and publication
 * fit does not need a fixture: a reference script publishes the same way
 * whatever the dispute around it looks like.
 *
 * That gap is how two resolvers grew past the reliable publication target
 * without any test noticing. This file closes it permanently: it publishes all
 * 91 applied semantic resolvers as reference scripts through the same helper
 * every other publication-fit suite uses, asserts the real 512-byte reserve on
 * each, and — under `MIDGARD_WRITE_FIT_LEDGER=1` — pins the measurements to
 * `docs/fault-proofs/size-plans/validation-trace-resolver-publication-fit-ledger.json`.
 * `buildVanRossemFitLedger` throws on any row without a positive Van Rossem
 * margin and on any publication row past the 15,872-byte target, so a
 * regression fails a test instead of surviving to delivery.
 */
import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import {
  VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES,
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
  VAN_ROSSEM_PUBLICATION_RESERVE_BYTES,
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

/**
 * Entry names in `semanticResolvers` order: the 90 keys of the `semantics`
 * title table, then the shared stage-one redeemer envelope that
 * `buildValidationTraceDisputeFaultProofContracts` appends as index 90.
 */
const SEMANTIC_RESOLVER_NAMES: readonly string[] = [
  ...Object.keys(VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.semantics),
  "scriptSourcesStageOneRedeemerEnvelope",
];

const ledgerPath = fileURLToPath(
  new URL(
    "../../../docs/fault-proofs/size-plans/validation-trace-resolver-publication-fit-ledger.json",
    import.meta.url,
  ),
);

describe("validation-trace resolver publication", () => {
  it("publishes every semantic resolver under the real L1 limit", async () => {
    const real = readBlueprint(realBlueprintPath);
    const publisher = generateEmulatorAccount({ lovelace: 400_000_000_000n });
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
    expect(SEMANTIC_RESOLVER_NAMES).toHaveLength(91);
    expect(new Set(SEMANTIC_RESOLVER_NAMES).size).toBe(91);
    expect(family.semanticResolvers.length).toBe(
      SEMANTIC_RESOLVER_NAMES.length,
    );

    const rows: VanRossemFitMeasurement[] = [];
    const blueprintBytes = readFileSync(realBlueprintPath);
    const blueprintSha256 = createHash("sha256")
      .update(blueprintBytes)
      .digest("hex");
    for (const [index, name] of SEMANTIC_RESOLVER_NAMES.entries()) {
      const contract = family.semanticResolvers[index];
      if (contract === undefined) {
        throw new Error(
          `semantic resolver ${index.toString()} is not deployed`,
        );
      }
      const result = await publishPlainReferenceScriptUtxo({
        lucid,
        script: contract.spendingScript,
        label: `validation semantic resolver ${name}`,
      });
      const measurement = result.publicationMeasurement;
      rows.push({
        name,
        maximumShape: "parameterized semantic resolver publication",
        kind: "publication",
        signedBytes: measurement.completeSignedBytes,
        memoryUnits: measurement.executionMemory,
        cpuUnits: measurement.executionSteps,
      });
      expect(measurement.l1ByteMargin, name).toBeGreaterThanOrEqual(
        VAN_ROSSEM_PUBLICATION_RESERVE_BYTES,
      );
    }

    const ledger = buildVanRossemFitLedger({
      category: "validationTraceDispute/semantic resolver publication",
      blueprintSha256,
      compilerVersion: JSON.parse(blueprintBytes.toString()).preamble.compiler
        .version,
      measurements: rows,
    });
    if (process.env.MIDGARD_WRITE_FIT_LEDGER === "1") {
      await writeVanRossemFitLedger(ledgerPath, ledger);
    }
  }, 1_800_000);
});
