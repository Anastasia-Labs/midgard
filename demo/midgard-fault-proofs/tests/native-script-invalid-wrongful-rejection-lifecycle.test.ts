import "node:crypto";
import "node:fs/promises";
import "node:url";
import "@aiken-lang/merkle-patricia-forestry";
import "@al-ft/midgard-core";
import "@al-ft/midgard-core/codec/forced";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "vitest";
import "../src/field-opening.js";
import "../src/native-script-invalid/forced-artifact.js";
import "../src/native-script-invalid/submit-cancel.js";
import "../src/native-script-invalid/submit-init.js";
import "../src/native-script-invalid/submit-step-01-forced.js";
import "../src/native-script-invalid/submit-step-02.js";
import "../src/native-script-invalid/submit-step-03.js";
import "../src/native-script-invalid/submit-step-03-staged.js";
import "../src/native-script-invalid/submit-step-04.js";
import "../src/native-script-invalid/submit-step-05.js";
import "../src/native-script-invalid/workflow-artifact.js";
import "../src/proof-fit/van-rossem-fit-ledger.js";
import "../src/remove-fraudulent-block.js";
import "../src/transition-trace/phas.js";
import "../src/transition-trace/reconstruct.js";
import "../src/workflow/complete-replay.js";
import "./support/emulator/blueprints.js";
import "./support/emulator/measurement.js";
import "./support/emulator/native-tx.js";
import "./support/final-catalogue-emulator.js";
import "./support/submit-init-emulator-fixtures.js";
import "./support/submit-init-emulator-shared.js";
import "./support/synthetic-deep-proof.js";
import "./native-script-invalid-wrongful-rejection-lifecycle.fit-rows.js";
import "./native-script-invalid-wrongful-rejection-lifecycle.setup.js";
import "./native-script-invalid-wrongful-rejection-lifecycle.run.js";

import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";

import { encodeMidgardFieldPreimage } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { afterAll, describe, expect, it } from "vitest";

import { admitNativeScriptInvalidForcedArtifact } from "../src/native-script-invalid/forced-artifact.js";
import { submitNativeScriptInvalidStep02 } from "../src/native-script-invalid/submit-step-02.js";
import { admitNativeScriptInvalidWorkflowArtifact } from "../src/native-script-invalid/workflow-artifact.js";
import {
  buildVanRossemFitLedger,
  writeVanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import { fitRows } from "./native-script-invalid-wrongful-rejection-lifecycle.fit-rows.js";
import { run } from "./native-script-invalid-wrongful-rejection-lifecycle.run.js";
import { setup } from "./native-script-invalid-wrongful-rejection-lifecycle.setup.js";
import { realBlueprintPath } from "./support/emulator/blueprints.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";

afterAll(async () => {
  if (process.env.MIDGARD_WRITE_FIT_LEDGER !== "1") return;
  expect(fitRows).toHaveLength(8);
  const blueprint = await readFile(realBlueprintPath);
  const ledger = buildVanRossemFitLedger({
    category: "nativeScriptInvalid",
    blueprintSha256: createHash("sha256").update(blueprint).digest("hex"),
    compilerVersion: JSON.parse(blueprint.toString("utf8")).preamble.compiler
      .version,
    measurements: fitRows.flatMap(({ shape, stages }) =>
      stages.map((row, index) => ({
        name: `${shape}/${index.toString().padStart(3, "0")}`,
        kind:
          row.executionMemory === 0n && row.executionSteps === 0n
            ? ("publication" as const)
            : ("lifecycle" as const),
        maximumShape: shape,
        signedBytes: row.completeSignedBytes,
        memoryUnits: row.executionMemory,
        cpuUnits: row.executionSteps,
      })),
    ),
  });
  await writeVanRossemFitLedger(
    fileURLToPath(
      new URL(
        "../../../docs/fault-proofs/size-plans/native-script-invalid-wrongful-rejection-v1-fit-ledger.json",
        import.meta.url,
      ),
    ),
    ledger,
  );
});

describe("native script wrongful forced rejection", { timeout: 30_000 }, () => {
  it.each([
    { shape: "direct-28-signers", signerCount: 28 },
    { shape: "staged-29-signers", signerCount: 29 },
    {
      shape: "last-raw-field",
      signerCount: 147,
      maximumField: true,
      fieldBytes: 15148,
      prefixCount: 64,
    },
    {
      shape: "first-certified-field",
      signerCount: 148,
      maximumField: true,
      fieldBytes: 15149,
      prefixCount: 64,
    },
    {
      shape: "maximum-fields-and-64-branch-source",
      signerCount: 318,
      maximumField: true,
      depth: 64,
      prefixCount: 64,
    },
    { shape: "maximum-native-depth", signerCount: 318, deepScript: true },
  ])(
    "measures $shape through permanent mint and removal",
    async (options) => {
      const f = await setup(options);
      const measured = await captureEmulatorSubmission(f.h.emulator, () =>
        run(f),
      );
      const stages = [...f.scriptPublications, ...measured.measurements];
      for (const row of stages) {
        expect(row.completeSignedBytes).toBeLessThanOrEqual(15872);
        expect(row.executionMemory).toBeLessThanOrEqual(13200000n);
        expect(row.executionSteps).toBeLessThanOrEqual(8000000000n);
      }
      fitRows.push({
        shape: options.shape,
        scriptIndex: f.scriptIndex,
        scriptFieldBytes: encodeMidgardFieldPreimage(f.scripts).length,
        signerFieldBytes: encodeMidgardFieldPreimage(
          f.witnesses.map((w) => w.item),
        ).length,
        stages,
      });
    },
    600000,
  );
  it("reopens durable evidence and convicts a true script through block removal", async () => {
    const f = await setup();
    const artifact = await f.prepareArtifact();
    const reopened = await admitNativeScriptInvalidForcedArtifact(
      JSON.parse(JSON.stringify(artifact)),
    );
    expect(reopened.evidence.state).toEqual(f.state);
    const workflow = await admitNativeScriptInvalidWorkflowArtifact(
      JSON.parse(JSON.stringify(artifact)),
    );
    expect(workflow.forced?.evidence.state).toEqual(f.state);
    expect(workflow.prepared.txInclusion).toBeUndefined();
    expect(workflow.witnessSetHash).toBe(f.state.bad_tx_witness_set_hash);
    await run(f);
  });
  it.each(["grammar", "signer"] as const)(
    "cancels at %s and restarts from admitted durable evidence",
    async (phase) => {
      const f = await setup({
        signerCount: 318,
        maximumField: true,
        prefixCount: 64,
      });
      const measured = await captureEmulatorSubmission(f.h.emulator, () =>
        run(f, false, phase),
      );
      const stages = [...f.scriptPublications, ...measured.measurements];
      for (const row of stages) {
        expect(row.completeSignedBytes).toBeLessThanOrEqual(15872);
        expect(row.executionMemory).toBeLessThanOrEqual(13200000n);
        expect(row.executionSteps).toBeLessThanOrEqual(8000000000n);
      }
      fitRows.push({ shape: `cancel-and-restart-${phase}`, stages });
    },
    600000,
  );
  it("refuses another exact authenticated rejection reason on chain", async () => {
    const f = await setup({ wrongReason: true });
    const initial = await f.init();
    await expect(
      f.bind(`${initial.txHash}#${initial.firstStepOutputIndex}`),
    ).rejects.toThrow();
  });
  it("rejects substituted source/header and another native script index on chain", async () => {
    const f = await setup({ prefixCount: 1 });
    const initial = await f.init();
    const outRef = `${initial.txHash}#${initial.firstStepOutputIndex}`;
    await expect(
      f.bind(outRef, {
        forcedSource: {
          ...f.source,
          header: {
            ...f.source.header,
            blockSlot: f.source.header.blockSlot + 1n,
          },
        },
      }),
    ).rejects.toThrow();
    await expect(
      f.bind(outRef, { forcedSource: { ...f.source, direction: 0n } }),
    ).rejects.toThrow();
    const bound = await f.bind(outRef);
    await expect(
      submitNativeScriptInvalidStep02({
        ...f.common,
        threadOutRef: bound.nextThreadOutRef,
        scriptWitnessItems: f.scripts,
        scriptIndex: 0n,
        referenceScriptUtxo: f.refs[1]!,
      }),
    ).rejects.toThrow();
  });
  it("rejects durable reason, index, header, source bytes and identity substitutions", async () => {
    const f = await setup({ prefixCount: 1 });
    for (const artifact of [
      { ...f.artifact, forcedSourceCbor: f.artifact.forcedSourceCbor + "00" },
      { ...f.artifact, headerHash: "11".repeat(28) },
      { ...f.artifact, detectionId: f.artifact.detectionId + ":changed" },
      {
        ...f.artifact,
        fullTransactionCbor: "ff" + f.artifact.fullTransactionCbor.slice(2),
      },
      {
        ...f.artifact,
        forcedSourceCbor: Data.to(
          {
            ...f.source,
            membership: {
              ...f.source.membership,
              value: {
                ...f.source.membership.value,
                verdict: { ForcedTxInvalid: { reason: "FeeBelowMinimum" } },
              },
            },
          } as never,
          SDK.NativeScriptInvalidForcedSourcePayloadSchema as never,
        ),
      },
    ])
      await expect(
        admitNativeScriptInvalidForcedArtifact(artifact),
      ).rejects.toThrow();
  });
  it.each([1, 29])(
    "refuses an honest false native script with %s signers on chain",
    async (signerCount) => {
      const f = await setup({ falseScript: true, signerCount });
      await expect(
        admitNativeScriptInvalidForcedArtifact(f.artifact),
      ).rejects.toThrow("no contradiction");
      await expect(run(f, true)).rejects.toThrow(
        /failed|Validation|Script|script/i,
      );
    },
    60_000,
  );
  it.each([1, 29])(
    "refuses a forged signer signature with %s signers on chain",
    async (signerCount) => {
      const f = await setup({ invalidSignature: true, signerCount });
      await expect(
        admitNativeScriptInvalidForcedArtifact(f.artifact),
      ).rejects.toThrow("no contradiction");
      await expect(run(f, true)).rejects.toThrow(
        /failed|Validation|Script|script/i,
      );
    },
    60_000,
  );
});
