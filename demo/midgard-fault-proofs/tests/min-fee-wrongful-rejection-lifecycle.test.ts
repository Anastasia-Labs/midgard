import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";

import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { afterAll, describe, expect, it } from "vitest";

import { admitMinFeeForcedArtifact } from "../src/min-fee-forced-artifact.js";
import {
  buildVanRossemFitLedger,
  type VanRossemFitMeasurement,
  writeVanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import { realBlueprintPath } from "./support/emulator/blueprints.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { makeMinFeeWrongfulRejectionScenario as setup } from "./support/min-fee-wrongful-emulator.js";
import {
  buildRemovalDeploymentInfo,
  network,
  publishRemovalReferenceScripts,
} from "./support/submit-init-emulator-shared.js";

const fitRows: VanRossemFitMeasurement[] = [];
afterAll(async () => {
  if (process.env.MIDGARD_WRITE_FIT_LEDGER !== "1") return;
  expect(new Set(fitRows.map((row) => row.maximumShape)).size).toBe(6);
  const blueprintBytes = await readFile(realBlueprintPath);
  const ledger = buildVanRossemFitLedger({
    category: "minFee",
    blueprintSha256: createHash("sha256").update(blueprintBytes).digest("hex"),
    compilerVersion: JSON.parse(blueprintBytes.toString()).preamble.compiler
      .version,
    measurements: fitRows,
  });
  await writeVanRossemFitLedger(
    fileURLToPath(
      new URL(
        "../../../docs/fault-proofs/size-plans/min-fee-wrongful-rejection-v1-fit-ledger.json",
        import.meta.url,
      ),
    ),
    ledger,
  );
});

describe("minFee wrongful rejection registered lifecycle", () => {
  it.each([999n, 1_000n])(
    "contradicts fee rejection at minimum %s through permanent proof and removal",
    async (minimum) => {
      const s = await setup(minimum);
      const initial = await s.init();
      await expect(
        s.bind(initial.nextThreadOutRef, {
          state: { ...s.state, min_fee_b: minimum + 1n },
        }),
      ).rejects.toThrow();
      await expect(
        s.bind(initial.nextThreadOutRef, {
          forcedSource: { ...s.forcedSource, direction: 0n },
        }),
      ).rejects.toThrow();
      if (minimum === 1_000n) {
        const source = s.forcedSource;
        for (const forcedSource of [
          {
            ...source,
            header: {
              ...source.header,
              blockSlot: source.header.blockSlot + 1n,
            },
          },
          {
            ...source,
            membership: {
              ...source.membership,
              count: source.membership.count + 1n,
            },
          },
          {
            ...source,
            membership: { ...source.membership, phas_root: "ff".repeat(32) },
          },
          {
            ...source,
            membership: {
              ...source.membership,
              key: {
                ...source.membership.key,
                outputIndex: source.membership.key.outputIndex + 1n,
              },
            },
          },
          {
            ...source,
            membership: {
              ...source.membership,
              value: { ...source.membership.value, tx_id: "ff".repeat(32) },
            },
          },
          {
            ...source,
            membership: {
              ...source.membership,
              value: {
                ...source.membership.value,
                submitted_source: {
                  ...source.membership.value.submitted_source,
                  field_preimage_lengths_cbor: "80",
                },
              },
            },
          },
        ])
          await expect(
            s.bind(initial.nextThreadOutRef, { forcedSource }),
          ).rejects.toThrow();
      }
      const bound = await s.bind(initial.nextThreadOutRef);
      const completed = await captureEmulatorSubmission(s.h.emulator, () =>
        s.finish(bound.nextThreadOutRef),
      );
      expect(completed.result.minimumFee).toBe(minimum);
      const publications = await publishRemovalReferenceScripts({
        lucid: s.h.proverLucid,
        contracts: s.h.contracts,
      });
      const now = BigInt(s.h.emulator.now());
      await submitRemoveFraudulentBlock({
        lucid: s.h.proverLucid,
        blueprint: s.h.realBlueprint,
        deploymentInfo: buildRemovalDeploymentInfo(
          s.h.contracts,
          s.h.catalogue,
          { removalReferenceScripts: publications.published },
        ),
        network,
        signer: s.h.proverSigner,
        fraudCategory: "minFee",
        fraudulentHeaderHash: s.seeded.headerHash,
        requireReferenceScripts: true,
        validFrom: now - 120_000n,
        validTo: now + 300_000n,
      });
      expect(
        await s.h.proverLucid.utxosAtWithUnit(
          s.h.contracts.stateQueue.spendingScriptAddress,
          s.seeded.stateQueueBlockUnit,
        ),
      ).toHaveLength(0);
      expect(
        await s.h.proverLucid.utxosAtWithUnit(
          s.contracts.fraudProof.spendingScriptAddress,
          completed.result.fraudProofUnit,
        ),
      ).toHaveLength(1);
    },
    600_000,
  );

  it.each([358, 378, 379, 819, -80, -64])(
    "measures forced field boundary %s",
    async (inputCount) => {
      const s = await setup(
        1_000n,
        inputCount === -64 ? 819 : Math.abs(inputCount),
        inputCount === -80,
        inputCount === -64 ? 64 : 0,
      );
      const init = await captureEmulatorSubmission(s.h.emulator, s.init);
      const bind = await captureEmulatorSubmission(s.h.emulator, () =>
        s.bind(init.result.nextThreadOutRef),
      );
      const publication = await captureEmulatorSubmission(
        s.h.emulator,
        s.prepareCarriages,
      );
      const finished = await captureEmulatorSubmission(s.h.emulator, () =>
        s.finish(bind.result.nextThreadOutRef),
      );
      const removal = await s.remove();
      const stages = [
        ...s.scriptPublications,
        ...init.measurements,
        ...bind.measurements,
        ...publication.measurements,
        ...finished.measurements,
        ...removal,
      ];
      for (const row of stages) {
        expect(row.completeSignedBytes).toBeLessThanOrEqual(15_872);
        expect(row.executionMemory).toBeLessThanOrEqual(13_200_000n);
        expect(row.executionSteps).toBeLessThanOrEqual(8_000_000_000n);
      }
      const shape =
        inputCount === -64
          ? "maximum-proof-64-and-certified-field"
          : inputCount === -80
            ? "all-nine-populated"
            : `field0-${inputCount}`;
      const groups = [
        {
          name: "script-publication",
          kind: "publication" as const,
          rows: s.scriptPublications,
        },
        { name: "init", kind: "lifecycle" as const, rows: init.measurements },
        { name: "bind", kind: "lifecycle" as const, rows: bind.measurements },
        {
          name: "field-publication",
          kind: "publication" as const,
          rows: publication.measurements,
        },
        {
          name: "final",
          kind: "lifecycle" as const,
          rows: finished.measurements,
        },
        {
          name: "removal-reference",
          kind: "publication" as const,
          rows: removal.slice(0, -1),
        },
        {
          name: "removal",
          kind: "lifecycle" as const,
          rows: removal.slice(-1),
        },
      ];
      for (const group of groups)
        group.rows.forEach((row, index) =>
          fitRows.push({
            name: `${shape}-${group.name}-${index}`,
            kind: group.kind,
            maximumShape: shape,
            signedBytes: row.completeSignedBytes,
            memoryUnits: row.executionMemory,
            cpuUnits: row.executionSteps,
          }),
        );
    },
    600_000,
  );

  it("proves the exact nonzero-slope fee boundary through all nine authenticated lengths", async () => {
    const s = await setup(1_000n, 1, false, 0, true);
    const initial = await s.init();
    const bound = await s.bind(initial.nextThreadOutRef);
    const result = await s.finish(bound.nextThreadOutRef);
    expect(result.minimumFee).toBe(1_000n);
    expect(result.canonicalTxSize + s.state.min_fee_b).toBe(1_000n);
    await s.remove();
  }, 600_000);

  it("reopens serialized forced evidence on restart and refuses every source substitution", async () => {
    const s = await setup(1_000n);
    const prepared = await s.prepareArtifact();
    expect(prepared).toEqual(s.artifact);
    const stored = JSON.parse(JSON.stringify(prepared));
    const admitted = await admitMinFeeForcedArtifact(stored);
    expect(admitted.evidence.state).toEqual(s.state);
    for (const mutation of [
      { ...stored, headerHash: "ff".repeat(28) },
      { ...stored, detectionId: "min-fee:forced:1:other" },
      { ...stored, fullTransactionCbor: stored.fullTransactionCbor + "00" },
      { ...stored, forcedSourceCbor: stored.forcedSourceCbor + "00" },
      { ...stored, unexpected: true },
    ])
      await expect(admitMinFeeForcedArtifact(mutation)).rejects.toThrow();
    const initial = await s.init();
    const bound = await s.bind(initial.nextThreadOutRef, {
      state: admitted.evidence.state,
      forcedSource: admitted.forcedSource,
    });
    // A fresh admission re-derives the exact same checkpoint after the bind.
    const resumed = await admitMinFeeForcedArtifact(
      JSON.parse(JSON.stringify(stored)),
    );
    const [utxo] = await s.h.proverLucid.utxosByOutRef([
      {
        txHash: bound.nextThreadOutRef.split("#")[0]!,
        outputIndex: Number(bound.nextThreadOutRef.split("#")[1]),
      },
    ]);
    expect(Data.from(utxo!.datum!, SDK.MinFeeStep02Datum).data).toEqual(
      resumed.evidence.state,
    );
    await s.finish(bound.nextThreadOutRef);
    await s.remove();
  }, 600_000);

  it("refuses the honest one-lovelace-below rejection on chain and cancels at both steps", async () => {
    const s = await setup(1_001n);
    const first = await s.init();
    await s.cancel(first.nextThreadOutRef, 0);
    const initial = await s.init();
    const bound = await s.bind(initial.nextThreadOutRef);
    await expect(s.finish(bound.nextThreadOutRef, true)).rejects.toThrow();
    expect(
      await s.h.proverLucid.utxosAtWithUnit(
        s.h.contracts.stateQueue.spendingScriptAddress,
        s.seeded.stateQueueBlockUnit,
      ),
    ).toHaveLength(1);
    await s.cancel(bound.nextThreadOutRef, 1);
  }, 600_000);
});
