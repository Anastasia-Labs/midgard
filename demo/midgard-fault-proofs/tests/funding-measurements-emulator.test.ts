import { createHash } from "node:crypto";
import { mkdirSync, writeFileSync } from "node:fs";

import { canonicalJson } from "@al-ft/midgard-core/canonical-json";
import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  assertCompleteWorkflowFundingMeasurements,
  createWorkflowFundingMeasurementContext,
  measureComputationThreadInitFunding,
  readWorkflowFundingMeasurementContext,
  workflowFundingMeasurementArtifactBytes,
  workflowFundingMeasurementCoverage,
} from "../src/workflow/funding-measurements.js";
import { submitCapturedTransaction } from "../src/workflow/transaction-boundary.js";
import { makeGenuineFundingInitCaptureForTest } from "./support/funding-measurement.js";

describe("genuine workflow funding measurement producer", () => {
  it("measures a real deployed SDK Init and refuses wrong binding and wallet-transfer substitutes", async () => {
    const { fixture, contextInput, transaction } =
      await makeGenuineFundingInitCaptureForTest();
    const { proverLucid, emulator } = fixture;
    const record = contextInput.buildRecord;
    const context = createWorkflowFundingMeasurementContext(contextInput);
    const { identity } = readWorkflowFundingMeasurementContext(context);
    expect(identity).toMatchObject({
      blueprintSha256: createHash("sha256")
        .update(contextInput.blueprintBytes)
        .digest("hex"),
      aikenSourceSha256: record.sourceHash,
      sourceSnapshotSha256: contextInput.sourceSnapshotSha256,
      deploymentProfileDigest: contextInput.manifest.deploymentProfileDigest,
      deploymentParameterRegistryDigest: computeDeploymentManifestJsonDigest(
        contextInput.deploymentParameterRegistry,
      ),
    });
    expect(() =>
      createWorkflowFundingMeasurementContext({
        ...contextInput,
        blueprintBytes: Buffer.from("00", "hex"),
      }),
    ).toThrow("blueprint differs");
    expect(() =>
      createWorkflowFundingMeasurementContext({
        ...contextInput,
        buildRecord: { ...record, sourceHash: "not-a-sha256" },
      }),
    ).toThrow("funding measurement Aiken sources");
    expect(() =>
      createWorkflowFundingMeasurementContext({
        ...contextInput,
        deploymentParameterRegistry: {
          ...contextInput.deploymentParameterRegistry,
          blueprintHash: "00".repeat(32),
        },
      }),
    ).toThrow("parameter registry belongs to another deployment");
    expect(() =>
      createWorkflowFundingMeasurementContext({
        ...contextInput,
        buildRecord: { ...record, profileDigest: "00".repeat(32) },
      }),
    ).toThrow("profile differs");
    expect(() => readWorkflowFundingMeasurementContext({ ...context })).toThrow(
      "not admitted",
    );
    const measure = (
      measuredContext = context,
      category = "doubleSpend" as const,
    ) =>
      measureComputationThreadInitFunding({
        context: measuredContext,
        lucid: proverLucid,
        transaction,
        category,
      });
    const diagnosticDirectory =
      process.env.MIDGARD_FUNDING_MEASUREMENT_DIAGNOSTIC_DIR;
    if (diagnosticDirectory !== undefined) {
      mkdirSync(diagnosticDirectory, { recursive: true, mode: 0o700 });
      const raw = transaction.signed.toCBOR();
      const requestedCanonical = transaction.signed.toCBOR({ canonical: true });
      const roundtrip = CML.Transaction.from_cbor_hex(requestedCanonical);
      writeFileSync(
        `${diagnosticDirectory}/signed-init.json`,
        JSON.stringify({
          raw,
          requestedCanonical,
          canonicalRoundtrip: roundtrip.to_canonical_cbor_hex(),
          bodyOriginal: roundtrip.body().to_cbor_hex(),
          bodyCanonical: roundtrip.body().to_canonical_cbor_hex(),
          actualHash: transaction.txHash,
          canonicalBodyHash: CML.hash_transaction(
            CML.TransactionBody.from_cbor_hex(
              roundtrip.body().to_canonical_cbor_hex(),
            ),
          ).to_hex(),
        }),
        { mode: 0o600 },
      );
    }
    const productionContext = createWorkflowFundingMeasurementContext({
      ...contextInput,
      environment: "local-node",
    });
    await expect(measure(productionContext)).rejects.toThrow(
      "local slot mapping differs",
    );
    const measured = await measure();
    expect(measured.artifact.identity).toEqual(identity);
    expect(measured.artifact.evidence.evaluatorSlotConfig).toEqual(
      proverLucid.config().slotConfig,
    );
    const report = workflowFundingMeasurementCoverage({
      context,
      measurements: [measured],
    });
    expect(report.complete).toBe(false);
    expect(
      report.categories.find(({ category }) => category === "doubleSpend")!
        .missing,
    ).toContain("maximum-reachable-shapes");
    expect(() =>
      assertCompleteWorkflowFundingMeasurements({
        context,
        measurements: [measured],
      }),
    ).toThrow("Incomplete genuine workflow funding coverage");
    expect(() =>
      workflowFundingMeasurementCoverage({
        context,
        measurements: [measured, measured],
      }),
    ).toThrow("duplicate or foreign");
    const protocolParameters = emulator.getProtocolParameters.bind(emulator);
    emulator.getProtocolParameters = async () => ({
      ...(await protocolParameters()),
      coinsPerUtxoByte: (await protocolParameters()).coinsPerUtxoByte + 1n,
    });
    try {
      await expect(measure()).rejects.toThrow(
        "local protocol parameters differ",
      );
    } finally {
      emulator.getProtocolParameters = protocolParameters;
    }
    const resolve = emulator.getUtxosByOutRef.bind(emulator);
    emulator.getUtxosByOutRef = async (refs) =>
      (await resolve(refs)).map((utxo) =>
        utxo.scriptRef == null
          ? utxo
          : { ...utxo, assets: { lovelace: utxo.assets.lovelace! } },
      );
    try {
      await expect(measure()).rejects.toThrow(
        "not the deployed program/outref",
      );
    } finally {
      emulator.getUtxosByOutRef = resolve;
    }
    expect(measured.artifact.status).toBe("partial");
    expect(measured.artifact.missingCoverage).toContain(
      "maximum-reachable-shapes",
    );
    expect(measured.measurementArtifactSha256).toBe(
      createHash("sha256")
        .update(
          canonicalJson(measured.artifact, "workflow funding measurement"),
        )
        .digest("hex"),
    );
    expect(
      createHash("sha256")
        .update(workflowFundingMeasurementArtifactBytes(measured))
        .digest("hex"),
    ).toBe(measured.measurementArtifactSha256);
    expect(() =>
      workflowFundingMeasurementArtifactBytes({ ...measured }),
    ).toThrow("unrecorded or changed");
    expect(measured.normalizedAction.transactionHash).toBe(transaction.txHash);
    expect(measured.artifact.evidence.evaluatedRedeemers).toHaveLength(2);
    expect(measured.normalizedAction.referenceScriptBytes).toBeGreaterThan(0);
    expect(
      measured.normalizedAction.fundingControlledOutputs.filter(
        ({ role }) => role === "locked_reusable",
      ),
    ).toEqual([
      expect.objectContaining({
        semanticRole: "proof_thread",
        custodyRole: "carrier",
        fundingAssets: [],
      }),
    ]);
    const wrongWallet = createWorkflowFundingMeasurementContext({
      ...contextInput,
      fundingPaymentKeyHash: "11".repeat(28),
    });
    await expect(measure(wrongWallet)).rejects.toThrow("valid witness");
    await expect(
      measureComputationThreadInitFunding({
        context,
        lucid: proverLucid,
        transaction,
        category: "invalidRange",
      }),
    ).rejects.toThrow("category/redeemer binding differs");
    const transfer = await proverLucid
      .newTx()
      .pay.ToAddress(await proverLucid.wallet().address(), {
        lovelace: 2_000_000n,
      })
      .complete({ localUPLCEval: true });
    const signed = await transfer.sign.withWallet().complete();
    await expect(
      measureComputationThreadInitFunding({
        context,
        lucid: proverLucid,
        transaction: { txHash: signed.toHash(), signed, referenceScripts: [] },
        category: "doubleSpend",
      }),
    ).rejects.toThrow("wallet transfer");
    expect(await submitCapturedTransaction(transaction)).toBe(
      transaction.txHash,
    );
    expect(await proverLucid.awaitTx(transaction.txHash)).toBe(true);
    const threadOutputs = await proverLucid.utxosByOutRef([
      {
        txHash: transaction.txHash,
        outputIndex: measured.normalizedAction.fundingControlledOutputs.find(
          ({ role }) => role === "locked_reusable",
        )!.outputIndex,
      },
    ]);
    expect(threadOutputs).toHaveLength(1);
    expect(threadOutputs[0]!.assets.lovelace!.toString()).toBe(
      measured.normalizedAction.fundingControlledOutputs.find(
        ({ role }) => role === "locked_reusable",
      )!.fundingLovelace,
    );
  }, 600_000);
});
