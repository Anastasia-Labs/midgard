import "./canonical-evidence-source.q03-canonical-block-evidence.js";

import { describe, expect, it } from "vitest";

import {
  blockTransactionsFromCanonicalEvidence,
  type CanonicalBlockEvidence,
  executeCanonicalPrepareCommand,
  prepareDoubleSpendFromCanonicalEvidence,
  prepareInvalidRangeFromCanonicalEvidence,
  prepareMinFeeFromCanonicalEvidence,
  prepareNonExistentInputFromCanonicalEvidence,
  prepareZeroInputFromCanonicalEvidence,
} from "../src/evidence/index.js";
import { decodeTransactionMaterial } from "../src/prepare-double-spend.js";
import {
  doubleSpendBlock,
  evidenceFor,
  rejectionCode,
  validBlock,
} from "./canonical-evidence-source.q03-provenance-admission.js";
import {
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  h32,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture.js";

describe("Q03 canonical-evidence builders", () => {
  it("exposes authenticated transaction material for detection", async () => {
    const fixture = await doubleSpendBlock();
    const evidence = await evidenceFor(fixture);
    expect(blockTransactionsFromCanonicalEvidence(evidence)).toHaveLength(3);
  });

  it("refuses to detect from evidence whose DA record was downgraded to diagnostic", async () => {
    const fixture = await doubleSpendBlock();
    const evidence = await evidenceFor(fixture);
    const downgraded: CanonicalBlockEvidence = {
      ...evidence,
      provenance: {
        ...evidence.provenance,
        da: {
          trustClass: "operator_only_diagnostic_endpoint",
          sourceId: "operator-diagnostics",
          grade: "diagnostic",
          diagnosticLabel: "operator diagnostics endpoint",
        },
      },
    };
    expect(() => blockTransactionsFromCanonicalEvidence(downgraded)).toThrow(
      /prohibited_trust_class/u,
    );
  });

  it("refuses every builder when transaction-source inclusion is not authenticated", async () => {
    const fixture = await doubleSpendBlock();
    const admitted = await evidenceFor(fixture);
    const evidence: CanonicalBlockEvidence = {
      ...admitted,
      inclusionRootAuthentication: {
        ...admitted.inclusionRootAuthentication,
        sourceValueCountedRoot: h32(0xff),
        sourceInclusionAuthenticated: false,
      },
    };
    for (const build of [
      async () => prepareDoubleSpendFromCanonicalEvidence({ evidence }),
      async () => prepareZeroInputFromCanonicalEvidence({ evidence }),
      async () => prepareInvalidRangeFromCanonicalEvidence({ evidence }),
      async () => prepareMinFeeFromCanonicalEvidence({ evidence }),
    ]) {
      expect(await rejectionCode(build)).toBe(
        "transaction_source_inclusion_root_unauthenticated",
      );
    }
  });

  it("emits each family artifact from source-root-authenticated canonical evidence", async () => {
    const doubleSpend = await evidenceFor(await doubleSpendBlock());
    expect(
      (
        await executeCanonicalPrepareCommand({
          request: { command: "prepare-double-spend" },
          evidence: doubleSpend,
        })
      ).txCount,
    ).toBe(3);

    const zeroInputFixture = await buildCanonicalBlockFixture({
      transactions: [buildFixtureTransaction({ spendInputs: [], fee: 1n })],
    });
    expect(
      (
        await executeCanonicalPrepareCommand({
          request: { command: "prepare-zero-input" },
          evidence: await evidenceFor(zeroInputFixture),
        })
      ).txCount,
    ).toBe(1);

    const minFeeFixture = await buildCanonicalBlockFixture({
      minFeeB: 2n,
      transactions: [
        buildFixtureTransaction({
          spendInputs: [outRefCbor(0x79, 0n)],
          fee: 1n,
        }),
      ],
    });
    const minFee = await executeCanonicalPrepareCommand({
      request: {
        command: "prepare-min-fee",
        categoryId: "00000013",
      },
      evidence: await evidenceFor(minFeeFixture),
    });
    if (!("threadTokenAssetName" in minFee) || !("tx" in minFee)) {
      throw new Error("prepare-min-fee router returned a different family");
    }
    expect(minFee.tx.minimumFee).toBe(2n);
    expect(minFee.threadTokenAssetName).toBe(
      `00000013${minFeeFixture.headerHash}`,
    );

    const invalidRangeFixture = await buildCanonicalBlockFixture({
      startTime: 10n,
      endTime: 20n,
      transactions: [
        buildFixtureTransaction({
          spendInputs: [outRefCbor(0x77, 0n)],
          fee: 1n,
          validityIntervalStart: 30n,
          validityIntervalEnd: 40n,
        }),
      ],
    });
    expect(
      (
        await executeCanonicalPrepareCommand({
          request: { command: "prepare-invalid-range" },
          evidence: await evidenceFor(invalidRangeFixture),
        })
      ).txCount,
    ).toBe(1);

    const nonExistentInputFixture = await buildCanonicalBlockFixture({
      transactions: [
        buildFixtureTransaction({
          spendInputs: [outRefCbor(0x88, 0n)],
          fee: 1n,
        }),
      ],
    });
    expect(
      (
        await executeCanonicalPrepareCommand({
          request: { command: "prepare-non-existent-input" },
          evidence: await evidenceFor(nonExistentInputFixture),
        })
      ).txCount,
    ).toBe(1);
  });

  it("rejects diagnostic grade before every gated builder can emit proof material", async () => {
    const fixture = await doubleSpendBlock();
    const evidence = await evidenceFor(fixture);
    const downgraded: CanonicalBlockEvidence = {
      ...evidence,
      provenance: {
        ...evidence.provenance,
        da: {
          trustClass: "operator_admin_api",
          sourceId: "midgard-node-url",
          grade: "diagnostic",
          diagnosticLabel: "operator REST diagnostic",
        },
      },
    };
    const builders = [
      async () =>
        await prepareDoubleSpendFromCanonicalEvidence({
          evidence: downgraded,
        }),
      async () =>
        await prepareZeroInputFromCanonicalEvidence({ evidence: downgraded }),
      async () =>
        await prepareInvalidRangeFromCanonicalEvidence({
          evidence: downgraded,
        }),
      async () =>
        await prepareMinFeeFromCanonicalEvidence({ evidence: downgraded }),
      async () =>
        await prepareNonExistentInputFromCanonicalEvidence({
          evidence: downgraded,
        }),
    ];
    for (const build of builders) {
      expect(await rejectionCode(build)).toBe("prohibited_trust_class");
    }
  });

  it("applies the provenance gate before the inclusion gate", async () => {
    const fixture = await doubleSpendBlock();
    const evidence = await evidenceFor(fixture);
    const downgraded: CanonicalBlockEvidence = {
      ...evidence,
      provenance: {
        ...evidence.provenance,
        l1: {
          trustClass: "operator_private_file",
          sourceId: "snapshot.json",
          grade: "diagnostic",
          diagnosticLabel: "operator snapshot",
        },
      },
    };
    expect(
      await rejectionCode(async () =>
        prepareDoubleSpendFromCanonicalEvidence({ evidence: downgraded }),
      ),
    ).toBe("prohibited_trust_class");
  });

  it("valid-block control: a block with no double spend yields no proof", async () => {
    const fixture = await validBlock();
    const evidence = await evidenceFor(fixture);
    expect(
      evidence.inclusionRootAuthentication.sourceInclusionAuthenticated,
    ).toBe(true);
    expect(fixture.header.transactionsRoot).toBe(
      fixture.payloadSourceTransactionsRoot,
    );
    const decoded = await Promise.all(
      evidence.transactions.map(decodeTransactionMaterial),
    );
    const inputs = decoded.flatMap((transaction) =>
      transaction.inputs.map(
        (input) => `${input.transactionId}#${input.outputIndex.toString()}`,
      ),
    );
    expect(new Set(inputs).size).toBe(inputs.length);
    await expect(
      prepareDoubleSpendFromCanonicalEvidence({ evidence }),
    ).rejects.toThrow("No double spend found in the selected block.");
  });
});
