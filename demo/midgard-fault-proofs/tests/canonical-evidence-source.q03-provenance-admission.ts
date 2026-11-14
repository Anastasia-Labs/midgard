import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  type CanonicalBlockEvidence,
  canonicalBlockEvidenceFromVerifiedPayload,
} from "../src/evidence/index.js";
import * as FaultProofs from "../src/index.js";
import type {
  RetainedDaPayloadSource,
  RetainedDaPayloadSourceResult,
} from "../src/transition-trace/fetch.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  type CanonicalBlockFixture,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture.js";

export const DA_PROVENANCE: SDK.EvidenceProvenance = {
  trustClass: "public_or_permissionless_da",
  sourceId: "libp2p/peer-a",
  grade: "security",
};

const sharedInput = outRefCbor(0x11, 7n);

export const doubleSpendBlock = async (): Promise<CanonicalBlockFixture> =>
  await buildCanonicalBlockFixture({
    transactions: [
      buildFixtureTransaction({
        spendInputs: [sharedInput, outRefCbor(0x22, 0n)],
        fee: 1n,
      }),
      buildFixtureTransaction({
        spendInputs: [outRefCbor(0x33, 0n), sharedInput],
        fee: 2n,
      }),
      buildFixtureTransaction({
        spendInputs: [outRefCbor(0x44, 0n)],
        fee: 3n,
      }),
    ],
  });

export const validBlock = async (): Promise<CanonicalBlockFixture> =>
  await buildCanonicalBlockFixture({
    transactions: [
      buildFixtureTransaction({
        spendInputs: [outRefCbor(0x55, 0n)],
        fee: 1n,
      }),
      buildFixtureTransaction({
        spendInputs: [outRefCbor(0x66, 1n)],
        fee: 2n,
      }),
    ],
  });

export const evidenceFor = async (
  fixture: CanonicalBlockFixture,
): Promise<CanonicalBlockEvidence> =>
  await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation(fixture),
    payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
    daProvenance: DA_PROVENANCE,
  });

export class StubDaSource implements RetainedDaPayloadSource {
  readonly sourceId: string;
  private readonly payloadEnvelopeCbor: Buffer | undefined;

  constructor(sourceId: string, payloadEnvelopeCbor?: Buffer) {
    this.sourceId = sourceId;
    this.payloadEnvelopeCbor = payloadEnvelopeCbor;
  }

  fetchPayloadByHeaderHash(): Promise<RetainedDaPayloadSourceResult> {
    if (this.payloadEnvelopeCbor === undefined) {
      return Promise.resolve({
        ok: false,
        sourceId: this.sourceId,
        attempts: [],
      });
    }
    return Promise.resolve({
      ok: true,
      provenance: SDK.assertSecurityGradeEvidence({
        trustClass: "public_or_permissionless_da",
        sourceId: `${this.sourceId}/peer-a`,
        grade: "security",
      }),
      sourceId: this.sourceId,
      sourcePeerId: "peer-a",
      payloadEnvelopeCbor: this.payloadEnvelopeCbor,
      attempts: [],
    });
  }
}

export const rejectionCode = async (
  run: () => Promise<unknown>,
): Promise<string> => {
  try {
    await run();
  } catch (error) {
    if (error instanceof SDK.CanonicalEvidenceRejection) {
      return error.code;
    }
    return `unexpected:${error instanceof Error ? error.message : String(error)}`;
  }
  return "no_rejection";
};

describe("Q03 provenance admission", () => {
  it("exports the W20 evidence API from both package roots", () => {
    expect(SDK.admitEvidenceProvenance).toBeTypeOf("function");
    expect(SDK.assertSecurityGradeEvidence).toBeTypeOf("function");
    expect(SDK.admitAuthenticatedL1Observation).toBeTypeOf("function");
    expect(FaultProofs.fetchCanonicalBlockEvidence).toBeTypeOf("function");
    expect(FaultProofs.executeCanonicalPrepareCommand).toBeTypeOf("function");
    expect(FaultProofs.prepareNonExistentInputFromCanonicalEvidence).toBeTypeOf(
      "function",
    );
    expect(FaultProofs.prepareMinFeeFromCanonicalEvidence).toBeTypeOf(
      "function",
    );
  });

  it("admits every enumerated public trust class at security grade", () => {
    for (const trustClass of SDK.ADMITTED_EVIDENCE_TRUST_CLASSES) {
      const admitted = SDK.assertSecurityGradeEvidence({
        trustClass,
        sourceId: "source",
        grade: "security",
      });
      expect(admitted.grade).toBe("security");
      expect(admitted.trustClass).toBe(trustClass);
    }
  });

  it("rejects every operator-private class as a security input", () => {
    for (const trustClass of SDK.PROHIBITED_EVIDENCE_TRUST_CLASSES) {
      expect(() =>
        SDK.assertSecurityGradeEvidence({
          trustClass,
          sourceId: "source",
          grade: "security",
        }),
      ).toThrowError(/prohibited_trust_class/u);
    }
  });

  it("rejects an operator-private class even when labelled, unless diagnostics are opted into", () => {
    const provenance: SDK.EvidenceProvenance = {
      trustClass: "operator_private_database",
      sourceId: "midgard-node-db",
      grade: "diagnostic",
      diagnosticLabel: "operator db",
    };
    expect(() => SDK.assertSecurityGradeEvidence(provenance)).toThrowError(
      /prohibited_trust_class/u,
    );
    expect(
      SDK.admitEvidenceProvenance({ provenance, allowDiagnostic: true }).grade,
    ).toBe("diagnostic");
  });

  it("fails closed on an unknown trust class instead of accepting it", () => {
    expect(() =>
      SDK.assertSecurityGradeEvidence({
        trustClass: "some_new_source" as SDK.EvidenceTrustClass,
        sourceId: "source",
        grade: "security",
      }),
    ).toThrowError(/unknown_trust_class/u);
  });

  it("refuses unlabelled diagnostics and refuses labels on security records", () => {
    expect(() =>
      SDK.admitEvidenceProvenance({
        provenance: {
          trustClass: "operator_admin_api",
          sourceId: "node",
          grade: "diagnostic",
        },
        allowDiagnostic: true,
      }),
    ).toThrowError(/missing_diagnostic_label/u);
    expect(() =>
      SDK.assertSecurityGradeEvidence({
        trustClass: "public_or_permissionless_da",
        sourceId: "peer",
        grade: "security",
        diagnosticLabel: "looks harmless",
      }),
    ).toThrowError(/diagnostic_label_on_security_evidence/u);
  });

  it("degrades a bundle to diagnostic when any contributing record is diagnostic", () => {
    expect(
      SDK.combineEvidenceGrade([
        {
          trustClass: "authenticated_cardano_l1",
          sourceId: "l1",
          grade: "security",
        },
        {
          trustClass: "operator_admin_api",
          sourceId: "node",
          grade: "diagnostic",
          diagnosticLabel: "label",
        },
      ]),
    ).toBe("diagnostic");
  });
});

describe("Q03 authenticated L1 observation admission", () => {
  it("admits a well-formed local-node header observation", async () => {
    const fixture = await validBlock();
    const admitted = await SDK.admitAuthenticatedStateQueueHeaderObservation({
      observation: authenticatedHeaderObservation(fixture),
    });
    expect(admitted.headerHash).toBe(fixture.headerHash);
    expect(admitted.provenance.trustClass).toBe("authenticated_cardano_l1");
  });

  it("rejects an unknown L1 source mode", async () => {
    const fixture = await validBlock();
    expect(
      await rejectionCode(async () =>
        SDK.admitAuthenticatedStateQueueHeaderObservation({
          observation: authenticatedHeaderObservation(fixture, {
            sourceMode: "operator_rest" as SDK.L1SourceMode,
          }),
        }),
      ),
    ).toBe("unknown_l1_source_mode");
  });

  it("rejects an observation whose provenance is not authenticated L1", async () => {
    const fixture = await validBlock();
    expect(
      await rejectionCode(async () =>
        SDK.admitAuthenticatedStateQueueHeaderObservation({
          observation: authenticatedHeaderObservation(fixture, {
            provenance: {
              trustClass: "operator_admin_api",
              sourceId: "node",
              grade: "diagnostic",
              diagnosticLabel: "operator node",
            },
          }),
        }),
      ),
    ).toBe("prohibited_trust_class");
  });

  it("rejects an observation below the required confirmation depth", async () => {
    const fixture = await validBlock();
    expect(
      await rejectionCode(async () =>
        SDK.admitAuthenticatedStateQueueHeaderObservation({
          observation: authenticatedHeaderObservation(fixture, {
            confirmationDepth: 3,
          }),
          minimumConfirmationDepth: 10,
        }),
      ),
    ).toBe("insufficient_confirmation_depth");
  });

  it("rejects a header paired with a foreign header hash", async () => {
    const fixture = await validBlock();
    const other = await doubleSpendBlock();
    expect(
      await rejectionCode(async () =>
        SDK.admitAuthenticatedStateQueueHeaderObservation({
          observation: authenticatedHeaderObservation(fixture, {
            headerHash: other.headerHash,
          }),
        }),
      ),
    ).toBe("header_hash_mismatch");
  });
});
