/**
 * `Q03` — canonical evidence-source API.
 *
 * Acceptance (GOAL_SPEC.md §9.2): "Builders consume verified `DaPayloadV1`/proof
 * bundles and authenticated L1 observations, not operator-private REST/DB/files
 * except labelled diagnostics."
 *
 * Positive coverage proves builders reach proof material from DA + L1 only.
 * Negative coverage proves every other input path is refused: prohibited trust
 * classes, unknown classes, diagnostic records, unauthenticated observations,
 * mutated payloads, foreign headers, and valid blocks with no violation.
 */
import { computeDaSha256Hash } from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  authenticateTransactionsInclusionRootsV1,
  blockTransactionsFromCanonicalEvidenceV1,
  canonicalBlockEvidenceFromVerifiedPayloadV1,
  type CanonicalBlockEvidenceV1,
  diagnosticBlockTransactionsFromMidgardNodeV1,
  diagnosticEvidenceBannerV1,
  fetchCanonicalBlockEvidenceV1,
  prepareDoubleSpendFromCanonicalEvidenceV1,
  prepareInvalidRangeFromCanonicalEvidenceV1,
  prepareZeroInputFromCanonicalEvidenceV1,
} from "../src/evidence/index.js";
import { decodeTransactionMaterial } from "../src/prepare-double-spend.js";
import type {
  RetainedDaPayloadSource,
  RetainedDaPayloadSourceResult,
} from "../src/transition-trace/fetch.js";
import {
  authenticatedHeaderObservationV1,
  buildCanonicalBlockFixtureV1,
  buildFixtureTransactionV1,
  type CanonicalBlockFixtureV1,
  h32,
  outRefCbor,
  reencodeFixturePayloadV1,
} from "./helpers/canonical-block-evidence-fixture-v1.js";

const DA_PROVENANCE: SDK.EvidenceProvenanceV1 = {
  trustClass: "public_or_permissionless_da",
  sourceId: "libp2p/peer-a",
  grade: "security",
};

const sharedInput = outRefCbor(0x11, 7n);

const doubleSpendBlock = async (): Promise<CanonicalBlockFixtureV1> =>
  await buildCanonicalBlockFixtureV1({
    transactions: [
      buildFixtureTransactionV1({
        spendInputs: [sharedInput, outRefCbor(0x22, 0n)],
        fee: 1n,
      }),
      buildFixtureTransactionV1({
        spendInputs: [outRefCbor(0x33, 0n), sharedInput],
        fee: 2n,
      }),
      buildFixtureTransactionV1({
        spendInputs: [outRefCbor(0x44, 0n)],
        fee: 3n,
      }),
    ],
  });

const validBlock = async (
  transactionsRootMode: "payloadSource" | "nativeCompact" = "payloadSource",
): Promise<CanonicalBlockFixtureV1> =>
  await buildCanonicalBlockFixtureV1({
    transactionsRootMode,
    transactions: [
      buildFixtureTransactionV1({
        spendInputs: [outRefCbor(0x55, 0n)],
        fee: 1n,
      }),
      buildFixtureTransactionV1({
        spendInputs: [outRefCbor(0x66, 1n)],
        fee: 2n,
      }),
    ],
  });

const evidenceFor = async (
  fixture: CanonicalBlockFixtureV1,
): Promise<CanonicalBlockEvidenceV1> => {
  // The public payload decoder intentionally authenticates the deployed node
  // source-leaf convention.  For the native-compact control, first obtain the
  // same canonical transaction material through that verified path, then
  // rebind the test-only header to the compact-leaf root and recompute the
  // inclusion authentication used by the proof builder.
  const payloadFixture =
    fixture.transactionsRootMode === "nativeCompact"
      ? await buildCanonicalBlockFixtureV1({
          transactions: fixture.transactions,
          startTime: fixture.header.startTime,
          endTime: fixture.header.endTime,
        })
      : fixture;
  const evidence = await canonicalBlockEvidenceFromVerifiedPayloadV1({
    observation: authenticatedHeaderObservationV1(payloadFixture),
    payloadEnvelopeCbor: payloadFixture.payloadEnvelopeCbor,
    daProvenance: DA_PROVENANCE,
  });
  if (fixture.transactionsRootMode !== "nativeCompact") {
    return evidence;
  }
  const inclusionRootAuthentication =
    await authenticateTransactionsInclusionRootsV1({
      header: fixture.header,
      reconstruction: evidence.reconstruction,
      transactions: evidence.transactions,
    });
  return {
    ...evidence,
    observation: authenticatedHeaderObservationV1(fixture),
    headerHash: fixture.headerHash,
    header: fixture.header,
    inclusionRootAuthentication,
  };
};

class StubDaSource implements RetainedDaPayloadSource {
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
      sourceId: this.sourceId,
      sourcePeerId: "peer-a",
      payloadEnvelopeCbor: this.payloadEnvelopeCbor,
      attempts: [],
    });
  }
}

const rejectionCode = async (run: () => Promise<unknown>): Promise<string> => {
  try {
    await run();
  } catch (error) {
    if (error instanceof SDK.CanonicalEvidenceRejectionV1) {
      return error.code;
    }
    return `unexpected:${error instanceof Error ? error.message : String(error)}`;
  }
  return "no_rejection";
};

describe("Q03 provenance admission", () => {
  it("admits every enumerated public trust class at security grade", () => {
    for (const trustClass of SDK.ADMITTED_EVIDENCE_TRUST_CLASSES_V1) {
      const admitted = SDK.assertSecurityGradeEvidenceV1({
        trustClass,
        sourceId: "source",
        grade: "security",
      });
      expect(admitted.grade).toBe("security");
      expect(admitted.trustClass).toBe(trustClass);
    }
  });

  it("rejects every operator-private class as a security input", () => {
    for (const trustClass of SDK.PROHIBITED_EVIDENCE_TRUST_CLASSES_V1) {
      expect(() =>
        SDK.assertSecurityGradeEvidenceV1({
          trustClass,
          sourceId: "source",
          grade: "security",
        }),
      ).toThrowError(/prohibited_trust_class/u);
    }
  });

  it("rejects an operator-private class even when labelled, unless diagnostics are opted into", () => {
    const provenance: SDK.EvidenceProvenanceV1 = {
      trustClass: "operator_private_database",
      sourceId: "midgard-node-db",
      grade: "diagnostic",
      diagnosticLabel: "operator db",
    };
    expect(() => SDK.assertSecurityGradeEvidenceV1(provenance)).toThrowError(
      /prohibited_trust_class/u,
    );
    expect(
      SDK.admitEvidenceProvenanceV1({ provenance, allowDiagnostic: true }).grade,
    ).toBe("diagnostic");
  });

  it("fails closed on an unknown trust class instead of accepting it", () => {
    expect(() =>
      SDK.assertSecurityGradeEvidenceV1({
        trustClass: "some_new_source" as SDK.EvidenceTrustClassV1,
        sourceId: "source",
        grade: "security",
      }),
    ).toThrowError(/unknown_trust_class/u);
  });

  it("refuses unlabelled diagnostics and refuses labels on security records", () => {
    expect(() =>
      SDK.admitEvidenceProvenanceV1({
        provenance: {
          trustClass: "operator_admin_api",
          sourceId: "node",
          grade: "diagnostic",
        },
        allowDiagnostic: true,
      }),
    ).toThrowError(/missing_diagnostic_label/u);
    expect(() =>
      SDK.assertSecurityGradeEvidenceV1({
        trustClass: "public_or_permissionless_da",
        sourceId: "peer",
        grade: "security",
        diagnosticLabel: "looks harmless",
      }),
    ).toThrowError(/diagnostic_label_on_security_evidence/u);
  });

  it("degrades a bundle to diagnostic when any contributing record is diagnostic", () => {
    expect(
      SDK.combineEvidenceGradeV1([
        { trustClass: "authenticated_cardano_l1", sourceId: "l1", grade: "security" },
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
    const admitted = await SDK.admitAuthenticatedStateQueueHeaderObservationV1({
      observation: authenticatedHeaderObservationV1(fixture),
    });
    expect(admitted.headerHash).toBe(fixture.headerHash);
    expect(admitted.provenance.trustClass).toBe("authenticated_cardano_l1");
  });

  it("rejects an unknown L1 source mode", async () => {
    const fixture = await validBlock();
    expect(
      await rejectionCode(async () =>
        SDK.admitAuthenticatedStateQueueHeaderObservationV1({
          observation: authenticatedHeaderObservationV1(fixture, {
            sourceMode: "operator_rest" as SDK.L1SourceModeV1,
          }),
        }),
      ),
    ).toBe("unknown_l1_source_mode");
  });

  it("rejects an observation whose provenance is not authenticated L1", async () => {
    const fixture = await validBlock();
    expect(
      await rejectionCode(async () =>
        SDK.admitAuthenticatedStateQueueHeaderObservationV1({
          observation: authenticatedHeaderObservationV1(fixture, {
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
        SDK.admitAuthenticatedStateQueueHeaderObservationV1({
          observation: authenticatedHeaderObservationV1(fixture, {
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
        SDK.admitAuthenticatedStateQueueHeaderObservationV1({
          observation: authenticatedHeaderObservationV1(fixture, {
            headerHash: other.headerHash,
          }),
        }),
      ),
    ).toBe("header_hash_mismatch");
  });
});

describe("Q03 canonical block evidence", () => {
  it("binds public DA payload bytes to the authenticated L1 header", async () => {
    const fixture = await doubleSpendBlock();
    const evidence = await evidenceFor(fixture);

    expect(evidence.grade).toBe("security");
    expect(evidence.headerHash).toBe(fixture.headerHash);
    expect(evidence.payloadEnvelopeSha256).toBe(
      computeDaSha256Hash(fixture.payloadEnvelopeCbor).toString("hex"),
    );
    expect(evidence.transactions.map((tx) => tx.nodeTxId).sort()).toEqual(
      fixture.transactions.map((tx) => tx.txId).sort(),
    );
    // Every transaction byte is the payload's authenticated canonical preimage.
    for (const tx of evidence.transactions) {
      const expected = fixture.transactions.find(
        (candidate) => candidate.txId === tx.nodeTxId,
      );
      expect(tx.txCbor).toBe(expected?.canonicalCbor.toString("hex"));
    }
  });

  it("fetches evidence over the public retained-DA source with peer fallback", async () => {
    const fixture = await doubleSpendBlock();
    const evidence = await fetchCanonicalBlockEvidenceV1({
      observation: authenticatedHeaderObservationV1(fixture),
      sources: [
        new StubDaSource("dead-peer"),
        new StubDaSource("live-peer", fixture.payloadEnvelopeCbor),
      ],
      retries: 0,
    });
    expect(evidence.provenance.da.trustClass).toBe(
      "public_or_permissionless_da",
    );
    expect(evidence.provenance.da.sourceId).toBe("live-peer/peer-a");
    expect(evidence.transactions).toHaveLength(3);
  });

  it("rejects DA bytes served for a different block", async () => {
    const fixture = await doubleSpendBlock();
    const other = await validBlock();
    await expect(
      canonicalBlockEvidenceFromVerifiedPayloadV1({
        observation: authenticatedHeaderObservationV1(fixture),
        payloadEnvelopeCbor: other.payloadEnvelopeCbor,
        daProvenance: DA_PROVENANCE,
      }),
    ).rejects.toThrowError(/header/iu);
  });

  it("rejects a mutated payload whose roots no longer match the committed header", async () => {
    const fixture = await doubleSpendBlock();
    const mutated = await reencodeFixturePayloadV1({
      ...fixture.payload,
      block_body: {
        ...fixture.payload.block_body,
        // Drop one committed transaction: roots and counts must both fail.
        transactions: fixture.payload.block_body.transactions.slice(1),
        transaction_preimages:
          fixture.payload.block_body.transaction_preimages.slice(1),
      },
    });
    await expect(
      canonicalBlockEvidenceFromVerifiedPayloadV1({
        observation: authenticatedHeaderObservationV1(fixture),
        payloadEnvelopeCbor: mutated,
        daProvenance: DA_PROVENANCE,
      }),
    ).rejects.toThrowError();
  });

  it("rejects a DA record that claims an operator-private origin", async () => {
    const fixture = await doubleSpendBlock();
    expect(
      await rejectionCode(async () =>
        canonicalBlockEvidenceFromVerifiedPayloadV1({
          observation: authenticatedHeaderObservationV1(fixture),
          payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
          daProvenance: {
            trustClass: "operator_private_database",
            sourceId: "node-db",
            grade: "security",
          },
        }),
      ),
    ).toBe("prohibited_trust_class");
  });

  it("rejects a security-grade record from a non-DA public class", async () => {
    const fixture = await doubleSpendBlock();
    expect(
      await rejectionCode(async () =>
        canonicalBlockEvidenceFromVerifiedPayloadV1({
          observation: authenticatedHeaderObservationV1(fixture),
          payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
          daProvenance: {
            trustClass: "deterministic_local_computation",
            sourceId: "local",
            grade: "security",
          },
        }),
      ),
    ).toBe("da_evidence_wrong_trust_class");
  });
});

describe("Q03 transactions-root inclusion authentication", () => {
  it("authenticates the payload-source convention and reports the native-compact divergence", async () => {
    const fixture = await doubleSpendBlock();
    const evidence = await evidenceFor(fixture);
    const authentication = evidence.inclusionRootAuthentication;

    // The node commits `(tx_id -> Data(L2TransactionSourceV1))` leaves, so the
    // payload-source convention is the one the header authenticates.
    expect(authentication.payloadSourceAuthenticated).toBe(true);
    expect(authentication.payloadSourceCountedRoot).toBe(
      fixture.header.transactionsRoot,
    );
    // The deployed Aiken step opens `(tx_id -> native_tx_compact_cbor)` leaves.
    // Those roots differ, so no submittable native inclusion argument exists
    // for a node-produced block until the conventions are reconciled.
    expect(authentication.nativeInclusionAuthenticated).toBe(false);
    expect(authentication.nativeCompactCountedRoot).not.toBe(
      fixture.header.transactionsRoot,
    );
    expect(authentication.l2TransactionCount).toBe(3n);
  });

  it("is recomputed from evidence, not trusted from the caller", async () => {
    const fixture = await doubleSpendBlock();
    const evidence = await evidenceFor(fixture);
    const recomputed = await authenticateTransactionsInclusionRootsV1({
      header: fixture.header,
      reconstruction: evidence.reconstruction,
      transactions: evidence.transactions,
    });
    expect(recomputed).toEqual(evidence.inclusionRootAuthentication);
  });

  it("accepts a native-compact root that does re-commit to the header", () => {
    const authenticated: SDK.TransactionsInclusionRootAuthenticationV1 = {
      headerTransactionsRoot: h32(0xab),
      l2TransactionCount: 2n,
      payloadSourcePhasRoot: h32(0xcd),
      payloadSourceCountedRoot: h32(0xab),
      nativeCompactPhasRoot: h32(0xef),
      nativeCompactCountedRoot: h32(0xab),
      nativeInclusionAuthenticated: true,
      payloadSourceAuthenticated: true,
    };
    expect(
      SDK.assertNativeInclusionRootAuthenticatedV1(authenticated),
    ).toEqual(authenticated);
  });
});

describe("Q03 canonical-evidence builders", () => {
  it("exposes authenticated transaction material for detection", async () => {
    const fixture = await doubleSpendBlock();
    const evidence = await evidenceFor(fixture);
    expect(blockTransactionsFromCanonicalEvidenceV1(evidence)).toHaveLength(3);
  });

  it("refuses to detect from evidence whose DA record was downgraded to diagnostic", async () => {
    const fixture = await doubleSpendBlock();
    const evidence = await evidenceFor(fixture);
    const downgraded: CanonicalBlockEvidenceV1 = {
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
    expect(() => blockTransactionsFromCanonicalEvidenceV1(downgraded)).toThrow(
      /prohibited_trust_class/u,
    );
  });

  it("refuses to emit a double-spend proof whose inclusion root cannot be authenticated", async () => {
    const fixture = await doubleSpendBlock();
    const evidence = await evidenceFor(fixture);
    expect(
      await rejectionCode(async () =>
        prepareDoubleSpendFromCanonicalEvidenceV1({ evidence }),
      ),
    ).toBe("native_inclusion_root_unauthenticated");
  });

  it("refuses to emit zero-input and invalid-range proofs on the same gate", async () => {
    const fixture = await doubleSpendBlock();
    const evidence = await evidenceFor(fixture);
    expect(
      await rejectionCode(async () =>
        prepareZeroInputFromCanonicalEvidenceV1({ evidence }),
      ),
    ).toBe("native_inclusion_root_unauthenticated");
    expect(
      await rejectionCode(async () =>
        prepareInvalidRangeFromCanonicalEvidenceV1({ evidence }),
      ),
    ).toBe("native_inclusion_root_unauthenticated");
  });

  it("applies the provenance gate before the inclusion gate", async () => {
    const fixture = await doubleSpendBlock();
    const evidence = await evidenceFor(fixture);
    const downgraded: CanonicalBlockEvidenceV1 = {
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
        prepareDoubleSpendFromCanonicalEvidenceV1({ evidence: downgraded }),
      ),
    ).toBe("prohibited_trust_class");
  });

  it("valid-block control: a block with no double spend yields no proof", async () => {
    const fixture = await validBlock("nativeCompact");
    const evidence = await evidenceFor(fixture);
    expect(
      evidence.inclusionRootAuthentication.nativeInclusionAuthenticated,
    ).toBe(true);
    expect(fixture.header.transactionsRoot).toBe(
      fixture.nativeCompactTransactionsRoot,
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
      prepareDoubleSpendFromCanonicalEvidenceV1({ evidence }),
    ).rejects.toThrow("No double spend found in the selected block.");
  });
});

describe("Q03 labelled diagnostics", () => {
  it("labels operator REST imports and keeps them out of security paths", async () => {
    const fixture = await doubleSpendBlock();
    const payloads = fixture.transactions.map((tx) => ({
      nodeTxId: tx.txId,
      txCbor: tx.canonicalCbor.toString("hex"),
    }));
    const fetchImpl = (input: string | URL): Promise<Response> => {
      const url = String(input);
      if (url.includes("/block")) {
        return Promise.resolve(
          new Response(
            JSON.stringify({ hashes: payloads.map((tx) => tx.nodeTxId) }),
          ),
        );
      }
      const txId = new URL(url).searchParams.get("tx_hash");
      const payload = payloads.find((tx) => tx.nodeTxId === txId);
      return Promise.resolve(
        new Response(JSON.stringify({ tx: payload?.txCbor })),
      );
    };

    const diagnostic = await diagnosticBlockTransactionsFromMidgardNodeV1({
      midgardNodeUrl: "http://operator.invalid",
      headerHash: fixture.headerHash,
      fetchImpl,
    });

    expect(diagnostic.provenance.grade).toBe("diagnostic");
    expect(diagnostic.provenance.trustClass).toBe("operator_admin_api");
    expect(diagnostic.provenance.diagnosticLabel).toMatch(/never a security/u);
    expect(diagnostic.transactions).toHaveLength(3);
    expect(diagnosticEvidenceBannerV1(diagnostic.provenance)).toMatch(
      /^DIAGNOSTIC EVIDENCE \(operator_admin_api\/midgard-node-url\)/u,
    );
    expect(() =>
      SDK.assertSecurityGradeEvidenceV1(diagnostic.provenance),
    ).toThrowError(/prohibited_trust_class/u);
  });
});
