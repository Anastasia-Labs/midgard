import {
  computeDaSha256Hash,
  DaRequestResponseProtocol,
  encodeDaEventToStepByEventResponseCbor,
  encodeDaPayloadByHeaderResponseCbor,
  encodeDaProofBundleByHeaderResponseCbor,
  encodeDaTraceStepByIndexResponseCbor,
} from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  authenticateTransactionsInclusionRoots,
  canonicalBlockEvidenceFromVerifiedPayload,
  fetchCanonicalBlockEvidence,
} from "../src/evidence/index.js";
import {
  DaLibp2pRetainedDaSource,
  fetchRetainedDaPayloadByHeaderHash,
  type RetainedDaLibp2pTransport,
} from "../src/transition-trace/fetch.js";
import {
  DA_PROVENANCE,
  doubleSpendBlock,
  evidenceFor,
  rejectionCode,
  StubDaSource,
  validBlock,
} from "./canonical-evidence-source.q03-provenance-admission.js";
import {
  authenticatedHeaderObservation,
  h32,
  reencodeFixturePayload,
} from "./helpers/canonical-block-evidence-fixture.js";

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
    const evidence = await fetchCanonicalBlockEvidence({
      observation: authenticatedHeaderObservation(fixture),
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

  it("admits payload and every retained proof surface at the public-DA boundary", async () => {
    const fixture = await doubleSpendBlock();
    const proofBundleBytes = Buffer.from("proof bundle");
    const transitionStepBytes = Buffer.from("transition step");
    const membershipProofBytes = Buffer.from("membership proof");
    const eventKey = Buffer.from("aabb", "hex");
    const eventEntry = Buffer.from("event entry");
    const transport: RetainedDaLibp2pTransport = {
      request: async ({ protocol }) => {
        // This fixture implements only the proof surfaces exercised here.
        // eslint-disable-next-line @typescript-eslint/switch-exhaustiveness-check
        switch (protocol) {
          case DaRequestResponseProtocol.payloadByHeader:
            return encodeDaPayloadByHeaderResponseCbor({
              status: "found_inline",
              headerHash: Buffer.from(fixture.headerHash, "hex"),
              payloadHash: computeDaSha256Hash(fixture.payloadEnvelopeCbor),
              payloadBytes: fixture.payloadEnvelopeCbor,
              chunkManifest: null,
              reasonCode: null,
            });
          case DaRequestResponseProtocol.proofBundleByHeader:
            return encodeDaProofBundleByHeaderResponseCbor({
              status: "found_inline",
              headerHash: Buffer.from(fixture.headerHash, "hex"),
              proofBundleHash: computeDaSha256Hash(proofBundleBytes),
              proofBundleBytes,
              chunkManifest: null,
              reasonCode: null,
            });
          case DaRequestResponseProtocol.traceStepByIndex:
            return encodeDaTraceStepByIndexResponseCbor({
              status: "found",
              headerHash: Buffer.from(fixture.headerHash, "hex"),
              stepIndex: 0,
              transitionStepBytes,
              membershipProofBytes,
            });
          case DaRequestResponseProtocol.eventToStepByEvent:
            return encodeDaEventToStepByEventResponseCbor({
              status: "found",
              headerHash: Buffer.from(fixture.headerHash, "hex"),
              eventKey,
              eventToStepEntryBytes: eventEntry,
              membershipOrNonmembershipProofBytes: membershipProofBytes,
            });
          default:
            throw new Error(`unsupported test protocol ${protocol}`);
        }
      },
    };
    const source = new DaLibp2pRetainedDaSource({
      sourceId: "public-da",
      deploymentFingerprint: h32(0x99),
      peers: [{ peerId: "peer-a" }],
      transport,
    });
    const payload = await fetchRetainedDaPayloadByHeaderHash({
      headerHash: fixture.headerHash,
      sources: [source],
      retries: 0,
    });
    const proofBundle = await source.fetchProofBundleByHeaderHash(
      fixture.headerHash,
    );
    const traceStep = await source.fetchTraceStepByIndex({
      headerHash: fixture.headerHash,
      stepIndex: 0,
    });
    const eventToStep = await source.fetchEventToStepByEvent({
      headerHash: fixture.headerHash,
      eventKey,
    });
    if (!proofBundle.ok || !traceStep.ok || !eventToStep.ok) {
      throw new Error("expected every retained proof surface");
    }
    for (const retained of [payload, proofBundle, traceStep, eventToStep]) {
      expect(retained.provenance).toMatchObject({
        trustClass: "public_or_permissionless_da",
        sourceId: "public-da/peer-a",
        grade: "security",
      });
      expect(() =>
        SDK.assertSecurityGradeEvidence(retained.provenance),
      ).not.toThrow();
    }
  });

  it("rejects DA bytes served for a different block", async () => {
    const fixture = await doubleSpendBlock();
    const other = await validBlock();
    await expect(
      canonicalBlockEvidenceFromVerifiedPayload({
        observation: authenticatedHeaderObservation(fixture),
        payloadEnvelopeCbor: other.payloadEnvelopeCbor,
        daProvenance: DA_PROVENANCE,
      }),
    ).rejects.toThrowError(/header/iu);
  });

  it("rejects a mutated payload whose roots no longer match the committed header", async () => {
    const fixture = await doubleSpendBlock();
    const mutated = await reencodeFixturePayload({
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
      canonicalBlockEvidenceFromVerifiedPayload({
        observation: authenticatedHeaderObservation(fixture),
        payloadEnvelopeCbor: mutated,
        daProvenance: DA_PROVENANCE,
      }),
    ).rejects.toThrowError();
  });

  it("rejects a DA record that claims an operator-private origin", async () => {
    const fixture = await doubleSpendBlock();
    expect(
      await rejectionCode(async () =>
        canonicalBlockEvidenceFromVerifiedPayload({
          observation: authenticatedHeaderObservation(fixture),
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
        canonicalBlockEvidenceFromVerifiedPayload({
          observation: authenticatedHeaderObservation(fixture),
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
  it("authenticates the exact transaction-source convention", async () => {
    const fixture = await doubleSpendBlock();
    const evidence = await evidenceFor(fixture);
    const authentication = evidence.inclusionRootAuthentication;

    expect(authentication.sourceInclusionAuthenticated).toBe(true);
    expect(authentication.sourceValueCountedRoot).toBe(
      fixture.header.transactionsRoot,
    );
    expect(authentication.sourceValueCount).toBe(3n);
    expect(authentication.l2TransactionCount).toBe(3n);
  });

  it("is recomputed from evidence, not trusted from the caller", async () => {
    const fixture = await doubleSpendBlock();
    const evidence = await evidenceFor(fixture);
    const recomputed = await authenticateTransactionsInclusionRoots({
      header: fixture.header,
      reconstruction: evidence.reconstruction,
      transactions: evidence.transactions,
    });
    expect(recomputed).toEqual(evidence.inclusionRootAuthentication);
  });

  it("accepts a source-value root that re-commits to the header", () => {
    const authenticated: SDK.TransactionsInclusionRootAuthentication = {
      headerTransactionsRoot: h32(0xab),
      l2TransactionCount: 2n,
      sourceValuePhasRoot: h32(0xcd),
      sourceValueCountedRoot: h32(0xab),
      sourceValueCount: 2n,
      sourceInclusionAuthenticated: true,
    };
    expect(
      SDK.assertTransactionSourceInclusionRootAuthenticated(authenticated),
    ).toEqual(authenticated);
  });
});
