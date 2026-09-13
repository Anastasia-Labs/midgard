/**
 * `Q03` — canonical evidence-source admission rules (SDK layer).
 *
 * GOAL_SPEC.md §9.2: "Builders consume verified `DaPayloadV1`/proof bundles and
 * authenticated L1 observations, not operator-private REST/DB/files except
 * labelled diagnostics."
 */
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  admitAuthenticatedL1Observation,
  admitAuthenticatedStateQueueHeaderObservation,
  admitEvidenceProvenance,
  ADMITTED_EVIDENCE_TRUST_CLASSES,
  assertSecurityGradeEvidence,
  assertTransactionSourceInclusionRootAuthenticated,
  type AuthenticatedStateQueueHeaderObservation,
  CANONICAL_EVIDENCE_SOURCE_SCHEMA_VERSION,
  CanonicalEvidenceRejection,
  combineEvidenceGrade,
  type EvidenceProvenance,
  type EvidenceTrustClass,
  hashBlockHeader,
  type Header,
  isAdmittedEvidenceTrustClass,
  type L1SourceMode,
  PROHIBITED_EVIDENCE_TRUST_CLASSES,
  type TransactionsInclusionRootAuthentication,
} from "@/index.js";

const h = (byte: number, size: number): string =>
  byte.toString(16).padStart(2, "0").repeat(size);

const EMPTY_ROOT = h(0, 32);

const header: Header = {
  prevUtxosRoot: EMPTY_ROOT,
  utxosRoot: EMPTY_ROOT,
  withdrawalsRoot: EMPTY_ROOT,
  forcedTransactionsRoot: EMPTY_ROOT,
  transactionsRoot: h(0xaa, 32),
  depositsRoot: EMPTY_ROOT,
  transitionTraceRoot: EMPTY_ROOT,
  eventToStepRoot: EMPTY_ROOT,
  validationTracesRoot: EMPTY_ROOT,
  withdrawalCount: 0n,
  forcedTransactionCount: 0n,
  l2TransactionCount: 2n,
  depositCount: 0n,
  totalEventCount: 2n,
  transitionStepCount: 0n,
  validationTraceCount: 2n,
  startTime: 1n,
  endTime: 2n,
  blockSlot: 0n,
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  prevHeaderHash: h(0x90, 28),
  operatorVkey: h(0x91, 28),
  protocolVersion: 1n,
};

const observation = async (
  overrides: Partial<AuthenticatedStateQueueHeaderObservation> = {},
): Promise<AuthenticatedStateQueueHeaderObservation> => ({
  schemaVersion: CANONICAL_EVIDENCE_SOURCE_SCHEMA_VERSION,
  sourceMode: "local_node",
  provenance: {
    trustClass: "authenticated_cardano_l1",
    sourceId: "watcher-node",
    grade: "security",
  },
  chainPoint: { slot: 10n, blockHash: h(0x07, 32) },
  confirmationDepth: 8,
  headerHash: await Effect.runPromise(hashBlockHeader(header)),
  header,
  ...overrides,
});

const code = async (run: () => Promise<unknown> | unknown): Promise<string> => {
  try {
    await run();
  } catch (error) {
    return error instanceof CanonicalEvidenceRejection
      ? error.code
      : `unexpected:${String(error)}`;
  }
  return "no_rejection";
};

describe("canonical evidence provenance", () => {
  it("enumerates disjoint admitted and prohibited class sets", () => {
    for (const trustClass of ADMITTED_EVIDENCE_TRUST_CLASSES) {
      expect(isAdmittedEvidenceTrustClass(trustClass)).toBe(true);
    }
    for (const trustClass of PROHIBITED_EVIDENCE_TRUST_CLASSES) {
      expect(isAdmittedEvidenceTrustClass(trustClass)).toBe(false);
    }
    expect(isAdmittedEvidenceTrustClass("anything_else")).toBe(false);
  });

  it("admits public classes and rejects operator-private ones", async () => {
    for (const trustClass of ADMITTED_EVIDENCE_TRUST_CLASSES) {
      expect(
        assertSecurityGradeEvidence({
          trustClass,
          sourceId: "src",
          grade: "security",
        }).trustClass,
      ).toBe(trustClass);
    }
    for (const trustClass of PROHIBITED_EVIDENCE_TRUST_CLASSES) {
      expect(
        await code(() =>
          assertSecurityGradeEvidence({
            trustClass,
            sourceId: "src",
            grade: "security",
          }),
        ),
      ).toBe("prohibited_trust_class");
    }
  });

  it("fails closed on unknown classes, empty ids, and unknown grades", async () => {
    expect(
      await code(() =>
        assertSecurityGradeEvidence({
          trustClass: "future_source" as EvidenceTrustClass,
          sourceId: "src",
          grade: "security",
        }),
      ),
    ).toBe("unknown_trust_class");
    expect(
      await code(() =>
        assertSecurityGradeEvidence({
          trustClass: "authenticated_cardano_l1",
          sourceId: "   ",
          grade: "security",
        }),
      ),
    ).toBe("empty_source_id");
    expect(
      await code(() =>
        assertSecurityGradeEvidence({
          trustClass: "authenticated_cardano_l1",
          sourceId: "src",
          grade: "trusted" as EvidenceProvenance["grade"],
        }),
      ),
    ).toBe("evidence_grade_mismatch");
  });

  it("requires an explicit opt-in and a label for diagnostics", async () => {
    const provenance: EvidenceProvenance = {
      trustClass: "operator_only_diagnostic_endpoint",
      sourceId: "node-diagnostics",
      grade: "diagnostic",
      diagnosticLabel: "operator diagnostics endpoint",
    };
    expect(await code(() => assertSecurityGradeEvidence(provenance))).toBe(
      "prohibited_trust_class",
    );
    expect(
      admitEvidenceProvenance({ provenance, allowDiagnostic: true }).grade,
    ).toBe("diagnostic");
    expect(
      await code(() =>
        admitEvidenceProvenance({
          provenance: { ...provenance, diagnosticLabel: undefined },
          allowDiagnostic: true,
        }),
      ),
    ).toBe("missing_diagnostic_label");
  });

  it("refuses to launder a diagnostic label onto security evidence", async () => {
    expect(
      await code(() =>
        assertSecurityGradeEvidence({
          trustClass: "public_or_permissionless_da",
          sourceId: "peer",
          grade: "security",
          diagnosticLabel: "label",
        }),
      ),
    ).toBe("diagnostic_label_on_security_evidence");
  });

  it("computes bundle grade as the weakest contributing record", () => {
    expect(
      combineEvidenceGrade([
        {
          trustClass: "authenticated_cardano_l1",
          sourceId: "a",
          grade: "security",
        },
        {
          trustClass: "public_or_permissionless_da",
          sourceId: "b",
          grade: "security",
        },
      ]),
    ).toBe("security");
    expect(
      combineEvidenceGrade([
        {
          trustClass: "authenticated_cardano_l1",
          sourceId: "a",
          grade: "security",
        },
        {
          trustClass: "operator_private_file",
          sourceId: "b",
          grade: "diagnostic",
          diagnosticLabel: "file",
        },
      ]),
    ).toBe("diagnostic");
  });
});

describe("authenticated L1 observations", () => {
  it("admits a valid observation and rebinds the header hash", async () => {
    const admitted = await admitAuthenticatedStateQueueHeaderObservation({
      observation: await observation(),
    });
    expect(admitted.header).toEqual(header);
    expect(admitted.sourceMode).toBe("local_node");
  });

  it("rejects unknown source modes, bad chain points, and shallow depth", async () => {
    expect(
      await code(async () =>
        admitAuthenticatedL1Observation({
          observation: await observation({
            sourceMode: "operator_api" as L1SourceMode,
          }),
        }),
      ),
    ).toBe("unknown_l1_source_mode");
    expect(
      await code(async () =>
        admitAuthenticatedL1Observation({
          observation: await observation({
            chainPoint: { slot: 1n, blockHash: "not-hex" },
          }),
        }),
      ),
    ).toBe("malformed_chain_point");
    expect(
      await code(async () =>
        admitAuthenticatedL1Observation({
          observation: await observation({ confirmationDepth: 0 }),
          minimumConfirmationDepth: 1,
        }),
      ),
    ).toBe("insufficient_confirmation_depth");
  });

  it("rejects invalid minimum confirmation depths", async () => {
    for (const minimumConfirmationDepth of [
      -1,
      0,
      Number.NaN,
      1.5,
      Number.POSITIVE_INFINITY,
    ]) {
      expect(
        await code(async () =>
          admitAuthenticatedL1Observation({
            observation: await observation({ confirmationDepth: 2 }),
            minimumConfirmationDepth,
          }),
        ),
      ).toBe("insufficient_confirmation_depth");
    }
  });

  it("accepts an observation at the exact minimum confirmation depth", async () => {
    const admitted = admitAuthenticatedL1Observation({
      observation: await observation({ confirmationDepth: 1 }),
      minimumConfirmationDepth: 1,
    });
    expect(admitted.confirmationDepth).toBe(1);
  });

  it("rejects a header hash that the canonical hasher does not reproduce", async () => {
    expect(
      await code(async () =>
        admitAuthenticatedStateQueueHeaderObservation({
          observation: await observation({ headerHash: h(0x0c, 28) }),
        }),
      ),
    ).toBe("header_hash_mismatch");
  });

  it("rejects a malformed header hash", async () => {
    expect(
      await code(async () =>
        admitAuthenticatedStateQueueHeaderObservation({
          observation: await observation({ headerHash: h(0x0c, 32) }),
        }),
      ),
    ).toBe("malformed_header_hash");
  });
});

describe("transaction-source inclusion-root gate", () => {
  const authentication = (
    sourceInclusionAuthenticated: boolean,
  ): TransactionsInclusionRootAuthentication => ({
    headerTransactionsRoot: h(0xaa, 32),
    l2TransactionCount: 2n,
    sourceValuePhasRoot: h(0xbb, 32),
    sourceValueCountedRoot: sourceInclusionAuthenticated
      ? h(0xaa, 32)
      : h(0xdd, 32),
    sourceValueCount: 2n,
    sourceInclusionAuthenticated,
  });

  it("passes an authenticated transaction-source root through unchanged", () => {
    const value = authentication(true);
    expect(assertTransactionSourceInclusionRootAuthenticated(value)).toEqual(
      value,
    );
  });

  it("rejects an unauthenticated transaction-source root", async () => {
    const value = authentication(false);
    expect(
      await code(() =>
        assertTransactionSourceInclusionRootAuthenticated(value),
      ),
    ).toBe("transaction_source_inclusion_root_unauthenticated");
    try {
      assertTransactionSourceInclusionRootAuthenticated(value);
    } catch (error) {
      expect((error as CanonicalEvidenceRejection).detail).toContain(
        "header_transactions_root=",
      );
      expect((error as CanonicalEvidenceRejection).detail).toContain(
        "source_value_counted_root=",
      );
    }
  });
});
