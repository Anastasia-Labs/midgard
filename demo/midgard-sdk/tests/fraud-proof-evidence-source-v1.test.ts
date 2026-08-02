/**
 * `Q03` — canonical evidence-source admission rules (SDK layer).
 *
 * GOAL_SPEC.md §9.2: "Builders consume verified `DaPayloadV1`/proof bundles and
 * authenticated L1 observations, not operator-private REST/DB/files except
 * labelled diagnostics."
 */
import { describe, expect, it } from "vitest";

import {
  ADMITTED_EVIDENCE_TRUST_CLASSES_V1,
  admitAuthenticatedL1ObservationV1,
  admitAuthenticatedStateQueueHeaderObservationV1,
  admitEvidenceProvenanceV1,
  assertNativeInclusionRootAuthenticatedV1,
  assertSecurityGradeEvidenceV1,
  type AuthenticatedStateQueueHeaderObservationV1,
  CANONICAL_EVIDENCE_SOURCE_V1_SCHEMA_VERSION,
  CanonicalEvidenceRejectionV1,
  combineEvidenceGradeV1,
  type EvidenceProvenanceV1,
  type EvidenceTrustClassV1,
  hashBlockHeaderV1,
  type HeaderV1,
  isAdmittedEvidenceTrustClassV1,
  type L1SourceModeV1,
  PROHIBITED_EVIDENCE_TRUST_CLASSES_V1,
  type TransactionsInclusionRootAuthenticationV1,
} from "@/index.js";
import { Effect } from "effect";

const h = (byte: number, size: number): string =>
  byte.toString(16).padStart(2, "0").repeat(size);

const EMPTY_ROOT = h(0, 32);

const header: HeaderV1 = {
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
  overrides: Partial<AuthenticatedStateQueueHeaderObservationV1> = {},
): Promise<AuthenticatedStateQueueHeaderObservationV1> => ({
  schemaVersion: CANONICAL_EVIDENCE_SOURCE_V1_SCHEMA_VERSION,
  sourceMode: "local_node",
  provenance: {
    trustClass: "authenticated_cardano_l1",
    sourceId: "watcher-node",
    grade: "security",
  },
  chainPoint: { slot: 10n, blockHash: h(0x07, 32) },
  confirmationDepth: 8,
  headerHash: await Effect.runPromise(hashBlockHeaderV1(header)),
  header,
  ...overrides,
});

const code = async (run: () => Promise<unknown> | unknown): Promise<string> => {
  try {
    await run();
  } catch (error) {
    return error instanceof CanonicalEvidenceRejectionV1
      ? error.code
      : `unexpected:${String(error)}`;
  }
  return "no_rejection";
};

describe("canonical evidence provenance", () => {
  it("enumerates disjoint admitted and prohibited class sets", () => {
    for (const trustClass of ADMITTED_EVIDENCE_TRUST_CLASSES_V1) {
      expect(isAdmittedEvidenceTrustClassV1(trustClass)).toBe(true);
    }
    for (const trustClass of PROHIBITED_EVIDENCE_TRUST_CLASSES_V1) {
      expect(isAdmittedEvidenceTrustClassV1(trustClass)).toBe(false);
    }
    expect(isAdmittedEvidenceTrustClassV1("anything_else")).toBe(false);
  });

  it("admits public classes and rejects operator-private ones", async () => {
    for (const trustClass of ADMITTED_EVIDENCE_TRUST_CLASSES_V1) {
      expect(
        assertSecurityGradeEvidenceV1({
          trustClass,
          sourceId: "src",
          grade: "security",
        }).trustClass,
      ).toBe(trustClass);
    }
    for (const trustClass of PROHIBITED_EVIDENCE_TRUST_CLASSES_V1) {
      expect(
        await code(() =>
          assertSecurityGradeEvidenceV1({
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
        assertSecurityGradeEvidenceV1({
          trustClass: "future_source" as EvidenceTrustClassV1,
          sourceId: "src",
          grade: "security",
        }),
      ),
    ).toBe("unknown_trust_class");
    expect(
      await code(() =>
        assertSecurityGradeEvidenceV1({
          trustClass: "authenticated_cardano_l1",
          sourceId: "   ",
          grade: "security",
        }),
      ),
    ).toBe("empty_source_id");
    expect(
      await code(() =>
        assertSecurityGradeEvidenceV1({
          trustClass: "authenticated_cardano_l1",
          sourceId: "src",
          grade: "trusted" as EvidenceProvenanceV1["grade"],
        }),
      ),
    ).toBe("evidence_grade_mismatch");
  });

  it("requires an explicit opt-in and a label for diagnostics", async () => {
    const provenance: EvidenceProvenanceV1 = {
      trustClass: "operator_only_diagnostic_endpoint",
      sourceId: "node-diagnostics",
      grade: "diagnostic",
      diagnosticLabel: "operator diagnostics endpoint",
    };
    expect(await code(() => assertSecurityGradeEvidenceV1(provenance))).toBe(
      "prohibited_trust_class",
    );
    expect(
      admitEvidenceProvenanceV1({ provenance, allowDiagnostic: true }).grade,
    ).toBe("diagnostic");
    expect(
      await code(() =>
        admitEvidenceProvenanceV1({
          provenance: { ...provenance, diagnosticLabel: undefined },
          allowDiagnostic: true,
        }),
      ),
    ).toBe("missing_diagnostic_label");
  });

  it("refuses to launder a diagnostic label onto security evidence", async () => {
    expect(
      await code(() =>
        assertSecurityGradeEvidenceV1({
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
      combineEvidenceGradeV1([
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
      combineEvidenceGradeV1([
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
    const admitted = await admitAuthenticatedStateQueueHeaderObservationV1({
      observation: await observation(),
    });
    expect(admitted.header).toEqual(header);
    expect(admitted.sourceMode).toBe("local_node");
  });

  it("rejects unknown source modes, bad chain points, and shallow depth", async () => {
    expect(
      await code(async () =>
        admitAuthenticatedL1ObservationV1({
          observation: await observation({
            sourceMode: "operator_api" as L1SourceModeV1,
          }),
        }),
      ),
    ).toBe("unknown_l1_source_mode");
    expect(
      await code(async () =>
        admitAuthenticatedL1ObservationV1({
          observation: await observation({
            chainPoint: { slot: 1n, blockHash: "not-hex" },
          }),
        }),
      ),
    ).toBe("malformed_chain_point");
    expect(
      await code(async () =>
        admitAuthenticatedL1ObservationV1({
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
          admitAuthenticatedL1ObservationV1({
            observation: await observation({ confirmationDepth: 2 }),
            minimumConfirmationDepth,
          }),
        ),
      ).toBe("insufficient_confirmation_depth");
    }
  });

  it("accepts an observation at the exact minimum confirmation depth", async () => {
    const admitted = admitAuthenticatedL1ObservationV1({
      observation: await observation({ confirmationDepth: 1 }),
      minimumConfirmationDepth: 1,
    });
    expect(admitted.confirmationDepth).toBe(1);
  });

  it("rejects a header hash that the canonical hasher does not reproduce", async () => {
    expect(
      await code(async () =>
        admitAuthenticatedStateQueueHeaderObservationV1({
          observation: await observation({ headerHash: h(0x0c, 28) }),
        }),
      ),
    ).toBe("header_hash_mismatch");
  });

  it("rejects a malformed header hash", async () => {
    expect(
      await code(async () =>
        admitAuthenticatedStateQueueHeaderObservationV1({
          observation: await observation({ headerHash: h(0x0c, 32) }),
        }),
      ),
    ).toBe("malformed_header_hash");
  });
});

describe("native inclusion-root gate", () => {
  const authentication = (
    nativeInclusionAuthenticated: boolean,
  ): TransactionsInclusionRootAuthenticationV1 => ({
    headerTransactionsRoot: h(0xaa, 32),
    l2TransactionCount: 2n,
    payloadSourcePhasRoot: h(0xbb, 32),
    payloadSourceCountedRoot: h(0xaa, 32),
    nativeCompactPhasRoot: h(0xcc, 32),
    nativeCompactCountedRoot: nativeInclusionAuthenticated
      ? h(0xaa, 32)
      : h(0xdd, 32),
    nativeInclusionAuthenticated,
    payloadSourceAuthenticated: true,
  });

  it("passes an authenticated native-compact root through unchanged", () => {
    const value = authentication(true);
    expect(assertNativeInclusionRootAuthenticatedV1(value)).toEqual(value);
  });

  it("rejects an unauthenticated native-compact root with both roots in the code", async () => {
    const value = authentication(false);
    expect(
      await code(() => assertNativeInclusionRootAuthenticatedV1(value)),
    ).toBe("native_inclusion_root_unauthenticated");
    try {
      assertNativeInclusionRootAuthenticatedV1(value);
    } catch (error) {
      expect((error as CanonicalEvidenceRejectionV1).detail).toContain(
        "header_transactions_root=",
      );
      expect((error as CanonicalEvidenceRejectionV1).detail).toContain(
        "native_compact_counted_root=",
      );
    }
  });
});
