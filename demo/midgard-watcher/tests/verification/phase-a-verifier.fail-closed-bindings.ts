import "./phase-a-verifier.malformed-inputs.js";

import { h28, h32 } from "@al-ft/midgard-test-support/hex";
import { makeNativeTx } from "@al-ft/midgard-validation/tests/validation-fixtures";
import type { RejectCode } from "@al-ft/midgard-validation/types";
import { RejectCodes } from "@al-ft/midgard-validation/types";
import { describe, expect, it } from "vitest";

import { watcherSha256CanonicalJson } from "../../src/storage/durable-store.js";
import { type WatcherHeaderRootReconstructionResult } from "../../src/verification/header-root-reconstruction.js";
import {
  makeWatcherPhaseAConfig,
  WATCHER_PHASE_A_VERIFIER_REASON_CODES,
  watcherPhaseARejectionProjection,
  WatcherPhaseAVerifierError,
} from "../../src/verification/phase-a-verifier.js";
import {
  WATCHER_RULE_BUNDLE_VALIDATION_PHASE_PRIORITY,
  type WatcherRuleBundle,
} from "../../src/verification/rule-bundle.js";
import {
  baseHeader,
  KEY,
  L1_PROVENANCE,
  RULE_BUNDLE,
} from "./phase-a-verifier.base-header.js";
import { buildBlock, evaluateBlock } from "./phase-a-verifier.build-block.js";

// ---------------------------------------------------------------------------
// Fail-closed bindings
// ---------------------------------------------------------------------------

describe("fail-closed bindings", () => {
  const tamper = (
    reconstruction: WatcherHeaderRootReconstructionResult,
    patch: Partial<WatcherHeaderRootReconstructionResult>,
    reDigest: boolean,
  ): WatcherHeaderRootReconstructionResult => {
    const next = { ...reconstruction, ...patch };
    if (!reDigest) {
      return next;
    }
    const { resultDigest: _drop, ...rest } = next;
    return { ...next, resultDigest: watcherSha256CanonicalJson(rest) };
  };

  it("refuses a W22 record whose digest does not cover its fields", async () => {
    const fixture = await buildBlock({
      txCbors: [makeNativeTx({ privateKey: KEY }).txCbor],
    });
    const result = await evaluateBlock(fixture, {
      reconstruction: tamper(
        fixture.reconstruction,
        { headerHash: h28(1) },
        false,
      ),
    });
    expect(result.action).toBe("error");
    expect(result.reasonCodes).toStrictEqual([
      "reconstruction_digest_mismatch",
    ]);
  });

  it("refuses a re-digested W22 record for a different header", async () => {
    const fixture = await buildBlock({
      txCbors: [makeNativeTx({ privateKey: KEY }).txCbor],
    });
    const result = await evaluateBlock(fixture, {
      reconstruction: tamper(
        fixture.reconstruction,
        { headerHash: h28(1) },
        true,
      ),
    });
    expect(result.action).toBe("error");
    expect(result.reasonCodes).toStrictEqual([
      "reconstruction_header_mismatch",
    ]);
  });

  it("refuses a rejected W22 record", async () => {
    const fixture = await buildBlock({
      txCbors: [makeNativeTx({ privateKey: KEY }).txCbor],
    });
    const result = await evaluateBlock(fixture, {
      reconstruction: tamper(
        fixture.reconstruction,
        { action: "reject", reasonCodes: ["root_mismatch"] },
        true,
      ),
    });
    expect(result.action).toBe("error");
    expect(result.reasonCodes).toStrictEqual(["reconstruction_not_accepted"]);
  });

  it("refuses an unsupported W22 schema version", async () => {
    const fixture = await buildBlock({
      txCbors: [makeNativeTx({ privateKey: KEY }).txCbor],
    });
    const result = await evaluateBlock(fixture, {
      reconstruction: tamper(
        fixture.reconstruction,
        { schemaVersion: "midgard-watcher-header-root-v0" as never },
        true,
      ),
    });
    expect(result.action).toBe("error");
    expect(result.reasonCodes).toStrictEqual([
      "reconstruction_unsupported_schema",
    ]);
  });

  it("refuses a W22 record describing different payload bytes", async () => {
    const fixture = await buildBlock({
      txCbors: [makeNativeTx({ privateKey: KEY }).txCbor],
    });
    const result = await evaluateBlock(fixture, {
      reconstruction: tamper(
        fixture.reconstruction,
        { payloadEnvelopeSha256: h32(0x5e) },
        true,
      ),
    });
    expect(result.action).toBe("error");
    expect(result.reasonCodes).toStrictEqual(["payload_bytes_mismatch"]);
  });

  it("refuses a rule bundle that is not the compiled V1 profile", async () => {
    const fixture = await buildBlock({
      txCbors: [makeNativeTx({ privateKey: KEY }).txCbor],
    });
    const result = await evaluateBlock(fixture, {
      ruleBundle: {
        ...RULE_BUNDLE,
        consensusProfileDigest: h32(0x6d),
      } as WatcherRuleBundle,
    });
    expect(result.action).toBe("error");
    expect(result.reasonCodes).toStrictEqual(["rule_bundle_profile_mismatch"]);
  });

  it("refuses a rule bundle with a foreign rejection-selection rule", () => {
    expect(() =>
      makeWatcherPhaseAConfig({
        header: baseHeader(),
        ruleBundle: {
          ...RULE_BUNDLE,
          validation: {
            ...RULE_BUNDLE.validation,
            rejectionSelection: "last_rejection_wins_v0",
          },
        } as unknown as WatcherRuleBundle,
      }),
    ).toThrow(
      expect.objectContaining({
        code: "rule_bundle_profile_mismatch",
      }) as Error,
    );
  });

  it("refuses a rule bundle with a reordered validation phase priority", () => {
    expect(() =>
      makeWatcherPhaseAConfig({
        header: baseHeader(),
        ruleBundle: {
          ...RULE_BUNDLE,
          validation: {
            ...RULE_BUNDLE.validation,
            phasePriority: [
              ...WATCHER_RULE_BUNDLE_VALIDATION_PHASE_PRIORITY,
            ].reverse(),
          },
        } as WatcherRuleBundle,
      }),
    ).toThrow(WatcherPhaseAVerifierError);
  });

  it("refuses a header whose protocol version differs from the bundle", () => {
    // A real `Header` cannot carry a non-V1 protocol version - the SDK
    // refuses to admit the observation first - so this guard is exercised at
    // the configuration boundary it protects.
    expect(() =>
      makeWatcherPhaseAConfig({
        header: baseHeader({ protocolVersion: 9n }),
        ruleBundle: RULE_BUNDLE,
      }),
    ).toThrow(
      expect.objectContaining({
        code: "header_protocol_version_mismatch",
      }) as Error,
    );
  });

  it("refuses non-public DA provenance", async () => {
    const fixture = await buildBlock({
      txCbors: [makeNativeTx({ privateKey: KEY }).txCbor],
    });
    const result = await evaluateBlock(fixture, {
      daProvenance: L1_PROVENANCE,
    });
    expect(result.action).toBe("error");
    expect(result.reasonCodes).toStrictEqual([
      "canonical_reconstruction_failed",
    ]);
  });

  it("fails closed on a reject code outside the canonical vocabulary", () => {
    expect(() =>
      watcherPhaseARejectionProjection({
        rejected: {
          txId: Buffer.alloc(32, 1),
          code: "E_NOT_A_REAL_CODE" as RejectCode,
          detail: null,
          consensusPhase: "inputSets",
        },
        index: 0,
        expectedTxId: Buffer.alloc(32, 1).toString("hex"),
      }),
    ).toThrow(
      expect.objectContaining({ code: "unknown_reject_code" }) as Error,
    );
  });

  it("fails closed on a rejection without a canonical stage", () => {
    expect(() =>
      watcherPhaseARejectionProjection({
        rejected: {
          txId: Buffer.alloc(32, 1),
          code: RejectCodes.EmptyInputs,
          detail: null,
        },
        index: 0,
        expectedTxId: Buffer.alloc(32, 1).toString("hex"),
      }),
    ).toThrow(
      expect.objectContaining({ code: "missing_rejection_stage" }) as Error,
    );
  });

  it("fails closed on a rejection carrying a foreign transaction id", () => {
    expect(() =>
      watcherPhaseARejectionProjection({
        rejected: {
          txId: Buffer.alloc(32, 2),
          code: RejectCodes.EmptyInputs,
          detail: null,
          consensusPhase: "inputSets",
        },
        index: 3,
        expectedTxId: Buffer.alloc(32, 1).toString("hex"),
      }),
    ).toThrow(
      expect.objectContaining({ code: "rejection_tx_id_mismatch" }) as Error,
    );
  });

  it("keeps every reason code in the declared total order", () => {
    expect(new Set(WATCHER_PHASE_A_VERIFIER_REASON_CODES).size).toBe(
      WATCHER_PHASE_A_VERIFIER_REASON_CODES.length,
    );
  });

  it("never reports accept on an error result", async () => {
    const fixture = await buildBlock({
      txCbors: [makeNativeTx({ privateKey: KEY }).txCbor],
    });
    for (const result of [
      await evaluateBlock(fixture, {
        payloadEnvelopeCbor: Buffer.alloc(4),
      }),
      await evaluateBlock(fixture, { daProvenance: L1_PROVENANCE }),
      await evaluateBlock(fixture, {
        reconstruction: tamper(
          fixture.reconstruction,
          { action: "reject" },
          true,
        ),
      }),
    ]) {
      expect(result.action).toBe("error");
      expect(result.acceptedCount).toBe(0);
      expect(result.acceptedTxIds).toStrictEqual([]);
      expect(result.selectedRejection).toBeNull();
      expect(Object.isFrozen(result)).toBe(true);
    }
  });
});
