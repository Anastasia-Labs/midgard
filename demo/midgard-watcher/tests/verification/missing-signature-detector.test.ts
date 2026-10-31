import { MissingSignatureProvability } from "@al-ft/midgard-fault-proofs";
import {
  type MidgardAddressWitness,
  MISSING_SIGNATURE_WITNESS_SCAN_BATCH_SIZE,
} from "@al-ft/midgard-sdk";
import { RejectCodes } from "@al-ft/midgard-validation/types";
import { describe, expect, it } from "vitest";

import {
  detectAndJournalMissingSignatureFindings,
  detectMissingSignatureFinding,
  recoverMissingSignatureVkey,
  WATCHER_MISSING_SIGNATURE_DETECTOR_SCHEMA_VERSION,
  type WatcherMissingSignatureCandidate,
} from "../../src/verification/missing-signature-detector.js";

/**
 * Independent oracle for the required-signer hash: `blake2b_224` of the raw
 * 32-byte verification key, computed outside this repository with CPython's
 * `hashlib.blake2b(bytes.fromhex(vkey), digest_size=28).hexdigest()`. These
 * vectors deliberately do NOT call `missingSignatureVkeyHash`, so a change to
 * the hash function (digest length, an added prefix, hashing the hex text
 * instead of the bytes) is visible here.
 */
const VKEY_A = "11".repeat(32);
const HASH_A = "8cf0020fd6584f7b130db5ca0229c51f934821a2eb07c1df512d8aca";
const VKEY_B = "aa".repeat(32);
const HASH_B = "45468fe6267091fd12af1f636487feba09d31c0f7da2660e3c19ac53";
const VKEY_C = "77".repeat(32);

const witnessFor = (vkey: string): MidgardAddressWitness => ({
  verification_key: vkey,
  signature: "22".repeat(64),
});

/** Deterministic filler witnesses, none of which is the accused signer. */
const fillerWitnesses = (count: number): readonly MidgardAddressWitness[] =>
  Array.from({ length: count }, (_unused, index) =>
    witnessFor(index.toString(16).padStart(4, "0").repeat(16)),
  );

/**
 * Independently designed reference model for the §6 plan-time thread
 * estimate: the thread is `init + step-01 + step-02 + step-03` plus one
 * step-04 transaction per bounded witness-scan batch, and step-04 always runs
 * at least once (an empty witness set still has to be scanned).
 */
const referenceThreadTxCount = (witnessCount: number): number =>
  4 +
  Math.max(
    1,
    Math.ceil(witnessCount / MISSING_SIGNATURE_WITNESS_SCAN_BATCH_SIZE),
  );

const makeCandidate = (
  overrides: Partial<WatcherMissingSignatureCandidate> = {},
): WatcherMissingSignatureCandidate => ({
  headerHash: "33".repeat(28),
  eventKey: { L2TransactionEventKey: { tx_id: "44".repeat(32) } },
  fraudulentBlockOutRef: `${"55".repeat(32)}#0`,
  txId: "44".repeat(32),
  nativeTxCompactCbor: "80",
  committedWitnessSetHash: "66".repeat(32),
  committedAccepted: true,
  replayRejectCode: RejectCodes.MissingRequiredWitness,
  requiredSignerHashes: [HASH_A],
  addrTxWits: [],
  vkeySources: { committedL2Vkeys: [], observedL1Vkeys: [VKEY_A] },
  ...overrides,
});

describe("watcher missing-signature vkey recovery", () => {
  const cases = [
    {
      name: "committed L2 index",
      sources: { committedL2Vkeys: [VKEY_A], observedL1Vkeys: [] },
      expected: VKEY_A,
    },
    {
      name: "observed L1 witness sets",
      sources: { committedL2Vkeys: [], observedL1Vkeys: [VKEY_A] },
      expected: VKEY_A,
    },
    {
      name: "operator-supplied last resort",
      sources: {
        committedL2Vkeys: [],
        observedL1Vkeys: [],
        operatorSuppliedVkey: VKEY_A,
      },
      expected: VKEY_A,
    },
    {
      name: "skips non-matching and malformed entries in earlier sources",
      sources: {
        committedL2Vkeys: [VKEY_B, "not-hex", "11".repeat(31)],
        observedL1Vkeys: [VKEY_C, VKEY_A],
      },
      expected: VKEY_A,
    },
    {
      name: "normalizes an uppercase preimage to lowercase",
      sources: {
        committedL2Vkeys: [VKEY_A.toUpperCase()],
        observedL1Vkeys: [],
      },
      expected: VKEY_A,
    },
    {
      name: "returns null when no source carries the preimage",
      sources: {
        committedL2Vkeys: [VKEY_B],
        observedL1Vkeys: [VKEY_C],
        operatorSuppliedVkey: "00".repeat(32),
      },
      expected: null,
    },
  ] as const;

  it.each(cases)(
    "recovers the accused preimage: $name",
    ({ sources, expected }) => {
      expect(
        recoverMissingSignatureVkey({ requiredSignerHash: HASH_A, sources }),
      ).toBe(expected);
    },
  );

  it("matches the accused hash rather than any required signer", () => {
    expect(
      recoverMissingSignatureVkey({
        requiredSignerHash: HASH_B,
        sources: { committedL2Vkeys: [VKEY_A, VKEY_B], observedL1Vkeys: [] },
      }),
    ).toBe(VKEY_B);
  });
});

describe("watcher missing-signature detector v1", () => {
  it("emits the complete provable finding for a recovered absent signer", () => {
    const candidate = makeCandidate({
      requiredSignerHashes: [HASH_B, HASH_A],
      addrTxWits: [witnessFor(VKEY_B)],
      vkeySources: { committedL2Vkeys: [], observedL1Vkeys: [VKEY_A] },
    });

    expect(detectMissingSignatureFinding({ candidate })).toEqual({
      schemaVersion: WATCHER_MISSING_SIGNATURE_DETECTOR_SCHEMA_VERSION,
      finding: {
        headerHash: "33".repeat(28),
        eventKey: { L2TransactionEventKey: { tx_id: "44".repeat(32) } },
        fraudulentBlockOutRef: `${"55".repeat(32)}#0`,
        txId: "44".repeat(32),
        nativeTxCompactCbor: "80",
        accusedRequiredSignerIndex: 1n,
        accusedRequiredSignerHash: HASH_A,
        resolvedVkey: VKEY_A,
        committedWitnessSetHash: "66".repeat(32),
        provability: MissingSignatureProvability.MissingWitness,
        estimatedThreadTxCount: referenceThreadTxCount(1),
      },
    });
  });

  it.each([0, 1, 2, 32, 33, 64, 65])(
    "budgets one step-04 batch per %i witnesses",
    (witnessCount) => {
      const detection = detectMissingSignatureFinding({
        candidate: makeCandidate({ addrTxWits: fillerWitnesses(witnessCount) }),
      });
      expect(detection?.finding.provability).toBe(
        MissingSignatureProvability.MissingWitness,
      );
      expect(detection?.finding.estimatedThreadTxCount).toBe(
        referenceThreadTxCount(witnessCount),
      );
    },
  );

  it("classifies an unrecoverable preimage without resolving a vkey", () => {
    const detection = detectMissingSignatureFinding({
      candidate: makeCandidate({
        vkeySources: { committedL2Vkeys: [VKEY_B], observedL1Vkeys: [VKEY_C] },
      }),
    });
    expect(detection?.finding.provability).toBe(
      MissingSignatureProvability.UnknownVkeyPreimage,
    );
    expect(detection?.finding.resolvedVkey).toBeNull();
    expect(detection?.finding.accusedRequiredSignerHash).toBe(HASH_A);
  });

  it("classifies a present-but-invalid witness as another family's fault", () => {
    const detection = detectMissingSignatureFinding({
      candidate: makeCandidate({
        replayRejectCode: RejectCodes.InvalidSignature,
        addrTxWits: [witnessFor(VKEY_A)],
      }),
    });
    expect(detection?.finding.provability).toBe(
      MissingSignatureProvability.PresentButInvalid,
    );
    expect(detection?.finding.resolvedVkey).toBeNull();
  });

  it.each([
    {
      name: "replay agrees with the committed acceptance",
      overrides: {
        replayRejectCode: null,
        addrTxWits: [witnessFor(VKEY_A)],
      },
    },
    {
      name: "the leaf was never committed as accepted",
      overrides: { committedAccepted: false },
    },
    {
      name: "the invalid-signature code is paired with an absent witness",
      overrides: { replayRejectCode: RejectCodes.InvalidSignature },
    },
    {
      name: "the missing-witness code is paired with a complete witness set",
      overrides: { addrTxWits: [witnessFor(VKEY_A)] },
    },
  ])("refuses to accuse when $name", ({ overrides }) => {
    expect(
      detectMissingSignatureFinding({ candidate: makeCandidate(overrides) })
        ?.finding.provability,
    ).toBe(MissingSignatureProvability.NotAFault);
  });

  it("journals every classification in candidate order", async () => {
    const journaled: string[] = [];
    const detections = await detectAndJournalMissingSignatureFindings({
      candidates: [
        makeCandidate({
          vkeySources: { committedL2Vkeys: [], observedL1Vkeys: [] },
        }),
        makeCandidate({
          replayRejectCode: RejectCodes.InvalidSignature,
          addrTxWits: [witnessFor(VKEY_A)],
        }),
        makeCandidate({
          replayRejectCode: null,
          addrTxWits: [witnessFor(VKEY_A)],
        }),
        makeCandidate(),
      ],
      journal: ({ finding }) => {
        journaled.push(finding.provability);
      },
    });

    const expectedOrder = [
      MissingSignatureProvability.UnknownVkeyPreimage,
      MissingSignatureProvability.PresentButInvalid,
      MissingSignatureProvability.NotAFault,
      MissingSignatureProvability.MissingWitness,
    ];
    expect(detections.map(({ finding }) => finding.provability)).toEqual(
      expectedOrder,
    );
    expect(journaled).toEqual(expectedOrder);
  });

  it("honors the isolated default-on kill switch without journaling", async () => {
    const candidate = makeCandidate();
    expect(detectMissingSignatureFinding({ candidate })).not.toBeNull();
    expect(
      detectMissingSignatureFinding({ candidate, config: { enabled: false } }),
    ).toBeNull();

    const journaled: string[] = [];
    const detections = await detectAndJournalMissingSignatureFindings({
      candidates: [candidate, candidate],
      config: { enabled: false },
      journal: ({ finding }) => {
        journaled.push(finding.provability);
      },
    });
    expect(detections).toEqual([]);
    expect(journaled).toEqual([]);
  });
});
