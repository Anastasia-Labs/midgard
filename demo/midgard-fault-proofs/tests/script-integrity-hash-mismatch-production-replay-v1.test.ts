import { beforeEach, describe, expect, it, vi } from "vitest";

const { buildAuthentication } = vi.hoisted(() => ({
  buildAuthentication: vi.fn(),
}));

vi.mock(
  "../src/script-integrity-hash-mismatch/retained-stage-three-v1.js",
  () => ({
    buildScriptIntegrityStageThreeAuthenticationFromRetainedDaV1:
      buildAuthentication,
  }),
);

import { detectScriptIntegrityHashMismatchCanonicalViolationsV1 } from "../src/script-integrity-hash-mismatch/production-replay-v1.js";

const expectedDual =
  "6d49b4f24c60bec1cb34a2538278252059ec0601b7f675ef73fe2b48e24317d8";
const redeemerHash = "11".repeat(32);
const acceptedId = "aa".repeat(32);
const forcedId = "bb".repeat(32);

const block = ({
  accepted = true,
  forcedReason = "ScriptIntegrityHashMismatch",
}: {
  accepted?: boolean;
  forcedReason?: string;
} = {}) =>
  ({
    headerHash: "44".repeat(32),
    header: { validationTracesRoot: "55".repeat(32) },
    transactions: accepted ? [{ nodeTxId: acceptedId }] : [],
    reconstruction: {
      payload: {
        block_body: {
          validation_traces: [],
          validation_trace_witnesses: [],
        },
      },
      forcedTransactions: [
        {
          key: { transactionId: forcedId, outputIndex: 0n },
          value: {
            tx_id: forcedId,
            verdict: { ForcedTxInvalid: { reason: forcedReason } },
          },
        },
      ],
    },
  }) as never;

const authentication = (scriptIntegrityHash: string) => ({
  scriptIntegrityHash,
  redeemerWitnessHash: redeemerHash,
  control: { language_bitmap: 3n, execution_count: 2n },
});

describe("scriptIntegrityHashMismatch canonical production replay", () => {
  beforeEach(() => buildAuthentication.mockReset());

  it("returns both canonical contradiction directions in stable order", async () => {
    buildAuthentication
      .mockResolvedValueOnce(authentication("ff".repeat(32)))
      .mockResolvedValueOnce(authentication(expectedDual));
    await expect(
      detectScriptIntegrityHashMismatchCanonicalViolationsV1(block()),
    ).resolves.toMatchObject([
      { detectionId: expect.stringContaining(":accepted:0:") },
      { detectionId: expect.stringContaining(":forced:0:") },
    ]);
  });

  it("returns no detection after equality/polarity mutations", async () => {
    buildAuthentication.mockResolvedValue(authentication(expectedDual));
    await expect(
      detectScriptIntegrityHashMismatchCanonicalViolationsV1(
        block({ forcedReason: "InvalidRange" }),
      ),
    ).resolves.toEqual([]);
    expect(buildAuthentication).toHaveBeenCalledOnce();
  });
});
