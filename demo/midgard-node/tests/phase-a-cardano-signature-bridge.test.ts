import fs from "node:fs";
import path from "node:path";
import { describe, expect, it } from "vitest";
import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import {
  cardanoTxBytesToMidgardNativeTxFullBytes,
  computeMidgardNativeTxIdFromFull,
  decodeMidgardNativeTxFull,
} from "@/midgard-tx-codec/index.js";
import { RejectCodes, runPhaseAValidation, type QueuedTx } from "@/validation/index.js";

type TxFixture = {
  readonly cborHex: string;
  readonly txId: string;
};

const fixturePath = path.resolve(__dirname, "./txs/txs_0.json");
const txFixtures = JSON.parse(
  fs.readFileSync(fixturePath, "utf8"),
) as readonly TxFixture[];

const phaseAConfig = {
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  concurrency: 1,
  strictnessProfile: "phase1_midgard",
} as const;

describe("phase-a cardano signature bridge", () => {
  it("accepts converted Cardano witnesses when submit path provides the original Cardano body hash", async () => {
    const cardanoBytes = Buffer.from(txFixtures[0].cborHex, "hex");
    const nativeBytes = cardanoTxBytesToMidgardNativeTxFullBytes(cardanoBytes);
    const nativeTx = decodeMidgardNativeTxFull(nativeBytes);
    const txId = computeMidgardNativeTxIdFromFull(nativeTx);
    const txBodyHashForWitnesses = Buffer.from(
      CML.hash_transaction(CML.Transaction.from_cbor_bytes(cardanoBytes).body()).to_raw_bytes(),
    );

    const withoutOverride: QueuedTx = {
      txId,
      txCbor: nativeBytes,
      arrivalSeq: 0n,
      createdAt: new Date(0),
    };
    const withoutOverrideResult = await Effect.runPromise(
      runPhaseAValidation([withoutOverride], phaseAConfig),
    );
    expect(withoutOverrideResult.accepted).toHaveLength(0);
    expect(withoutOverrideResult.rejected).toHaveLength(1);
    expect(withoutOverrideResult.rejected[0].code).toBe(RejectCodes.InvalidSignature);

    const withOverride: QueuedTx = {
      ...withoutOverride,
      txBodyHashForWitnesses,
    };
    const withOverrideResult = await Effect.runPromise(
      runPhaseAValidation([withOverride], phaseAConfig),
    );
    expect(withOverrideResult.rejected).toHaveLength(0);
    expect(withOverrideResult.accepted).toHaveLength(1);
  });
});
