import { hashMidgardValidationRejectionReasonV1 } from "@al-ft/midgard-core";
import { encodeMidgardForcedTxCanonical } from "@al-ft/midgard-core/codec/forced";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { beforeAll, describe, expect, it } from "vitest";

import {
  decideValidationChallengeV1,
  openValidationTraceV1,
  RejectCodes,
  type ValidationTraceLeavesV1,
} from "../src/index.js";
import { phaseARejection } from "./reject-subject.support.js";
import {
  encodeByteList,
  encodeRecomputedNativeTx,
  FUNDED_OUTPUT_LOVELACE,
  makeNativeTx,
  makeOutput,
  outRefFromByte,
} from "./validation-fixtures.js";
import { context } from "./validation-machine.validate-boundary-abi-and-collect-auxiliary-kinds.js";

/**
 * With several invalid address witnesses, the one a rejection names is the
 * first the signature scan reaches: the scan runs in order-key order (signer
 * hash, then witness bytes, then index), not in witness-list order. Here the
 * two orders differ: the witness at list index 0 has the higher signer hash,
 * so the scan reaches list index 1 first.
 */
const signerHash = (key: CML.PrivateKey): Buffer =>
  Buffer.from(key.to_public().hash().to_raw_bytes());

const [lowerSigner, higherSigner] = [0x01, 0x02]
  .map((seed) => CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, seed)))
  .sort((left, right) => Buffer.compare(signerHash(left), signerHash(right)));

/** A witness by `key` over bytes that are not the transaction id. */
const invalidWitness = (key: CML.PrivateKey): Buffer =>
  Buffer.from(
    CML.make_vkey_witness(
      CML.TransactionHash.from_raw_bytes(Buffer.alloc(32, 0x7f)),
      key,
    ).to_cbor_bytes(),
  );

const spent = outRefFromByte(0x11);
const output = makeOutput(FUNDED_OUTPUT_LOVELACE);
const base = makeNativeTx({
  version: 1n,
  spendInputs: [spent],
  outputs: [output],
});
const transaction = encodeRecomputedNativeTx({
  ...base.tx,
  witnessSet: {
    ...base.tx.witnessSet,
    addrTxWitsPreimageCbor: encodeByteList([
      invalidWitness(higherSigner!),
      invalidWitness(lowerSigner!),
    ]),
  },
});

const FIRST_IN_SCAN_ORDER = 1n;
const FIRST_IN_LIST_ORDER = 0n;

const rejectionCodeHashNaming = (witnessIndex: bigint): Buffer =>
  hashMidgardValidationRejectionReasonV1({
    AddressWitnessSignatureInvalid: { witness_index: witnessIndex },
  });

describe("first invalid address witness", { timeout: 120_000 }, () => {
  let forcedLeaves: ValidationTraceLeavesV1;

  beforeAll(async () => {
    forcedLeaves = await Effect.runPromise(
      openValidationTraceV1({
        l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
        ...context,
        sourceKind: "forced",
        transactionId: transaction.txId,
        canonicalTransactionCbor: encodeMidgardForcedTxCanonical(
          transaction.tx,
        ),
        priorUtxosRoot: "03".repeat(32),
        postUtxosRoot: "03".repeat(32),
        ledgerWitnessEntries: [{ outRef: spent, output }],
        expectedLedgerOps: [],
        expectedVerdict: "rejected",
        expectedRejectionCode: RejectCodes.InvalidSignature,
      }),
    );
  });

  it("is named by Phase A in scan order", async () => {
    const rejection = await phaseARejection(
      transaction,
      RejectCodes.InvalidSignature,
    );
    expect(rejection.subject).toStrictEqual({
      arm: "AddressWitnessSignatureInvalid",
      index: FIRST_IN_SCAN_ORDER,
    });
  });

  it("is the one the committed rejection names", () => {
    expect(forcedLeaves.descriptor.verdict).toBe("rejected");
    expect(
      Buffer.from(forcedLeaves.descriptor.rejectionCodeHash).equals(
        rejectionCodeHashNaming(FIRST_IN_SCAN_ORDER),
      ),
    ).toBe(true);
  });

  it("agrees with a descriptor naming it", () => {
    expect(
      decideValidationChallengeV1(forcedLeaves, {
        ...forcedLeaves.descriptor,
        rejectionCodeHash: rejectionCodeHashNaming(FIRST_IN_SCAN_ORDER),
      }),
    ).toEqual({ kind: "agree" });
  });

  it("disputes a descriptor naming the first in list order", () => {
    expect(
      decideValidationChallengeV1(forcedLeaves, {
        ...forcedLeaves.descriptor,
        rejectionCodeHash: rejectionCodeHashNaming(FIRST_IN_LIST_ORDER),
      }),
    ).toEqual({
      kind: "terminalIncoherent",
      terminalStateIndex: forcedLeaves.stepCount,
    });
  });
});
