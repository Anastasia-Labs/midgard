import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core/codec";
import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { midgardOutRefToCbor, runPhaseAValidation } from "../src/index.js";
import type {
  PhaseAValidatedTx,
  RejectCode,
  RejectedTx,
} from "../src/types.js";
import {
  makeNativeTx,
  makeQueued,
  TEST_ADDRESS_BYTES,
} from "./validation-fixtures.js";

export const phaseAConfig = {
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  concurrency: 1,
  strictnessProfile: "phase-a-unit",
};

export const addressAtRawNetworkNibble = (networkNibble: number): Buffer => {
  const address = Buffer.from(TEST_ADDRESS_BYTES);
  address[0] = (address[0] & 0xf0) | networkNibble;
  return address;
};

/**
 * This suite used to assert that `midgardOutRefToCbor` reproduced CML's
 * canonical `TransactionInput` CBOR. That is no longer the contract, and the
 * change is deliberate (#586's owner ruling): the ledger out-ref key is spec
 * §5.3's field-0/1 item, `82 ‖ 58 20 tx_id ‖ 19 index_be16`, a fixed 38 bytes
 * with a non-minimal 3-byte index — the same bytes on-chain `ledger_outref_key`
 * derives. CML minimises the index, so it disagrees for every index below 256,
 * and it is the *disagreement* that has to be pinned now.
 */
describe("outref projection", () => {
  it.each([0n, 23n, 24n, 255n, 256n, 65_535n])(
    "encodes the §5.3 fixed-index ledger key for index %s",
    (index) => {
      const txId = Buffer.alloc(32, Number(index & 0xffn));
      const encoded = midgardOutRefToCbor({ txId, index });

      expect(encoded).toStrictEqual(
        encodeMidgardSpendInputItem({ txId, outputIndex: Number(index) }),
      );
      // The fixed width is the point of the encoding: it is what makes the
      // stride-40 arithmetic access in the field-access door sound.
      expect(encoded.length).toBe(38);
      expect(encoded.subarray(0, 3)).toStrictEqual(
        Buffer.from([0x82, 0x58, 0x20]),
      );
      expect(encoded.subarray(3, 35)).toStrictEqual(txId);
      expect(encoded.subarray(35)).toStrictEqual(
        Buffer.from([0x19, Number(index >> 8n), Number(index & 0xffn)]),
      );
    },
  );

  it.each([0n, 23n, 24n, 255n])(
    "deliberately differs from CML's minimal-index CBOR for index %s",
    (index) => {
      const txId = Buffer.alloc(32, Number(index & 0xffn));
      const hash = CML.TransactionHash.from_raw_bytes(txId);
      const input = CML.TransactionInput.new(hash, index);
      try {
        // Below 256 CML spells the index in one or two bytes, so its encoding is
        // 36 or 37 bytes. Accepting it as a ledger key would mean two byte forms
        // for one out-ref — exactly what on-chain `decode_midgard_tx_input_cbor`
        // rejects when it asserts the `0x19` head.
        expect(Buffer.from(input.to_cbor_bytes())).not.toStrictEqual(
          midgardOutRefToCbor({ txId, index }),
        );
      } finally {
        input.free();
        hash.free();
      }
    },
  );

  it.each([256n, 65_535n])(
    "coincides with CML's minimal-index CBOR only where minimal is already uint16, at index %s",
    (index) => {
      const txId = Buffer.alloc(32, Number(index & 0xffn));
      const hash = CML.TransactionHash.from_raw_bytes(txId);
      const input = CML.TransactionInput.new(hash, index);
      try {
        expect(midgardOutRefToCbor({ txId, index })).toStrictEqual(
          Buffer.from(input.to_cbor_bytes()),
        );
      } finally {
        input.free();
        hash.free();
      }
    },
  );

  it.each([65_536n, 0xffff_ffff_ffff_ffffn])(
    "refuses index %s, which is outside §5.3's uint16 output-index domain",
    (index) => {
      const txId = Buffer.alloc(32, Number(index & 0xffn));
      expect(() => midgardOutRefToCbor({ txId, index })).toThrow();
    },
  );
});

export const runPhaseA = (
  queued: Parameters<typeof runPhaseAValidation>[0],
  config = phaseAConfig,
) => Effect.runPromise(runPhaseAValidation(queued, config));

export const queuedNativeTx = (
  fixture: Pick<ReturnType<typeof makeNativeTx>, "txId" | "txCbor">,
) => makeQueued(fixture.txId, fixture.txCbor);

export const expectSinglePhaseAAcceptance = async (
  fixture: Pick<ReturnType<typeof makeNativeTx>, "txId" | "txCbor">,
): Promise<PhaseAValidatedTx> => {
  const result = await runPhaseA([queuedNativeTx(fixture)]);
  expect(result.rejected).toHaveLength(0);
  expect(result.accepted).toHaveLength(1);
  return result.accepted[0];
};

export const expectSinglePhaseARejection = async (
  fixture: Pick<ReturnType<typeof makeNativeTx>, "txId" | "txCbor">,
  expectedCode: RejectCode,
  config = phaseAConfig,
): Promise<RejectedTx> => {
  const result = await runPhaseA([queuedNativeTx(fixture)], config);
  expect(result.accepted).toHaveLength(0);
  expect(result.rejected).toHaveLength(1);
  expect(result.rejected[0].code).toBe(expectedCode);
  return result.rejected[0];
};
