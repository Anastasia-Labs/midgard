// The checks `/submit` and `/peer/txs` run on a tx before durable admission
// (`checkSubmittedTransaction`): the refusal for every path in front of the
// writer, and the facts an admitted tx carries so no later stage decodes it.
import { ledgerEd25519SignatureReady } from "@al-ft/midgard-core";
import {
  computeMidgardNativeTxFullHashFromCanonicalCbor,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  EMPTY_CBOR_LIST,
  encodeMidgardNativeTxCanonical,
  encodeMidgardTxOutput,
  materializeMidgardNativeTxFromCanonical,
  type MidgardNativeTxCanonical,
} from "@al-ft/midgard-core/codec";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { beforeAll, describe, expect, it } from "vitest";

import {
  buildLoadTxShape,
  loadOutRef,
  type LoadTx,
  makeLoadKey,
} from "../scripts/submit-ingress-load.tx.mjs";
import {
  checkSubmittedTransaction,
  SUBMIT_EXPIRY_CLOCK_MARGIN_MS,
  type SubmittedTxRules,
} from "../src/commands/http/transaction-admission.js";
import { wallSlotAt } from "../src/services/l1-ledger-tip.js";
import { makeCardanoSignedMapOutputTxBytes } from "./helpers/cardano-native-fixtures.js";

// Re-encodes a canonical tx with some body or witness fields replaced.
const withNativeFields = (
  txCbor: Buffer,
  change: (tx: MidgardNativeTxCanonical) => MidgardNativeTxCanonical,
): Buffer => {
  const decoded = decodeMidgardNativeTxFullFromCanonicalCbor(txCbor);
  return encodeMidgardNativeTxCanonical(
    materializeMidgardNativeTxFromCanonical(
      change({
        version: decoded.version,
        validity: decoded.validity,
        body: decoded.body,
        witnessSet: decoded.witnessSet,
      }),
    ),
  );
};

// The public testnet's rules (`config.preprod-public.example.yaml`).
const RULES: SubmittedTxRules = {
  expectedNetworkId: 0n,
  minFeeA: 1n,
  minFeeB: 2_000n,
  slotConfig: { ...NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod },
};
const NOW_MS = RULES.slotConfig.zeroTime + 100_000_000_000;
// The latest slot ingress treats as passed: the wall slot less the margin.
const PAST_SLOT = BigInt(
  wallSlotAt(RULES.slotConfig, NOW_MS - SUBMIT_EXPIRY_CLOCK_MARGIN_MS),
);

beforeAll(async () => {
  await ledgerEd25519SignatureReady();
});

describe("checkSubmittedTransaction", () => {
  const valid = buildLoadTxShape({ inputCount: 2, outputCount: 3 });
  const maxBytes = 32_768;
  const check = (bytes: Uint8Array) =>
    checkSubmittedTransaction(bytes, maxBytes, RULES, NOW_MS);

  // One vector per refusal path in front of durable admission. Each expected
  // result is the one the check returned before it decoded once; the
  // single-decode check must keep every status, body and log line.
  it.each([
    {
      name: "an empty body",
      bytes: () => Buffer.alloc(0),
      expected: {
        ok: false,
        status: 400,
        body: { error: "Invalid canonical transaction CBOR payload" },
        log: ["Submit rejected: Invalid canonical transaction CBOR payload"],
      },
    },
    {
      name: "a body over the size cap",
      bytes: () => Buffer.alloc(maxBytes + 1, 0x80),
      expected: {
        ok: false,
        status: 413,
        body: {
          error: `Canonical transaction CBOR exceeds max size (${(maxBytes + 1).toString()} > ${maxBytes.toString()})`,
        },
        log: [
          `Submit rejected: Canonical transaction CBOR exceeds max size (${(maxBytes + 1).toString()} > ${maxBytes.toString()})`,
        ],
      },
    },
    {
      name: "bytes that are not CBOR",
      bytes: () => Buffer.from("ffff", "hex"),
      expected: {
        ok: false,
        status: 400,
        body: { error: "Invalid canonical transaction CBOR payload" },
        log: [
          "Invalid canonical transaction CBOR payload",
          "canonical native transaction decode failed",
        ],
      },
    },
    {
      name: "a Cardano-signed tx",
      bytes: () => makeCardanoSignedMapOutputTxBytes(),
      expected: {
        ok: false,
        status: 400,
        body: { error: "Invalid canonical transaction CBOR payload" },
        log: [
          "Invalid canonical transaction CBOR payload",
          "canonical native transaction decode failed",
        ],
      },
    },
    {
      name: "a native tx whose address witness is not a vkey witness",
      bytes: () =>
        withNativeFields(valid.txCbor, (tx) => ({
          ...tx,
          witnessSet: {
            ...tx.witnessSet,
            addrTxWitsPreimageCbor: encodeCbor([Buffer.from("00", "hex")]),
          },
        })),
      expected: {
        ok: false,
        status: 400,
        body: { error: "Invalid canonical transaction CBOR payload" },
        log: [
          "Invalid canonical transaction CBOR payload",
          "canonical native transaction decode failed",
        ],
      },
    },
    {
      name: "a native tx committing auxiliary data",
      bytes: () =>
        withNativeFields(valid.txCbor, (tx) => ({
          ...tx,
          body: { ...tx.body, auxiliaryDataHash: Buffer.alloc(32, 0x0a) },
        })),
      expected: {
        ok: false,
        status: 400,
        body: {
          error: "E_AUX_DATA_FORBIDDEN",
          feature: "auxiliary_data",
          detail: "V1 has no authenticated auxiliary-data preimage",
        },
        log: [],
      },
    },
    {
      name: "a native tx with an oversized output value",
      bytes: () =>
        withNativeFields(valid.txCbor, (tx) => ({
          ...tx,
          body: {
            ...tx.body,
            outputsPreimageCbor: encodeCbor([
              encodeMidgardTxOutput({
                address: makeLoadKey("value-size", 0).addressBytes,
                value: {
                  lovelace: 2_000_000n,
                  assets: new Map([
                    [
                      "ab".repeat(28),
                      new Map(
                        Array.from({ length: 160 }, (_, index) => [
                          index.toString(16).padStart(64, "0"),
                          1n,
                        ]),
                      ),
                    ],
                  ]),
                },
              }),
            ]),
          },
        })),
      expected: {
        ok: false,
        status: 400,
        body: {
          error: "E_VALUE_SIZE",
          feature: "output_value",
          detail: "output[0] Cardano Value 5639 > 5000",
        },
        log: [],
      },
    },
  ])("refuses $name exactly as before", ({ bytes, expected }) => {
    expect(check(bytes())).toEqual(expected);
  });

  it("admits a valid native tx with every fact admission needs", () => {
    const checked = check(valid.txCbor);
    expect(checked).toEqual({
      ok: true,
      normalized: {
        txId: valid.txId,
        txIdHex: valid.txId.toString("hex"),
        txCanonicalCbor: valid.txCbor,
        txFullHash: computeMidgardNativeTxFullHashFromCanonicalCbor(
          valid.txCbor,
        ),
        spentOutRefs: valid.inputs
          .map((input) => input.outRef)
          .sort(Buffer.compare)
          .map((outRef) => outRef.toString("hex")),
        producedOutRefs: [0, 1, 2].map((index) =>
          loadOutRef(valid.txId, index).toString("hex"),
        ),
        fee: valid.fee,
        byteLength: valid.txCbor.length,
        source: "native",
      },
    });
  });

  // Design D6: the junk classes Phase A refuses on stateless grounds, each
  // with the code and detail the validation pool would give it.
  it.each([
    {
      name: "a signature over another tx id",
      tx: () =>
        buildLoadTxShape({
          inputCount: 2,
          outputCount: 3,
          signedTxId: Buffer.alloc(32, 0x11),
        }),
      error: "E_INVALID_SIGNATURE",
      detail: (): unknown => expect.any(String),
    },
    {
      name: "a fee under MIN_FEE_A x size + MIN_FEE_B",
      tx: () =>
        buildLoadTxShape({
          inputCount: 2,
          outputCount: 3,
          feeOverride: 2_000n,
        }),
      error: "E_MIN_FEE",
      detail: (tx: LoadTx): unknown =>
        `2000 < ${(BigInt(tx.txCbor.length) + 2_000n).toString()}`,
    },
    {
      name: "a mainnet body on a testnet node",
      tx: () =>
        buildLoadTxShape({ inputCount: 2, outputCount: 3, networkId: 1n }),
      error: "E_NETWORK_ID_MISMATCH",
      detail: (): unknown => "1 != 0",
    },
    {
      name: "a validity interval that ended a margin ago",
      tx: () =>
        buildLoadTxShape({
          inputCount: 2,
          outputCount: 3,
          validityIntervalEnd: PAST_SLOT,
        }),
      error: "E_VALIDITY_INTERVAL_MISMATCH",
      detail: (): unknown =>
        `${PAST_SLOT.toString()} >= ${PAST_SLOT.toString()}`,
    },
  ])("refuses $name with Phase A's code", ({ tx, error, detail }) => {
    const built = tx();
    expect(check(built.txCbor)).toEqual({
      ok: false,
      status: 400,
      body: { error, detail: detail(built) },
      log: [],
    });
  });

  it("admits a tx whose validity ended within the clock margin", () => {
    const tx = buildLoadTxShape({
      inputCount: 2,
      outputCount: 3,
      validityIntervalEnd: PAST_SLOT + 1n,
    });
    expect(check(tx.txCbor)).toMatchObject({ ok: true });
  });

  it("admits a tx paying exactly the minimum fee", () => {
    expect(valid.fee).toBe(BigInt(valid.txCbor.length) + 2_000n);
    expect(check(valid.txCbor)).toMatchObject({ ok: true });
  });

  it("leaves Phase A's other verdicts to the validation pool", () => {
    // No spend inputs: Phase A says E_EMPTY_INPUTS before it reads the
    // (now stale) signatures, and ingress refuses only the classes above.
    const noInputs = withNativeFields(valid.txCbor, (tx) => ({
      ...tx,
      body: { ...tx.body, spendInputsPreimageCbor: EMPTY_CBOR_LIST },
    }));
    expect(check(noInputs)).toMatchObject({ ok: true });
  });
});
