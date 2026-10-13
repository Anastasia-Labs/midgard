import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import {
  encodeCbor,
  encodeMidgardForcedTxCanonical,
  encodeMidgardMintPolicyItem,
  materializeMidgardForcedTxFromCanonical,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
} from "@al-ft/midgard-core";
import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import * as SDK from "@al-ft/midgard-sdk";
import {
  getAddressDetails,
  Lucid,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { beforeAll, describe, expect, it, vi } from "vitest";

import { mintReferenceOnlyOrder } from "./support/emulator/reference-only-order.js";
import { publishForcedOrderCarriage } from "./support/forced-order-settlement.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  expectOnchainRefusal,
  makeFaultProofEmulatorHarness,
  makeHeader,
  publishPlainReferenceScriptUtxo,
  submitSetupTx,
  withPlaceholderDeposit,
} from "./support/submit-init-emulator-shared.js";

// An adversary skips the SDK's admission screen. Refused rows switch it off so
// the order reaches the tx-order mint, whose own rules must refuse it.
const screen = vi.hoisted(() => ({ enabled: true }));
vi.mock("@al-ft/midgard-core/consensus-validation", async (load) => {
  const original =
    await load<typeof import("@al-ft/midgard-core/consensus-validation")>();
  return {
    ...original,
    screenMidgardForcedTxAdmission: (
      tx: Parameters<typeof original.screenMidgardForcedTxAdmission>[0],
    ) => (screen.enabled ? original.screenMidgardForcedTxAdmission(tx) : null),
  };
});

/**
 * The `forced-tx-admission-v1` golden rows (midgard-core), minted through the
 * real tx-order mint: each refused row fails at the check its label names, and
 * each admitted neighbour mints. Rows whose field items are not §5.1 envelopes
 * the forced codec can encode are left to the Aiken and TS golden runs.
 */
type Row = {
  readonly name: string;
  readonly expected: string | null;
  readonly fields: readonly string[];
  readonly validityIntervalEnd: string;
  readonly scriptIntegrityHash: string;
  readonly auxiliaryDataHash: string;
  readonly networkId: string;
};

const golden = JSON.parse(
  readFileSync(
    fileURLToPath(
      new URL(
        "../../midgard-core/tests/fixtures/forced-tx-admission-v1.generated.json",
        import.meta.url,
      ),
    ),
    "utf8",
  ),
) as { readonly rows: readonly Row[] };

const row = (name: string): Row => {
  const found = golden.rows.find((candidate) => candidate.name === name);
  if (found === undefined) throw new Error(`no golden row ${name}`);
  return found;
};

const submittedBytes = (golden: Row): Buffer => {
  const field = (index: number) => Buffer.from(golden.fields[index]!, "hex");
  return encodeMidgardForcedTxCanonical(
    materializeMidgardForcedTxFromCanonical({
      version: MIDGARD_NATIVE_TX_VERSION,
      body: {
        spendInputsPreimageCbor: field(0),
        referenceInputsPreimageCbor: field(1),
        outputsPreimageCbor: field(2),
        fee: 0n,
        validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
        validityIntervalEnd: BigInt(golden.validityIntervalEnd),
        requiredObserversPreimageCbor: field(3),
        requiredSignersPreimageCbor: field(4),
        mintPreimageCbor: field(5),
        scriptIntegrityHash: Buffer.from(golden.scriptIntegrityHash, "hex"),
        auxiliaryDataHash: Buffer.from(golden.auxiliaryDataHash, "hex"),
        networkId: BigInt(golden.networkId),
      },
      witnessSet: {
        addrTxWitsPreimageCbor: field(7),
        scriptTxWitsPreimageCbor: field(6),
        redeemerTxWitsPreimageCbor: field(8),
      },
    }),
  );
};

const escape = (text: string) => text.replace(/[.*+?^${}()|[\]\\]/gu, "\\$&");

const REFUSED = [
  "upper_bound_present",
  "no_spend_input",
  "auxiliary_data",
  "observed_on_untagged_network",
  "redeemer",
  "plutus_v3_script_witness",
  "midgard_v1_script_witness",
  "native_reference_script",
  "plutus_reference_script",
  "inline_datum_and_native_reference_script",
  "value_of_5001_bytes_before_a_datum",
  "spend_input_second_item_byte_2_broken",
  "reference_input_second_item_byte_37_broken",
  "required_signer_item_a_byte_short",
  "address_witness_second_item_byte_38_broken",
  "mint_names_in_length_first_order",
  "mint_policies_out_of_order",
  "mint_zero_quantity",
  "mint_policy_with_no_asset",
] as const;

const ADMITTED = [
  "base",
  "native_script_witness",
  "canonical_inline_datum",
  "noncanonical_inline_datum",
  "unchunked_100_byte_inline_datum",
  "two_outputs_without_data",
  "value_of_5000_bytes_before_a_datum",
  "mint_names_in_byte_order",
  "mint_two_policies_burning",
] as const;

/**
 * `lucid` whose transaction builders set the L1 lower bound from
 * `lowerBound(validTo)` when the upper bound is set, ignoring the builder's
 * own lower bound; null leaves the range open below.
 */
const withLowerBound = (
  lucid: LucidEvolution,
  lowerBound: (validTo: number) => number | null,
): LucidEvolution => {
  const intercept = (builder: object): object => {
    const wrap = (target: object): object =>
      new Proxy(target, {
        get(on, property) {
          if (on === builder && property === "validFrom") return () => proxy;
          const value: unknown = Reflect.get(on, property, on);
          if (on === builder && property === "validTo")
            return (validTo: number) => {
              const lower = lowerBound(validTo);
              const set = builder as {
                validFrom: (time: number) => object;
                validTo: (time: number) => object;
              };
              if (lower !== null) set.validFrom(lower);
              set.validTo(validTo);
              return proxy;
            };
          if (typeof value === "function")
            return (...args: unknown[]) => {
              const result: unknown = value.apply(on, args);
              return result === builder ? proxy : result;
            };
          return value !== null && typeof value === "object"
            ? wrap(value)
            : value;
        },
      });
    const proxy = wrap(builder);
    return proxy;
  };
  return new Proxy(lucid, {
    get(on, property) {
      const value: unknown = Reflect.get(on, property, on);
      if (property === "newTx")
        return () => intercept(on.newTx()) as ReturnType<typeof on.newTx>;
      return typeof value === "function" ? value.bind(on) : value;
    },
  });
};

/** The deployment's longest closed L1 range a tx-order may carry (ms). */
const MAX_EVENT_RANGE_MS = 600_000;

describe("forced-tx admission at the real tx-order mint", () => {
  let h: Awaited<ReturnType<typeof makeFaultProofEmulatorHarness>>;
  let lucid: LucidEvolution;
  let txOrderMinting: UTxO;
  /** Emulator time when `lucid` was made: its slot config's zero time. */
  let lucidZeroTime: number;

  beforeAll(async () => {
    h = await makeFaultProofEmulatorHarness({
      contractOptions: { realTxOrder: true },
    });
    // The order builder dates `inclusion_time` from the compiled profile's
    // event wait, which the SDK reads only on the profile's own network.
    lucidZeroTime = h.emulator.now();
    lucid = await Lucid(h.emulator, SELECTED_DEPLOYMENT_PROFILE.network);
    h.proverSigner.selectWallet(lucid);
    const operator = getAddressDetails(
      await h.funderLucid.wallet().address(),
    ).paymentCredential!;
    await submitSetupTx({
      lucid: h.funderLucid,
      contracts: h.contracts,
      nonceUtxo: h.nonceUtxo,
      catalogue: h.catalogue,
      header: withPlaceholderDeposit(
        makeHeader(
          operator.hash,
          alignUnixTimeToEmulatorSlotBoundary(
            h.funderLucid,
            h.emulator.now() + 120_000,
          ) - 1,
        ),
      ),
    });
    // Deployed as a reference script, as the mint is in production.
    txOrderMinting = (
      await publishPlainReferenceScriptUtxo({
        lucid: h.funderLucid,
        script: h.contracts.txOrder.mintingScript,
        label: "order mint",
      })
    ).utxo;
  }, 300_000);

  /**
   * Publish the row's carriage and build its order; the build evaluates the
   * mint. `lowerBound`, given the upper bound the SDK chose, replaces the
   * SDK's L1 lower bound (null leaves it open), as a modified builder would.
   */
  const mintOrder = async (
    bytes: Buffer,
    lowerBound?: (validTo: number) => number | null,
  ) => {
    const carriage = await publishForcedOrderCarriage(h, lucid, bytes);
    const [nonce] = (await lucid.wallet().getUtxos())
      .filter(
        (utxo) => utxo.datum === undefined && utxo.datumHash === undefined,
      )
      .sort((left, right) =>
        Number(right.assets.lovelace - left.assets.lovelace),
      );
    if (nonce === undefined) throw new Error("no nonce UTxO in the wallet");
    const refundAddress = await Effect.runPromise(
      SDK.addressDataFromBech32(await lucid.wallet().address()),
    );
    const clock = vi
      .spyOn(Date, "now")
      .mockImplementation(() => h.emulator.now());
    const built = await Effect.runPromise(
      SDK.buildUnsignedTxOrderTxWithMetadataProgram(
        lowerBound === undefined ? lucid : withLowerBound(lucid, lowerBound),
        h.contracts,
        {
          nonceInput: nonce,
          submittedTxCbor: bytes.toString("hex"),
          carriageReferenceInputs: carriage.referenceInputs,
          fieldPreimageCertificatePolicyId:
            h.contracts.fieldPreimageCertificate.policyId,
          refundAddress,
          lovelace: 3_000_000n,
          referenceScripts: { txOrderMinting },
        },
      ),
    ).finally(() => clock.mockRestore());
    const signed = await built.tx.sign.withWallet().complete();
    await lucid.awaitTx(await signed.submit());
  };

  it.each([false, true])(
    "authenticates one reference-only order with independent policy=%s",
    async (independentPolicy) => {
      await mintReferenceOnlyOrder(() => {}, false, independentPolicy);
    },
    300_000,
  );

  it("refuses a second own-policy order NFT at the mint", async () => {
    await expect(mintReferenceOnlyOrder(() => {}, true)).rejects.toThrow(
      /failed script execution Mint\[\d+\]/u,
    );
  }, 300_000);

  it.each(ADMITTED)(
    "%s mints",
    async (name) => {
      const admitted = row(name);
      expect(admitted.expected).toBeNull();
      screen.enabled = true;
      const before = (
        await lucid.utxosAt(h.contracts.txOrder.spendingScriptAddress)
      ).length;
      await mintOrder(submittedBytes(admitted));
      expect(
        (await lucid.utxosAt(h.contracts.txOrder.spendingScriptAddress)).length,
      ).toBe(before + 1);
    },
    300_000,
  );

  it.each(REFUSED)(
    "%s is refused by the mint at its named check",
    async (name) => {
      const refused = row(name);
      expect(refused.expected).not.toBeNull();
      // The SDK refuses it before signing, at the same check.
      screen.enabled = true;
      expect(() =>
        SDK.deriveTxOrderMaterial({
          submittedTxCbor: submittedBytes(refused),
          owner: Buffer.alloc(28),
        }),
      ).toThrow(`Forced admission refused at ${refused.expected!}:`);
      screen.enabled = false;
      try {
        await expectOnchainRefusal(() => mintOrder(submittedBytes(refused)), {
          refusedBy: "user_events/tx_order_v1",
          check: new RegExp(escape(refused.expected!), "u"),
        });
      } finally {
        screen.enabled = true;
      }
    },
    300_000,
  );

  /**
   * A full event range ends about now, so it starts up to 601 slots in the
   * past; `lucid`'s zero time must lie before that or the evaluator cannot
   * place the lower bound.
   */
  const elapseOneEventRange = () => {
    const behind =
      MAX_EVENT_RANGE_MS + 2_000 - (h.emulator.now() - lucidZeroTime);
    if (behind > 0) h.emulator.awaitSlot(Math.ceil(behind / 1_000));
  };

  // The ledger bounds a range in whole slots and its upper bound is exclusive,
  // so a range of exactly 600 slots spans 599,999 ms inclusive and one slot
  // more spans 600,999 ms. The millisecond boundary itself (600,000 admitted,
  // 600,001 refused) is pinned by the utils Aiken tests.
  it("mints an order whose L1 range is the deployment's event range", async () => {
    screen.enabled = true;
    elapseOneEventRange();
    const before = (
      await lucid.utxosAt(h.contracts.txOrder.spendingScriptAddress)
    ).length;
    await mintOrder(
      submittedBytes(row("base")),
      (validTo) => validTo - MAX_EVENT_RANGE_MS,
    );
    expect(
      (await lucid.utxosAt(h.contracts.txOrder.spendingScriptAddress)).length,
    ).toBe(before + 1);
  }, 300_000);

  it("refuses an order whose L1 range is one slot longer than the event range", async () => {
    screen.enabled = true;
    const slot = 1_000;
    elapseOneEventRange();
    await expectOnchainRefusal(
      () =>
        mintOrder(
          submittedBytes(row("base")),
          (validTo) => validTo - MAX_EVENT_RANGE_MS - slot,
        ),
      {
        refusedBy: "user_events/tx_order_v1",
        check: /upper - lower <= max_length/u,
      },
    );
  }, 300_000);

  it("refuses an order whose L1 range is open below", async () => {
    screen.enabled = true;
    await expectOnchainRefusal(
      () => mintOrder(submittedBytes(row("base")), () => null),
      {
        refusedBy: "user_events/tx_order_v1",
        check: /Validity range was expected to be a closed range/u,
      },
    );
  }, 300_000);

  it("mints an order at the SDK's mint-policy capacity limit", async () => {
    // The base row with field 5 widened to the most single-asset policies the
    // SDK's capacity check admits: the real mint must fit it.
    const withPolicies = (count: number): Buffer =>
      submittedBytes({
        ...row("base"),
        fields: row("base").fields.map((field, index) =>
          index === 5
            ? encodeCbor(
                Array.from({ length: count }, (_, policy) =>
                  encodeMidgardMintPolicyItem({
                    policyId: Buffer.concat([
                      Buffer.alloc(26, 0x44),
                      Buffer.from([policy >> 8, policy & 0xff]),
                    ]),
                    assets: [{ assetName: Buffer.alloc(0), quantity: 1n }],
                  }),
                ),
              ).toString("hex")
            : field,
        ),
      });
    const limit = 121;
    expect(() =>
      SDK.deriveTxOrderMaterial({
        submittedTxCbor: withPolicies(limit),
        owner: Buffer.alloc(28),
      }),
    ).not.toThrow();
    expect(() =>
      SDK.deriveTxOrderMaterial({
        submittedTxCbor: withPolicies(limit + 1),
        owner: Buffer.alloc(28),
      }),
    ).toThrow("Forced transaction exceeds the tx-order mint's capacity:");
    screen.enabled = true;
    const before = (
      await lucid.utxosAt(h.contracts.txOrder.spendingScriptAddress)
    ).length;
    await mintOrder(withPolicies(limit));
    expect(
      (await lucid.utxosAt(h.contracts.txOrder.spendingScriptAddress)).length,
    ).toBe(before + 1);
  }, 300_000);

  it("mints the measured frontier with maximal signed quantity, name and pair widths", async () => {
    const withAssets = (count: number): Buffer => {
      const item = encodeMidgardMintPolicyItem({
        policyId: Buffer.alloc(28, 0x44),
        assets: Array.from({ length: count }, (_, index) => ({
          assetName: Buffer.concat([
            Buffer.alloc(30, 0x44),
            Buffer.from([index >> 8, index & 0xff]),
          ]),
          quantity: -0x10000000000000000n,
        })),
      });
      const wideItem = Buffer.concat([
        Buffer.from("9a00000002", "hex"),
        item.subarray(1),
      ]);
      return submittedBytes({
        ...row("base"),
        fields: row("base").fields.map((field, index) =>
          index === 5 ? encodeCbor([wideItem]).toString("hex") : field,
        ),
      });
    };
    const limit = 235;
    expect(() =>
      SDK.deriveTxOrderMaterial({
        submittedTxCbor: withAssets(limit),
        owner: Buffer.alloc(28),
      }),
    ).not.toThrow();
    expect(() =>
      SDK.deriveTxOrderMaterial({
        submittedTxCbor: withAssets(limit + 1),
        owner: Buffer.alloc(28),
      }),
    ).toThrow("Forced transaction exceeds the tx-order mint's capacity:");
    screen.enabled = true;
    const before = (
      await lucid.utxosAt(h.contracts.txOrder.spendingScriptAddress)
    ).length;
    await mintOrder(withAssets(limit));
    expect(
      (await lucid.utxosAt(h.contracts.txOrder.spendingScriptAddress)).length,
    ).toBe(before + 1);
  }, 300_000);
});
