import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import {
  encodeMidgardForcedTxCanonical,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  materializeMidgardForcedTxFromCanonical,
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

import { publishForcedOrderCarriage } from "./support/forced-order-settlement.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  expectOnchainRefusal,
  makeFaultProofEmulatorHarness,
  makeHeader,
  publishPlainReferenceScriptUtxo,
  submitSetupTx,
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

describe("forced-tx admission at the real tx-order mint", () => {
  let h: Awaited<ReturnType<typeof makeFaultProofEmulatorHarness>>;
  let lucid: LucidEvolution;
  let txOrderMinting: UTxO;

  beforeAll(async () => {
    h = await makeFaultProofEmulatorHarness({
      contractOptions: { realTxOrder: true },
    });
    // The order builder dates `inclusion_time` from the compiled profile's
    // event wait, which the SDK reads only on the profile's own network.
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
      header: makeHeader(
        operator.hash,
        alignUnixTimeToEmulatorSlotBoundary(
          h.funderLucid,
          h.emulator.now() + 120_000,
        ) - 1,
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

  /** Publish the row's carriage and build its order; the build evaluates the mint. */
  const mintOrder = async (golden: Row) => {
    const bytes = submittedBytes(golden);
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
      SDK.buildUnsignedTxOrderTxWithMetadataProgram(lucid, h.contracts, {
        nonceInput: nonce,
        submittedTxCbor: bytes.toString("hex"),
        carriageReferenceInputs: carriage.referenceInputs,
        fieldPreimageCertificatePolicyId:
          h.contracts.fieldPreimageCertificate.policyId,
        refundAddress,
        lovelace: 3_000_000n,
        referenceScripts: { txOrderMinting },
      }),
    ).finally(() => clock.mockRestore());
    const signed = await built.tx.sign.withWallet().complete();
    await lucid.awaitTx(await signed.submit());
  };

  it.each(ADMITTED)(
    "%s mints",
    async (name) => {
      const admitted = row(name);
      expect(admitted.expected).toBeNull();
      screen.enabled = true;
      const before = (
        await lucid.utxosAt(h.contracts.txOrder.spendingScriptAddress)
      ).length;
      await mintOrder(admitted);
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
        await expectOnchainRefusal(() => mintOrder(refused), {
          refusedBy: "user_events/tx_order_v1",
          check: new RegExp(escape(refused.expected!), "u"),
        });
      } finally {
        screen.enabled = true;
      }
    },
    300_000,
  );
});
