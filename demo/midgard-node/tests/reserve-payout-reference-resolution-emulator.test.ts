/**
 * A payout conclusion resolves its reference scripts through their role
 * tokens. Anyone can pay the same script, without the role token, to the
 * reference-script address at a lower outRef; the transaction still
 * references the genuine holder, and with only such UTxOs there the build is
 * refused rather than referencing one.
 */
import "./helpers/follower-emulator-installed.js";

import { compareOutRefs, outRefLabel } from "@al-ft/midgard-core/out-ref";
import * as SDK from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  Data,
  Emulator,
  generateEmulatorAccount,
  Lucid as makeLucid,
  type Script,
  scriptHashToCredential,
  toUnit,
  type TxSignBuilder,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  __reservePayoutTest,
  buildConcludePayoutTxProgram,
} from "../src/transactions/reserve-payout.js";
import { makeSeededScriptAccount } from "./reserve-payout-builders.make-reserve-payout-builder-fixture.js";
import {
  canonicalDatumCbor,
  EMULATOR_PROTOCOL_PARAMETERS,
  findPureAdaUtxo,
  findUtxoWithUnit,
  loadRealContracts,
  submitWithWallet,
} from "./reserve-payout-builders.submit-with-wallet.js";

const payoutRoles = ["payout spending", "payout minting"] as const;
type PayoutRole = (typeof payoutRoles)[number];

/** A token named like the role under a policy that is not the deployment's
 * reference-script auth policy. */
const FOREIGN_POLICY = "ab".repeat(28);

const makeFixture = async ({ genuine }: { readonly genuine: boolean }) => {
  const operator = generateEmulatorAccount({ lovelace: 30_000_000_000n });
  const beneficiary = generateEmulatorAccount({ lovelace: 2_000_000n });
  const contracts = await loadRealContracts({
    txHash: "00".repeat(32),
    outputIndex: 0,
  });
  const authPolicy = contracts.referenceScriptAuth.policyId;
  const scripts: Record<PayoutRole, Script> = {
    "payout spending": contracts.payout.spendingScript,
    "payout minting": contracts.payout.mintingScript,
  };
  const payoutUnit = toUnit(contracts.payout.policyId, "aa");
  const hubUnit = toUnit(
    contracts.hubOracle.policyId,
    SDK.HUB_ORACLE_ASSET_NAME,
  );
  const hubOracleAddress = credentialToAddress(
    "Custom",
    scriptHashToCredential(contracts.hubOracle.policyId),
  );
  const payoutDatum: SDK.PayoutDatum = {
    l2_value: __reservePayoutTest.assetsToValue({ lovelace: 7_000_000n }),
    l1_address: await Effect.runPromise(
      SDK.addressDataFromBech32(beneficiary.address),
    ),
    l1_datum: "NoDatum",
  };
  const foreignRoleUnit = (role: PayoutRole): string =>
    SDK.referenceScriptAuthUnit(FOREIGN_POLICY, role);
  // Genesis outputs share one transaction id and take their account's
  // position as output index, so these sort before every genuine holder.
  const imposters = payoutRoles.flatMap((role) => [
    makeSeededScriptAccount({
      address: operator.address,
      assets: { lovelace: 4_000_000n },
      scriptRef: scripts[role],
    }),
    makeSeededScriptAccount({
      address: operator.address,
      assets: { lovelace: 5_000_000n, [foreignRoleUnit(role)]: 1n },
      scriptRef: scripts[role],
    }),
  ]);
  const holders = genuine
    ? payoutRoles.map((role) =>
        makeSeededScriptAccount({
          address: operator.address,
          assets: {
            lovelace: 3_000_000n,
            [SDK.referenceScriptAuthUnit(authPolicy, role)]: 1n,
          },
          scriptRef: scripts[role],
        }),
      )
    : [];
  const emulator = new Emulator(
    [
      operator,
      beneficiary,
      ...imposters,
      ...holders,
      makeSeededScriptAccount({
        address: operator.address,
        assets: { lovelace: 11_000_000n },
      }),
      makeSeededScriptAccount({
        address: hubOracleAddress,
        assets: { lovelace: 3_000_000n, [hubUnit]: 1n },
        inlineDatum: Data.to(
          await Effect.runPromise(SDK.makeHubOracleDatum(contracts)),
          SDK.HubOracleDatum,
        ),
      }),
      makeSeededScriptAccount({
        address: contracts.payout.spendingScriptAddress,
        assets: { lovelace: 7_000_000n, [payoutUnit]: 1n },
        inlineDatum: canonicalDatumCbor(Data.to(payoutDatum, SDK.PayoutDatum)),
      }),
    ],
    EMULATOR_PROTOCOL_PARAMETERS,
  );
  const lucid = await makeLucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(operator.seedPhrase);

  const walletUtxos = await lucid.utxosAt(operator.address);
  const withScript = (role: PayoutRole) =>
    walletUtxos.filter(
      (utxo) =>
        utxo.scriptRef != null &&
        validatorToScriptHash(utxo.scriptRef) ===
          validatorToScriptHash(scripts[role]),
    );
  const holding = (role: PayoutRole) =>
    withScript(role).filter(
      (utxo) => utxo.assets[SDK.referenceScriptAuthUnit(authPolicy, role)],
    );
  // Effect values are lazy, so each run of `conclude()` reads the chain
  // afresh.
  const conclude = () =>
    Effect.gen(function* () {
      const read = (address: string) =>
        Effect.promise(() => lucid.utxosAt(address));
      return yield* buildConcludePayoutTxProgram(lucid, contracts, {
        hubOracleRefInput: findUtxoWithUnit(
          yield* read(hubOracleAddress),
          hubUnit,
        ),
        feeInput: findPureAdaUtxo(walletUtxos, 11_000_000n),
        payoutInput: findUtxoWithUnit(
          yield* read(contracts.payout.spendingScriptAddress),
          payoutUnit,
        ),
        referenceScriptsAddress: operator.address,
      });
    });
  return {
    beneficiary,
    conclude,
    contracts,
    holding,
    lucid,
    operator,
    payoutUnit,
    withScript,
  };
};

const referenceInputLabels = (tx: TxSignBuilder): readonly string[] => {
  const inputs = tx.toTransaction().body().reference_inputs();
  return Array.from({ length: inputs?.len() ?? 0 }, (_, index) => {
    const input = inputs!.get(index);
    return `${input.transaction_id().to_hex()}#${input.index().toString()}`;
  });
};

describe("payout conclusion reference-script resolution", () => {
  it("references the role-token holder, not a lower-outRef copy of the script without its role token", async () => {
    const fixture = await makeFixture({ genuine: true });
    const { contracts, lucid, payoutUnit } = fixture;

    const built = await Effect.runPromise(fixture.conclude());
    const labels = referenceInputLabels(built.tx);
    await lucid.awaitTx(await submitWithWallet(built.tx));

    for (const role of payoutRoles) {
      const [holder, ...others] = fixture.holding(role);
      expect(holder).toBeDefined();
      expect(others).toHaveLength(0);
      const imposters = fixture
        .withScript(role)
        .filter((utxo) => outRefLabel(utxo) !== outRefLabel(holder!));
      expect(imposters).toHaveLength(2);
      for (const imposter of imposters) {
        expect(compareOutRefs(imposter, holder!)).toBeLessThan(0);
        expect(labels).not.toContain(outRefLabel(imposter));
      }
      expect(labels).toContain(outRefLabel(holder!));
    }
    expect(
      (await lucid.utxosAt(contracts.payout.spendingScriptAddress)).some(
        (utxo) => utxo.assets[payoutUnit] === 1n,
      ),
    ).toBe(false);
    expect(
      (await lucid.utxosAt(fixture.beneficiary.address)).some(
        (utxo) =>
          utxo.assets.lovelace === 7_000_000n &&
          Object.keys(utxo.assets).length === 1,
      ),
    ).toBe(true);
  });

  it("refuses to build when only copies of the script without its role token are published", async () => {
    const fixture = await makeFixture({ genuine: false });
    expect(fixture.withScript("payout spending")).toHaveLength(2);
    expect(fixture.holding("payout spending")).toHaveLength(0);

    const error = await Effect.runPromise(Effect.flip(fixture.conclude()));
    expect(error).toBeInstanceOf(SDK.StateQueueError);
    expect(error.message).toBe("Missing reference script");
    expect(String((error as SDK.StateQueueError).cause)).toMatch(
      new RegExp(
        `^payout (spending|minting) at ${fixture.operator.address} with role token Payout(Spend|Mint)`,
        "u",
      ),
    );
  });
});
