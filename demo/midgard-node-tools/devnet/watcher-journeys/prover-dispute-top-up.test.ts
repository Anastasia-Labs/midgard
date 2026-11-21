import type { DeploymentManifestCardanoProtocolParameters } from "@al-ft/midgard-core/deployment-manifest-identity";
import { isPlainFundingOutput } from "@al-ft/midgard-fault-proofs/funding/plain-funding-output";
import { validationDisputeWalletMoveReserve } from "@al-ft/midgard-fault-proofs/test-support/validation-dispute-wallet-move-reserve";
import { VALIDATION_CASE_FLOOR_LOVELACE } from "@al-ft/midgard-sdk/da";
import type { UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  ensureProverDisputeFunding,
  JOURNEY_PROVER_FUNDING_OUTPUTS_LOVELACE,
  type ProverDisputeFundingPorts,
} from "./prover-dispute-top-up.js";

const KEY_HASH = "11".repeat(28);
const ADDRESS =
  "addr_test1vqgpzyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zygs3a2ux";
const BOND = 12_000_000_000n;
// A devnet manifest's signed snapshot (Preprod's parameters).
const PARAMETERS: DeploymentManifestCardanoProtocolParameters = {
  minFeeA: "44",
  minFeeB: "155381",
  priceMemory: { numerator: "577", denominator: "10000" },
  priceSteps: { numerator: "721", denominator: "10000000" },
  coinsPerUtxoByte: "4310",
  collateralPercentage: "150",
  maxCollateralInputs: "3",
  maxTxSize: "16384",
  maxValueSize: "5000",
  maxTxExUnits: { memory: "17500000", steps: "10000000000" },
  referenceScriptFee: {
    base: { numerator: "15", denominator: "1" },
    range: "25600",
    multiplier: { numerator: "6", denominator: "5" },
    maximumSizeBytes: "204800",
  },
};

/** What the prover's VerifySource and first wallet move spend, from the
 * three product values: the deployment-bound bond, the SDK's case floor and
 * the exported wallet-move reserve. */
const productRequirement = () =>
  BOND +
  VALIDATION_CASE_FLOOR_LOVELACE +
  validationDisputeWalletMoveReserve({
    parameters: PARAMETERS,
    fundingPaymentKeyHash: KEY_HASH,
  });

const coin = (index: number, lovelace: bigint, extra: Partial<UTxO> = {}) =>
  ({
    txHash: index.toString(16).padStart(64, "0"),
    outputIndex: 0,
    address: ADDRESS,
    assets: { lovelace },
    ...extra,
  }) as UTxO;

/** The run's actor funding of the prover, as the session pays it. */
const actorFunding = () =>
  JOURNEY_PROVER_FUNDING_OUTPUTS_LOVELACE.map((lovelace, index) =>
    coin(index + 1, lovelace),
  );

const spendable = (utxos: readonly UTxO[]) =>
  utxos
    .filter(isPlainFundingOutput)
    .reduce((total, utxo) => total + (utxo.assets.lovelace ?? 0n), 0n);

/** Fake ports over an in-memory prover wallet; every call is recorded. */
const fakePorts = (
  input: Readonly<{ wallet: UTxO[]; bond?: bigint }>,
): ProverDisputeFundingPorts & { calls: string[]; transfers: bigint[] } => {
  const calls: string[] = [];
  const transfers: bigint[] = [];
  return {
    calls,
    transfers,
    disputeTerms: () => {
      calls.push("disputeTerms");
      return input.bond === undefined
        ? {}
        : { validationDispute: { bondLovelace: input.bond } };
    },
    protocolParameters: () => {
      calls.push("protocolParameters");
      return PARAMETERS;
    },
    proverKeyHash: () => {
      calls.push("proverKeyHash");
      return KEY_HASH;
    },
    readProverUtxos: () => {
      calls.push("readProverUtxos");
      return Promise.resolve([...input.wallet]);
    },
    transfer: (lovelace) => {
      calls.push("transfer");
      transfers.push(lovelace);
      input.wallet.push(coin(1000 + transfers.length, lovelace));
      return Promise.resolve();
    },
    onStage: () => {},
  };
};

describe("ensureProverDisputeFunding", () => {
  it("tops a short prover up once, to the product's requirement over its actor funding, leaving those coins", async () => {
    const wallet = actorFunding();
    const before = [...wallet];
    const ports = fakePorts({ wallet, bond: BOND });

    await expect(ensureProverDisputeFunding(ports)).resolves.toBe("topped-up");

    expect(ports.transfers).toHaveLength(1);
    expect(wallet.slice(0, before.length)).toStrictEqual(before);
    const margin = spendable(before);
    expect(spendable(wallet)).toBe(productRequirement() + margin);
    expect(spendable(wallet)).toBeGreaterThanOrEqual(productRequirement());
  });

  it("counts a tagged refund as funding, as the prover's wallet does", async () => {
    const wallet = [
      ...actorFunding(),
      coin(51, 20_000_000_000n, { datum: "d87980" }),
    ];
    const ports = fakePorts({ wallet, bond: BOND });

    await ensureProverDisputeFunding(ports);

    expect(ports.transfers).toHaveLength(0);
  });

  it("leaves a coin holding a token out, as the prover's wallet does", async () => {
    const token = { [`${"22".repeat(28)}74`]: 1n };
    const wallet = [
      ...actorFunding(),
      coin(50, 20_000_000_000n, {
        assets: { lovelace: 20_000_000_000n, ...token },
      }),
    ];
    const ports = fakePorts({ wallet, bond: BOND });

    await ensureProverDisputeFunding(ports);

    expect(ports.transfers).toHaveLength(1);
    expect(spendable(wallet)).toBe(
      productRequirement() + spendable(actorFunding()),
    );
  });

  it("sends nothing to a prover that already covers it, and stays so on repeat", async () => {
    const wallet = actorFunding();
    const first = fakePorts({ wallet, bond: BOND });
    await ensureProverDisputeFunding(first);
    const repeat = fakePorts({ wallet, bond: BOND });

    await expect(ensureProverDisputeFunding(repeat)).resolves.toBe("covered");

    expect(repeat.transfers).toStrictEqual([]);
    expect(repeat.calls).not.toContain("transfer");
  });

  it("does nothing beyond the terms call for a family whose dispute posts no bond", async () => {
    const wallet = actorFunding();
    const ports = fakePorts({ wallet });

    await expect(ensureProverDisputeFunding(ports)).resolves.toBe(
      "posts-no-dispute-bond",
    );

    expect(ports.calls).toStrictEqual(["disputeTerms"]);
    expect(wallet).toStrictEqual(actorFunding());
  });
});
