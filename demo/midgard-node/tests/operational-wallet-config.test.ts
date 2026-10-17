import "./utils.js";

import {
  credentialToAddress,
  generateSeedPhrase,
  paymentCredentialOf,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterEach, describe, expect, it, vi } from "vitest";

import { NodeConfig } from "../src/services/config.js";

const loadConfig = () =>
  Effect.runPromise(
    Effect.gen(function* () {
      return yield* NodeConfig;
    }).pipe(Effect.provide(NodeConfig.layer)),
  );

afterEach(() => {
  vi.unstubAllEnvs();
});

const operator = generateSeedPhrase();
const merge = generateSeedPhrase();
const settlement = generateSeedPhrase();
const reference = generateSeedPhrase();
const addressOf = (seed: string) =>
  walletFromSeed(seed, { network: "Preprod" }).address;

const SEED_SETTINGS = {
  operator: "L1_OPERATOR_SEED_PHRASE",
  merge: "L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX",
  settlement: "L1_SETTLEMENT_SEED_PHRASE",
  reference: "L1_REFERENCE_SCRIPT_SEED_PHRASE",
} as const;
type Role = keyof typeof SEED_SETTINGS;
const SEEDS: Record<Role, string> = { operator, merge, settlement, reference };

/** Four distinct wallets, the deploy address left to its default. */
const stubWallets = (overrides: Partial<Record<Role, string>> = {}) => {
  for (const [role, setting] of Object.entries(SEED_SETTINGS))
    vi.stubEnv(setting, overrides[role as Role] ?? SEEDS[role as Role]);
  vi.stubEnv("L1_REFERENCE_SCRIPT_ADDRESS", "");
  vi.stubEnv("L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS", undefined);
};

/** Same payment key as `seed`, without a stake part: another address. */
const enterpriseAddressOf = (seed: string) =>
  credentialToAddress("Preprod", paymentCredentialOf(addressOf(seed)));

/** A non-spendable script address, as the production deploy address. */
const SCRIPT_DEPLOY_ADDRESS = credentialToAddress("Preprod", {
  type: "Script",
  hash: "ab".repeat(28),
});

/**
 * Config load is the one place the node's wallet topology is checked: the
 * operator, merge, settlement (when set) and reference-script roles are four
 * wallets, compared by derived payment key hash and address, and the
 * reference-script deploy address is none of the other three.
 */
describe("operational wallet topology at config load", () => {
  it("loads four distinct wallets and defaults the deploy address to the reference-script wallet", async () => {
    stubWallets();
    const config = await loadConfig();
    expect(config.L1_REFERENCE_SCRIPT_SEED_PHRASE).toBe(reference);
    expect(config.L1_REFERENCE_SCRIPT_ADDRESS).toBe(addressOf(reference));
    expect(config.L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS).toBe(
      addressOf(reference),
    );
  });

  it("loads without a settlement wallet", async () => {
    stubWallets({ settlement: "" });
    await expect(loadConfig()).resolves.toMatchObject({
      L1_SETTLEMENT_SEED_PHRASE: "",
    });
  });

  it("refuses a missing or empty reference-script seed instead of using the operator wallet", async () => {
    stubWallets();
    vi.stubEnv("L1_REFERENCE_SCRIPT_SEED_PHRASE", undefined);
    await expect(loadConfig()).rejects.toThrow(
      /L1_REFERENCE_SCRIPT_SEED_PHRASE/,
    );
    vi.stubEnv("L1_REFERENCE_SCRIPT_SEED_PHRASE", "");
    await expect(loadConfig()).rejects.toThrow(
      /L1_REFERENCE_SCRIPT_SEED_PHRASE is required/,
    );
  });

  const roles = Object.keys(SEED_SETTINGS) as Role[];
  const pairs = roles.flatMap((left, index) =>
    roles.slice(index + 1).map((right) => [left, right] as const),
  );
  it.each(pairs)(
    "refuses the %s and %s roles on one wallet",
    async (left, right) => {
      stubWallets({ [right]: SEEDS[left] });
      await expect(loadConfig()).rejects.toThrow(
        new RegExp(
          `${SEED_SETTINGS[left]} and ${SEED_SETTINGS[right]} derive the same wallet`,
        ),
      );
    },
  );

  // The phrases differ as text and derive one key: a phrase-text comparison
  // passes both of these, the derived-key comparison refuses them.
  it("refuses two spellings of one seed phrase", async () => {
    const respaced = operator.replace(" ", " ");
    expect(respaced).not.toBe(operator);
    expect(addressOf(respaced)).toBe(addressOf(operator));
    stubWallets({ reference: respaced });
    // Not the defaulted reference address, so only the pairwise check refuses.
    vi.stubEnv("L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS", SCRIPT_DEPLOY_ADDRESS);
    await expect(loadConfig()).rejects.toThrow(
      /L1_OPERATOR_SEED_PHRASE and L1_REFERENCE_SCRIPT_SEED_PHRASE derive the same wallet/,
    );
    stubWallets({ settlement: `  ${merge}\n` });
    await expect(loadConfig()).rejects.toThrow(
      /L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX and L1_SETTLEMENT_SEED_PHRASE derive the same wallet/,
    );
    // A case change is not a spelling of the same phrase: it does not parse.
    stubWallets({ reference: operator.toUpperCase() });
    await expect(loadConfig()).rejects.toThrow(
      /L1_REFERENCE_SCRIPT_SEED_PHRASE is not a valid wallet seed phrase/,
    );
  });

  it.each(["operator", "merge", "settlement"] as const)(
    "refuses the %s wallet as the reference-script deploy address",
    async (role) => {
      for (const deployAddress of [
        addressOf(SEEDS[role]),
        enterpriseAddressOf(SEEDS[role]),
      ]) {
        stubWallets();
        vi.stubEnv("L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS", deployAddress);
        await expect(loadConfig()).rejects.toThrow(
          new RegExp(
            `L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS is the ${SEED_SETTINGS[role]} wallet`,
          ),
        );
      }
    },
  );

  it("accepts the reference-script wallet or a non-spendable deploy address", async () => {
    for (const deployAddress of [addressOf(reference), SCRIPT_DEPLOY_ADDRESS]) {
      stubWallets();
      vi.stubEnv("L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS", deployAddress);
      await expect(loadConfig()).resolves.toMatchObject({
        L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS: deployAddress,
      });
    }
  });
});

/**
 * Two later checks were deleted as covered by the config refusal: the Lucid
 * layer's operational-wallet overlap check and the public listener's
 * publisher/operator separation check. Both run on a loaded NodeConfig, so a
 * topology config load refuses never reaches them.
 */
describe("config refusal covers the deleted wallet-overlap checks", () => {
  const reachedAfterLoad = async () => {
    let reached = false;
    const exit = await Effect.runPromiseExit(
      Effect.gen(function* () {
        yield* NodeConfig;
        reached = true;
      }).pipe(Effect.provide(NodeConfig.layer)),
    );
    return { reached, failed: exit._tag === "Failure" };
  };

  // The Lucid layer compared the operator-main, operator-merge and
  // reference-scripts wallet addresses.
  it.each([
    ["merge", "operator"],
    ["reference", "operator"],
    ["reference", "merge"],
  ] as const)(
    "refuses the %s role on the %s wallet before the Lucid layer is built",
    async (role, wallet) => {
      stubWallets({ [role]: SEEDS[wallet] });
      expect(await reachedAfterLoad()).toEqual({
        reached: false,
        failed: true,
      });
    },
  );

  // The public listener compared the publisher seed with both operator seeds.
  it.each(["operator", "merge"] as const)(
    "refuses the reference publisher on the %s wallet before the public listener reads the config",
    async (wallet) => {
      stubWallets({ reference: SEEDS[wallet] });
      expect(await reachedAfterLoad()).toEqual({
        reached: false,
        failed: true,
      });
    },
  );
});
