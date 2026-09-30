import { readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import { asLucidSchema } from "@al-ft/midgard-core/lucid-data";
import { STATE_QUEUE_NODE_ASSET_NAME_PREFIX } from "@al-ft/midgard-sdk";
import {
  EMPTY_MERKLE_TREE_ROOT,
  type FaultProofBlueprint,
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
  type FraudProofCatalogueDeploymentInfo,
  parseFaultProofBlueprint,
  REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
  ScriptHashSchema,
} from "@al-ft/midgard-sdk";
import {
  CML,
  Data,
  Emulator,
  Lucid,
  type UTxO,
  validatorToScriptHash,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  resolveFraudulentHeaderHash,
  resolveProverSigner,
} from "../src/index.js";

const seedPhrase =
  "test test test test test test test test test test test junk";

const moduleDir = dirname(fileURLToPath(import.meta.url));

const repoRoot = resolve(moduleDir, "../../..");

const blueprintPath = resolve(repoRoot, "onchain/aiken/plutus.json");

export const h28 = "11".repeat(28);

export const h28b = "22".repeat(28);

export const placeholderInvalidRange = "55".repeat(28);

export const referenceScriptAuthNativeScript = "820500";

export const deploymentManifest = (contracts: Record<string, unknown>) => ({
  referenceScriptAuthPolicy: {
    policyId: validatorToScriptHash({
      type: "Native",
      script: referenceScriptAuthNativeScript,
    }),
    nativeScript: {
      type: "Native",
      cborHex: referenceScriptAuthNativeScript,
      expiresAtSlot: 0,
      expiresAtUnixTime: 0,
      timelockDurationMs: 1,
    },
    tokenNames: REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
    postTimelockAudit: {
      required: true,
      rule: "test fixture",
    },
  },
  contracts,
});

export const readBlueprint = (): FaultProofBlueprint =>
  parseFaultProofBlueprint(
    JSON.parse(readFileSync(blueprintPath, "utf8")) as unknown,
  );

export const filterBlueprint = (
  blueprint: FaultProofBlueprint,
  titles: readonly string[],
): FaultProofBlueprint => {
  const titleSet = new Set(titles);
  return {
    validators: blueprint.validators.filter((validator) =>
      titleSet.has(validator.title),
    ),
  };
};

const encodeCatalogueKey = (id: string): Buffer =>
  Buffer.from(
    Data.to(
      id,
      asLucidSchema(
        Data.Bytes({
          minLength: FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
          maxLength: FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
        }),
      ),
    ),
    "hex",
  );

const encodeCatalogueValue = (scriptHash: string): Buffer =>
  Buffer.from(Data.to(scriptHash, asLucidSchema(ScriptHashSchema)), "hex");

const trieRootHex = (trie: Trie): string =>
  trie.hash == null
    ? EMPTY_MERKLE_TREE_ROOT
    : Buffer.from(trie.hash).toString("hex");

export const catalogueFor = async (
  scriptHashes: Partial<Record<string, string>>,
): Promise<FraudProofCatalogueDeploymentInfo> => {
  const categories = Object.fromEntries(
    FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.map((name, index) => [
      name,
      {
        categoryId: FRAUD_PROOF_CATALOGUE_CATEGORY_IDS[name],
        scriptHash:
          scriptHashes[name] ??
          `${(index + 3).toString(16).padStart(2, "0")}`.repeat(28),
        membershipProofCbor: "",
      },
    ]),
  ) as FraudProofCatalogueDeploymentInfo["categories"];

  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  for (const name of FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER) {
    const category = categories[name];
    await trie.insert(
      encodeCatalogueKey(category.categoryId),
      encodeCatalogueValue(category.scriptHash),
    );
  }
  const withProofs = { ...categories };
  for (const name of FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER) {
    const category = categories[name];
    const proof = await trie.prove(encodeCatalogueKey(category.categoryId));
    withProofs[name] = {
      ...category,
      membershipProofCbor: proof.toCBOR().toString("hex"),
    };
  }

  return {
    root: trieRootHex(trie),
    categories: withProofs,
  };
};

const expectedSeedPaymentKeyHash = (): string =>
  CML.PrivateKey.from_bech32(
    walletFromSeed(seedPhrase, { network: "Preprod" }).paymentKey,
  )
    .to_public()
    .hash()
    .to_hex();

const makeUtxo = (assets: Record<string, bigint>): UTxO =>
  ({
    txHash: "aa".repeat(32),
    outputIndex: 0,
    address: "addr_test1vqz2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3d7t0s7uqthvq7n",
    assets,
  }) as UTxO;

describe("submit-init signer resolution", () => {
  it("uses USER_WALLET as the default seed phrase source like submit-withdrawal", () => {
    const signer = resolveProverSigner(
      { network: "Preprod" },
      { USER_WALLET: seedPhrase },
    );

    expect(signer.source).toBe("USER_WALLET");
    expect(signer.address).toBe(
      walletFromSeed(seedPhrase, {
        addressType: "Enterprise",
        network: "Preprod",
      }).address,
    );
    expect(signer.paymentKeyHash).toBe(expectedSeedPaymentKeyHash());
  });

  it("derives the same enterprise signer identity from a seed and its payment private key", () => {
    const wallet = walletFromSeed(seedPhrase, {
      addressType: "Enterprise",
      network: "Preprod",
    });
    const seedSigner = resolveProverSigner({
      network: "Preprod",
      walletSeedPhrase: seedPhrase,
    });
    const privateKeySigner = resolveProverSigner({
      network: "Preprod",
      walletPrivateKey: wallet.paymentKey,
    });

    expect(seedSigner.address).toBe(wallet.address);
    expect(privateKeySigner.address).toBe(wallet.address);
    expect(seedSigner.paymentKeyHash).toBe(privateKeySigner.paymentKeyHash);
  });

  it("does not silently select a funded base address for an enterprise seed signer", async () => {
    const baseWallet = walletFromSeed(seedPhrase, {
      addressType: "Base",
      network: "Custom",
    });
    const emulator = new Emulator([
      {
        address: baseWallet.address,
        assets: { lovelace: 10_000_000n },
        privateKey: "",
        seedPhrase,
      },
    ]);
    const lucid = await Lucid(emulator, "Custom");
    const signer = resolveProverSigner({
      network: "Custom",
      walletSeedPhrase: seedPhrase,
    });

    signer.selectWallet(lucid);

    expect(signer.address).not.toBe(baseWallet.address);
    expect(await emulator.getUtxos(baseWallet.address)).toHaveLength(1);
    expect(await lucid.wallet().getUtxos()).toHaveLength(0);
  });

  it("uses a direct seed phrase before the configured seed env var", () => {
    const signer = resolveProverSigner(
      {
        network: "Preprod",
        walletSeedPhrase: seedPhrase,
        walletSeedPhraseEnv: "OTHER_WALLET",
      },
      { OTHER_WALLET: "env env env env env env env env env env env env" },
    );

    expect(signer.source).toBe("direct-seed-phrase");
    expect(signer.paymentKeyHash).toBe(expectedSeedPaymentKeyHash());
  });

  it("accepts a direct private key even when USER_WALLET is present", () => {
    const privateKey = CML.PrivateKey.generate_ed25519();
    const signer = resolveProverSigner(
      {
        network: "Preprod",
        walletPrivateKey: privateKey.to_bech32(),
      },
      { USER_WALLET: seedPhrase },
    );

    expect(signer.source).toBe("direct-private-key");
    expect(signer.paymentKeyHash).toBe(privateKey.to_public().hash().to_hex());
    expect(signer.address).toContain("addr_test");
  });

  it("rejects ambiguous direct seed and private-key signer inputs", () => {
    expect(() =>
      resolveProverSigner(
        {
          network: "Preprod",
          walletSeedPhrase: seedPhrase,
          walletPrivateKey: CML.PrivateKey.generate_ed25519().to_bech32(),
        },
        {},
      ),
    ).toThrow("wallet seed phrase or a wallet private key");
  });
});

describe("submit-init state queue header resolution", () => {
  it("derives the fraudulent header hash from the selected state-queue block UTxO", () => {
    const stateQueuePolicyId = "11".repeat(28);
    const headerHash = "22".repeat(28);
    const unit = `${stateQueuePolicyId}${STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${headerHash}`;

    expect(
      resolveFraudulentHeaderHash({
        stateQueuePolicyId,
        fraudulentBlockUtxo: makeUtxo({ lovelace: 5_000_000n, [unit]: 1n }),
      }),
    ).toBe(headerHash);
  });

  it("rejects a configured header hash that does not match the UTxO block token", () => {
    const stateQueuePolicyId = "33".repeat(28);
    const headerHash = "44".repeat(28);
    const unit = `${stateQueuePolicyId}${STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${headerHash}`;

    expect(() =>
      resolveFraudulentHeaderHash({
        stateQueuePolicyId,
        fraudulentBlockUtxo: makeUtxo({ [unit]: 1n }),
        configuredHeaderHash: "55".repeat(28),
      }),
    ).toThrow("--fraudulent-header-hash mismatch");
  });
});
